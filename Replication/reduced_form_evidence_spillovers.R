##### Wesley Janson and Santiago Lacouture
#' Sales Taxes
#' Replication File. Updated on 8/8/2022
#' Step 3: Reduced Form Evidence with spillovers portion of replication

library(data.table)
library(futile.logger)
library(lfe)
library(multcomp)
library(Matrix)
library(zoo)
library(tidyverse)
library(stringr)
library(nloptr)
library(doParallel)
library(MASS)
library(pracma)

setwd("/project2/igaarder")
rm(list = ls())

## input filepath ----------------------------------------------
all_pi_spill <- fread("Data/Replication/all_pi_spill.csv")
all_pi_spill_econ <- fread("Data/Replication/all_pi_spill_econ.csv")

## output filepath ----------------------------------------------
results.file.spillovers <- "Data/Replication/LRdiff_semesterly_spillovers.csv"


### 3. Reduced Form Evidence spillovers -----------------

LRdiff_res <- data.table(NULL)


### Distributed lag model - yearly -----------------


## Collapse to yearly data
yearly_data <- all_pi_spill_econ[, .(ln_cpricei2 = log(mean(exp(ln_cpricei2))), 
                                     ln_quantity3 = log(mean(exp(ln_quantity3))), 
                                     sales = sum(sales), 
                                     ln_home_price = log(mean(exp(ln_home_price))), 
                                     ln_unemp = log(mean(exp(ln_unemp))), 
                                     ln_sales_tax = log(weighted.mean(exp(ln_sales_tax), w = sales)),
                                     taxability = max(taxability, na.rm = T)
), 
by = .(store_code_uc, product_module_code,  fips_state, 
       fips_county, year, module_by_state, store_by_module)]
# rm(all_pi_spill)

# Redefine base.sales
base <- yearly_data[year == 2008, .(base.sales = mean(sales)), by = c("store_code_uc", "product_module_code")]
yearly_data<- merge(yearly_data, base, by = c("store_code_uc", "product_module_code")) 

yearly_data[, store_by_time := .GRP, by = .(store_code_uc, year)]
yearly_data[, module_by_time := .GRP, by = .(product_module_code, year)]
yearly_data[, ln_sales := log(sales)]

## Create census region/division info to recover FEs
geo_dt <- structure(list(
  fips_state = c(1L, 2L, 4L, 5L, 6L, 8L, 9L, 10L, 12L, 13L, 15L, 16L, 17L, 18L,
                 19L, 20L, 21L, 22L, 23L, 24L, 25L, 26L, 27L, 28L, 29L, 30L,
                 31L, 32L, 33L, 34L, 35L, 36L, 37L, 38L, 39L, 40L, 41L, 42L,
                 44L, 45L, 46L, 47L, 48L, 49L, 50L, 51L, 53L, 54L, 55L, 56L),
  region = c(3L, 4L, 4L, 3L, 4L, 4L, 1L, 3L, 3L, 3L, 4L, 4L, 2L, 2L, 2L, 2L, 3L,
             3L, 1L, 3L, 1L, 2L, 2L, 3L, 2L, 4L, 2L, 4L, 1L, 1L, 4L, 1L, 3L, 2L,
             2L, 3L, 4L, 1L, 1L, 3L, 2L, 3L, 3L, 4L, 1L, 3L, 4L, 3L, 2L, 4L),
  division = c(6L, 9L, 8L,  7L, 9L, 8L, 1L, 5L, 5L, 5L, 9L, 8L, 3L, 3L, 4L, 4L,
               6L, 7L, 1L, 5L, 1L, 3L, 4L, 6L, 4L, 8L, 4L, 8L, 1L, 2L, 8L, 2L,
               5L, 4L, 3L,  7L, 9L, 2L, 1L, 5L, 4L, 6L, 7L, 8L, 1L, 5L, 9L, 5L, 3L, 8L)),
  class = "data.frame", row.names = c(NA, -50L))
setDT(geo_dt)


## merge on the census region/division info
yearly_data <- merge(yearly_data, geo_dt, by = "fips_state")

## Define FEs
yearly_data[, region_by_module_by_time := .GRP, by = .(region, product_module_code, year)]
yearly_data[, division_by_module_by_time := .GRP, by = .(division, product_module_code, year)]



# Identify always taxable and always tax-exempt
yearly_data[, tax_exempt := taxability == 0]
yearly_data[, taxable := taxability == 1]
yearly_data[, T_tax_exempt := sum(tax_exempt), by = .(store_by_module)]
yearly_data[, T_taxable := sum(taxable), by = .(store_by_module)]
yearly_data[, T_total := .N, by = .(store_by_module)]
yearly_data[, all_taxable:= ifelse(T_taxable == T_total,1,0)]
yearly_data[, all_taxexempt:= ifelse(T_tax_exempt == T_total,1,0)]

# Identify statutory tax rate
yearly_data[, ln_statutory_tax := max(ln_sales_tax, na.rm = T), by = .(fips_state, fips_county, year)]
yearly_data[, ln_statutory_tax := ifelse(taxability == 1, ln_sales_tax, ln_statutory_tax)]

yearly_data <- yearly_data[order(store_code_uc, product_module_code, year),] ##Sort on store by year-quarter (in ascending order)

# First Differences
yearly_data[, D.ln_statutory_tax := ln_statutory_tax - shift(ln_statutory_tax, n=1, type="lag"),
            by = .(store_code_uc, product_module_code)]
yearly_data[, D.ln_cpricei2 := ln_cpricei2 - shift(ln_cpricei2, n=1, type="lag"),
            by = .(store_code_uc, product_module_code)]
yearly_data[, D.ln_quantity3 := ln_quantity3 - shift(ln_quantity3, n=1, type="lag"),
            by = .(store_code_uc, product_module_code)]
yearly_data[, D.ln_home_price := ln_home_price - shift(ln_home_price, n=1, type="lag"),
            by = .(store_code_uc, product_module_code)] # SAME AS STATE-COUNTY AS PRODUCT MODULE DONT CHANGE AREA
yearly_data[, D.ln_unemp := ln_unemp - shift(ln_unemp, n=1, type="lag"),
            by = .(store_code_uc, product_module_code)]



# Define samples
yearly_data[, all := 1]
yearly_data[(year > 2007 & year < 2015), non_imp_tax := 1]




## Prepare regressions
formula_lags <- paste0("L", 1:2, "D.ln_statutory_tax", collapse = "+")
formula_leads <- paste0("F", 1:2, "D.ln_statutory_tax", collapse = "+")
formula_RHS <- paste0("D.ln_statutory_tax + ", formula_lags, "+", formula_leads)

FE_opts <- c("region_by_module_by_time", "division_by_module_by_time")
outcomes <- c("D.ln_cpricei2", "D.ln_quantity3")
subsamples <- c("all_taxable", "all_taxexempt")
samples <- c("all", "non_imp_tax")

## for linear hypothesis tests
lead.vars <- paste(paste0("F", 2:1, "D.ln_statutory_tax"), collapse = " + ")
lag.vars <- paste(paste0("L", 2:1, "D.ln_statutory_tax"), collapse = " + ")
lead.lp.restr <- paste(lead.vars, "= 0")
lag.lp.restr <- paste(lag.vars, "+ D.ln_statutory_tax = 0")
total.lp.restr <- paste(lag.vars, "+", lead.vars, "+ D.ln_statutory_tax = 0")


## FE vary across subsamples
for (s in samples) {
  for (sam in subsamples) {
    sample <- yearly_data[get(sam) == 1 & get(s) == 1]
    
    # Print basic stats of sample as double check
    print(nrow(sample))
    print(nrow(sample[!is.na(D.ln_statutory_tax) & is.finite(D.ln_statutory_tax) ]))
    print(mean(sample[is.finite(D.ln_statutory_tax)]$D.ln_statutory_tax, na.rm = T))
    
    
    sample <- sample[order(store_code_uc, product_module_code, year),] ##Sort on store by year (in ascending order)
    # Leads and lags of FD
    for (lag.val in 1:2) {
      
      # STATUTORY TAXES
      lag.X <- paste0("L", lag.val, "D.ln_statutory_tax")
      sample[, (lag.X) := shift(D.ln_statutory_tax, n=lag.val, type="lag"),
                  by = .(store_code_uc, product_module_code)]
      lead.X <- paste0("F", lag.val, "D.ln_statutory_tax")
      sample[, (lead.X) := shift(D.ln_statutory_tax, n=lag.val, type="lead"),
                  by = .(store_code_uc, product_module_code)]
      
    }
    # Lags of FD of controls
    for (lag.val in 1:2) {
      
      lag.X <- paste0("L", lag.val, ".D.ln_home_price")
      sample[, (lag.X) := shift(D.ln_home_price, n=lag.val, type="lag"),
             by = .(store_code_uc, product_module_code)]
      
      lag.X <- paste0("L", lag.val, ".D.ln_unemp")
      sample[, (lag.X) := shift(D.ln_unemp, n=lag.val, type="lag"),
             by = .(store_code_uc, product_module_code)]
      
    }    
    
    for (Y in c(outcomes)) {
      for (FE in FE_opts) {
        
        
        for(i in 0:2) {
          
          if (i>0) {
            

            
            # Create list of economic controls  
            lag.home <- paste(paste0("L", i:1, ".D.ln_home_price"), collapse = " + ")
            lag.unemp <- paste(paste0("L", i:1, ".D.ln_unemp"), collapse = " + ")
            lag.econ <- paste(lag.home, lag.unemp, sep = " + ")
            
            
            formula1 <- as.formula(paste0(
              Y, "~", formula_RHS, " + ", lag.econ, "| ", FE, " | 0 | module_by_state"
            ))
            flog.info("Estimating with %s as outcome with %s FE in samples %s and %s. Econ control %s", Y, FE, s, sam, i)
            res1 <- felm(formula = formula1, data = sample,
                         weights = sample$base.sales)
            flog.info("Finished estimating with %s as outcome with %s FE in samples %s and %s. Econ control %s", Y, FE, s, sam, i)
            
            
            
          } else {
            
            formula1 <- as.formula(paste0(
              Y, "~", formula_RHS, "| ", FE, " | 0 | module_by_state"
            ))
            flog.info("Estimating with %s as outcome with %s FE in samples %s and %s.", Y, FE, s, sam)
            res1 <- felm(formula = formula1, data = sample,
                         weights = sample$base.sales)
            flog.info("Finished estimating with %s as outcome with %s FE in samples %s and %s.", Y, FE, s, sam)
        
          }
        
          ## attach results
          flog.info("Writing results...")
          res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
          res1.dt[, outcome := Y]
          res1.dt[, controls := FE]
          res1.dt[, sample := s]
          res1.dt[, subsample := sam]
          res1.dt[, econ := i]
          res1.dt[, spec := "DL year"]
          res1.dt$Rsq <- summary(res1)$r.squared
          res1.dt$adj.Rsq <- summary(res1)$adj.r.squared
          
          res1.dt$N_obs <- nrow(sample)
          res1.dt$N_stores <- uniqueN(sample, by = c("store_code_uc") )
          res1.dt$N_modules <- length(unique(sample$product_module_code))
          res1.dt$N_counties <- uniqueN(sample, by = c("fips_state", "fips_county"))
          res1.dt$N_years <- uniqueN(sample, by = c("year"))
          res1.dt$N_county_modules <- uniqueN(sample, by = c("fips_state", "fips_county",
                                                             "product_module_code"))
          LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
          fwrite(LRdiff_res, results.file.spillovers)
          
          ## sum leads
          flog.info("Summing leads...")
          lead.test <- glht(res1, linfct = lead.lp.restr)
          lead.test.est <- coef(summary(lead.test))[[1]]
          lead.test.se <- sqrt(vcov(summary(lead.test)))[[1]]
          lead.test.pval <- 2*(1 - pnorm(abs(lead.test.est/lead.test.se)))
          
          ## sum lags
          flog.info("Summing lags...")
          lag.test <- glht(res1, linfct = lag.lp.restr)
          lag.test.est <- coef(summary(lag.test))[[1]]
          lag.test.se <- sqrt(vcov(summary(lag.test)))[[1]]
          lag.test.pval <- 2*(1 - pnorm(abs(lag.test.est/lag.test.se)))
          
          ## sum all
          flog.info("Summing all...")
          total.test <- glht(res1, linfct = total.lp.restr)
          total.test.est <- coef(summary(total.test))[[1]]
          total.test.se <- sqrt(vcov(summary(total.test)))[[1]]
          total.test.pval <- 2*(1 - pnorm(abs(total.test.est/total.test.se)))
          
          ## linear hypothesis results
          lp.dt <- data.table(
            rn = c("Pre.D.ln_statutory_tax", "Post.D.ln_statutory_tax", "All.D.ln_statutory_tax"),
            Estimate = c(lead.test.est, lag.test.est, total.test.est),
            `Cluster s.e.` = c(lead.test.se, lag.test.se, total.test.se),
            `Pr(>|t|)` = c(lead.test.pval, lag.test.pval, total.test.pval),
            outcome = Y,
            controls = FE,
            sample = s,
            subsample = sam,
            econ = i,
            spec = "DL year",
            Rsq = summary(res1)$r.squared,
            adj.Rsq = summary(res1)$adj.r.squared
          )
          
          
          ##### Add the cumulative effect at each lead/lag (relative to -1)
          cumul.lead1.est <- 0
          cumul.lead1.se <- NA
          cumul.lead1.pval <- NA
          
          #cumul.lead3.est is just equal to minus the change between -2 and -1
          cumul.lead2.est <- - coef(summary(res1))[ "F1D.ln_statutory_tax", "Estimate"]
          cumul.lead2.se <- coef(summary(res1))[ "F1D.ln_statutory_tax", "Cluster s.e."]
          cumul.lead2.pval <- coef(summary(res1))[ "F1D.ln_statutory_tax", "Pr(>|t|)"]
          
          ##LEADS
          for(j in 3:3) {
            
            ## Create a name for estimate, se and pval of each lead
            cumul.test.est.name <- paste("cumul.lead", j, ".est", sep = "")
            cumul.test.se.name <- paste("cumul.lead", j, ".se", sep = "")
            cumul.test.pval.name <- paste("cumul.lead", j, ".pval", sep = "")
            
            ## Create the formula to compute cumulative estimate at each lead/lag
            cumul.test.form <- paste0("-", paste(paste0("F", (j-1):1, "D.ln_statutory_tax"), collapse = " - "))
            cumul.test.form <- paste(cumul.test.form, " = 0")
            
            ## Compute estimate and store in variables names
            cumul.test <- glht(res1, linfct = cumul.test.form)
            
            assign(cumul.test.est.name, coef(summary(cumul.test))[[1]])
            assign(cumul.test.se.name, sqrt(vcov(summary(cumul.test)))[[1]])
            assign(cumul.test.pval.name, 2*(1 - pnorm(abs(coef(summary(cumul.test))[[1]]/sqrt(vcov(summary(cumul.test)))[[1]]))))
          }
          
          
          ##LAGS
          ## On Impact --> Effect = coefficient on D.ln_sales_tax + F1.D.ln_sales_tax
          cumul.lag0.est <- coef(summary(res1))[ "D.ln_statutory_tax", "Estimate"]
          cumul.lag0.se <- coef(summary(res1))[ "D.ln_statutory_tax", "Cluster s.e."]
          cumul.lag0.pval <- coef(summary(res1))[ "D.ln_statutory_tax", "Pr(>|t|)"]
          
          
          for(j in 1:2) {
            
            ## Create a name for estimate, se and pval of each lead
            cumul.test.est.name <- paste("cumul.lag", j, ".est", sep = "")
            cumul.test.se.name <- paste("cumul.lag", j, ".se", sep = "")
            cumul.test.pval.name <- paste("cumul.lag", j, ".pval", sep = "")
            
            ## Create the formula to compute cumulative estimate at each lead/lag
            cumul.test.form <- paste("D.ln_statutory_tax + ", paste(paste0("L", 1:j, "D.ln_statutory_tax"), collapse = " + "), sep = "")
            cumul.test.form <- paste(cumul.test.form, " = 0")
            
            ## Compute estimate and store in variables names
            cumul.test <- glht(res1, linfct = cumul.test.form)
            
            assign(cumul.test.est.name, coef(summary(cumul.test))[[1]])
            assign(cumul.test.se.name, sqrt(vcov(summary(cumul.test)))[[1]])
            assign(cumul.test.pval.name, 2*(1 - pnorm(abs(coef(summary(cumul.test))[[1]]/sqrt(vcov(summary(cumul.test)))[[1]]))))
          }    
          
          
          ## linear hypothesis results
          lp.dt <- data.table(
            rn = c("cumul.lead3.D.ln_statutory_tax", "cumul.lead2.D.ln_statutory_tax", "cumul.lead1.D.ln_statutory_tax", 
                   "cumul.lag0.D.ln_statutory_tax", "cumul.lag1.D.ln_statutory_tax", "cumul.lag2.D.ln_statutory_tax"),
            Estimate = c(cumul.lead3.est, cumul.lead2.est, cumul.lead1.est, cumul.lag0.est, cumul.lag1.est, cumul.lag2.est),
            `Cluster s.e.` = c(cumul.lead3.se, cumul.lead2.se, cumul.lead1.se, cumul.lag0.se, cumul.lag1.se, cumul.lag2.se),
            `Pr(>|t|)` = c(cumul.lead3.pval, cumul.lead2.pval, cumul.lead1.pval, cumul.lag0.pval, cumul.lag1.pval, cumul.lag2.pval),
            outcome = Y,
            controls = FE,
            sample = s,
            subsample = sam,
            econ = i,
            spec = "DL year",
            Rsq = summary(res1)$r.squared,
            adj.Rsq = summary(res1)$adj.r.squared
          )
          lp.dt$N_obs <- nrow(sample)
          lp.dt$N_stores <- uniqueN(sample, by = c("store_code_uc") )
          lp.dt$N_modules <- length(unique(sample$product_module_code))
          lp.dt$N_counties <- uniqueN(sample, by = c("fips_state", "fips_county"))
          lp.dt$N_years <- uniqueN(sample, by = c("year"))
          lp.dt$N_county_modules <- uniqueN(sample, by = c("fips_state", "fips_county",
                                                           "product_module_code"))
          
          LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T)
          fwrite(LRdiff_res, results.file.spillovers)
        }
      }
    }
  }
}

rm(yearly_data)
####### TWFE specification ------

## De-mean statutory tax rate
all_pi_spill[, w.ln_statutory_tax := ln_statutory_tax - mean(ln_statutory_tax, na.rm = T), by = store_by_module]


outcomes <- c("w.ln_cpricei2", "w.ln_quantity3", "w.ln_pricei2", "w.ln_sales")
FE_opts <- c("region_by_module_by_time", "division_by_module_by_time")

# Define samples
subsamples <- c("all_taxable", "all_taxexempt")
samples <- c("all", "non_imp_tax")


## Run
for (s in samples) {
  
  for (sam in subsamples) {
    all_pi_spill[, sample := get(sam)]
    data.est <- all_pi_spill[sample == 1 & get(s) == 1]
    
    for (Y in c(outcomes)) {
      for (FE in FE_opts) {
        
        formula1 <- as.formula(paste0(
          Y, "~ w.ln_statutory_tax | ", FE, " | 0 | module_by_state"
        ))
        flog.info("Estimating with %s as outcome with %s FE in sample %s.", Y, FE, s)
        res1 <- felm(formula = formula1, data = data.est,
                     weights = data.est$base.sales)
        flog.info("Finished estimating with %s as outcome with %s FE in sample %s.", Y, FE, s)
        
        
        ## attach results
        flog.info("Writing results...")
        res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
        res1.dt[, outcome := Y]
        res1.dt[, controls := FE]
        res1.dt[, subsample := sam]
        res1.dt[, sample := s]
        res1.dt[, spec := "TWFE"]
        # Add summary values
        res1.dt[, Rsq := summary(res1)$r.squared]
        res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
        res1.dt[, N_obs := nrow(data.est)]
        res1.dt[, N_modules := length(unique(data.est$product_module_code))]
        res1.dt[, N_stores :=  length(unique(data.est$store_code_uc))]
        res1.dt[, N_counties := uniqueN(data.est, by = c("fips_state", "fips_county"))]
        res1.dt[, N_years := uniqueN(data.est, by = c("year"))]
        res1.dt[, N_county_modules := uniqueN(data.est, by = c("fips_state", "fips_county",
                                                               "product_module_code"))]
        LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
        fwrite(LRdiff_res, results.file.spillovers)
        
      }
    }
  }
}



### Distributed lag model - semester -----------------
formula_lags <- paste0("L", 1:4, "D.ln_statutory_tax", collapse = "+")
formula_leads <- paste0("F", 1:4, "D.ln_statutory_tax", collapse = "+")
formula_RHS <- paste0("D.ln_statutory_tax + ", formula_lags, "+", formula_leads)

FE_opts <- c("region_by_module_by_time", "division_by_module_by_time")
outcomes <- c("D.ln_cpricei2", "D.ln_quantity3")
subsamples <- c("all_taxable", "all_taxexempt")
samples <- c("all", "non_imp_tax")

## for linear hypothesis tests
lead.vars <- paste(paste0("F", 4:1, "D.ln_statutory_tax"), collapse = " + ")
lag.vars <- paste(paste0("L", 4:1, "D.ln_statutory_tax"), collapse = " + ")
lead.lp.restr <- paste(lead.vars, "= 0")
lag.lp.restr <- paste(lag.vars, "+ D.ln_statutory_tax = 0")
total.lp.restr <- paste(lag.vars, "+", lead.vars, "+ D.ln_statutory_tax = 0")
## FE vary across subsamples
for (s in samples) {
  for (sam in subsamples) {

    # Print basic stats of sample as double check
    print(nrow(all_pi_spill[get(sam) == 1 & get(s) == 1]))
    print(nrow(all_pi_spill[get(sam) == 1 & get(s) == 1 & 
                              !is.na(D.ln_statutory_tax) & is.finite(D.ln_statutory_tax) ]))
    print(mean(all_pi_spill[get(sam) == 1 & get(s) == 1 & 
                              is.finite(D.ln_statutory_tax)]$D.ln_statutory_tax, na.rm = T))
    
    for (Y in c(outcomes)) {
      for (FE in FE_opts) {
        
        for(i in 0:4) {
          
          if (i>0) {
            
            sample <- all_pi_spill_econ[get(sam) == 1 & get(s) == 1]
            # Create list of economic controls  
            lag.home <- paste(paste0("L", i:1, ".D.ln_home_price"), collapse = " + ")
            lag.unemp <- paste(paste0("L", i:1, ".D.ln_unemp"), collapse = " + ")
            lag.econ <- paste(lag.home, lag.unemp, sep = " + ")
            
            
            formula1 <- as.formula(paste0(
              Y, "~", formula_RHS, " + ", lag.econ, "| ", FE, " | 0 | module_by_state"
            ))
            flog.info("Estimating with %s as outcome with %s FE in samples %s and %s. Econ control %s", Y, FE, s, sam, i)
            res1 <- felm(formula = formula1, data = sample,
                         weights = sample$base.sales)
            flog.info("Finished estimating with %s as outcome with %s FE in samples %s and %s.", Y, FE, s, sam)
            
            
            
          } else {
            sample <- all_pi_spill[get(sam) == 1 & get(s) == 1]
            
            formula1 <- as.formula(paste0(
              Y, "~", formula_RHS, "| ", FE, " | 0 | module_by_state"
            ))
            flog.info("Estimating with %s as outcome with %s FE in samples %s and %s.", Y, FE, s, sam)
            res1 <- felm(formula = formula1, data = sample,
                         weights = sample$base.sales)
            flog.info("Finished estimating with %s as outcome with %s FE in samples %s and %s.", Y, FE, s, sam)
            
            
          }
          
          
          ## attach results
          flog.info("Writing results...")
          res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
          res1.dt[, outcome := Y]
          res1.dt[, controls := FE]
          res1.dt[, sample := s]
          res1.dt[, subsample := sam]
          res1.dt[, econ := i]
          res1.dt[, spec := "DL"]
          res1.dt$Rsq <- summary(res1)$r.squared
          res1.dt$adj.Rsq <- summary(res1)$adj.r.squared
          
          res1.dt$N_obs <- nrow(sample)
          res1.dt$N_stores <- uniqueN(sample, by = c("store_code_uc") )
          res1.dt$N_modules <- length(unique(sample$product_module_code))
          res1.dt$N_counties <- uniqueN(sample, by = c("fips_state", "fips_county"))
          res1.dt$N_years <- uniqueN(sample, by = c("year"))
          res1.dt$N_county_modules <- uniqueN(sample, by = c("fips_state", "fips_county",
                                                             "product_module_code"))
          LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
          fwrite(LRdiff_res, results.file.spillovers)
          
          ## sum leads
          flog.info("Summing leads...")
          lead.test <- glht(res1, linfct = lead.lp.restr)
          lead.test.est <- coef(summary(lead.test))[[1]]
          lead.test.se <- sqrt(vcov(summary(lead.test)))[[1]]
          lead.test.pval <- 2*(1 - pnorm(abs(lead.test.est/lead.test.se)))
          
          ## sum lags
          flog.info("Summing lags...")
          lag.test <- glht(res1, linfct = lag.lp.restr)
          lag.test.est <- coef(summary(lag.test))[[1]]
          lag.test.se <- sqrt(vcov(summary(lag.test)))[[1]]
          lag.test.pval <- 2*(1 - pnorm(abs(lag.test.est/lag.test.se)))
          
          ## sum all
          flog.info("Summing all...")
          total.test <- glht(res1, linfct = total.lp.restr)
          total.test.est <- coef(summary(total.test))[[1]]
          total.test.se <- sqrt(vcov(summary(total.test)))[[1]]
          total.test.pval <- 2*(1 - pnorm(abs(total.test.est/total.test.se)))
          
          ## linear hypothesis results
          lp.dt <- data.table(
            rn = c("Pre.D.ln_statutory_tax", "Post.D.ln_statutory_tax", "All.D.ln_statutory_tax"),
            Estimate = c(lead.test.est, lag.test.est, total.test.est),
            `Cluster s.e.` = c(lead.test.se, lag.test.se, total.test.se),
            `Pr(>|t|)` = c(lead.test.pval, lag.test.pval, total.test.pval),
            outcome = Y,
            controls = FE,
            sample = s,
            subsample = sam,
            econ = i,
            spec = "DL",
            Rsq = summary(res1)$r.squared,
            adj.Rsq = summary(res1)$adj.r.squared
          )
          
          
          ##### Add the cumulative effect at each lead/lag (relative to -1)
          cumul.lead1.est <- 0
          cumul.lead1.se <- NA
          cumul.lead1.pval <- NA
          
          #cumul.lead3.est is just equal to minus the change between -2 and -1
          cumul.lead2.est <- - coef(summary(res1))[ "F1D.ln_statutory_tax", "Estimate"]
          cumul.lead2.se <- coef(summary(res1))[ "F1D.ln_statutory_tax", "Cluster s.e."]
          cumul.lead2.pval <- coef(summary(res1))[ "F1D.ln_statutory_tax", "Pr(>|t|)"]
          
          ##LEADS
          for(j in 3:5) {
            
            ## Create a name for estimate, se and pval of each lead
            cumul.test.est.name <- paste("cumul.lead", j, ".est", sep = "")
            cumul.test.se.name <- paste("cumul.lead", j, ".se", sep = "")
            cumul.test.pval.name <- paste("cumul.lead", j, ".pval", sep = "")
            
            ## Create the formula to compute cumulative estimate at each lead/lag
            cumul.test.form <- paste0("-", paste(paste0("F", (j-1):1, "D.ln_statutory_tax"), collapse = " - "))
            cumul.test.form <- paste(cumul.test.form, " = 0")
            
            ## Compute estimate and store in variables names
            cumul.test <- glht(res1, linfct = cumul.test.form)
            
            assign(cumul.test.est.name, coef(summary(cumul.test))[[1]])
            assign(cumul.test.se.name, sqrt(vcov(summary(cumul.test)))[[1]])
            assign(cumul.test.pval.name, 2*(1 - pnorm(abs(coef(summary(cumul.test))[[1]]/sqrt(vcov(summary(cumul.test)))[[1]]))))
          }
          
          
          ##LAGS
          ## On Impact --> Effect = coefficient on D.ln_sales_tax + F1.D.ln_sales_tax
          cumul.lag0.est <- coef(summary(res1))[ "D.ln_statutory_tax", "Estimate"]
          cumul.lag0.se <- coef(summary(res1))[ "D.ln_statutory_tax", "Cluster s.e."]
          cumul.lag0.pval <- coef(summary(res1))[ "D.ln_statutory_tax", "Pr(>|t|)"]
          
          
          for(j in 1:4) {
            
            ## Create a name for estimate, se and pval of each lead
            cumul.test.est.name <- paste("cumul.lag", j, ".est", sep = "")
            cumul.test.se.name <- paste("cumul.lag", j, ".se", sep = "")
            cumul.test.pval.name <- paste("cumul.lag", j, ".pval", sep = "")
            
            ## Create the formula to compute cumulative estimate at each lead/lag
            cumul.test.form <- paste("D.ln_statutory_tax + ", paste(paste0("L", 1:j, "D.ln_statutory_tax"), collapse = " + "), sep = "")
            cumul.test.form <- paste(cumul.test.form, " = 0")
            
            ## Compute estimate and store in variables names
            cumul.test <- glht(res1, linfct = cumul.test.form)
            
            assign(cumul.test.est.name, coef(summary(cumul.test))[[1]])
            assign(cumul.test.se.name, sqrt(vcov(summary(cumul.test)))[[1]])
            assign(cumul.test.pval.name, 2*(1 - pnorm(abs(coef(summary(cumul.test))[[1]]/sqrt(vcov(summary(cumul.test)))[[1]]))))
          }    
          
          
          ## linear hypothesis results
          lp.dt <- data.table(
            rn = c("cumul.lead5.D.ln_statutory_tax", "cumul.lead4.D.ln_statutory_tax", "cumul.lead3.D.ln_statutory_tax", "cumul.lead2.D.ln_statutory_tax", 
                   "cumul.lead1.D.ln_statutory_tax", "cumul.lag0.D.ln_statutory_tax", "cumul.lag1.D.ln_statutory_tax", "cumul.lag2.D.ln_statutory_tax", "cumul.lag3.D.ln_statutory_tax", "cumul.lag4.D.ln_statutory_tax"),
            Estimate = c(cumul.lead5.est, cumul.lead4.est, cumul.lead3.est, cumul.lead2.est, cumul.lead1.est, cumul.lag0.est, cumul.lag1.est, cumul.lag2.est, cumul.lag3.est, cumul.lag4.est),
            `Cluster s.e.` = c(cumul.lead5.se, cumul.lead4.se, cumul.lead3.se, cumul.lead2.se, cumul.lead1.se, cumul.lag0.se, cumul.lag1.se, cumul.lag2.se, cumul.lag3.se, cumul.lag4.se),
            `Pr(>|t|)` = c(cumul.lead5.pval, cumul.lead4.pval, cumul.lead3.pval, cumul.lead2.pval, cumul.lead1.pval, cumul.lag0.pval, cumul.lag1.pval, cumul.lag2.pval, cumul.lag3.pval, cumul.lag4.pval),
            outcome = Y,
            controls = FE,
            sample = s,
            subsample = sam,
            econ = i,
            spec = "DL",
            Rsq = summary(res1)$r.squared,
            adj.Rsq = summary(res1)$adj.r.squared
          )
          lp.dt$N_obs <- nrow(sample)
          lp.dt$N_stores <- uniqueN(sample, by = c("store_code_uc") )
          lp.dt$N_modules <- length(unique(sample$product_module_code))
          lp.dt$N_counties <- uniqueN(sample, by = c("fips_state", "fips_county"))
          lp.dt$N_years <- uniqueN(sample, by = c("year"))
          lp.dt$N_county_modules <- uniqueN(sample, by = c("fips_state", "fips_county",
                                                           "product_module_code"))
          
          LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T)
          fwrite(LRdiff_res, results.file.spillovers)
          
          
          
        }
      }
    }
  }
  
}






