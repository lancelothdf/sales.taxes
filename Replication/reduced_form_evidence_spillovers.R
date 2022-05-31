##### Wesley Janson
#' Sales Taxes
#' Replication File. Updated on 5/21/2022
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
results.file.spillovers <- "Data/Replication/DiD_spillover_estimates_csinitprice_semester.csv"


### 3. Reduced Form Evidence spillovers -----------------
formula_lags <- paste0("L", 1:4, ".D.ln_statutory_tax", collapse = "+")
formula_leads <- paste0("F", 1:4, ".D.ln_statutory_tax", collapse = "+")
formula_RHS <- paste0("D.ln_statutory_tax + ", formula_lags, "+", formula_leads)

FE_opts <- c("region_by_module_by_time", "division_by_module_by_time")
outcomes <- c("D.ln_cpricei2", "D.ln_quantity3")
samples <- c("all_taxable", "all_taxexempt")

all_pi_spill_econ[, tax_exempt := taxability == 0]
all_pi_spill_econ[, taxable := taxability == 1]
all_pi_spill_econ[, T_tax_exempt := sum(tax_exempt), by = .(store_by_module)]
all_pi_spill_econ[, T_taxable := sum(taxable), by = .(store_by_module)]
all_pi_spill_econ[, T_total := .N, by = .(store_by_module)]
all_pi_spill_econ[, all_taxable:= ifelse(T_taxable == T_total,1,0)]
all_pi_spill_econ[, all_taxexempt:= ifelse(T_tax_exempt == T_total,1,0)]

all_pi_spill_econ[, ln_statutory_tax := max(ln_sales_tax, na.rm = T), by = .(fips_state, fips_county, year, semester)]
all_pi_spill_econ[, ln_statutory_tax := ifelse(taxability == 1, ln_sales_tax, ln_statutory_tax)]

# Create statutory leads and lags
LLs <- c(paste0("L", 1:4, ".D"), paste0("F", 1:4, ".D"), "D")
for (Td in LLs) {
  
  actual <- paste0(Td, ".ln_sales_tax")
  statu <- paste0(Td, ".ln_statutory_tax")
  
  all_pi_spill[, (statu) := max(get(actual), na.rm = T), by = .(fips_state, fips_county, year, semester)]
  all_pi_spill[, (statu) := ifelse(taxability == 1, get(actual), get(statu))]
  
  all_pi_spill_econ[, (statu) := max(get(actual), na.rm = T), by = .(fips_state, fips_county, year, semester)]
  all_pi_spill_econ[, (statu) := ifelse(taxability == 1, get(actual), get(statu))]
  
}

## for linear hypothesis tests
lead.vars <- paste(paste0("F", 4:1, ".D.ln_statutory_tax"), collapse = " + ")
lag.vars <- paste(paste0("L", 4:1, ".D.ln_statutory_tax"), collapse = " + ")
lead.lp.restr <- paste(lead.vars, "= 0")
lag.lp.restr <- paste(lag.vars, "+ D.ln_statutory_tax = 0")
total.lp.restr <- paste(lag.vars, "+", lead.vars, "+ D.ln_statutory_tax = 0")


LRdiff_res <- data.table(NULL)
## FE vary across samples
for (sam in samples) {
  all_pi_spill[, sample := get(sam)]
  all_pi_spill_econ[, sample := get(sam)]
  for (Y in c(outcomes)) {
    for (FE in FE_opts) {
      
      for(i in 0:4) {
        
        if (i>0) {
          
          sample <- all_pi_spill_econ[sample == 1]
          # Create list of economic controls  
          lag.home <- paste(paste0("L", i:1, ".D.ln_home_price"), collapse = " + ")
          lag.unemp <- paste(paste0("L", i:1, ".D.ln_unemp"), collapse = " + ")
          lag.econ <- paste(lag.home, lag.unemp, sep = " + ")
          
          
          formula1 <- as.formula(paste0(
            Y, "~", formula_RHS, " + ", lag.econ, "| ", FE, " | 0 | module_by_state"
          ))
          flog.info("Estimating with %s as outcome with %s FE.", Y, FE)
          res1 <- felm(formula = formula1, data = sample,
                       weights = sample$base.sales)
          flog.info("Finished estimating with %s as outcome with %s FE.", Y, FE)
          
          
          
        } else {
          
          sample <- all_pi_spill[sample == 1]
          formula1 <- as.formula(paste0(
            Y, "~", formula_RHS, "| ", FE, " | 0 | module_by_state"
          ))
          flog.info("Estimating with %s as outcome with %s FE.", Y, FE)
          res1 <- felm(formula = formula1, data = sample,
                       weights = sample$base.sales)
          flog.info("Finished estimating with %s as outcome with %s FE.", Y, FE)
          
          
        }
        
        
        ## attach results
        flog.info("Writing results...")
        res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
        res1.dt[, outcome := Y]
        res1.dt[, controls := FE]
        res1.dt[, sample := sam]
        res1.dt[, econ := i]
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
          sample = sam,
          econ = i,
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
        
        
        ##### Add the cumulative effect at each lead/lag (relative to -2)
        cumul.lead2.est <- 0
        cumul.lead2.se <- NA
        cumul.lead2.pval <- NA
        
        #cumul.lead3.est is just equal to minus the change between -3 and -2
        cumul.lead3.est <- - coef(summary(res1))[ "F2.D.ln_statutory_tax", "Estimate"]
        cumul.lead3.se <- coef(summary(res1))[ "F2.D.ln_statutory_tax", "Cluster s.e."]
        cumul.lead3.pval <- coef(summary(res1))[ "F2.D.ln_statutory_tax", "Pr(>|t|)"]
        
        ##LEADS
        for(j in 4:5) {
          
          ## Create a name for estimate, se and pval of each lead
          cumul.test.est.name <- paste("cumul.lead", j, ".est", sep = "")
          cumul.test.se.name <- paste("cumul.lead", j, ".se", sep = "")
          cumul.test.pval.name <- paste("cumul.lead", j, ".pval", sep = "")
          
          ## Create the formula to compute cumulative estimate at each lead/lag
          cumul.test.form <- paste0("-", paste(paste0("F", (j-1):2, ".D.ln_statutory_tax"), collapse = " - "))
          cumul.test.form <- paste(cumul.test.form, " = 0")
          
          ## Compute estimate and store in variables names
          cumul.test <- glht(res1, linfct = cumul.test.form)
          
          assign(cumul.test.est.name, coef(summary(cumul.test))[[1]])
          assign(cumul.test.se.name, sqrt(vcov(summary(cumul.test)))[[1]])
          assign(cumul.test.pval.name, 2*(1 - pnorm(abs(coef(summary(cumul.test))[[1]]/sqrt(vcov(summary(cumul.test)))[[1]]))))
        }
        
        
        ##First lead
        ## First lead --> Effect = coefficient on F1.D.ln_statutory_tax
        cumul.lead1.est <- coef(summary(res1))[ "F1.D.ln_statutory_tax", "Estimate"]
        cumul.lead1.se <- coef(summary(res1))[ "F1.D.ln_statutory_tax", "Cluster s.e."]
        cumul.lead1.pval <- coef(summary(res1))[ "F1.D.ln_statutory_tax", "Pr(>|t|)"]
        
        ##LAGS
        ## On Impact --> Effect = coefficient on D.ln_statutory_tax + F1.D.ln_statutory_tax
        cumul.test.form <- "F1.D.ln_statutory_tax + D.ln_statutory_tax = 0"
        
        ## Compute estimate and store in variables names
        cumul.test <- glht(res1, linfct = cumul.test.form)
        cumul.lag0.est <- coef(summary(cumul.test))[[1]]
        cumul.lag0.se <- sqrt(vcov(summary(cumul.test)))[[1]]
        cumul.lag0.pval <- 2*(1 - pnorm(abs(coef(summary(cumul.test))[[1]]/sqrt(vcov(summary(cumul.test)))[[1]])))
        
        for(j in 1:4) {
          
          ## Create a name for estimate, se and pval of each lead
          cumul.test.est.name <- paste("cumul.lag", j, ".est", sep = "")
          cumul.test.se.name <- paste("cumul.lag", j, ".se", sep = "")
          cumul.test.pval.name <- paste("cumul.lag", j, ".pval", sep = "")
          
          ## Create the formula to compute cumulative estimate at each lead/lag
          cumul.test.form <- paste("F1.D.ln_statutory_tax + D.ln_statutory_tax + ", paste(paste0("L", 1:j, ".D.ln_statutory_tax"), collapse = " + "), sep = "")
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
          sample = sam,
          econ = i,
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
