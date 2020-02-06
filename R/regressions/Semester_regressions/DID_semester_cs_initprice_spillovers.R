#' Sales Taxes Project
#' Robustness Check Spillovers:
#' Check estimates of statutory sales tax at the county level for tax-exempt items. Also estimate main spec on taxable items only
#' We estimate the LR DiD and DLM.


library(data.table)
library(futile.logger)
library(lfe)
library(multcomp)

setwd("/project2/igaarder")


## input filepaths -----------------------------------------------
#' This data is the same as all_goods_pi_path, except it has 2015-2016 data as well.
data.semester <- "Data/Nielsen/semester_nielsen_data.csv"
data.taxes <- "Data/county_monthly_tax_rates_2008_2014.csv"


## output filepaths ----------------------------------------------
results.file <- "Data/DiD_spillover_estimates_csinitprice_semester.csv"

## Open all data and compute statutory tax rate -----
all_pi <- fread(data.semester)
all_pi[, ln_statutory_tax := max(ln_sales_tax, na.rm = T), by = .(fips_state, fips_county, year, semester)]

### Set up Semester Data ---------------------------------
all_pi[, w.ln_sales_tax := ln_sales_tax - mean(ln_sales_tax), by = .(store_by_module)]
all_pi[, w.ln_statutory_tax := ln_statutory_tax - mean(ln_statutory_tax), by = .(store_by_module)]
all_pi[, w.ln_cpricei2 := ln_cpricei2 - mean(ln_cpricei2), by = .(store_by_module)]
all_pi[, w.ln_quantity3 := ln_quantity3 - mean(ln_quantity3), by = .(store_by_module)]

# Try double demeaning
all_pi[, mi.ln_statutory_tax := mean(ln_sales_tax), by = .(store_by_module)]
all_pi[, mi.ln_cpricei2 := mean(ln_cpricei2), by = .(store_by_module)]
all_pi[, mi.ln_quantity3 := mean(ln_quantity3), by = .(store_by_module)]
all_pi[, mfe.ln_statutory_tax := weighted.mean(ln_sales_tax, w = base.sales), by = .(division_by_module_by_time)]
all_pi[, mfe.ln_cpricei2 := weighted.mean(ln_cpricei2, w = base.sales), by = .(division_by_module_by_time)]
all_pi[, mfe.ln_quantity3 := weighted.mean(ln_quantity3, w = base.sales), by = .(division_by_module_by_time)]
all_pi[, dd.ln_statutory_tax := ln_statutory_tax - mi.ln_statutory_tax - mfe.ln_statutory_tax + weighted.mean(ln_statutory_tax, w = base.sales)]
all_pi[, dd.ln_cpricei2 := ln_cpricei2 - mi.ln_cpricei2 - mfe.ln_cpricei2 + weighted.mean(ln_cpricei2, w = base.sales)]
all_pi[, dd.ln_quantity3 := ln_quantity3 - mi.ln_quantity3 - mfe.ln_quantity3 + weighted.mean(ln_quantity3, w = base.sales)]


## Create statutory leads and lags
LLs <- c(paste0("L", 1:4, ".D"), paste0("F", 1:4, ".D"), "D")
for (Td in LLs) {
  
  sales <- paste0(Td, ".ln_sales_tax")
  statu <- paste0(Td, ".ln_statutory_tax")
  
  all_pi[, (statu) := max(get(sales), na.rm = T), by = .(fips_state, fips_county, year, semester)]
  
}


## De-mean First Differences and Leads and Lags (the same as residualizing FE)
FDs <- c(paste0("L", 1:4, ".D.ln_statutory_tax"), paste0("F", 1:4, ".D.ln_statutory_tax"), "D.ln_statutory_tax", "D.ln_quantity3","D.ln_cpricei2")
for (var in FDs) {
  
  name <- paste0("d.", var)
  all_pi[, (name) := get(var) - weighted.mean(get(var), w = base.sales), by = .(division_by_module_by_time)]
  
}


# Need to demean
all_pi[, module_by_time := .GRP, by = .(product_module_code, semester, year)]
all_pi[, L.ln_cpricei2 := ln_cpricei2 - D.ln_cpricei2]
all_pi[, dm.L.ln_cpricei2 := L.ln_cpricei2 - mean(L.ln_cpricei2, na.rm = T), by = module_by_time]
all_pi[, dm.ln_cpricei2 := ln_cpricei2 - mean(ln_cpricei2, na.rm = T), by = module_by_time]
all_pi[, dm.ln_quantity3 := ln_quantity3 - mean(ln_quantity3, na.rm = T), by = module_by_time]


# Defining common support
control <- all_pi[D.ln_sales_tax == 0,]
treated <- all_pi[D.ln_sales_tax != 0,]

# Price 
pct1.control <- quantile(control$dm.L.ln_cpricei2, probs = 0.01, na.rm = T, weight=control$base.sales)
pct1.treated <- quantile(treated$dm.L.ln_cpricei2, probs = 0.01, na.rm = T, weight=treated$base.sales)

pct99.control <- quantile(control$dm.L.ln_cpricei2, probs = 0.99, na.rm = T, weight=control$base.sales)
pct99treated <- quantile(treated$dm.L.ln_cpricei2, probs = 0.99, na.rm = T, weight=treated$base.sales)

all_pi[, cs_price := ifelse(dm.L.ln_cpricei2 > max(pct1.treated, pct1.control) & 
                              dm.L.ln_cpricei2 < min(pct99treated, pct99.control), 1, 0)]
# Make sure missings are 0s
all_pi[, cs_price := ifelse(is.na(dm.L.ln_cpricei2), 0, cs_price)]

## Keep within the common support
all_pi <- all_pi[cs_price == 1,]

## cut the tails (keep between 1st and 99th percentile)
pct1 <- quantile(all_pi$dm.ln_cpricei2, probs = 0.01, na.rm = T, weight=base.sales)
pct99 <- quantile(all_pi$dm.ln_cpricei2, probs = 0.99, na.rm = T, weight=base.sales)
all_pi <- all_pi[(dm.ln_cpricei2 > pct1 & dm.ln_cpricei2 < pct99),]


## Divide samples: always tax-exempt, always taxable, change taxability ------
all_pi[, tax_exempt := ln_sales_tax == 0]
all_pi[, T_tax_exempt := sum(tax_exempt), by = .(store_by_module)]
all_pi[, T_taxable := sum(1-tax_exempt), by = .(store_by_module)]
all_pi[, T_total := .N, by = .(store_by_module)]

all_pi[, all_taxable:= ifelse(T_taxable == T_total,1,0)]
all_pi[, all_taxexempt:= ifelse(T_tax_exempt == T_total,1,0)]
all_pi[, change_taxab:= ifelse(T_tax_exempt != T_total & T_taxable != T_total, 1, 0)]


## Run estimations DiD -----------------

FE_opts <- c("region_by_module_by_time", "division_by_module_by_time")
outcomes <- c("w.ln_cpricei2", "w.ln_quantity3")
samples <- c("all_taxable", "all_taxexempt", "change_taxab")


LRdiff_res <- data.table(NULL)
## Loop over the sample we look at
## Estimate RF and FS
for (sam in samples) {
  all_pi[, sample := get(sam)]
  sample <- all_pi[sample == 1]
  for (FE in FE_opts) {
    for (Y in outcomes) {
      formula1 <- as.formula(paste0(
        Y, " ~ w.ln_statutory_tax | ", FE, "| 0 | module_by_state"
      ))
      res1 <- felm(formula = formula1, data = sample,
                   weights = sample$base.sales)
      
      
      ## attach results
      res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
      res1.dt[, outcome := Y]
      res1.dt[, controls := FE]
      res1.dt[, sample := sam]
      res1.dt[, spec := "DiD FE vary"]
      
      ## Descriptives
      res1.dt$N_obs <- nrow(sample)
      res1.dt$N_stores <- uniqueN(sample, by = c("store_code_uc") )
      res1.dt$N_modules <- length(unique(sample$product_module_code))
      res1.dt$N_counties <- uniqueN(sample, by = c("fips_state", "fips_county"))
      res1.dt$N_years <- uniqueN(sample, by = c("year"))
      res1.dt$N_county_modules <- uniqueN(sample, by = c("fips_state", "fips_county",
                                                         "product_module_code"))
      LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
      fwrite(LRdiff_res, results.file)
    }
  }
}


## Run double demeaned estimations
outcomes.dd <- c("dd.ln_cpricei2", "dd.ln_quantity3")

for (sam in samples) {
  all_pi[, sample := get(sam)]
  sample <- all_pi[sample == 1]
  for (Y in outcomes.dd) {
    formula1 <- as.formula(paste0(
      Y, " ~ dd.ln_statutory_tax -1| 0 | 0 | module_by_state"
    ))
    res1 <- felm(formula = formula1, data = sample, 
                 weights = sample$base.sales)
    
    
    ## attach results
    res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
    res1.dt[, outcome := Y]
    res1.dt[, controls := "division_by_module_by_time"]
    res1.dt[, sample := sam]
    res1.dt[, spec := "DiD FE cons"]
    
    ## Descriptives
    res1.dt$N_obs <- nrow(sample)
    res1.dt$N_stores <- uniqueN(sample, by = c("store_code_uc") )
    res1.dt$N_modules <- length(unique(sample$product_module_code))
    res1.dt$N_counties <- uniqueN(sample, by = c("fips_state", "fips_county"))
    res1.dt$N_years <- uniqueN(sample, by = c("year"))
    res1.dt$N_county_modules <- uniqueN(sample, by = c("fips_state", "fips_county",
                                                       "product_module_code"))
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, results.file)
  }
}


## Run Distributed Lag Model estimations  --------------

formula_lags <- paste0("L", 1:4, ".D.ln_statutory_tax", collapse = "+")
formula_leads <- paste0("F", 1:4, ".D.ln_statutory_tax", collapse = "+")
formula_RHS <- paste0("D.ln_statutory_tax + ", formula_lags, "+", formula_leads)

outcomes <- c("D.ln_cpricei2",  "D.ln_quantity3")
FE_opts <- c("region_by_module_by_time", "division_by_module_by_time")


## for linear hypothesis tests
lead.vars <- paste(paste0("F", 4:1, ".D.ln_statutory_tax"), collapse = " + ")
lag.vars <- paste(paste0("L", 4:1, ".D.ln_statutory_tax"), collapse = " + ")
lead.lp.restr <- paste(lead.vars, "= 0")
lag.lp.restr <- paste(lag.vars, "+ D.ln_statutory_tax = 0")
total.lp.restr <- paste(lag.vars, "+", lead.vars, "+ D.ln_statutory_tax = 0")


## FE vary across samples
for (sam in samples) {
  all_pi[, sample := get(sam)]
  sample <- all_pi[sample == 1]
  for (Y in c(outcomes)) {
    for (FE in FE_opts) {
      
      formula1 <- as.formula(paste0(
        Y, "~", formula_RHS, "| ", FE, " | 0 | module_by_state"
      ))
      flog.info("Estimating with %s as outcome with %s FE.", Y, FE)
      res1 <- felm(formula = formula1, data = sample,
                   weights = sample$base.sales)
      flog.info("Finished estimating with %s as outcome with %s FE.", Y, FE)
      
      
      ## attach results
      flog.info("Writing results...")
      res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
      res1.dt[, outcome := Y]
      res1.dt[, controls := FE]
      res1.dt[, sample := sam]
      res1.dt[, spec := "DLM FE vary"]
      res1.dt$N_obs <- nrow(sample)
      res1.dt$N_stores <- uniqueN(sample, by = c("store_code_uc") )
      res1.dt$N_modules <- length(unique(sample$product_module_code))
      res1.dt$N_counties <- uniqueN(sample, by = c("fips_state", "fips_county"))
      res1.dt$N_years <- uniqueN(sample, by = c("year"))
      res1.dt$N_county_modules <- uniqueN(sample, by = c("fips_state", "fips_county",
                                                         "product_module_code"))
      LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
      fwrite(LRdiff_res, results.file)
      
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
        spec = "DLM FE vary")
      lp.dt$N_obs <- nrow(sample)
      lp.dt$N_stores <- uniqueN(sample, by = c("store_code_uc") )
      lp.dt$N_modules <- length(unique(sample$product_module_code))
      lp.dt$N_counties <- uniqueN(sample, by = c("fips_state", "fips_county"))
      lp.dt$N_years <- uniqueN(sample, by = c("year"))
      lp.dt$N_county_modules <- uniqueN(sample, by = c("fips_state", "fips_county",
                                                         "product_module_code"))
      
      
      LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T)
      fwrite(LRdiff_res, results.file)
      
      
      ##### Add the cumulative effect at each lead/lag (relative to -1)
      cumul.lead1.est <- 0
      cumul.lead1.se <- NA
      cumul.lead1.pval <- NA
      
      #cumul.lead2.est is just equal to minus the change between -2 and -1
      cumul.lead2.est <- - coef(summary(res1))[ "F1.D.ln_statutory_tax", "Estimate"]
      cumul.lead2.se <- coef(summary(res1))[ "F1.D.ln_statutory_tax", "Cluster s.e."]
      cumul.lead2.pval <- coef(summary(res1))[ "F1.D.ln_statutory_tax", "Pr(>|t|)"]
      
      ##LEADS
      for(j in 3:5) {
        
        ## Create a name for estimate, se and pval of each lead
        cumul.test.est.name <- paste("cumul.lead", j, ".est", sep = "")
        cumul.test.se.name <- paste("cumul.lead", j, ".se", sep = "")
        cumul.test.pval.name <- paste("cumul.lead", j, ".pval", sep = "")
        
        ## Create the formula to compute cumulative estimate at each lead/lag
        cumul.test.form <- paste0("-", paste(paste0("F", (j-1):1, ".D.ln_statutory_tax"), collapse = " - "))
        cumul.test.form <- paste(cumul.test.form, " = 0")
        
        ## Compute estimate and store in variables names
        cumul.test <- glht(res1, linfct = cumul.test.form)
        
        assign(cumul.test.est.name, coef(summary(cumul.test))[[1]])
        assign(cumul.test.se.name, sqrt(vcov(summary(cumul.test)))[[1]])
        assign(cumul.test.pval.name, 2*(1 - pnorm(abs(coef(summary(cumul.test))[[1]]/sqrt(vcov(summary(cumul.test)))[[1]]))))
      }
      
      
      ##LAGS
      ## On Impact --> Effect = coefficient on D.ln_sales_tax
      cumul.lag0.est <- coef(summary(res1))[ "D.ln_statutory_tax", "Estimate"]
      cumul.lag0.se <- coef(summary(res1))[ "D.ln_statutory_tax", "Cluster s.e."]
      cumul.lag0.pval <- coef(summary(res1))[ "D.ln_statutory_tax", "Pr(>|t|)"]
      
      for(j in 1:4) {
        
        ## Create a name for estimate, se and pval of each lead
        cumul.test.est.name <- paste("cumul.lag", j, ".est", sep = "")
        cumul.test.se.name <- paste("cumul.lag", j, ".se", sep = "")
        cumul.test.pval.name <- paste("cumul.lag", j, ".pval", sep = "")
        
        ## Create the formula to compute cumulative estimate at each lead/lag
        cumul.test.form <- paste("D.ln_statutory_tax + ", paste(paste0("L", 1:j, ".D.ln_statutory_tax"), collapse = " + "), sep = "")
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
        spec = "DLM FE vary")
      lp.dt$N_obs <- nrow(sample)
      lp.dt$N_stores <- uniqueN(sample, by = c("store_code_uc") )
      lp.dt$N_modules <- length(unique(sample$product_module_code))
      lp.dt$N_counties <- uniqueN(sample, by = c("fips_state", "fips_county"))
      lp.dt$N_years <- uniqueN(sample, by = c("year"))
      lp.dt$N_county_modules <- uniqueN(sample, by = c("fips_state", "fips_county",
                                                       "product_module_code"))
      
      LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T)
      fwrite(LRdiff_res, results.file)
      
      
      
    }
  }
}

formula_lags <- paste0("d.L", 1:4, ".D.ln_statutory_tax", collapse = "+")
formula_leads <- paste0("d.F", 1:4, ".D.ln_statutory_tax", collapse = "+")
formula_RHS <- paste0("d.D.ln_statutory_tax + ", formula_lags, "+", formula_leads)

outcomes <- c("d.D.ln_cpricei2",  "d.D.ln_quantity3")


## for linear hypothesis tests
lead.vars <- paste(paste0("d.F", 4:1, ".D.ln_statutory_tax"), collapse = " + ")
lag.vars <- paste(paste0("d.L", 4:1, ".D.ln_statutory_tax"), collapse = " + ")
lead.lp.restr <- paste(lead.vars, "= 0")
lag.lp.restr <- paste(lag.vars, "+ d.D.ln_statutory_tax = 0")
total.lp.restr <- paste(lag.vars, "+", lead.vars, "+ d.D.ln_statutory_tax = 0")


## FE vary across samples
for (sam in samples) {
  all_pi[, sample := get(sam)]
  sample <- all_pi[sample == 1]
  for (Y in c(outcomes)) {

      formula1 <- as.formula(paste0(
        Y, "~", formula_RHS, " - 1| 0 | 0 | module_by_state"
      ))
      flog.info("Estimating with %s as outcome with %s FE.", Y, FE)
      res1 <- felm(formula = formula1, data = sample,
                   weights = sample$base.sales)
      flog.info("Finished estimating with %s as outcome with %s FE.", Y, FE)
      
      
      ## attach results
      flog.info("Writing results...")
      res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
      res1.dt[, outcome := Y]
      res1.dt[, controls := "division_by_module_by_time"]
      res1.dt[, sample := sam]
      res1.dt[, spec := "DLM FE cons"]
      res1.dt$N_obs <- nrow(sample)
      res1.dt$N_stores <- uniqueN(sample, by = c("store_code_uc") )
      res1.dt$N_modules <- length(unique(sample$product_module_code))
      res1.dt$N_counties <- uniqueN(sample, by = c("fips_state", "fips_county"))
      res1.dt$N_years <- uniqueN(sample, by = c("year"))
      res1.dt$N_county_modules <- uniqueN(sample, by = c("fips_state", "fips_county",
                                                         "product_module_code"))
      LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
      fwrite(LRdiff_res, results.file)
      
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
        controls = "division_by_module_by_time",
        sample = sam,
        spec = "DLM FE cons")
      lp.dt$N_obs <- nrow(sample)
      lp.dt$N_stores <- uniqueN(sample, by = c("store_code_uc") )
      lp.dt$N_modules <- length(unique(sample$product_module_code))
      lp.dt$N_counties <- uniqueN(sample, by = c("fips_state", "fips_county"))
      lp.dt$N_years <- uniqueN(sample, by = c("year"))
      lp.dt$N_county_modules <- uniqueN(sample, by = c("fips_state", "fips_county",
                                                       "product_module_code"))
      
      
      LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T)
      fwrite(LRdiff_res, results.file)
      
      
      ##### Add the cumulative effect at each lead/lag (relative to -1)
      cumul.lead1.est <- 0
      cumul.lead1.se <- NA
      cumul.lead1.pval <- NA
      
      #cumul.lead2.est is just equal to minus the change between -2 and -1
      cumul.lead2.est <- - coef(summary(res1))[ "d.F1.D.ln_statutory_tax", "Estimate"]
      cumul.lead2.se <- coef(summary(res1))[ "d.F1.D.ln_statutory_tax", "Cluster s.e."]
      cumul.lead2.pval <- coef(summary(res1))[ "d.F1.D.ln_statutory_tax", "Pr(>|t|)"]
      
      ##LEADS
      for(j in 3:5) {
        
        ## Create a name for estimate, se and pval of each lead
        cumul.test.est.name <- paste("cumul.lead", j, ".est", sep = "")
        cumul.test.se.name <- paste("cumul.lead", j, ".se", sep = "")
        cumul.test.pval.name <- paste("cumul.lead", j, ".pval", sep = "")
        
        ## Create the formula to compute cumulative estimate at each lead/lag
        cumul.test.form <- paste0("-", paste(paste0("d.F", (j-1):1, ".D.ln_statutory_tax"), collapse = " - "))
        cumul.test.form <- paste(cumul.test.form, " = 0")
        
        ## Compute estimate and store in variables names
        cumul.test <- glht(res1, linfct = cumul.test.form)
        
        assign(cumul.test.est.name, coef(summary(cumul.test))[[1]])
        assign(cumul.test.se.name, sqrt(vcov(summary(cumul.test)))[[1]])
        assign(cumul.test.pval.name, 2*(1 - pnorm(abs(coef(summary(cumul.test))[[1]]/sqrt(vcov(summary(cumul.test)))[[1]]))))
      }
      
      
      ##LAGS
      ## On Impact --> Effect = coefficient on D.ln_sales_tax
      cumul.lag0.est <- coef(summary(res1))[ "d.D.ln_statutory_tax", "Estimate"]
      cumul.lag0.se <- coef(summary(res1))[ "d.D.ln_statutory_tax", "Cluster s.e."]
      cumul.lag0.pval <- coef(summary(res1))[ "d.D.ln_statutory_tax", "Pr(>|t|)"]
      
      for(j in 1:4) {
        
        ## Create a name for estimate, se and pval of each lead
        cumul.test.est.name <- paste("cumul.lag", j, ".est", sep = "")
        cumul.test.se.name <- paste("cumul.lag", j, ".se", sep = "")
        cumul.test.pval.name <- paste("cumul.lag", j, ".pval", sep = "")
        
        ## Create the formula to compute cumulative estimate at each lead/lag
        cumul.test.form <- paste("d.D.ln_statutory_tax + ", paste(paste0("d.L", 1:j, ".D.ln_statutory_tax"), collapse = " + "), sep = "")
        cumul.test.form <- paste(cumul.test.form, " = 0")
        
        ## Compute estimate and store in variables names
        cumul.test <- glht(res1, linfct = cumul.test.form)
        
        assign(cumul.test.est.name, coef(summary(cumul.test))[[1]])
        assign(cumul.test.se.name, sqrt(vcov(summary(cumul.test)))[[1]])
        assign(cumul.test.pval.name, 2*(1 - pnorm(abs(coef(summary(cumul.test))[[1]]/sqrt(vcov(summary(cumul.test)))[[1]]))))
      }
      
      
      ## linear hypothesis results
      lp.dt <- data.table(
        rn = c("cumul.lead5.D.ln_statutory_tax", "cumul.lead4.D.ln_statutory_tax", "cumul.lead3.D.ln_statutory_tax", "cumul.lead2.D.ln_statutory_tax", "cumul.lead1.D.ln_statutory_tax", "cumul.lag0.D.ln_statutory_tax",
               "cumul.lag1.D.ln_statutory_tax", "cumul.lag2.D.ln_statutory_tax", "cumul.lag3.D.ln_statutory_tax", "cumul.lag4.D.ln_statutory_tax"),
        Estimate = c(cumul.lead5.est, cumul.lead4.est, cumul.lead3.est, cumul.lead2.est, cumul.lead1.est, cumul.lag0.est, cumul.lag1.est, cumul.lag2.est, cumul.lag3.est, cumul.lag4.est),
        `Cluster s.e.` = c(cumul.lead5.se, cumul.lead4.se, cumul.lead3.se, cumul.lead2.se, cumul.lead1.se, cumul.lag0.se, cumul.lag1.se, cumul.lag2.se, cumul.lag3.se, cumul.lag4.se),
        `Pr(>|t|)` = c(cumul.lead5.pval, cumul.lead4.pval, cumul.lead3.pval, cumul.lead2.pval, cumul.lead1.pval, cumul.lag0.pval, cumul.lag1.pval, cumul.lag2.pval, cumul.lag3.pval, cumul.lag4.pval),
        outcome = Y,
        controls = "division_by_module_by_time",
        sample = sam,
        spec = "DLM FE cons")
      lp.dt$N_obs <- nrow(sample)
      lp.dt$N_stores <- uniqueN(sample, by = c("store_code_uc") )
      lp.dt$N_modules <- length(unique(sample$product_module_code))
      lp.dt$N_counties <- uniqueN(sample, by = c("fips_state", "fips_county"))
      lp.dt$N_years <- uniqueN(sample, by = c("year"))
      lp.dt$N_county_modules <- uniqueN(sample, by = c("fips_state", "fips_county",
                                                       "product_module_code"))
      
      LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T)
      fwrite(LRdiff_res, results.file)
      
    
  }
}

