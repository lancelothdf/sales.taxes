 ################################################################################
### Project: Sales Taxes
## Title: Main_semesterly_regressions.R
## Authors: Lancelot Henry de Frahan and John Bonney
## Description: Same as LRdiff_diff_semesterly_preferred_part1 but we plot the 
##              cumulative effect as opposed to changes. Code that runs distributed 
##              lags regressions (found in Part 2: Reduced Form Evidence).
################################################################################


library(data.table)
library(futile.logger)
library(lfe)
library(multcomp)

setwd("/project2/igaarder")


### Input filepaths -----------------------------------------------
#' This data set contains quarterly Laspeyres indices and sales from 2006 to
#' 2014. It also contains sales tax rates from 2008-2014.
all_goods_pi_path <- "Data/Nielsen/price_quantity_indices_allitems_2006-2016_notaxinfo.csv"
#' This data set contains an old price index that Lance constructed, from
old_pi_path <- "Data/Nielsen/Quarterly_old_pi.csv"
#' This data is the same as all_goods_pi_path, except it has 2015-2016 data as well.
data.full.path <- "Data/Nielsen/semester_nielsen_data.csv"
##### 
all_pi <- fread(data.full.path)

zillow_path <- "Data/covariates/zillow_long_by_county_clean.csv"
zillow_state_path <- "Data/covariates/zillow_long_by_state_clean.csv"
unemp.path <- "Data/covariates/county_monthly_unemp_clean.csv"
wage.path <- "Data/covariates/qcew_quarterly_clean.csv"


### Output filepath ----------------------------------------------
output.results.file <- "Data/LRdiff_semesterly_main.csv"
output.results.file.econ <- "Data/LRdiff_semesterly_w_econ.csv"


##### Reduced Form Evidence
## Set Up
formula_lags <- paste0("L", 1:4, ".D.ln_sales_tax", collapse = "+")
formula_leads <- paste0("F", 1:4, ".D.ln_sales_tax", collapse = "+")
formula_RHS <- paste0("D.ln_sales_tax + ", formula_lags, "+", formula_leads)
outcomes <- c("D.ln_cpricei", "D.ln_cpricei2", "D.ln_quantity", "D.ln_quantity2", "D.ln_quantity3", "D.ln_UPC", "D.ln_raw_quant")
FE_opts <- c("module_by_time", "region_by_module_by_time", "division_by_module_by_time")


## For Linear Hypothesis tests
lead.vars <- paste(paste0("F", 4:1, ".D.ln_sales_tax"), collapse = " + ")
lag.vars<- paste(paste0("L", 4:1, ".D.ln_sales_tax"), collapse = " + ")
lead.lp.restr <- paste(lead.vars, "= 0")
lag.lp.restr <- paste(lag.vars, "+ D.ln_sales_tax = 0")
total.lp.restr <- paste(lag.vars, "+", lead.vars, "+ D.ln_sales_tax = 0")


### 1. Run regression and estimate leads and lags directly (without imposing smoothness)
## No economic controls (but we run the placebos)
LRdiff_res <- data.table(NULL)
for (Y in c(outcomes)) {
  for (FE in FE_opts) {
    
    formula1 <- as.formula(paste0(
      Y, "~", formula_RHS, "| ", FE, " | 0 | module_by_state"
    ))
    flog.info("Estimating with %s as outcome with %s FE.", Y, FE)
    res1 <- felm(formula = formula1, data = all_pi,
                 weights = all_pi$base.sales)
    flog.info("Finished estimating with %s as outcome with %s FE.", Y, FE)
    
    
    ## attach results
    flog.info("Writing results...")
    res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
    res1.dt[, outcome := Y]
    res1.dt[, controls := FE]
    res1.dt[, econ := "none"]
    res1.dt[, Rsq := summary(res1)$r.squared]
    res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)
    
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
    
    ## Linear hypothesis results
    lp.dt <- data.table(
      rn = c("Pre.D.ln_sales_tax", "Post.D.ln_sales_tax", "All.D.ln_sales_tax"),
      Estimate = c(lead.test.est, lag.test.est, total.test.est),
      `Cluster s.e.` = c(lead.test.se, lag.test.se, total.test.se),
      `Pr(>|t|)` = c(lead.test.pval, lag.test.pval, total.test.pval),
      outcome = Y,
      controls = FE,
      econ = "none",
      Rsq = summary(res1)$r.squared,
      adj.Rsq = summary(res1)$adj.r.squared)
    LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)


    ##### Add the cumulative effect at each lead/lag (relative to -1)
    cumul.lead1.est <- 0
    cumul.lead1.se <- NA
    cumul.lead1.pval <- NA
    
    #cumul.lead2.est is just equal to minus the change between -2 and -1
    cumul.lead2.est <- - coef(summary(res1))[ "F1.D.ln_sales_tax", "Estimate"]
    cumul.lead2.se <- coef(summary(res1))[ "F1.D.ln_sales_tax", "Cluster s.e."]
    cumul.lead2.pval <- coef(summary(res1))[ "F1.D.ln_sales_tax", "Pr(>|t|)"]
    
    ##LEADS
    for(j in 3:5) {

      ## Create a name for estimate, se and pval of each lead
      cumul.test.est.name <- paste("cumul.lead", j, ".est", sep = "")
      cumul.test.se.name <- paste("cumul.lead", j, ".se", sep = "")
      cumul.test.pval.name <- paste("cumul.lead", j, ".pval", sep = "")

      ## Create the formula to compute cumulative estimate at each lead/lag
      cumul.test.form <- paste0("-", paste(paste0("F", (j-1):1, ".D.ln_sales_tax"), collapse = " - "))
      cumul.test.form <- paste(cumul.test.form, " = 0")

      ## Compute estimate and store in variables names
      cumul.test <- glht(res1, linfct = cumul.test.form)

      assign(cumul.test.est.name, coef(summary(cumul.test))[[1]])
      assign(cumul.test.se.name, sqrt(vcov(summary(cumul.test)))[[1]])
      assign(cumul.test.pval.name, 2*(1 - pnorm(abs(coef(summary(cumul.test))[[1]]/sqrt(vcov(summary(cumul.test)))[[1]]))))
    }

    ##First lead
    ## First lead --> Effect = coefficient on F1.D.ln_sales_tax
    cumul.lead1.est <- coef(summary(res1))[ "F1.D.ln_sales_tax", "Estimate"]
    cumul.lead1.se <- coef(summary(res1))[ "F1.D.ln_sales_tax", "Cluster s.e."]
    cumul.lead1.pval <- coef(summary(res1))[ "F1.D.ln_sales_tax", "Pr(>|t|)"]

    ##LAGS
    ## On Impact --> Effect = coefficient on D.ln_sales_tax + F1.D.ln_sales_tax
    cumul.test.form <- "F1.D.ln_sales_tax + D.ln_sales_tax = 0"
    
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
      cumul.test.form <- paste("F1.D.ln_sales_tax + D.ln_sales_tax + ", paste(paste0("L", 1:j, ".D.ln_sales_tax"), collapse = " + "), sep = "")
      cumul.test.form <- paste(cumul.test.form, " = 0")

      ## Compute estimate and store in variables names
      cumul.test <- glht(res1, linfct = cumul.test.form)

      assign(cumul.test.est.name, coef(summary(cumul.test))[[1]])
      assign(cumul.test.se.name, sqrt(vcov(summary(cumul.test)))[[1]])
      assign(cumul.test.pval.name, 2*(1 - pnorm(abs(coef(summary(cumul.test))[[1]]/sqrt(vcov(summary(cumul.test)))[[1]]))))
    }


    ## Linear hypothesis results
    lp.dt <- data.table(
      rn = c("cumul.lead5.D.ln_sales_tax", "cumul.lead4.D.ln_sales_tax", "cumul.lead3.D.ln_sales_tax", "cumul.lead2.D.ln_sales_tax", "cumul.lead1.D.ln_sales_tax", "cumul.lag0.D.ln_sales_tax", "cumul.lag1.D.ln_sales_tax", "cumul.lag2.D.ln_sales_tax", "cumul.lag3.D.ln_sales_tax", "cumul.lag4.D.ln_sales_tax"),
      Estimate = c(cumul.lead5.est, cumul.lead4.est, cumul.lead3.est, cumul.lead2.est, cumul.lead1.est, cumul.lag0.est, cumul.lag1.est, cumul.lag2.est, cumul.lag3.est, cumul.lag4.est),
      `Cluster s.e.` = c(cumul.lead5.se, cumul.lead4.se, cumul.lead3.se, cumul.lead2.se, cumul.lead1.se, cumul.lag0.se, cumul.lag1.se, cumul.lag2.se, cumul.lag3.se, cumul.lag4.se),
      `Pr(>|t|)` = c(cumul.lead5.pval, cumul.lead4.pval, cumul.lead3.pval, cumul.lead2.pval, cumul.lead1.pval, cumul.lag0.pval, cumul.lag1.pval, cumul.lag2.pval, cumul.lag3.pval, cumul.lag4.pval),
      outcome = Y,
      controls = FE,
      econ = "none",
      Rsq = summary(res1)$r.squared,
      adj.Rsq = summary(res1)$adj.r.squared)
    LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)


  }
}


### Summary Values -----------
LRdiff_res$N_obs <- nrow(all_pi)
LRdiff_res$N_modules <- length(unique(all_pi$product_module_code))
LRdiff_res$N_stores <- length(unique(all_pi$store_code_uc))
LRdiff_res$N_counties <- uniqueN(all_pi, by = c("fips_state", "fips_county"))
LRdiff_res$N_years <- uniqueN(all_pi, by = c("year")) # should be 6 (we lose one because we difference)
LRdiff_res$N_county_modules <- uniqueN(all_pi, by = c("fips_state", "fips_county",
                                                      "product_module_code"))                                                   

### Write Output Files
fwrite(LRdiff_res, output.results.file)



############################################
### 2. Reduced Form Evidence with Controls--------------------------------------------------------------
############################################

#### Prep the unemployment, house price data and quarterly wage data --------------------------
### Start with house prices
# First build a frame to make sure we can assign every county a home price
all_counties <- unique(all_pi[, .(fips_state, fips_county)])
county_skeleton <- data.table(NULL)
for (X in 2006:2016) {
  for (Y in 1:12) {
    all_counties[, year := X]
    all_counties[, month := Y]
    county_skeleton <- rbind(county_skeleton, all_counties)
  }
}

## prep house price data
zillow_dt <- fread(zillow_path)
zillow_dt <- zillow_dt[between(year, 2006, 2016)]
zillow_dt <- zillow_dt[, .(fips_state, fips_county, median_home_price, year, month)]
zillow_dt <- merge(county_skeleton, zillow_dt, all.x = T,
                   by = c("fips_state", "fips_county", "year", "month"))

## prep state-level house prices (for when county-level is missing)
zillow_state_dt <- fread(zillow_state_path)
zillow_state_dt <- zillow_state_dt[between(year, 2006, 2016)]
zillow_state_dt <- zillow_state_dt[, .(fips_state, median_home_price, year, month)]
setnames(zillow_state_dt, "median_home_price", "state_median_home_price")
zillow_state_dt$month <- as.integer(round(zillow_state_dt$month))

zillow_dt <- merge(zillow_dt, zillow_state_dt, all.x = T,
                   by = c("fips_state", "year", "month"))
zillow_dt[is.na(median_home_price), median_home_price := state_median_home_price]
zillow_dt[, state_median_home_price := NULL]


## collapse to semesters
zillow_dt <- zillow_dt[, semester := ceiling((month/12)*2)]
zillow_dt <- zillow_dt[, list(ln_home_price = log(mean(median_home_price))),
                       by = .(year, semester, fips_state, fips_county)]

##


### Unemployment data
unemp.data <- fread(unemp.path)
unemp.data <- unemp.data[, c("fips_state", "fips_county", "year", "month", "rate")]
unemp.data <- unemp.data[, semester := ceiling((month/12)*2)]
unemp.data <- unemp.data[, list(unemp = mean(rate)), by = .(year, semester, fips_state, fips_county)]
unemp.data <- unemp.data[year >= 2006 & year <= 2016,]
unemp.data <- unemp.data[, ln_unemp := log(unemp)]

##
zillow_dt <- merge(zillow_dt, unemp.data, by = c("fips_state", "fips_county", "year", "semester"), all.x = T)
rm(unemp.data)



### Wage data
wage.data <- fread(wage.path)
wage.data <- wage.data[, c("fips_state", "fips_county", "year", "quarter", "total_mean_wage", "total_employment")]
wage.data <- wage.data[year >= 2006 & year <= 2016,]
wage.data <- wage.data[, semester := ceiling(quarter/2)]
wage.data <- wage.data[, list(total_mean_wage = weighted.mean(total_mean_wage, w = total_employment)), by = .(year, semester, fips_state, fips_county)]
wage.data[, ln_wage := log(total_mean_wage)]
wage.data <- wage.data[, c("fips_state", "fips_county", "year", "semester", "ln_wage")]

##
zillow_dt <- merge(zillow_dt, wage.data, by = c("fips_state", "fips_county", "year", "semester"), all.x = T)
rm(wage.data)

### Balance the sample
zillow_dt <- zillow_dt[!is.na(ln_wage) & !is.na(ln_unemp) & !is.na(ln_home_price)]


keep_counties <- zillow_dt[, list(n = .N),
                           by = .(fips_state, fips_county)]
keep_counties <- keep_counties[n == (2016 - 2005) * 2]

setkey(zillow_dt, fips_state, fips_county)
setkey(keep_counties, fips_state, fips_county)

zillow_dt <- zillow_dt[keep_counties]
setkey(zillow_dt, fips_state, fips_county, year, semester)

### Difference the econ data + leads and lags
zillow_dt <- zillow_dt[order(fips_state, fips_county, year, semester),]

zillow_dt[, D.ln_home_price := ln_home_price - shift(ln_home_price, n=1, type="lag"),
          by = .(fips_state, fips_county)]

zillow_dt[, D.ln_unemp := ln_unemp - shift(ln_unemp, n=1, type="lag"),
          by = .(fips_state, fips_county)]

zillow_dt[, D.ln_wage := ln_wage - shift(ln_wage, n=1, type="lag"),
          by = .(fips_state, fips_county)]

## generate lags
for (lag.val in 1:4) {
  lag.X <- paste0("L", lag.val, ".D.ln_home_price")
  zillow_dt[, (lag.X) := shift(D.ln_home_price, n=lag.val, type="lag"),
            by = .(fips_state, fips_county)]
  
  lag.X <- paste0("L", lag.val, ".D.ln_unemp")
  zillow_dt[, (lag.X) := shift(D.ln_unemp, n=lag.val, type="lag"),
            by = .(fips_state, fips_county)]
  
  lag.X <- paste0("L", lag.val, ".D.ln_wage")
  zillow_dt[, (lag.X) := shift(D.ln_wage, n=lag.val, type="lag"),
            by = .(fips_state, fips_county)]
  
}



### Merge econ data to price and quantity data then run estimations
all_pi <- merge(all_pi, zillow_dt, by = c("fips_state", "fips_county", "year", "semester"))


## Construct polynomials in lagged econ variables
all_pi[, lag.unemp0 := D.ln_unemp + L4.D.ln_unemp + L3.D.ln_unemp + L2.D.ln_unemp + L1.D.ln_unemp ]
all_pi[, lag.unemp1 := 4*L4.D.ln_unemp + 3*L3.D.ln_unemp + 2*L2.D.ln_unemp + L1.D.ln_unemp ]
#all_pi[, lag.unemp2 := 16*L4.D.ln_unemp + 9*L3.D.ln_unemp + 4*L2.D.ln_unemp + L1.D.ln_unemp ]

all_pi[, lag.home_price0 := D.ln_home_price + L4.D.ln_home_price + L3.D.ln_home_price + L2.D.ln_home_price + L1.D.ln_home_price ]
all_pi[, lag.home_price1 := 4*L4.D.ln_home_price + 3*L3.D.ln_home_price + 2*L2.D.ln_home_price + L1.D.ln_home_price ]
#all_pi[, lag.home_price2 := 16*L4.D.ln_home_price + 9*L3.D.ln_home_price + 4*L2.D.ln_home_price + L1.D.ln_home_price ]

all_pi[, lag.wage0 := D.ln_wage + L4.D.ln_wage + L3.D.ln_wage + L2.D.ln_wage + L1.D.ln_wage ]
all_pi[, lag.wage1 := 4*L4.D.ln_wage + 3*L3.D.ln_wage + 2*L2.D.ln_wage + L1.D.ln_wage ]
#all_pi[, lag.wage2 := 16*L4.D.ln_wage + 9*L3.D.ln_wage + 4*L2.D.ln_wage + L1.D.ln_wage ]
#############################################

formula_lags <- paste0("L", 1:4, ".D.ln_sales_tax", collapse = "+")
formula_leads <- paste0("F", 1:4, ".D.ln_sales_tax", collapse = "+")
formula_RHS <- paste0("D.ln_sales_tax + ", formula_lags, "+", formula_leads)

outcomes <- c("D.ln_cpricei", "D.ln_cpricei2", "D.ln_quantity", "D.ln_quantity2", "D.ln_quantity3", "D.ln_sales_share")
FE_opts <- c("region_by_module_by_time", "division_by_module_by_time")

## for linear hypothesis tests
lead.vars <- paste(paste0("F", 4:1, ".D.ln_sales_tax"), collapse = " + ")
lag.vars <- paste(paste0("L", 4:1, ".D.ln_sales_tax"), collapse = " + ")
lead.lp.restr <- paste(lead.vars, "= 0")
lag.lp.restr <- paste(lag.vars, "+ D.ln_sales_tax = 0")
total.lp.restr <- paste(lag.vars, "+", lead.vars, "+ D.ln_sales_tax = 0")


### First: run regression and estimate leads and lags directly (without imposing smoothness)
## No Econ controls (but we run the placebos)
LRdiff_res <- data.table(NULL)
for (Y in c(outcomes)) {
  for (FE in FE_opts) {
    for(i in 1:4) {
      
      # Create list of economic controls  
      lag.home <- paste(paste0("L", 4:1, ".D.ln_home_price"), collapse = " + ")
      lag.unemp <- paste(paste0("L", 4:1, ".D.ln_unemp"), collapse = " + ")
      lag.wage <- paste(paste0("L", 4:1, ".D.ln_wage"), collapse = " + ")
      lag.econ <- paste(lag.home, lag.unemp, lag.wage, sep = " + ")
      
      
      formula1 <- as.formula(paste0(
        Y, "~", formula_RHS, " + ", lag.econ, "| ", FE, " | 0 | module_by_state"
      ))
      flog.info("Estimating with %s as outcome with %s FE.", Y, FE)
      res1 <- felm(formula = formula1, data = all_pi,
                   weights = all_pi$base.sales)
      flog.info("Finished estimating with %s as outcome with %s FE.", Y, FE)
      
      
      ## attach results
      flog.info("Writing results...")
      res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
      res1.dt[, outcome := Y]
      res1.dt[, controls := FE]
      res1.dt[, econ := i]
      res1.dt[, Rsq := summary(res1)$r.squared]
      res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
      LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
      fwrite(LRdiff_res, output.results.file.econ)
      
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
        rn = c("Pre.D.ln_sales_tax", "Post.D.ln_sales_tax", "All.D.ln_sales_tax"),
        Estimate = c(lead.test.est, lag.test.est, total.test.est),
        `Cluster s.e.` = c(lead.test.se, lag.test.se, total.test.se),
        `Pr(>|t|)` = c(lead.test.pval, lag.test.pval, total.test.pval),
        outcome = Y,
        controls = FE,
        econ = i,
        Rsq = summary(res1)$r.squared,
        adj.Rsq = summary(res1)$adj.r.squared)
      LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T)
      fwrite(LRdiff_res, output.results.file.econ)
      
      
      ##### Add the cumulative effect at each lead/lag (relative to -2)
      cumul.lead2.est <- 0
      cumul.lead2.se <- NA
      cumul.lead2.pval <- NA
      
      #cumul.lead3.est is just equal to minus the change between -3 and -2
      cumul.lead3.est <- - coef(summary(res1))[ "F2.D.ln_sales_tax", "Estimate"]
      cumul.lead3.se <- coef(summary(res1))[ "F2.D.ln_sales_tax", "Cluster s.e."]
      cumul.lead3.pval <- coef(summary(res1))[ "F2.D.ln_sales_tax", "Pr(>|t|)"]
      
      ##LEADS
      for(j in 3:5) {
        
        ## Create a name for estimate, se and pval of each lead
        cumul.test.est.name <- paste("cumul.lead", j, ".est", sep = "")
        cumul.test.se.name <- paste("cumul.lead", j, ".se", sep = "")
        cumul.test.pval.name <- paste("cumul.lead", j, ".pval", sep = "")
        
        ## Create the formula to compute cumulative estimate at each lead/lag
        cumul.test.form <- paste0("-", paste(paste0("F", (j-1):1, ".D.ln_sales_tax"), collapse = " - "))
        cumul.test.form <- paste(cumul.test.form, " = 0")
        
        ## Compute estimate and store in variables names
        cumul.test <- glht(res1, linfct = cumul.test.form)
        
        assign(cumul.test.est.name, coef(summary(cumul.test))[[1]])
        assign(cumul.test.se.name, sqrt(vcov(summary(cumul.test)))[[1]])
        assign(cumul.test.pval.name, 2*(1 - pnorm(abs(coef(summary(cumul.test))[[1]]/sqrt(vcov(summary(cumul.test)))[[1]]))))
      }
      
      
      ##First lead
      ## First lead --> Effect = coefficient on F1.D.ln_sales_tax
      cumul.lead1.est <- coef(summary(res1))[ "F1.D.ln_sales_tax", "Estimate"]
      cumul.lead1.se <- coef(summary(res1))[ "F1.D.ln_sales_tax", "Cluster s.e."]
      cumul.lead1.pval <- coef(summary(res1))[ "F1.D.ln_sales_tax", "Pr(>|t|)"]
      
      ##LAGS
      ## On Impact --> Effect = coefficient on D.ln_sales_tax + F1.D.ln_sales_tax
      cumul.test.form <- "F1.D.ln_sales_tax + D.ln_sales_tax = 0"
      
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
        cumul.test.form <- paste("F1.D.ln_sales_tax + D.ln_sales_tax + ", paste(paste0("L", 1:j, ".D.ln_sales_tax"), collapse = " + "), sep = "")
        cumul.test.form <- paste(cumul.test.form, " = 0")
        
        ## Compute estimate and store in variables names
        cumul.test <- glht(res1, linfct = cumul.test.form)
        
        assign(cumul.test.est.name, coef(summary(cumul.test))[[1]])
        assign(cumul.test.se.name, sqrt(vcov(summary(cumul.test)))[[1]])
        assign(cumul.test.pval.name, 2*(1 - pnorm(abs(coef(summary(cumul.test))[[1]]/sqrt(vcov(summary(cumul.test)))[[1]]))))
      }
      
      ## linear hypothesis results
      lp.dt <- data.table(
        rn = c("cumul.lead5.D.ln_sales_tax", "cumul.lead4.D.ln_sales_tax", "cumul.lead3.D.ln_sales_tax", "cumul.lead2.D.ln_sales_tax", "cumul.lead1.D.ln_sales_tax", "cumul.lag0.D.ln_sales_tax", "cumul.lag1.D.ln_sales_tax", "cumul.lag2.D.ln_sales_tax", "cumul.lag3.D.ln_sales_tax", "cumul.lag4.D.ln_sales_tax"),
        Estimate = c(cumul.lead5.est, cumul.lead4.est, cumul.lead3.est, cumul.lead2.est, cumul.lead1.est, cumul.lag0.est, cumul.lag1.est, cumul.lag2.est, cumul.lag3.est, cumul.lag4.est),
        `Cluster s.e.` = c(cumul.lead5.se, cumul.lead4.se, cumul.lead3.se, cumul.lead2.se, cumul.lead1.se, cumul.lag0.se, cumul.lag1.se, cumul.lag2.se, cumul.lag3.se, cumul.lag4.se),
        `Pr(>|t|)` = c(cumul.lead5.pval, cumul.lead4.pval, cumul.lead3.pval, cumul.lead2.pval, cumul.lead1.pval, cumul.lag0.pval, cumul.lag1.pval, cumul.lag2.pval, cumul.lag3.pval, cumul.lag4.pval),
        outcome = Y,
        controls = FE,
        econ = i,
        Rsq = summary(res1)$r.squared,
        adj.Rsq = summary(res1)$adj.r.squared)
      LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T)
      fwrite(LRdiff_res, output.results.file.econ)
      
    }
  }
}

LRdiff_res$N_obs <- nrow(all_pi)
LRdiff_res$N_modules <- length(unique(all_pi$product_module_code))
LRdiff_res$N_stores <- length(unique(all_pi$store_code_uc))
LRdiff_res$N_counties <- uniqueN(all_pi, by = c("fips_state", "fips_county"))
LRdiff_res$N_years <- uniqueN(all_pi, by = c("year")) # should be 6 (we lose one because we difference)
LRdiff_res$N_county_modules <- uniqueN(all_pi, by = c("fips_state", "fips_county",
                                                           "product_module_code"))
fwrite(LRdiff_res, output.results.file.econ)

