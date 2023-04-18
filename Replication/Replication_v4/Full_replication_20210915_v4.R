#' Sales Taxes
#' Main Replication File. Updated on 9/15/2020
#' This Code replicates all presented results in the current version of the draft 
#' 

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


## input filepaths -----------------------------------------------
data.semester <- "Data/Nielsen/semester_nielsen_data.csv"
data.taxability <- "Data/taxability_state_panel.csv"
zillow_path <- "Data/covariates/zillow_long_by_county_clean.csv"
zillow_state_path <- "Data/covariates/zillow_long_by_state_clean.csv"
unemp.path <- "Data/covariates/county_monthly_unemp_clean.csv"
wage.path <- "Data/covariates/qcew_quarterly_clean.csv"
data.hh <- "cleaning/consumer_panel_q_hh_group_2006-2016.csv"
data.year <- "Data/Nielsen/yearly_nielsen_data.csv"

## output filepaths ----------------------------------------------
output.results.file <- "Data/LRdiff_semesterly_main.csv"
output.results.file.econ <- "Data/LRdiff_semesterly_w_econ.csv"
results.file.spillovers <- "Data/DiD_spillover_estimates_csinitprice_semester.csv"
output.results.file.hh <- "Data/HH_cross_sectional_design_income_salesweight.csv"
output.results.file.crossec <- "Data/LRdiff_cross_sectional_design.csv"
output.results.file.TWFE <- "Data/LR_TWFE_dessign.csv"
iv.output.results.file <- "Data/Demand_iv_sat_initial_price_semester_boot_r.csv"
theta.output.results.file <- "Data/Demand_theta_sat_initial_price_semester_boot_r.csv"
theta.berstein <- "Data/Demand_gamma_sat_initial_price_semester_boot_r_K"
pq.output.results.file <- "Data/Demand_pq_sat_initial_price_semester_boot_r.csv"
output.table.avelas <- "Data/summary_elasticity_states.csv"
out.file.elast <- "Data/elasticity_bounds_table_berns_monot_mincreterion_d.csv"
out.file.mc <-  "Data/table_berns_monot_mincreteria_d.csv"
iv.output.results.pretax <- "Data/DID_iv_sat_initial_price_pretax_semester.csv"
theta.output.results.pretax <- "Data/Demand_theta_sat_initial_price_pretax_semester.csv"
iv.output.salience.results.file <- "Data/Demand_iv_sat_initial_price_semester_salience.csv"
theta.output.salience.results.file <- "Data/Demand_theta_sat_initial_price_semester_salience.csv"
conduct.parameter.file <- "Data/salience_conduct_parameter_at_p.csv"
pq.output.salience.results.file <- "Data/Demand_pq_sat_initial_price_semester_salience.csv"
theta.berstein.sal <- "Data/Demand_gamma_sat_initial_price_semester_salience_K"
out.file.hyp.nonmarginal <- "Output/nonmarginal_hypothetical_def2.csv"
out.file.hyp.marginal <- "Output/marginal_hypothetical_def2.csv"
out.file.mc.welf.hyp <- "Output/mincriteria_all.csv"
binned.data.price <- "Data/extraction_state_binned_price.csv"
binned.data.tax <- "Data/extraction_state_binned_tax.csv"
out.file.marginal <- "Data/marginal_extrapoaltion_state_priority.csv"
out.file.nonmarginal <- "Data/nonmarginal_extrapoaltion_state_priority.csv"
out.file.average <- "Data/average_extrapolation_state_priority.csv"


## Open Data ----------------

# Semesterly data
all_pi <- fread(data.semester)

## Open Taxability panel
taxability <- fread(data.taxability)


# collapse taxability to the semester
taxability[, semester := ceiling(month/6)]
taxability <- taxability[, .(taxability = mean(taxability),
                             reduced_rate = mean(reduced_rate, na.rm = T)), 
                         by = .(product_module_code, semester, year, fips_state)]
taxability[, taxability := ifelse(!is.nan(reduced_rate), 2, taxability)]

## Open clean Household Panel
purchases.sample <- fread(data.hh)

# Yearly Data
yearly_data <- fread(data.year)

#### Prepare Household Panel -----------------------


### Drop observations for which the sales tax rate is imputed
purchases.sample <- purchases.sample[year >= 2008 & year <= 2014]
purchases.sample$year <- factor(purchases.sample$year) ##Convert the indicator for year to a factor variable (needed for interaction in the regression between ln_sales_tax and dummy for year)

# Compute sales weight: by year, how large are purchases in each group in this sample?
purchases.sample[, sales.weight := sum(expenditures, na.rm = T), by = .(product_group_code, year)]
purchases.sample[, sales.weight := sales.weight / sum(sales.weight, na.rm = T), by = .(year)]

# Build new weight as the prdocut of both household and group weights
purchases.sample[, projection_factor := projection_factor*sales.weight]

# Drop observations without weights at the end
purchases.sample <- purchases.sample[!is.na(projection_factor)]

# FE
purchases.sample[, income_by_group_by_time := .GRP, by = .(household_income, product_group_code, year)]
purchases.sample[, group_by_time := .GRP, by = .(product_group_code, year)]
purchases.sample[, household_by_time := .GRP, by = .(year, household_code)]

cohort.weights <- rep(1, 7) ##Construct weights to average across cohorts/years.  Start with equal weights
cohort.weights <- cohort.weights/sum(cohort.weights)


#### Prepare the unemployment, house price data and quarterly wage data --------------------------
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

### Unemployment data
unemp.data <- fread(unemp.path)
unemp.data <- unemp.data[, c("fips_state", "fips_county", "year", "month", "rate")]
unemp.data <- unemp.data[, semester := ceiling((month/12)*2)]
unemp.data <- unemp.data[, list(unemp = mean(rate)), by = .(year, semester, fips_state, fips_county)]
unemp.data <- unemp.data[year >= 2006 & year <= 2016,]
unemp.data <- unemp.data[, ln_unemp := log(unemp)]

## merge
zillow_dt <- merge(zillow_dt, unemp.data, by = c("fips_state", "fips_county", "year", "semester"), all.x = T)
rm(unemp.data)


### Balance the sample
zillow_dt <- zillow_dt[!is.na(ln_unemp) & !is.na(ln_home_price)]


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


## generate lags
for (lag.val in 1:4) {
  lag.X <- paste0("L", lag.val, ".D.ln_home_price")
  zillow_dt[, (lag.X) := shift(D.ln_home_price, n=lag.val, type="lag"),
            by = .(fips_state, fips_county)]
  
  lag.X <- paste0("L", lag.val, ".D.ln_unemp")
  zillow_dt[, (lag.X) := shift(D.ln_unemp, n=lag.val, type="lag"),
            by = .(fips_state, fips_county)]
  
}


## Nielsen Retailer Data Cleaning. Yearly Data -----------------------
### Drop observations for which the sales tax rate is imputed
yearly_data <- yearly_data[year >= 2008 & year <= 2014]
yearly_data$year <- factor(yearly_data$year) ##Convert the indicator for year to a factor variable (needed for interaction in the regression between ln_sales_tax and dummy for year)

cohort.weights <- rep(1, 7) ##Construct weights to average across cohorts/years.  Start with equal weights
cohort.weights <- cohort.weights/sum(cohort.weights)

## Nielsen Retailer Data Cleaning. Semester -----------------------

#### Create Variables
all_pi[, w.ln_sales_tax := ln_sales_tax - mean(ln_sales_tax), by = .(store_by_module)]
all_pi[, w.ln_cpricei2 := ln_cpricei2 - mean(ln_cpricei2), by = .(store_by_module)]
all_pi[, w.ln_quantity3 := ln_quantity3 - mean(ln_quantity3), by = .(store_by_module)]
all_pi[, w.ln_pricei2 := ln_pricei2 - mean(ln_pricei2), by = .(store_by_module)]
all_pi[, w.ln_sales := ln_sales - mean(ln_sales), by = .(store_by_module)]

## Create lead demeaned tax rate for pre-trends
all_pi <- all_pi[order(store_code_uc, product_module_code, year, semester),] ##Sort on store by year-semester (in ascending order)
for (val in 1:4) {
  
  lead.X <- paste0("F", val, ".w.ln_sales_tax")
  all_pi[, (lead.X) := shift(w.ln_sales_tax, n=val, type="lead"),
         by = .(store_code_uc, product_module_code)]
}

# Create some necesary variables
all_pi[, module_by_time := .GRP, by = .(product_module_code, semester, year)]
all_pi[, L.ln_cpricei2 := ln_cpricei2 - D.ln_cpricei2]
all_pi[, dm.L.ln_cpricei2 := L.ln_cpricei2 - mean(L.ln_cpricei2, na.rm = T), by = module_by_time]
all_pi[, dm.ln_cpricei2 := ln_cpricei2 - mean(ln_cpricei2, na.rm = T), by = module_by_time]
all_pi[, dm.ln_quantity3 := ln_quantity3 - mean(ln_quantity3, na.rm = T), by = module_by_time]
all_pi[, dm.L.ln_pricei2 := L.ln_pricei2 - mean(L.ln_pricei2, na.rm = T), by = module_by_time]
all_pi[, dm.ln_pricei2 := ln_pricei2 - mean(ln_pricei2, na.rm = T), by = module_by_time]




#### Define Common Support
control <- all_pi[D.ln_sales_tax == 0,]
treated <- all_pi[D.ln_sales_tax != 0,]

pct1.control <- quantile(control$dm.L.ln_cpricei2, probs = 0.01, na.rm = T, weight=control$base.sales)
pct1.treated <- quantile(treated$dm.L.ln_cpricei2, probs = 0.01, na.rm = T, weight=treated$base.sales)

pct99.control <- quantile(control$dm.L.ln_cpricei2, probs = 0.99, na.rm = T, weight=control$base.sales)
pct99treated <- quantile(treated$dm.L.ln_cpricei2, probs = 0.99, na.rm = T, weight=treated$base.sales)

all_pi[, cs_price := ifelse(dm.L.ln_cpricei2 > max(pct1.treated, pct1.control) & 
                                    dm.L.ln_cpricei2 < min(pct99treated, pct99.control), 1, 0)]
# Make sure missings are 0s
all_pi[, cs_price := ifelse(is.na(dm.L.ln_cpricei2), 0, cs_price)]

## Keep within the common support
all_pi_cs <- all_pi[cs_price == 1,]

## cut the tails (keep between 1st and 99th percentile)
pct1 <- quantile(all_pi$dm.ln_cpricei2, probs = 0.01, na.rm = T, weight=base.sales)
pct99 <- quantile(all_pi$dm.ln_cpricei2, probs = 0.99, na.rm = T, weight=base.sales)
all_pi_cs <- all_pi_cs[(dm.ln_cpricei2 > pct1 & dm.ln_cpricei2 < pct99),]
all_pi_cs <- all_pi_cs[, c("year", "semester", "fips_state", "fips_county" , "product_module_code")]

##### Merges

# Merge econ data to price and quantity data then run estimations
all_pi_econ <- merge(all_pi, zillow_dt, by = c("fips_state", "fips_county", "year", "semester"))

# Merge to create data set for spillovers
all_pi_spill_econ <- merge(all_pi_econ, taxability, by = c("year", "semester", "fips_state", "product_module_code"), all.x = T)
all_pi_spill <- merge(all_pi, taxability, by = c("year", "semester", "fips_state", "product_module_code"), all.x = T)

##### Additional set up and CS restrictions

# Identify always taxable and always tax-exempt
all_pi_spill[, tax_exempt := taxability == 0]
all_pi_spill[, taxable := taxability == 1]
all_pi_spill[, T_tax_exempt := sum(tax_exempt), by = .(store_by_module)]
all_pi_spill[, T_taxable := sum(taxable), by = .(store_by_module)]
all_pi_spill[, T_total := .N, by = .(store_by_module)]
all_pi_spill[, all_taxable:= ifelse(T_taxable == T_total,1,0)]
all_pi_spill[, all_taxexempt:= ifelse(T_tax_exempt == T_total,1,0)]

# Identify statutory tax rate
all_pi_spill[, ln_statutory_tax := max(ln_sales_tax, na.rm = T), by = .(fips_state, fips_county, year, semester)]
all_pi_spill[, ln_statutory_tax := ifelse(taxability == 1, ln_sales_tax, ln_statutory_tax)]

# Create statutory leads and lags
LLs <- c(paste0("L", 1:4, ".D"), paste0("F", 1:4, ".D"), "D")
for (Td in LLs) {
  
  actual <- paste0(Td, ".ln_sales_tax")
  statu <- paste0(Td, ".ln_statutory_tax")
  
  all_pi_spill[, (statu) := max(get(actual), na.rm = T), by = .(fips_state, fips_county, year, semester)]
  all_pi_spill[, (statu) := ifelse(taxability == 1, get(actual), get(statu))]
}

# Keeping common support
# Spillovers
all_pi_spill <- merge(all_pi_spill, all_pi_cs, by = c("year", "semester", "fips_state", "fips_county" , "product_module_code"))
all_pi_spill_econ <- merge(all_pi_spill_econ, all_pi_cs, by = c("year", "semester", "fips_state", "fips_county" , "product_module_code"))
# Main data w. econ vars
all_pi_econ <- merge(all_pi_econ, all_pi_cs, by = c("year", "semester", "fips_state", "fips_county" , "product_module_code"))
# Main data set
all_pi <- merge(all_pi, all_pi_cs, by = c("year", "semester", "fips_state", "fips_county" , "product_module_code"))



########## Estimations -----------------

### 1. Reduced Form Evidence -----------------

## Set up
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


## Run Estimations
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
    
    ## linear hypothesis results
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
    
    
    ##### Add the cumulative effect at each lead/lag (relative to -2)
    cumul.lead2.est <- 0
    cumul.lead2.se <- NA
    cumul.lead2.pval <- NA
    
    #cumul.lead3.est is just equal to minus the change between -3 and -2
    cumul.lead3.est <- - coef(summary(res1))[ "F2.D.ln_sales_tax", "Estimate"]
    cumul.lead3.se <- coef(summary(res1))[ "F2.D.ln_sales_tax", "Cluster s.e."]
    cumul.lead3.pval <- coef(summary(res1))[ "F2.D.ln_sales_tax", "Pr(>|t|)"]
    
    ##LEADS
    for(j in 4:5) {
      
      ## Create a name for estimate, se and pval of each lead
      cumul.test.est.name <- paste("cumul.lead", j, ".est", sep = "")
      cumul.test.se.name <- paste("cumul.lead", j, ".se", sep = "")
      cumul.test.pval.name <- paste("cumul.lead", j, ".pval", sep = "")
      
      ## Create the formula to compute cumulative estimate at each lead/lag
      cumul.test.form <- paste0("-", paste(paste0("F", (j-1):2, ".D.ln_sales_tax"), collapse = " - "))
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
      econ = "none",
      Rsq = summary(res1)$r.squared,
      adj.Rsq = summary(res1)$adj.r.squared)
    LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)
    
    
  }
}

LRdiff_res$N_obs <- nrow(all_pi)
LRdiff_res$N_modules <- length(unique(all_pi$product_module_code))
LRdiff_res$N_stores <- length(unique(all_pi$store_code_uc))
LRdiff_res$N_counties <- uniqueN(all_pi, by = c("fips_state", "fips_county"))
LRdiff_res$N_years <- uniqueN(all_pi, by = c("year")) # should be 6 (we lose one because we difference)
LRdiff_res$N_county_modules <- uniqueN(all_pi, by = c("fips_state", "fips_county",
                                                      "product_module_code"))
fwrite(LRdiff_res, output.results.file)

### 2. Reduced Form Evidence with Controls -----------------
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
      lag.home <- paste(paste0("L", i:1, ".D.ln_home_price"), collapse = " + ")
      lag.unemp <- paste(paste0("L", i:1, ".D.ln_unemp"), collapse = " + ")
      lag.econ <- paste(lag.home, lag.unemp, lag.wage, sep = " + ")
      
      
      formula1 <- as.formula(paste0(
        Y, "~", formula_RHS, " + ", lag.econ, "| ", FE, " | 0 | module_by_state"
      ))
      flog.info("Estimating with %s as outcome with %s FE.", Y, FE)
      res1 <- felm(formula = formula1, data = all_pi_econ,
                   weights = all_pi_econ$base.sales)
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
      for(j in 4:5) {
        
        ## Create a name for estimate, se and pval of each lead
        cumul.test.est.name <- paste("cumul.lead", j, ".est", sep = "")
        cumul.test.se.name <- paste("cumul.lead", j, ".se", sep = "")
        cumul.test.pval.name <- paste("cumul.lead", j, ".pval", sep = "")
        
        ## Create the formula to compute cumulative estimate at each lead/lag
        cumul.test.form <- paste0("-", paste(paste0("F", (j-1):2, ".D.ln_sales_tax"), collapse = " - "))
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

LRdiff_res$N_obs <- nrow(all_pi_econ)
LRdiff_res$N_modules <- length(unique(all_pi_econ$product_module_code))
LRdiff_res$N_stores <- length(unique(all_pi_econ$store_code_uc))
LRdiff_res$N_counties <- uniqueN(all_pi_econ, by = c("fips_state", "fips_county"))
LRdiff_res$N_years <- uniqueN(all_pi_econ, by = c("year")) # should be 6 (we lose one because we difference)
LRdiff_res$N_county_modules <- uniqueN(all_pi_econ, by = c("fips_state", "fips_county",
                                                      "product_module_code"))
fwrite(LRdiff_res, output.results.file.econ)

### 3. Reduced Form Evidence spillovers -----------------

formula_lags <- paste0("L", 1:4, ".D.ln_statutory_tax", collapse = "+")
formula_leads <- paste0("F", 1:4, ".D.ln_statutory_tax", collapse = "+")
formula_RHS <- paste0("D.ln_statutory_tax + ", formula_lags, "+", formula_leads)

FE_opts <- c("region_by_module_by_time", "division_by_module_by_time")
outcomes <- c("D.ln_cpricei2", "D.ln_quantity3")
samples <- c("all_taxable", "all_taxexempt")


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
          lag.econ <- paste(lag.home, lag.unemp, lag.wage, sep = " + ")
          
          
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

### 4A. Cross-Sectional Estimates Retailer Data -----------------------

### Price
formula0 <- as.formula(paste0(
  "ln_cpricei2 ~ ln_sales_tax:year | module_by_time + store_by_time | 0 | state_by_module "
))

res0 <- felm(data = yearly_data,
             formula = formula0,
             weights = yearly_data$base.sales)


## attach results
flog.info("Writing results...")
res1.dt <- data.table(coef(summary(res0)), keep.rownames=T)
res1.dt[, outcome := "ln_cpricei2"]
res1.dt[, Rsq := summary(res0)$r.squared]
res1.dt[, adj.Rsq := summary(res0)$adj.r.squared]
res1.dt[, specification := "LR"]
LRdiff_res <- res1.dt ### Create table LRdiff_res in which we store all results (we start with the results we had just stored in res1.dt)
fwrite(LRdiff_res, output.results.file.crossec)  ## Write results to a csv file 



### Take linear combinations of coefficients and attach results (this is the coefficient of interest)
lc.lr0 <- paste0(cohort.weights[1], "*ln_sales_tax:year2008 + ", cohort.weights[2], "*ln_sales_tax:year2009 + ", cohort.weights[3], "*ln_sales_tax:year2010 + ", cohort.weights[4], "*ln_sales_tax:year2011 + ", cohort.weights[5], "*ln_sales_tax:year2012 + ", cohort.weights[6], "*ln_sales_tax:year2013 + ", cohort.weights[7], "*ln_sales_tax:year2014", sep = "")
lc.formula0 <- paste0(lc.lr0, " = 0", sep = "")
lc.test0 <- glht(res0, linfct = c(lc.formula0))

# Calculate the p-value
pval <- 2*(1 - pnorm(abs(coef(summary(lc.test0))[[1]]/sqrt(vcov(summary(lc.test0)))[[1]])))


lp.dt <- data.table(
  rn = "avg.ln_sales_tax",
  Estimate = coef(summary(lc.test0))[[1]],
  `Cluster s.e.` = sqrt(vcov(summary(lc.test0)))[[1]],
  `Pr(>|t|)` = pval,
  outcome = "ln_cpricei2",
  Rsq = summary(res0)$r.squared,
  adj.Rsq = summary(res0)$adj.r.squared,
  specification = "LR")
LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T) ## Merge results to LRdiff_res
fwrite(LRdiff_res, output.results.file.crossec) ## Write resulting file to a csv file




### Quantity
formula1 <- as.formula(paste0(
  "ln_quantity2 ~ ln_sales_tax:year | module_by_time + store_by_time | 0 | state_by_module "
))

res1 <- felm(data = yearly_data,
             formula = formula1,
             weights = yearly_data$base.sales)


## attach results
flog.info("Writing results...")
res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
res1.dt[, outcome := "ln_quantity2"]
res1.dt[, Rsq := summary(res1)$r.squared]
res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
res1.dt[, specification := "LR"]
LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T) ## Merge results to LRdiff_res
fwrite(LRdiff_res, output.results.file.crossec)  ## Write results to a csv file 


# Take linear combinations of coefficients
lc.formula1 <- paste0(cohort.weights[1], "*ln_sales_tax:year2008 + ", cohort.weights[2], "*ln_sales_tax:year2009 + ", cohort.weights[3], "*ln_sales_tax:year2010 + ", cohort.weights[4], "*ln_sales_tax:year2011 + ", cohort.weights[5], "*ln_sales_tax:year2012 + ", cohort.weights[6], "*ln_sales_tax:year2013 + ", cohort.weights[7], "*ln_sales_tax:year2014 = 0", sep = "")
lc.test1 <- glht(res1, linfct = c(lc.formula1))

# Calculate the p-value
pval <- 2*(1 - pnorm(abs(coef(summary(lc.test1))[[1]]/sqrt(vcov(summary(lc.test1)))[[1]])))



lp.dt <- data.table(
  rn = "avg.ln_sales_tax",
  Estimate = coef(summary(lc.test1))[[1]],
  `Cluster s.e.` = sqrt(vcov(summary(lc.test1)))[[1]],
  `Pr(>|t|)` = pval,
  outcome = "ln_quantity2",
  Rsq = summary(res1)$r.squared,
  adj.Rsq = summary(res1)$adj.r.squared,
  specification = "LR")
LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T) ## Merge results to LRdiff_res
fwrite(LRdiff_res, output.results.file.crossec) ## Write resulting file to a csv file



LRdiff_res$N_obs <- nrow(yearly_data)
LRdiff_res$N_modules <- length(unique(yearly_data$product_module_code))
LRdiff_res$N_stores <- length(unique(yearly_data$store_code_uc))
LRdiff_res$N_counties <- uniqueN(yearly_data, by = c("fips_state", "fips_county"))
LRdiff_res$N_years <- uniqueN(yearly_data, by = c("year")) 
LRdiff_res$N_county_modules <- uniqueN(yearly_data, by = c("fips_state", "fips_county",
                                                           "product_module_code"))
LRdiff_res$N_store_modules <- uniqueN(yearly_data, by = c("store_code_uc", "product_module_code"))
LRdiff_res$N_state_modules <- uniqueN(yearly_data, by = c("fips_state", "product_module_code"))

fwrite(LRdiff_res, output.results.file.crossec)





### 4B. Cross-Sectional Estimates from Consumer Panel ------------------

FE_opts <- c("income_by_group_by_time", "group_by_time")

LRdiff_res <- data.table(NULL)

for (FE in FE_opts) {
  data.est <- purchases.sample[!is.na(get(FE))]
  formula1 <- as.formula(paste0(
    "ln_expenditures ~ ln_sales_tax:year | household_by_time + ", FE, " | 0 | household_code"
  ))
  flog.info("Estimating with %s as outcome", Y)
  res1 <- felm(formula = formula1, data = data.est,
               weights = data.est$projection_factor)
  flog.info("Finished estimating with %s as outcome", Y)
  
  ## attach results
  flog.info("Writing results...")
  res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
  res1.dt[, outcome := "ln_expenditures"]
  res1.dt[, Rsq := summary(res1)$r.squared]
  res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
  res1.dt[, N.obs := nrow(data.est)]
  res1.dt[, N_hholds := uniqueN(data.est, by = c("household_code"))]
  res1.dt[, N_groups := uniqueN(data.est, by = c("product_group_code"))]
  res1.dt[, FE_d := FE]
  LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
  fwrite(LRdiff_res, output.results.file)
  
  ### Take linear combinations of coefficients and attach results (this is the coefficient of interest)
  lc.lr0 <- paste0(cohort.weights[1], "*ln_sales_tax:year2008 + ", cohort.weights[2], "*ln_sales_tax:year2009 + ", cohort.weights[3], "*ln_sales_tax:year2010 + ", cohort.weights[4], "*ln_sales_tax:year2011 + ", cohort.weights[5], "*ln_sales_tax:year2012 + ", cohort.weights[6], "*ln_sales_tax:year2013 + ", cohort.weights[7], "*ln_sales_tax:year2014", sep = "")
  lc.formula0 <- paste0(lc.lr0, " = 0", sep = "")
  lc.test0 <- glht(res1, linfct = c(lc.formula0))
  
  # Calculate the p-value
  pval <- 2*(1 - pnorm(abs(coef(summary(lc.test0))[[1]]/sqrt(vcov(summary(lc.test0)))[[1]])))
  
  
  lp.dt <- data.table(
    rn = "avg.ln_sales_tax",
    Estimate = coef(summary(lc.test0))[[1]],
    `Cluster s.e.` = sqrt(vcov(summary(lc.test0)))[[1]],
    `Pr(>|t|)` = pval,
    outcome = "ln_expenditures",
    Rsq = summary(res1)$r.squared,
    adj.Rsq = summary(res1)$adj.r.squared,
    N.obs = nrow(data.est),
    N_hholds = uniqueN(data.est, by = c("household_code")),
    N_groups = uniqueN(data.est, by = c("product_group_code")),
    FE_d = FE
  )
  LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T) ## Merge results to LRdiff_res
  fwrite(LRdiff_res, output.results.file) ## Write resulting file to a csv file
  
  
}



### 5. Two-way FE estimates and pre-trends ---------------

outcomes <- c("w.ln_cpricei2", "w.ln_quantity3", "w.ln_pricei2", "w.ln_sales")
FE_opts <- c("region_by_module_by_time", "division_by_module_by_time")

LRdiff_res <- data.table(NULL)
## Run in levels
for (Y in c(outcomes)) {
  for (FE in FE_opts) {
    
    formula1 <- as.formula(paste0(
      Y, "~ w.ln_sales_tax | ", FE, " | 0 | module_by_state"
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
    # Add summary values
    res1.dt[, Rsq := summary(res1)$r.squared]
    res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
    res1.dt[, N_obs := nrow(all_pi)]
    res1.dt[, N_modules := length(unique(all_pi$product_module_code))]
    res1.dt[, N_stores :=  length(unique(all_pi$store_code_uc))]
    res1.dt[, N_counties := uniqueN(all_pi, by = c("fips_state", "fips_county"))]
    res1.dt[, N_years := uniqueN(all_pi, by = c("year"))]
    res1.dt[, N_county_modules := uniqueN(all_pi, by = c("fips_state", "fips_county",
                                                         "product_module_code"))]
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)
    
    ## Run pre-trends
    RHS <- "ln_sales_tax"
    for (k in 2:4) {
      
      formula1 <- as.formula(paste0(
        Y, "~ F", k,".w.ln_sales_tax | ", FE, " | 0 | module_by_state"
      ))
      flog.info("Estimating pretrend %k with %s as outcome with %s FE.", Y, FE)
      res1 <- felm(formula = formula1, data = all_pi,
                   weights = all_pi$base.sales)
      flog.info("Finished estimating with %s as outcome with %s FE.", Y, FE)
      
      
      ## attach results
      flog.info("Writing results...")
      res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
      res1.dt[, outcome := Y]
      res1.dt[, controls := FE]
      # Add summary values
      res1.dt[, Rsq := summary(res1)$r.squared]
      res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
      res1.dt[, N_obs := nrow(all_pi)]
      res1.dt[, N_modules := length(unique(all_pi$product_module_code))]
      res1.dt[, N_stores :=  length(unique(all_pi$store_code_uc))]
      res1.dt[, N_counties := uniqueN(all_pi, by = c("fips_state", "fips_county"))]
      res1.dt[, N_years := uniqueN(all_pi, by = c("year"))]
      res1.dt[, N_county_modules := uniqueN(all_pi, by = c("fips_state", "fips_county",
                                                           "product_module_code"))]
      LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
      fwrite(LRdiff_res, output.results.file.TWFE)
    }
  }
}

### 6. Point identification (K = L). L= 1, ..., 5 Main estimates and Bootstrap -----------

outcomes <- c("w.ln_cpricei2", "w.ln_quantity3")
FE_opts <- c("group_region_by_module_by_time", "group_division_by_module_by_time")


LRdiff_res <- data.table(NULL)
target_res <- data.table(NULL)
## Run within
flog.info("Iteration 0")
for (n.g in 1:5) {
  
  # Create groups of initial values of tax rate
  # We use the full weighted distribution
  all_pi <- all_pi[, quantile := cut(dm.L.ln_cpricei2,
                                     breaks = quantile(dm.L.ln_cpricei2, probs = seq(0, 1, by = 1/n.g), na.rm = T, weight = base.sales),
                                     labels = 1:n.g, right = FALSE)]
  quantlab <- round(quantile(all_pi$dm.L.ln_cpricei2, 
                             probs = seq(0, 1, by = 1/n.g), na.rm = T, 
                             weight = all_pi$base.sales), digits = 4)
  # Saturate fixed effects
  all_pi[, group_region_by_module_by_time := .GRP, by = .(region_by_module_by_time, quantile)]
  all_pi[, group_division_by_module_by_time := .GRP, by = .(division_by_module_by_time, quantile)]
  
  ## Estimate RF and FS
  for (FE in FE_opts) {
    for (Y in outcomes) {
      formula1 <- as.formula(paste0(
        Y, " ~ w.ln_sales_tax:quantile | ", FE, "+ quantile"
      ))
      if (n.g == 1) { formula1 <- as.formula(paste0(Y, " ~ w.ln_sales_tax | ", FE)) }
      res1 <- felm(formula = formula1, data = all_pi,
                   weights = all_pi$base.sales)
      
      
      ## attach results
      res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
      res1.dt[, outcome := Y]
      res1.dt[, controls := FE]
      res1.dt[, n.groups := n.g]
      res1.dt[, lev := quantlab[-1]]
      res1.dt[, iter := 0]
      
      LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
      fwrite(LRdiff_res, iv.output.results.file)
    }
    
    ## Estimate IVs and retrieve in vector
    IV <- LRdiff_res[outcome == "w.ln_quantity3" & n.groups == n.g & controls == FE,][["Estimate"]]/LRdiff_res[outcome == "w.ln_cpricei2" & n.groups == n.g & controls == FE,][["Estimate"]]
    
    ## Estimate the matrix of the implied system of equations
    if (n.g > 1) {
      # Get the empirical distribution of prices by quantile
      all_pi[, base.sales.q := base.sales/sum(base.sales), by = .(quantile)]
      all_pi[, p_group := floor((dm.ln_cpricei2 - min(dm.ln_cpricei2, na.rm = T))/((max(dm.ln_cpricei2, na.rm = T)-min(dm.ln_cpricei2, na.rm = T))/500)), by = .(quantile)]
      all_pi[, p_ll := p_group*((max(dm.ln_cpricei2, na.rm = T)-min(dm.ln_cpricei2, na.rm = T))/500), by = .(quantile)]
      all_pi[, p_ll := p_ll + min(dm.ln_cpricei2, na.rm = T), by = .(quantile)]
      all_pi[, p_ul := p_ll + ((max(dm.ln_cpricei2, na.rm = T)-min(dm.ln_cpricei2, na.rm = T))/500), by = .(quantile)]
      
      ed.price.quantile <- all_pi[, .(w1 = (sum(base.sales.q))), by = .(p_ul, p_ll, quantile)]
      ed.price.quantile[, p_m := (p_ul+p_ll)/2]
      
      
      # Create the derivative of the polynomial of prices and multiplicate by weights
      for (n in 1:n.g){
        ed.price.quantile[, paste0("b",n) := (n)*w1*(p_m^(n-1))]
      }
      # Calculate integral
      gamma <- ed.price.quantile[ , lapply(.SD, sum), by = .(quantile), .SDcols = paste0("b",1:n.g)]
      gamma <- gamma[!is.na(quantile),][order(quantile)][, -c("quantile")]
      
      ## Retrieve target parameters
      beta_hat <- as.vector(solve(as.matrix(gamma))%*%(as.matrix(IV)))
      # Estimate intercept
      mean.q <- all_pi[, mean(ln_quantity3, weights = base.sales)]
      mean.p <- all_pi[, mean(dm.ln_cpricei2, weights = base.sales)]
      beta_0_hat <- mean.q - sum((beta_hat)*(mean.p^(1:n.g)))
      beta_hat <- c(beta_0_hat, beta_hat)
      
      ## Export estimated target parameters
      estimated.target <- data.table(beta_hat)
      estimated.target[, beta_n := .I-1]
      estimated.target[, n.groups := n.g]
      estimated.target[, controls := FE]
      estimated.target[, iter := 0]
      target_res <- rbind(target_res, estimated.target)
      fwrite(target_res, theta.output.results.file)
    }
  } 
}

### Start manual bootstrap
set.seed(2019)
ids <- unique(all_pi$module_by_state)

for (rep in 1:100) {
  
  flog.info("Iteration %s", rep)
  
  # Sample by block
  sampled.ids <- data.table(sample(ids, replace = T))
  setnames(sampled.ids, old= "V1", new = "module_by_state")
  
  # Merge data to actual data
  sampled.data <- merge(sampled.ids, all_pi, by = c("module_by_state") , allow.cartesian = T, all.x = T)
  
  for (n.g in 1:5) {
    
    # Create groups of initial values of tax rate
    # We use the full weighted distribution
    sampled.data <- sampled.data[, quantile := cut(dm.L.ln_cpricei2,
                                                   breaks = quantile(dm.L.ln_cpricei2, probs = seq(0, 1, by = 1/n.g), na.rm = T, weight = base.sales),
                                                   labels = 1:n.g, right = FALSE)]
    quantlab <- round(quantile(sampled.data$dm.L.ln_cpricei2, 
                               probs = seq(0, 1, by = 1/n.g), na.rm = T, 
                               weight = sampled.data$base.sales), digits = 4)
    # Saturate fixed effects
    sampled.data[, group_region_by_module_by_time := .GRP, by = .(region_by_module_by_time, quantile)]
    sampled.data[, group_division_by_module_by_time := .GRP, by = .(division_by_module_by_time, quantile)]
    
    ## Estimate RF and FS
    for (FE in FE_opts) {
      for (Y in outcomes) {
        formula1 <- as.formula(paste0(
          Y, " ~ w.ln_sales_tax:quantile | ", FE, "+ quantile"
        ))
        if (n.g == 1) { formula1 <- as.formula(paste0(Y, " ~ w.ln_sales_tax | ", FE)) }
        res1 <- felm(formula = formula1, data = sampled.data,
                     weights = sampled.data$base.sales)
        
        
        ## attach results
        res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
        res1.dt[, outcome := Y]
        res1.dt[, controls := FE]
        res1.dt[, n.groups := n.g]
        res1.dt[, lev := quantlab[-1]]
        res1.dt[, iter := rep]
        
        LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
        fwrite(LRdiff_res, iv.output.results.file)
      }
      
      ## Estimate IVs and retrieve in vector
      IV <- LRdiff_res[outcome == "w.ln_quantity3" & n.groups == n.g & controls == FE & iter == rep,][["Estimate"]]/LRdiff_res[outcome == "w.ln_cpricei2" & n.groups == n.g & controls == FE & iter == rep,][["Estimate"]]
      
      ## Estimate the matrix of the implied system of equations
      if (n.g > 1) {
        
        # Get the empirical distribution of prices by quantile
        sampled.data[, base.sales.q := base.sales/sum(base.sales), by = .(quantile)]
        sampled.data[, p_group := floor((dm.ln_cpricei2 - min(dm.ln_cpricei2, na.rm = T))/((max(dm.ln_cpricei2, na.rm = T)-min(dm.ln_cpricei2, na.rm = T))/500)), by = .(quantile)]
        sampled.data[, p_ll := p_group*((max(dm.ln_cpricei2, na.rm = T)-min(dm.ln_cpricei2, na.rm = T))/500), by = .(quantile)]
        sampled.data[, p_ll := p_ll + min(dm.ln_cpricei2, na.rm = T), by = .(quantile)]
        sampled.data[, p_ul := p_ll + ((max(dm.ln_cpricei2, na.rm = T)-min(dm.ln_cpricei2, na.rm = T))/500), by = .(quantile)]
        
        ed.price.quantile <- sampled.data[, .(w1 = (sum(base.sales.q))), by = .(p_ul, p_ll, quantile)]
        ed.price.quantile[, p_m := (p_ul+p_ll)/2]
        
        
        # Create the derivative of the polynomial of prices and multiplicate by weights
        for (n in 1:n.g){
          ed.price.quantile[, paste0("b",n) := (n)*w1*(p_m^(n-1))]
        }
        # Calculate integral
        gamma <- ed.price.quantile[ , lapply(.SD, sum), by = .(quantile), .SDcols = paste0("b",1:n.g)]
        gamma <- gamma[!is.na(quantile),][order(quantile)][, -c("quantile")]
        
        ## Retrieve target parameters
        beta_hat <- as.vector(solve(as.matrix(gamma))%*%(as.matrix(IV)))
        # Estimate intercept
        mean.q <- sampled.data[, mean(ln_quantity3, weights = base.sales)]
        mean.p <- sampled.data[, mean(dm.ln_cpricei2, weights = base.sales)]
        beta_0_hat <- mean.q - sum((beta_hat)*(mean.p^(1:n.g)))
        beta_hat <- c(beta_0_hat, beta_hat)
        
        ## Export estimated target parameters
        estimated.target <- data.table(beta_hat)
        estimated.target[, beta_n := .I-1]
        estimated.target[, n.groups := n.g]
        estimated.target[, controls := FE]
        estimated.target[, iter := rep]
        target_res <- rbind(target_res, estimated.target)
        fwrite(target_res, theta.output.results.file)
      }
    } 
  }
}

### 7. Partial identification. Extract Matrices of Weigths based on Bernestein Polynomials ------------

## Bernstein basis Function
bernstein <- function(x, k, K){
  choose(K, k) * x^k * (1 - x)^(K - k)
}

## Bernstein basis Function Derivative
d.bernstein <-function(x, k, K) {
  K*(bernstein(x, k-1, K-1) - bernstein(x, k, K-1))
}

# Re-Define de-meaned prices
min.p <- all_pi[, min(dm.ln_cpricei2)]
max.p <- all_pi[, max(dm.ln_cpricei2)]
all_pi[, r.dm.ln_cpricei2 := (dm.ln_cpricei2 - min.p)/(max.p - min.p) ]

LRdiff_res <- data.table(NULL)
pq_res <- data.table(NULL)
## Not bootstrapping!!
# To estimate the intercept
mean.q <- all_pi[, mean(ln_quantity3, weights = base.sales, na.rm = T)]
mean.p <- all_pi[, mean(r.dm.ln_cpricei2, weights = base.sales, na.rm = T)]

estimated.pq <- data.table(mean.q, mean.p, min.p, max.p)
pq_res <- rbind(pq_res, estimated.pq)
fwrite(pq_res, pq.output.results.file)
# Run others
for (n.g in 1:5) {
  
  # Create groups of initial values of tax rate
  # We use the full weighted distribution
  all_pi <- all_pi[, quantile := cut(dm.L.ln_cpricei2,
                                     breaks = quantile(dm.L.ln_cpricei2, probs = seq(0, 1, by = 1/n.g), na.rm = T, weight = base.sales),
                                     labels = 1:n.g, right = FALSE)]
  quantlab <- round(quantile(all_pi$dm.L.ln_cpricei2, 
                             probs = seq(0, 1, by = 1/n.g), na.rm = T, 
                             weight = all_pi$base.sales), digits = 4)
  
  ## Do partial identification
  ## Estimate the matrix of the implied system of equations. For each possible polynomial degree and compute 
  # Get the empirical distribution of prices by quantile
  all_pi[, base.sales.q := base.sales/sum(base.sales), by = .(quantile)]
  all_pi[, p_group := floor((r.dm.ln_cpricei2 - min(r.dm.ln_cpricei2, na.rm = T))/((max(r.dm.ln_cpricei2, na.rm = T)-min(r.dm.ln_cpricei2, na.rm = T))/100)), by = .(quantile)]
  all_pi[, p_ll := p_group*((max(r.dm.ln_cpricei2, na.rm = T)-min(r.dm.ln_cpricei2, na.rm = T))/100), by = .(quantile)]
  all_pi[, p_ll := p_ll + min(r.dm.ln_cpricei2, na.rm = T), by = .(quantile)]
  all_pi[, p_ul := p_ll + ((max(r.dm.ln_cpricei2, na.rm = T)-min(r.dm.ln_cpricei2, na.rm = T))/100), by = .(quantile)]
  
  ed.price.quantile <- all_pi[, .(w1 = (sum(base.sales.q))), by = .(p_ul, p_ll, quantile)]
  ed.price.quantile[, p_m := (p_ul+p_ll)/2]
  
  for (K in (n.g):10) {
    
    # Create the derivative of the polynomial of prices and multiplicate by weights
    for (n in 0:(K-1)){
      ed.price.quantile[, paste0("b",n) := w1*(bernstein(p_m,n,K-1))]
    }
    
    # Calculate integral
    gamma <- ed.price.quantile[ , lapply(.SD, sum), by = .(quantile), .SDcols = paste0("b",0:(K-1))]
    gamma <- gamma[!is.na(quantile),][order(quantile)][, -c("quantile")]
    
    # Export Calculation
    gamma[, n.groups := n.g]
    gamma[, iter := 0]
    
    ## Read Previous and write
    theta.output.results.file <- paste0(theta.bernestein, K,"_bern.csv")
    
    if (n.g == 1) {
      fwrite(gamma, theta.output.results.file)
    } else {
      previous.data <- fread(theta.output.results.file)
      previous.data <- rbind(previous.data, gamma)
      fwrite(previous.data, theta.output.results.file)
    }
    
  }
  
}



### 8. Calculate Bounds on elasticities and demand. This Needs to be done in local computer because Gurobi is not installed on the server ----------
library(Matrix)
library(gurobi)
library(zoo)
library(tidyverse)
library(stringr)

bernstein <- function(x, k, K){
  choose(K, k) * x^k * (1 - x)^(K - k)
}
int.bernstein <- function(x,k,K) {
  
  result <- 0
  for (j in (k+1):(K+1)) {
    result <- result + bernstein(x,j,K+1)
  }
  return(result/(K+1))
  
}



## 1. Load IVs
res.ivs <- fread(theta.output.results.file)
## Keep iterest results
res.ivs <- res.ivs[iter == 0 & controls == "group_division_by_module_by_time"]
# dcast outcomes
res.ivs <- dcast(res.ivs, n.groups + lev ~ outcome,  fun=sum, value.var = c("Estimate"))
# Calculate IV
res.ivs[, estimate := w.ln_quantity3/w.ln_cpricei2]
# Order appropiately
res.ivs <- res.ivs[order(n.groups, lev)]

## 2. Load average p and q's 
res.pq <- fread(pq.output.results.file)
res.pq[, iter := .I - 1]
res.pq <- res.pq[iter == 0]
p.bar <- res.pq[["mean.p"]]
q.bar <- res.pq[["mean.q"]]
p.min <- res.pq[["min.p"]]
p.max <- res.pq[["max.p"]]

## 2. range of p to bound elasticity
prices <- seq(-.25, .25, 0.001)

## 3. Output files
# Up

## 4. Set up Optimization Parameters
# This options will make Gurobi think more about numerical issues
params <- list()
params$NumericFocus <- 3
params$ScaleFlag <- 2
params$Method <- 1
params$Presolve <- 0

## 5. Set up Tolerance
tolerance <- 1e-6
params$FeasibilityTol <- tolerance

## 6. Start Loop for K
elasticity <- data.table(NULL)
mincriteria <- data.table(NULL)
for (K in 2:10) {
  
  ## 6.1. Load Matrix of gamma (this extrictly depends on K since the basis change)
  in.file <- paste0(theta.bernestein, K,"_bern.csv")
  gamma.full.data <- fread(in.file)
  
  ## 6.2 Restrict gamma file. Constant across p
  gamma <- gamma.full.data[ iter == 0][, c(paste0("b", 0:(K-1)), "n.groups"), with = F]             ## For elasticity
  
  ## 6.3 Start Loop at number of groups
  for (D in unique(gamma$n.groups)) {
    
    ## A1. Build the constraints matrix 
    constr <- as.matrix(gamma[n.groups == D][, -c("n.groups")])   ## For elasticity
    constr.dd <- cbind(constr,0)                                  ## For demand
    
    ## A2. Build RHS
    RHS <- res.ivs[n.groups == D][["estimate"]]    
    if (is_empty(RHS)) {RHS <- -0.5605} ## Manually include D=1
    
    ## A3. Set monotonicity of bernstein polynomials. Elasticity < 0 
    constr.mono <- Diagonal(ncol(constr))              ## For elasticity
    constr.mono.dd <- cbind(constr.mono,0)             ## For demand
    RHS.mono <- rep(0, K)
    
    
    ## A4. Get intercept constraint. Demand
    constr.inter <- rep(0, K)
    for (i in 0:(K-1)) {
      constr.inter[i+1] <- -int.bernstein(p.bar,i,K-1)
    }
    constr.inter <- t(as.matrix(c(constr.inter,1))) ## Add the intercept and transform to matrix
    RHS.inter <- q.bar
    
    ## A4. If D > 1 we have to estimate the minimum criterion: min sum_s abs(gamma_s(theta) - beta_s)  
    # To do this I have to define a set of auxiliar variables a_s such that: 
    # a_s + gamma_s >= beta_s and a_s - gamma_s(theta) >= - beta_s
    # And I minimize over them - thetas are now 0s in the objective function:
    # obj is sum_s (1*a_s) + 0 *theta
    # Shape constraints still hold
    if (D > 1) {
      
      ## Define the problem
      min.crit <- list() 
      min.crit$A <- rbind(cbind(Diagonal(nrow(constr)), constr), 
                          cbind(Diagonal(nrow(constr)), -constr),
                          cbind(matrix(0, nrow(constr.mono), nrow(constr)), constr.mono)
      )
      min.crit$rhs <- c(RHS, -RHS, RHS.mono)
      min.crit$sense <- c( rep('>=', 2*length(RHS)), rep('<=',K))
      min.crit$obj <- c(rep(1, nrow(constr)), rep(0, ncol(constr)))
      min.crit$lb <- c(rep(0, nrow(constr)), rep(-Inf, ncol(constr)))  
      min.crit$modelsense <- 'min'
      
      ## Solve for the minimum criteria
      min.crit.sol <- gurobi(min.crit)
      
      ## Get the minimum criterion estimated and modify the setting of the problem
      min.criteria <- min.crit.sol$objval
      tuning <- min.criteria*(1 + tolerance)
      
      ## Export the min creterion for each case to check
      mincriteria <- rbind(mincriteria, data.table(min.criteria, D, K))
    }
    
    ## A5. Start loop at a given price
    for (p in prices) {
      
      ## B0. Normalize price
      n.p <- (p - round(p.min, 3))/(round(p.max, 3) - round(p.min, 3))
      
      
      ## B1. Specify objective function. Elasticity at p
      objec <- rep(0, K)
      for (i in 0:(K-1)) {
        objec[i+1] <- bernstein(n.p,i,K-1)
        if (is.nan(objec[i+1])) {objec[i+1] <- 0}
      }
      
      ## B2. Set-Up LP with all the inputs created. Elasticity
      model <- list()                                        ## Create
      model$A <- rbind(constr, constr.mono)                  ## Constraints
      model$rhs <- c(RHS, RHS.mono)                          ## RHS
      model$sense <- c(rep('=', length(RHS)), rep('<=',K))   ## Equalities
      model$obj <- objec                                     ## Objective function
      model$lb <- rep(-Inf, length(objec))                   ## Let theta be negative
      
      
      ## B2.A. If D > 1 we have to modify the problem to allow for the inequalities up to the estimated tuning parameter
      if (D > 1) {
        
        model$A <- rbind(constr, constr, constr.mono)                                  ## Constraints
        model$rhs <- c(c(RHS + tuning), c(RHS - tuning), RHS.mono)                     ## RHS
        model$sense <- c(rep('<=', length(RHS)), rep('>=', length(RHS)), rep('<=',K))  ## Equalities
        
      }
      
      ## B3. Upper bound. Elasticity
      model$modelsense <- 'max'
      result <- gurobi(model, params)
      elas.up <- result$objval
      theta.up <- result$x
      if(is.null(elas.up) | is_empty(elas.up)) {elas.up <- NA}
      
      
      ## B4. Lower bound. Elasticity
      model$modelsense <- 'min'
      result <- gurobi(model, params)
      elas.down <- result$objval
      theta.down <- result$x 
      if(is.null(elas.down) | is_empty(elas.down)) {elas.down <- NA}
      
      ## B5. Specify objective function. Demand at p
      objec <- rep(0, K)
      for (i in 0:(K-1)) {
        objec[i+1] <- int.bernstein(n.p,i,K-1)
        if (is.nan(objec[i+1])) {objec[i+1] <- 0}
      }
      objec <- c(objec, 1) ## Add the intercept
      
      
      ## B6. Set-Up LP with all the inputs created. Demand
      model <- list()                                            ## Create
      model$A <- rbind(constr.dd, constr.mono.dd, constr.inter)  ## Constraints
      model$rhs <- c(RHS, RHS.mono, RHS.inter)                   ## RHS
      model$sense <- c(rep('=', length(RHS)), rep('<=',K), '=')  ## Equalities
      model$obj <- objec                                         ## Objective function
      model$lb <- rep(-Inf, length(objec))                       ## Let theta be negative
      
      
      ## B6.A. If D > 1 we have to modify the problem to allow for the inequalities up to the estimated tuning parameter
      if (D > 1) {
        
        model$A <- rbind(constr.dd, constr.dd, constr.mono.dd, constr.inter)                   ## Constraints
        model$rhs <- c(c(RHS + tuning), c(RHS - tuning), RHS.mono, RHS.inter)               ## RHS
        model$sense <- c(rep('<=', length(RHS)), rep('>=', length(RHS)), rep('<=',K), '=')  ## Equalities
        
      }
      
      ## B3. Upper bound. Elasticity
      model$modelsense <- 'max'
      result <- gurobi(model, params)
      dd.up <- result$objval
      theta.up <- result$x
      if(is.null(dd.up) | is_empty(dd.up)) {dd.up <- NA}
      
      
      ## B4. Lower bound. Elasticity
      model$modelsense <- 'min'
      result <- gurobi(model, params)
      dd.down <- result$objval
      theta.down <- result$x 
      if(is.null(dd.down) | is_empty(dd.down)) {dd.down <- NA}
      
      
      ## B5. Save. Elasticity bounds
      elasticity.p <- data.table(elas.down, elas.up, dd.down, dd.up, p, D , K)
      elasticity <- rbind(elasticity, elasticity.p)
      
    }
  }
}

## 7. Export Results
fwrite(elasticity, out.file.elast)
fwrite(mincriteria, out.file.mc)


### 9. Extract average elasticities previously estimated ------------

## Point identified case
res.all.full <- fread(theta.output.results.file)
res.all <- res.all.full[iter == 0 & controls == "group_division_by_module_by_time"]


linear.elas <- -0.5760/1.0508
quad.elas <- res.all[n.groups ==2][["beta_hat"]][-1]*c(1,2)
cubic.elas <- res.all[n.groups ==3][["beta_hat"]][-1]*c(1,2,3)
tetra.elas <- res.all[n.groups ==4][["beta_hat"]][-1]**c(1,2,3,4)

bounds <- fread(out.file.elast)

## Keep all bounds for d <=5 and K <= 7
bounds <- bounds[ D <= 5 & K %in% c(2,7)]

## dcast data (long to wide)
bounds <- dcast(bounds, "p + D ~ K", value.var = c("elas.down", "elas.up"), fun = sum)

#### Calculate eslaticities

## Keep interest states
#elasticities <- all_pi[ fips_state %in% states]
elasticities <- all_pi

## Keep only taxable items as those are whose responses we care
elasticities <- elasticities[ln_sales_tax > 0]

## Need to round to the third decimal point to match the bounds we have estimated
elasticities[, p := round(dm.ln_cpricei2, 3)]

## Merge estimated bounds
elasticities <- merge(elasticities, bounds, by = "p", allow.cartesian=T)

## Calculate all elasticities
elasticities <- elasticities[, .( av.elas_1 = linear.elas,
                                  av.elas_2 = weighted.mean(quad.elas[1] + quad.elas[2]*dm.ln_cpricei2, w = base.sales),
                                  av.elas_3 = weighted.mean(cubic.elas[1] + cubic.elas[2]*dm.ln_cpricei2 + cubic.elas[3]*dm.ln_cpricei2^2, w = base.sales),
                                  av.elas_4 = weighted.mean(tetra.elas[1] + tetra.elas[2]*dm.ln_cpricei2 + tetra.elas[3]*dm.ln_cpricei2^2 + tetra.elas[4]*dm.ln_cpricei2^3, w = base.sales),
                                  av.elas.down_2 = weighted.mean(elas.down_2 , w = base.sales),
                                  av.elas.down_3 = weighted.mean(elas.down_3 , w = base.sales),
                                  av.elas.down_4 = weighted.mean(elas.down_4 , w = base.sales),
                                  av.elas.down_5 = weighted.mean(elas.down_5 , w = base.sales),
                                  av.elas.down_6 = weighted.mean(elas.down_6 , w = base.sales),
                                  av.elas.down_7 = weighted.mean(elas.down_7 , w = base.sales),
                                  av.elas.up_2 = weighted.mean(elas.up_2 , w = base.sales),
                                  av.elas.up_3 = weighted.mean(elas.up_3 , w = base.sales),
                                  av.elas.up_4 = weighted.mean(elas.up_4 , w = base.sales),
                                  av.elas.up_5 = weighted.mean(elas.up_5 , w = base.sales),
                                  av.elas.up_6 = weighted.mean(elas.up_6 , w = base.sales),
                                  av.elas.up_7 = weighted.mean(elas.up_7 , w = base.sales),
                                  N = .N
) , by = .(fips_state, D)]

## Export results
fwrite(elasticities, output.table.avelas)


### 10. Repeat estimation for partial identification but with demand under imperfect salience ------------

#### IVs

## Options
FE_opts <- c("region_by_module_by_time", "division_by_module_by_time")


LRdiff_res <- data.table(NULL)
target_res <- data.table(NULL)
## Loop across sigma 
for (sig in c(0.25, 0.5, 0.75, 1)) {
  
  outcomes <- c(paste0("w.ln_cpricei2_sig",sig), "w.ln_quantity3")
  ## cut the tails (keep between 1st and 99th percentile)
  pct1 <- quantile(all_pi[[paste0("dm.ln_cpricei2_sig", sig)]], probs = 0.01, na.rm = T, weight=base.sales)
  pct99 <- quantile(all_pi[[paste0("dm.ln_cpricei2_sig", sig)]], probs = 0.99, na.rm = T, weight=base.sales)
  all_pi_est <- all_pi[(get(paste0("dm.ln_cpricei2_sig", sig)) > pct1 & get(paste0("dm.ln_cpricei2_sig", sig)) < pct99),]
  
  ## Full sample estimates
  for (FE in FE_opts) {
    ## Full sample IV
    formula1 <- as.formula(paste0(
      "w.ln_quantity3 ~ 0 | ", FE, " | (w.ln_cpricei2_sig", sig," ~ w.ln_sales_tax) | module_by_state"
    ))
    res1 <- felm(formula = formula1, data = all_pi_est,
                 weights = all_pi_est$base.sales)
    
    
    ## attach results
    res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
    res1.dt[, outcome := "IV"]
    res1.dt[, controls := FE]
    res1.dt[, lev := 100]
    res1.dt[, n.groups := 1]
    res1.dt[, sigma := sig]
    
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, iv.output.results.file)
    ## Full sample passthrough
    formula1 <- as.formula(paste0(
      "w.ln_cpricei2_sig", sig," ~ w.ln_sales_tax | ", FE, " | 0 | module_by_state"
    ))
    res1 <- felm(formula = formula1, data = all_pi_est,
                 weights = all_pi_est$base.sales)
    
    
    ## attach results
    res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
    res1.dt[, outcome := "rho"]
    res1.dt[, controls := FE]
    res1.dt[, lev := 100]
    res1.dt[, n.groups := 1]
    res1.dt[, sigma := sig]
    
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, iv.output.salience.results.file)      
  }
  ## Demand for K=L=2
  # Create groups of initial values of tax rate
  # We use the full weighted distribution
  all_pi_est <- all_pi_est[, quantile := cut(get(paste0("dm.L.ln_cpricei2_sig", sig)),
                                             breaks = quantile(get(paste0("dm.L.ln_cpricei2_sig", sig)), probs = seq(0, 1, by = 1/2), na.rm = T, weight = base.sales),
                                             labels = 1:2, right = FALSE)]
  quantlab <- round(quantile(all_pi_est[[paste0("dm.L.ln_cpricei2_sig", sig)]], 
                             probs = seq(0, 1, by = 1/2), na.rm = T, 
                             weight = all_pi_est$base.sales), digits = 4)
  # Saturate fixed effects
  all_pi_est[, group_region_by_module_by_time := .GRP, by = .(region_by_module_by_time, quantile)]
  all_pi_est[, group_division_by_module_by_time := .GRP, by = .(division_by_module_by_time, quantile)]
  
  ## Estimate RF and FS
  for (FE in FE_opts) {
    for (Y in outcomes) {
      formula1 <- as.formula(paste0(
        Y, " ~ w.ln_sales_tax:quantile | group_", FE, "+ quantile"
      ))
      res1 <- felm(formula = formula1, data = all_pi_est,
                   weights = all_pi_est$base.sales)
      
      
      ## attach results
      res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
      res1.dt[, outcome := Y]
      res1.dt[, controls := FE]
      res1.dt[, n.groups := 2]
      res1.dt[, lev := quantlab[-1]]
      res1.dt[, sigma := sig]
      
      LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
      fwrite(LRdiff_res, iv.output.salience.results.file)
    }
    
    ## Estimate IVs and retrieve in vector
    
    IV <- LRdiff_res[outcome == "w.ln_quantity3" & n.groups == 2 & controls == FE & sigma == sig,][["Estimate"]]/LRdiff_res[outcome == paste0("w.ln_cpricei2_sig",sig) & n.groups == 2 & controls == FE & sigma == sig,][["Estimate"]]
    
    ## Estimate the matrix of the implied system of equations
    # Get the empirical distribution of prices by quantile
    all_pi_est[, base.sales.q := base.sales/sum(base.sales), by = .(quantile)]
    all_pi_est[, p_group := floor((get(paste0("dm.ln_cpricei2_sig", sig)) - min(get(paste0("dm.ln_cpricei2_sig", sig)), na.rm = T))/((max(get(paste0("dm.ln_cpricei2_sig", sig)), na.rm = T)-min(get(paste0("dm.ln_cpricei2_sig", sig)), na.rm = T))/500)), by = .(quantile)]
    all_pi_est[, p_ll := p_group*((max(get(paste0("dm.ln_cpricei2_sig", sig)), na.rm = T)-min(get(paste0("dm.ln_cpricei2_sig", sig)), na.rm = T))/500), by = .(quantile)]
    all_pi_est[, p_ll := p_ll + min(get(paste0("dm.ln_cpricei2_sig", sig)), na.rm = T), by = .(quantile)]
    all_pi_est[, p_ul := p_ll + ((max(get(paste0("dm.ln_cpricei2_sig", sig)), na.rm = T)-min(get(paste0("dm.ln_cpricei2_sig", sig)), na.rm = T))/500), by = .(quantile)]
    
    ed.price.quantile <- all_pi_est[, .(w1 = (sum(base.sales.q))), by = .(p_ul, p_ll, quantile)]
    ed.price.quantile[, p_m := (p_ul+p_ll)/2]
    
    
    # Create the derivative of the polynomial of prices and multiplicate by weights
    for (n in 1:2){
      ed.price.quantile[, paste0("b",n) := (n)*w1*(p_m^(n-1))]
    }
    # Calculate integral
    gamma <- ed.price.quantile[ , lapply(.SD, sum), by = .(quantile), .SDcols = paste0("b",1:2)]
    gamma <- gamma[!is.na(quantile),][order(quantile)][, -c("quantile")]
    
    ## Retrieve target parameters
    beta_hat <- as.vector(solve(as.matrix(gamma))%*%(as.matrix(IV)))
    # Estimate intercept
    mean.q <- all_pi_est[, mean(ln_quantity3, weights = base.sales)]
    mean.p <- all_pi_est[, mean(get(paste0("dm.ln_cpricei2_sig", sig)), weights = base.sales)]
    beta_0_hat <- mean.q - sum((beta_hat)*(mean.p^(1:2)))
    beta_hat <- c(beta_0_hat, beta_hat)
    
    ## Export estimated target parameters
    estimated.target <- data.table(beta_hat)
    estimated.target[, beta_n := .I-1]
    estimated.target[, n.groups := 2]
    estimated.target[, controls := FE]
    estimated.target[, sigma := sig]
    target_res <- rbind(target_res, estimated.target)
    fwrite(target_res, theta.output.salience.results.file)
    
  } 
  
}


### Matrices for different types of extrapolation supports

## Loop across sigmas
LRdiff_res <- data.table(NULL)
pq_res <- data.table(NULL)
for (sig in c(0.25, 0.5, 0.75, 1)) {
  
  ## cut the tails (keep between 1st and 99th percentile)
  pct1 <- quantile(all_pi[[paste0("dm.ln_cpricei2_sig", sig)]], probs = 0.01, na.rm = T, weight=base.sales)
  pct99 <- quantile(all_pi[[paste0("dm.ln_cpricei2_sig", sig)]], probs = 0.99, na.rm = T, weight=base.sales)
  all_pi_est <- all_pi[(get(paste0("dm.ln_cpricei2_sig", sig)) > pct1 & get(paste0("dm.ln_cpricei2_sig", sig)) < pct99),]
  
  ###### Original Range
  extrap <- "Original"
  ## Normalize
  min.p.or <- min.p <- all_pi_est[, min(get(paste0("dm.ln_cpricei2_sig", sig)))]
  max.p.or <- max.p <- all_pi_est[, max(get(paste0("dm.ln_cpricei2_sig", sig)))]
  all_pi_est[, r.dm.ln_cpricei2 := (get(paste0("dm.ln_cpricei2_sig", sig)) - min.p)/(max.p - min.p)]
  
  ## Export values to re-estimate the intercept
  mean.q <- all_pi_est[, mean(ln_quantity3, weights = base.sales, na.rm = T)]
  mean.p <- all_pi_est[, mean(r.dm.ln_cpricei2, weights = base.sales, na.rm = T)]
  
  estimated.pq <- data.table(mean.q, mean.p, min.p, max.p, sigma = sig, extrap)
  pq_res <- rbind(pq_res, estimated.pq)
  fwrite(pq_res, pq.output.salience.results.file)
  
  for (n.g in 1:2) {
    
    
    # Create groups of initial values of tax rate
    # We use the full weighted distribution
    all_pi_est <- all_pi_est[, quantile := cut(get(paste0("dm.L.ln_cpricei2_sig", sig)),
                                               breaks = quantile(get(paste0("dm.L.ln_cpricei2_sig", sig)), probs = seq(0, 1, by = 1/n.g), na.rm = T, weight = base.sales),
                                               labels = 1:n.g, right = FALSE)]
    quantlab <- round(quantile(all_pi_est[[paste0("dm.L.ln_cpricei2_sig", sig)]], 
                               probs = seq(0, 1, by = 1/n.g), na.rm = T, 
                               weight = all_pi_est$base.sales), digits = 4)
    
    ## Do partial identification
    ## Estimate the matrix of the implied system of equations. For each possible polynomial degree and compute 
    # Get the empirical distribution of prices by quantile
    all_pi_est[, base.sales.q := base.sales/sum(base.sales), by = .(quantile)]
    all_pi_est[, p_group := floor((r.dm.ln_cpricei2 - min(r.dm.ln_cpricei2, na.rm = T))/((max(r.dm.ln_cpricei2, na.rm = T)-min(r.dm.ln_cpricei2, na.rm = T))/100)), by = .(quantile)]
    all_pi_est[, p_ll := p_group*((max(r.dm.ln_cpricei2, na.rm = T)-min(r.dm.ln_cpricei2, na.rm = T))/100), by = .(quantile)]
    all_pi_est[, p_ll := p_ll + min(r.dm.ln_cpricei2, na.rm = T), by = .(quantile)]
    all_pi_est[, p_ul := p_ll + ((max(r.dm.ln_cpricei2, na.rm = T)-min(r.dm.ln_cpricei2, na.rm = T))/100), by = .(quantile)]
    
    ed.price.quantile <- all_pi_est[, .(w1 = (sum(base.sales.q))), by = .(p_ul, p_ll, quantile)]
    ed.price.quantile[, p_m := (p_ul+p_ll)/2]
    
    #### Matrices of Polynomials for Elasticity: elasticity is itself a bernstein Polynomial
    for (K in (n.g):10) {
      
      if (K>1){
        # Create the derivative of the polynomial of prices and multiplicate by weights
        for (n in 0:(K-1)){
          ed.price.quantile[, paste0("b",n) := w1*(bernstein(p_m,n,K-1))]
        }
        
        # Calculate integral
        gamma <- ed.price.quantile[ , lapply(.SD, sum), by = .(quantile), .SDcols = paste0("b",0:(K-1))]
        gamma <- gamma[!is.na(quantile),][order(quantile)][, -c("quantile")]
        
        # Export Calculation
        gamma[, n.groups := n.g]
        gamma[, sigma := sig]
        gamma[, extrap := "Original"]
        
        ## Read Previous and write
        theta.output.results.file <- paste0(theta.berstein.sal, K,"_bern.csv")
        
        if (n.g == 1 & sig == 0.25) {
          fwrite(gamma, theta.output.results.file)
        } else {
          previous.data <- fread(theta.output.results.file)
          previous.data <- rbind(previous.data, gamma)
          fwrite(previous.data, theta.output.results.file)
        }
      }
    }
  }
  
  
  ##### No tax Case
  extrap <- "No Tax"
  all_pi_est[, ex_p := get(paste0("dm.ln_cpricei2_sig", sig)) - ln_sales_tax]
  
  ## Define re-scaled prices to use Bernstein polynomials in that range
  min.p <- min(all_pi_est[, min(ex_p)], min.p.or)
  max.p <- max(all_pi_est[, max(ex_p)], max.p.or)
  ## Normalize
  all_pi_est[, r.dm.ln_cpricei2 := (get(paste0("dm.ln_cpricei2_sig", sig)) - min.p)/(max.p - min.p)]
  
  ## Export values to re-estimate the intercept
  mean.q <- all_pi_est[, mean(ln_quantity3, weights = base.sales, na.rm = T)]
  mean.p <- all_pi_est[, mean(r.dm.ln_cpricei2, weights = base.sales, na.rm = T)]
  
  estimated.pq <- data.table(mean.q, mean.p, min.p, max.p, sigma = sig, extrap)
  pq_res <- rbind(pq_res, estimated.pq)
  fwrite(pq_res, pq.output.salience.results.file)
  
  for (n.g in 1:2) {
    
    
    # Create groups of initial values of tax rate
    # We use the full weighted distribution
    all_pi_est <- all_pi_est[, quantile := cut(get(paste0("dm.L.ln_cpricei2_sig", sig)),
                                               breaks = quantile(get(paste0("dm.L.ln_cpricei2_sig", sig)), probs = seq(0, 1, by = 1/n.g), na.rm = T, weight = base.sales),
                                               labels = 1:n.g, right = FALSE)]
    quantlab <- round(quantile(all_pi_est[[paste0("dm.L.ln_cpricei2_sig", sig)]], 
                               probs = seq(0, 1, by = 1/n.g), na.rm = T, 
                               weight = all_pi_est$base.sales), digits = 4)
    
    ## Do partial identification
    ## Estimate the matrix of the implied system of equations. For each possible polynomial degree and compute 
    # Get the empirical distribution of prices by quantile
    all_pi_est[, base.sales.q := base.sales/sum(base.sales), by = .(quantile)]
    all_pi_est[, p_group := floor((r.dm.ln_cpricei2 - min(r.dm.ln_cpricei2, na.rm = T))/((max(r.dm.ln_cpricei2, na.rm = T)-min(r.dm.ln_cpricei2, na.rm = T))/100)), by = .(quantile)]
    all_pi_est[, p_ll := p_group*((max(r.dm.ln_cpricei2, na.rm = T)-min(r.dm.ln_cpricei2, na.rm = T))/100), by = .(quantile)]
    all_pi_est[, p_ll := p_ll + min(r.dm.ln_cpricei2, na.rm = T), by = .(quantile)]
    all_pi_est[, p_ul := p_ll + ((max(r.dm.ln_cpricei2, na.rm = T)-min(r.dm.ln_cpricei2, na.rm = T))/100), by = .(quantile)]
    
    ed.price.quantile <- all_pi_est[, .(w1 = (sum(base.sales.q))), by = .(p_ul, p_ll, quantile)]
    ed.price.quantile[, p_m := (p_ul+p_ll)/2]
    
    #### Matrices of Polynomials for Elasticity: elasticity is itself a bernstein Polynomial
    for (K in (n.g):10) {
      
      if (K>1){
        # Create the derivative of the polynomial of prices and multiplicate by weights
        for (n in 0:(K-1)){
          ed.price.quantile[, paste0("b",n) := w1*(bernstein(p_m,n,K-1))]
        }
        
        # Calculate integral
        gamma <- ed.price.quantile[ , lapply(.SD, sum), by = .(quantile), .SDcols = paste0("b",0:(K-1))]
        gamma <- gamma[!is.na(quantile),][order(quantile)][, -c("quantile")]
        
        # Export Calculation
        gamma[, n.groups := n.g]
        gamma[, sigma := sig]
        gamma[, extrap := "No Tax"]
        
        ## Read Previous and write
        theta.output.results.file <- paste0(theta.berstein.sal, K,"_bern.csv")
        previous.data <- fread(theta.output.results.file)
        previous.data <- rbind(previous.data, gamma)
        fwrite(previous.data, theta.output.results.file)
      }
    }
  }
  
  
  ##### Plus 5 range case
  extrap <- "plus 5 Tax"
  all_pi_est[, ex_p := get(paste0("dm.ln_cpricei2_sig", sig)) + log(1+0.05)]
  
  ## Define re-scaled prices to use Bernstein polynomials in that range
  min.p <- min(all_pi_est[, min(ex_p)], min.p.or)
  max.p <- max(all_pi_est[, max(ex_p)], max.p.or)
  ## Normalize
  all_pi_est[, r.dm.ln_cpricei2 := (get(paste0("dm.ln_cpricei2_sig", sig)) - min.p)/(max.p - min.p)]
  
  ## Export values to re-estimate the intercept
  mean.q <- all_pi_est[, mean(ln_quantity3, weights = base.sales, na.rm = T)]
  mean.p <- all_pi_est[, mean(r.dm.ln_cpricei2, weights = base.sales, na.rm = T)]
  
  estimated.pq <- data.table(mean.q, mean.p, min.p, max.p, sigma = sig, extrap)
  pq_res <- rbind(pq_res, estimated.pq)
  fwrite(pq_res, pq.output.salience.results.file)
  
  for (n.g in 1:2) {
    
    
    # Create groups of initial values of tax rate
    # We use the full weighted distribution
    all_pi_est <- all_pi_est[, quantile := cut(get(paste0("dm.L.ln_cpricei2_sig", sig)),
                                               breaks = quantile(get(paste0("dm.L.ln_cpricei2_sig", sig)), probs = seq(0, 1, by = 1/n.g), na.rm = T, weight = base.sales),
                                               labels = 1:n.g, right = FALSE)]
    quantlab <- round(quantile(all_pi_est[[paste0("dm.L.ln_cpricei2_sig", sig)]], 
                               probs = seq(0, 1, by = 1/n.g), na.rm = T, 
                               weight = all_pi_est$base.sales), digits = 4)
    
    ## Do partial identification
    ## Estimate the matrix of the implied system of equations. For each possible polynomial degree and compute 
    # Get the empirical distribution of prices by quantile
    all_pi_est[, base.sales.q := base.sales/sum(base.sales), by = .(quantile)]
    all_pi_est[, p_group := floor((r.dm.ln_cpricei2 - min(r.dm.ln_cpricei2, na.rm = T))/((max(r.dm.ln_cpricei2, na.rm = T)-min(r.dm.ln_cpricei2, na.rm = T))/100)), by = .(quantile)]
    all_pi_est[, p_ll := p_group*((max(r.dm.ln_cpricei2, na.rm = T)-min(r.dm.ln_cpricei2, na.rm = T))/100), by = .(quantile)]
    all_pi_est[, p_ll := p_ll + min(r.dm.ln_cpricei2, na.rm = T), by = .(quantile)]
    all_pi_est[, p_ul := p_ll + ((max(r.dm.ln_cpricei2, na.rm = T)-min(r.dm.ln_cpricei2, na.rm = T))/100), by = .(quantile)]
    
    ed.price.quantile <- all_pi_est[, .(w1 = (sum(base.sales.q))), by = .(p_ul, p_ll, quantile)]
    ed.price.quantile[, p_m := (p_ul+p_ll)/2]
    
    #### Matrices of Polynomials for Elasticity: elasticity is itself a bernstein Polynomial
    for (K in (n.g):10) {
      
      if (K>1){
        # Create the derivative of the polynomial of prices and multiplicate by weights
        for (n in 0:(K-1)){
          ed.price.quantile[, paste0("b",n) := w1*(bernstein(p_m,n,K-1))]
        }
        
        # Calculate integral
        gamma <- ed.price.quantile[ , lapply(.SD, sum), by = .(quantile), .SDcols = paste0("b",0:(K-1))]
        gamma <- gamma[!is.na(quantile),][order(quantile)][, -c("quantile")]
        
        # Export Calculation
        gamma[, n.groups := n.g]
        gamma[, sigma := sig]
        gamma[, extrap := "plus 5 Tax"]
        
        ## Read Previous and write
        theta.output.results.file <- paste0(theta.berstein.sal, K,"_bern.csv")
        previous.data <- fread(theta.output.results.file)
        previous.data <- rbind(previous.data, gamma)
        fwrite(previous.data, theta.output.results.file)
        
      }
    }
  }
  
  
}


### 11. Estimate Average conduct parameter under salience   --------------------------------

## function to identify the asymptote
asymptote.fun <- function(theta, q1, q2, es) {
  if (is.infinite(es)) {
    return(q1*(q1 + theta) - theta*q2)
  } else { 
    return((q1 + theta)*(es*q1 - 1) - theta*q2*es)
  }
}

## function to solve for the root
pass.through.eq <- function(theta, q1, q2, es, rho){
  if (is.infinite(es)) {
    return(rho - (q1*(q1 + theta))/(q1*(q1 + theta) - theta*q2))
  } else {
    return(rho - (es*q1*(q1 + theta))/((q1 + theta)*(es*q1 - 1) - theta*q2*es))
  }
}


## Function that directly solves for theta
theta.direct <- function(q1, q2, es, rho, sigma) {
  ems.inv <- (q1-q2)/(q1^2)
  
  if (is.infinite(es)) {
    return((1-rho)/(rho*(ems.inv)-sigma/q1-(1-sigma)*ems.inv))
  } else {
    return((1-(1-sigma)*q1/es-rho*(1-q1/es))/(rho*(ems.inv-1/es)-sigma/q1+(1-sigma)*(1/es-ems.inv)))
  }
}


### Set-up previous estimates

#### passthourghs
rho.old <- fread(iv.output.salience.results.file)
rho.old <- rho.old[ outcome == "rho" & controls == "division_by_module_by_time",]

## Betas estimation
betas.old <- fread(theta.output.salience.results.file)
betas.old <- betas.old[controls == "division_by_module_by_time",]


### Manually incorporate values needed
epsilon <- 0.0000001
ed <- 0.54811
sol <- c(0.02, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.6459489, 0.7, 0.8, 0.9, 1)
val.es <- sol*ed / (1- sol)


results <- data.table(NULL)
###  Loop across values of sigma
for (sig in c(0.25, 0.5, 0.75, 1)) {
  
  
  ## cut the tails (keep between 1st and 99th percentile)
  pct1 <- quantile(all_pi[[paste0("dm.ln_cpricei2_sig", sig)]], probs = 0.01, na.rm = T, weight=base.sales)
  pct99 <- quantile(all_pi[[paste0("dm.ln_cpricei2_sig", sig)]], probs = 0.99, na.rm = T, weight=base.sales)
  all_pi_est <- all_pi[(get(paste0("dm.ln_cpricei2_sig", sig)) > pct1 & get(paste0("dm.ln_cpricei2_sig", sig)) < pct99),]
  
  # Prices to evaluate the function
  pctiles <- seq(0,1,0.01)
  values <- round(quantile(all_pi_est[[paste0("dm.ln_cpricei2_sig", sig)]], 
                           probs = pctiles, na.rm = T, 
                           weight = all_pi_est$base.sales), digits = 6)
  prices <- (values[-1] + values[-length(values)])/2 # find the mid point
  
  ## Capture passthorugh
  rho <- 1.0508 #estimated effect on producer price
  ## Capture estimated demand
  demand <- betas.old[sigma == sig][["beta_hat"]]
  
  
  for (es.val in val.es) {
    for (p in prices) {
      
      ## 1. build estimated elasticity at p
      q1 <- 0
      for (k in 2:length(demand)) {
        q1 <- q1 + (k-1)*demand[k]*(p)^(k-2)
      }
      
      ## 2. build estimated second derivative at p
      q2 <- 0
      for (k in 3:length(demand)) {
        q2 <- q2 + (k-1)*(k-2)*demand[k]*(p)^(k-3)
      } 

      # 3. Find the value of theta solviving directly
      theta <- theta.direct( q1 = q1, q2 = q2, es = es.val, rho = rho, sigma = sig)
      
      
      ## 6. Export
      results <- rbind(results, data.table(sigma = sig, es.val, p, q1, q2, rho, theta))
    }
  }
  fwrite(results, conduct.parameter.file)  
}







### 12. Functions for Welfare effect extrapolations ----------------


#### Objective functions

### Marginal changes

# Normalization function for bernstein polynomial
normalize <- function(p, min, max) {
  (p - min)/(max - min)
}

# Bernstein polynomial basis 
bernstein <- function(p, t, sigma, k, K, min, max){
  p <- normalize(p +t*sigma, min, max)
  choose(K, k) * p^k * (1 - p)^(K - k)
}

# Elasticity
log.elas <- function(mu, p, t, sigma, min, max) {
  t <- log(1+t)
  K <- length(mu)
  j <- 0:(K-1)
  b_k <- sapply(j, function (j, p, t, sigma, k, K, min, max) bernstein(p = p, t = t, sigma = sigma, k = j, K = K, min = min, max = max), 
                p = p, t = t, sigma = sigma, K = K - 1, min = min, max = max)
  return(-(sum(mu*b_k)))
}

# Scaling by sigma salience
A.sigma <- function(t, sigma) sigma*((1+t) - (1+t)^sigma)/(1+t)

# Scalar in the numerator
M.theta <- function(t, theta)  (1+theta+t)/(1+t)

# Scaling to calculate FE
B.sigma <- function(t, sigma) sigma*t/(1+t)


# Marginal change function 
marginal.change <- function(mu, data, pp, tau, theta, sigma, w, min, max, constr_mat, IV_mat, min.crit = 0, elas = T) {
  
  # Take producer price
  p <- data[[pp]] 
  
  # Get tax rate (t not log(1+t))
  t <- exp(data[[tau]]) - 1
  
  # Put together and transform to list to apply
  X <- rbind(p, t)
  X <- lapply(seq_len(ncol(X)), function(i) X[,i])
  
  # Numerator
  numer.vector <- sapply(X, function(x, mu, theta, sigma, min, max) 
    M.theta(x["t"], theta) - A.sigma(x["t"], sigma)*log.elas(mu, x["p"], x["t"], sigma, min, max),
    mu = mu, theta = theta, sigma = sigma, min = min, max = max)
  
  # Denominator
  denom.vector <- sapply(X, function(x, mu, theta, sigma, min, max) 
    B.sigma(x["t"], sigma)*log.elas(mu, x["p"], x["t"], sigma, min, max),
    mu = mu, theta = theta, sigma = sigma, min = min, max = max)
  # Get weighted average
  w <- data[[w]]
  return(weighted.mean(numer.vector, w = w)/(1-weighted.mean(denom.vector, w = w)))
}

max.marginal.change <- function(mu, data, pp, tau, theta, sigma, w, min, max, constr_mat, IV_mat, min.crit = 0, elas = T) {
  return(-marginal.change(mu, data, pp, tau, theta, sigma, w, min, max, constr_mat, IV_mat, min.crit, elas))
}

### Non-Marginal changes

# Integral of bernstein
int.bernstein <- function(p, t, sigma, k, K, min, max) {
  
  j <- (k+1):(K+1)
  b_k <- sapply(j, function (j, p, t, sigma, k, K, min, max) bernstein(p = p, t = t, sigma = sigma, k = j, K = K, min = min, max = max), 
                p = p, t = t, sigma = sigma, K = K + 1, min = min, max = max)
  
  return(sum(b_k)/(K+1))
}
# log demand
log.demand <- function(p, t, sigma, mu, mu0 = NULL, min, max) {
  # If we provide a constant then use it do retrieve the implied demand
  t <- log(1+t)
  K <- length(mu)
  if (!is.null(mu0)) {
    mu.p <- mu0
    for (k in 1:(K+1)) {
      if (k < (K+1)) mu.p[k+1] <- mu.p[k] + mu[k]
    }
    j <- 0:K
    b_k <- sapply(j, function (j, p, t, sigma, k, K, min, max) bernstein(p = p, t = t, sigma = sigma, k = j, K = K, min = min, max = max), 
                  p = p, t = t, sigma = sigma, K = K, min = min, max = max)
    return((sum(mu.p*b_k)))    
  }
  # If mu0 is not provided, then use integrated bernstein polynomial and omit constant
  if (is.null(mu0)) {
    polynomial <- rep(0,K)
    for (k in 1:K) {
      polynomial[k] <- int.bernstein(p, t, sigma, k, K-1, min, max)
    }
    return((sum(mu*polynomial)))    
  }  
}


# Demand
demand <- function(p, t, sigma, mu, mu0 = NULL, min, max) {
  return(exp(log.demand(p, t, sigma, mu, mu0, min, max)))
}

# Integrand numerator
integrand.num <- function(t, p, sigma, theta, mu, min, max, mu0 = NULL) {
  return(demand(p, t, sigma, mu, mu0, min, max)*(M.theta(t,theta)-A.sigma(t, sigma)*log.elas(mu, p, t, sigma, min, max))/100)
}
# Apply integral to every value of t (this so the integral function can use vectors)
int.apply.num <- function(x, mu, p, sigma, theta, mu0, min, max) {
  sapply(x, integrand.num, p = p, sigma = sigma, theta = theta, mu=mu, mu0=mu0, min=min, max=max)
}

#New: Integral in denominator too
integrand.den <- function(t, p, sigma, mu, min, max, mu0 = NULL) {
  return(demand(p, t, sigma, mu, mu0, min, max)*(1-B.sigma(t, sigma)*log.elas(mu, p, t, sigma, min, max))/100)
}
int.apply.den <- function(x, mu, p, sigma, mu0, min, max) {
  sapply(x, integrand.den, p = p, sigma = sigma, mu=mu, mu0=mu0, min=min, max=max)
}


# Funtion to extract mu0
implied.mu0 <- function(p, t, sigma, mu, min, max, n.value) {
  num <- demand(p = p, t = t, sigma = sigma, mu = mu, min = min, max = max)
  c <- rep(1, length(mu) + 1)
  den <- log.elas(p = p, t = t, sigma = sigma, mu = c, min = min, max = max)
  return((n.value - num)/den)
}

# Non-Marginal change function
non.marginal.change <- function(mu, data, pp, t0, t1, theta, sigma, w, min, max, constr_mat, IV_mat, min.crit = 0, elas = T) {
  
  # Normalize to get a value of \mu_0: set to NULL we know is the same
  mu0 <- NULL
  
  # Use vectors 
  ll <- exp(data[[t0]])-1
  ul <- exp(data[[t1]])-1
  p <- data[[pp]]
  
  
  # Put together and transform to list
  X <- rbind(ll, ul, p)
  X <- lapply(seq_len(ncol(X)), function(i) X[,i])
  
  # Calculate initial demand
  init.dem <- sapply(X, function(x, mu, sigma, min, max, mu0) 
    demand(p = x["p"], t = x["ll"], sigma = sigma, 
           mu = mu, mu0 = mu0, min = min, max = max), 
    mu = mu, sigma = sigma, min = min, max = max, mu0 = mu0)
  
  # sapply from list to numerator
  int.num <- sapply(X, function(x, p, mu, sigma, theta, min, max, mu0) 
    integrate(int.apply.num, 
              lower = x["ll"], 
              upper = x["ul"], 
              p = x["p"],
              mu = mu,
              sigma = sigma,
              theta = theta,
              min = min, 
              max = max,
              mu0 = mu0)$value, mu = mu, sigma = sigma, theta = theta, min = min, max = max, mu0 = mu0)
  
  
  ## OLD
  # # Denominator
  # denom.vector <- ul*(demand(p, ul, sigma, mu, mu0, min, max)/demand(p, ll, sigma, mu, mu0, min, max)) - ll
  
  ## NEW denominator
  int.den <- sapply(X, function(x, p, mu, sigma, theta, min, max, mu0) 
    integrate(int.apply.den, 
              lower = x["ll"], 
              upper = x["ul"], 
              p = x["p"],
              mu = mu,
              sigma = sigma,
              min = min, 
              max = max,
              mu0 = mu0)$value, mu = mu, sigma = sigma, min = min, max = max, mu0 = mu0)
  
  # divide numerator and denominator by initial demand
  numer.vector <- int.num/init.dem
  denom.vector <- int.den/init.dem
  
  # Get weighted average
  w <- data[[w]] 
  w <- w/sum(w)
  
  numer <- weighted.mean(numer.vector, w = w)
  denom <- weighted.mean(denom.vector, w = w)
  # Sometimes we get Infinite as result as R can't handle large precission numbers. In such case we return 1
  res <-numer/denom
  if(is.infinite(numer) | is.infinite(denom)) res <- 1
  
  return(res)
}



max.non.marginal.change <- function(mu, data, pp, t0, t1, theta, sigma, w, min, max, constr_mat, IV_mat, min.crit = 0, elas = T) {
  return(-non.marginal.change(mu, data, pp, t0, t1, theta, sigma, w, min, max, constr_mat, IV_mat, min.crit, elas))
}


# Average across states

# Marginal change function 
av.marginal.change <- function(mu, data, pp, tau, theta, sigma, w, st.code, min, max, constr_mat, IV_mat, min.crit = 0, elas = T) {
  
  # Take producer price
  p <- data[[pp]] 
  
  # Get tax rate (t not log(1+t))
  t <- exp(data[[tau]]) - 1
  
  # Put together and transform to list to apply
  X <- rbind(p, t)
  X <- lapply(seq_len(ncol(X)), function(i) X[,i])
  
  # Numerator
  numer.vector <- sapply(X, function(x, mu, theta, sigma, min, max) 
    M.theta(x["t"], theta) - A.sigma(x["t"], sigma)*log.elas(mu, x["p"], x["t"], sigma, min, max),
    mu = mu, theta = theta, sigma = sigma, min = min, max = max)
  
  # Denominator
  denom.vector <- sapply(X, function(x, mu, theta, sigma, min, max) 
    B.sigma(x["t"], sigma)*log.elas(mu, x["p"], x["t"], sigma, min, max),
    mu = mu, theta = theta, sigma = sigma, min = min, max = max)
  # Put together info: vectors are organized as provided
  w <- data[[w]]
  st.code <- data[[st.code]]
  data.final <- data.table(numer.vector, denom.vector, w, st.code)
  # Get weighted average by state
  data.final <- data.final[, .(MVPF.state = weighted.mean(numer.vector, w = w)/(1-weighted.mean(denom.vector, w = w))), by =.(st.code)]  
  # Average across states
  
  return(mean(data.final$MVPF.state))
}

max.av.marginal.change <- function(mu, data, pp, tau, theta, sigma, w, st.code, min, max, constr_mat, IV_mat, min.crit = 0, elas = T) {
  return(-av.marginal.change(mu, data, pp, tau, theta, sigma, w, st.code, min, max, constr_mat, IV_mat, min.crit, elas))
}

av.non.marginal.change <- function(mu, data, pp, t0, t1, theta, sigma, w, st.code, min, max, constr_mat, IV_mat, min.crit = 0, elas = T) {
  
  # Normalize to get a value of \mu_0: set to NULL we know is the same
  mu0 <- NULL
  
  # Use vectors 
  ll <- exp(data[[t0]])-1
  ul <- exp(data[[t1]])-1
  p <- data[[pp]]
  
  
  # Put together and transform to list
  X <- rbind(ll, ul, p)
  X <- lapply(seq_len(ncol(X)), function(i) X[,i])
  
  # Calculate initial demand
  init.dem <- sapply(X, function(x, mu, sigma, min, max, mu0) 
    demand(p = x["p"], t = x["ll"], sigma = sigma, 
           mu = mu, mu0 = mu0, min = min, max = max), 
    mu = mu, sigma = sigma, min = min, max = max, mu0 = mu0)
  
  # sapply from list to numerator
  int.num <- sapply(X, function(x, p, mu, sigma, theta, min, max, mu0) 
    integrate(int.apply.num, 
              lower = x["ll"], 
              upper = x["ul"], 
              p = x["p"],
              mu = mu,
              sigma = sigma,
              theta = theta,
              min = min, 
              max = max,
              mu0 = mu0)$value, mu = mu, sigma = sigma, theta = theta, min = min, max = max, mu0 = mu0)
  
  ## OLD
  # # Denominator
  # denom.vector <- ul*(demand(p, ul, sigma, mu, mu0, min, max)/demand(p, ll, sigma, mu, mu0, min, max)) - ll
  
  ## NEW denominator
  int.den <- sapply(X, function(x, p, mu, sigma, theta, min, max, mu0) 
    integrate(int.apply.den, 
              lower = x["ll"], 
              upper = x["ul"], 
              p = x["p"],
              mu = mu,
              sigma = sigma,
              min = min, 
              max = max,
              mu0 = mu0)$value, mu = mu, sigma = sigma, min = min, max = max, mu0 = mu0)
  
  # divide numerator and denominator by initial demand
  numer.vector <- int.num/init.dem
  denom.vector <- int.den/init.dem
  
  
  
  w <- data[[w]]
  w <- w/sum(w)
  st.code <- data[[st.code]]
  data.final <- data.table(numer.vector, denom.vector, w, st.code)
  # Get weighted average by state
  data.final <- data.final[, .(MVPF.state = weighted.mean(numer.vector, w = w)/(weighted.mean(denom.vector, w = w))), by =.(st.code)]  
  # Average across states
  return(mean(data.final$MVPF.state))
}

max.av.non.marginal.change <- function(mu, data, pp, t0, t1, theta, sigma, w, st.code, min, max, constr_mat, IV_mat, min.crit = 0, elas = T) {
  return(-(av.non.marginal.change(mu, data, pp, t0, t1, theta, sigma, w, st.code, min, max, constr_mat, IV_mat, min.crit, elas)))
}

av.non.marginal.change.parallel <- function(mu, data, pp, t0, t1, theta, sigma, w, st.code, min, max, constr_mat, IV_mat, min.crit = 0, elas = T) {
  
  # Normalize to get a value of \mu_0: set to NULL we know is the same
  mu0 <- NULL
  
  # Use vectors 
  ll <- exp(data[[t0]])-1
  ul <- exp(data[[t1]])-1
  p <- data[[pp]]
  
  
  # Put together and transform to list
  X <- rbind(ll, ul, p)
  X <- lapply(seq_len(ncol(X)), function(i) X[,i])
  
  # Calculate initial demand
  init.dem <- sapply(X, function(x, mu, sigma, min, max, mu0) 
    demand(p = x["p"], t = x["ll"], sigma = sigma, 
           mu = mu, mu0 = mu0, min = min, max = max), 
    mu = mu, sigma = sigma, min = min, max = max, mu0 = mu0)
  
  # sapply from list to numerator
  int.num <- foreach(i=1:length(p),.combine="c") %dopar% {
    integrate(int.apply.num, 
              lower = ll[i], 
              upper = ul[i], 
              p = p[i],
              mu = mu,
              sigma = sigma,
              theta = theta,
              min = min, 
              max = max,
              mu0 = mu0)$value
  }
  
  ## OLD
  # # Denominator
  # denom.vector <- ul*(demand(p, ul, sigma, mu, mu0, min, max)/demand(p, ll, sigma, mu, mu0, min, max)) - ll
  
  ## NEW denominator
  int.den <- foreach(i=1:length(p),.combine="c") %dopar% {
    integrate(int.apply.den, 
              lower = ll[i], 
              upper = ul[i], 
              p = p[i],
              mu = mu,
              sigma = sigma,
              min = min, 
              max = max,
              mu0 = mu0)$value
  }
  
  # divide numerator and denominator by initial demand
  numer.vector <- int.num/init.dem
  denom.vector <- int.den/init.dem
  
  
  
  w <- data[[w]]
  w <- w/sum(w)
  st.code <- data[[st.code]]
  data.final <- data.table(numer.vector, denom.vector, w, st.code)
  # Get weighted average by state
  data.final <- data.final[, .(MVPF.state = weighted.mean(numer.vector, w = w)/(weighted.mean(denom.vector, w = w))), by =.(st.code)]  
  # Average across states
  return(mean(data.final$MVPF.state))
}

max.av.non.marginal.change.parallel <- function(mu, data, pp, t0, t1, theta, sigma, w, st.code, min, max, constr_mat, IV_mat, min.crit = 0, elas = T) {
  return(-(av.non.marginal.change.parallel(mu, data, pp, t0, t1, theta, sigma, w, st.code, min, max, constr_mat, IV_mat, min.crit, elas)))
}

#### Constraints functions

## Now, we put together functions that create the restrictions for the problem and will be used in the NLOPT program

# The final function takes 5 main inputs:
# mu: the control variables
# constr_mat = the matrix of constraint
# IV_mat = the vector of IVs, used for the RHS of the restriction
# elas = T (default) indicates we are imposing shape constraint on the elasticity (F is for demand)
# min.crit = (default 0) indicates the value of the minimum criterion for the problem (as solved earlier). If NULL the problem is on an equality

## Function without min.criterion
constraint <- function(mu, constr_mat, IV_mat) {
  
  if (dim(constr_mat)[1] != length(IV_mat)) { stop("constr_mat and IV_mat dimensions must match") }
  if (length(mu) != dim(constr_mat)[2]) { stop("constr_mat and mu dimensions must match") }
  
  constraints <- NULL
  for (r in 1:dim(constr_mat)[1]) {
    constraints <- rbind(constraints, c(sum(constr_mat[r,]*mu) - IV_mat[r]))
  }
  return(constraints)
}
## Function to add min.cretrion = value
constr.min.crit <- function(mu, constr_mat, IV_mat, min.crit) {
  
  if(length(min.crit) > 1){ stop("minimum criterion should be a value") }
  
  return(
    rbind(
      constraint(mu, constr_mat, IV_mat+min.crit),
      constraint(mu, -constr_mat, -IV_mat+min.crit)
    )
  )
}
## Function to create the shape constraint
shape.constr<-function(mu, elas) {
  constr.mono <- NULL
  if (elas) {
    for (k in 1:K) {
      
      constr.mono <- rbind(constr.mono,
                           c(mu[k]))
    }
  } else {
    for (k in 1:(K-1)) {
      
      constr.mono <- rbind(constr.mono,
                           c( -mu[k] + mu[k+1]))
    }
  }
  return(constr.mono)
}
## Function for constraint: includes the arguments from evaluation function even if not needed so it runs
eval_restrictions_marg <- function(mu, data, pp, tau, theta, sigma, w, min, max, constr_mat, IV_mat, min.crit = 0, elas = T) {
  
  return(
    as.matrix(
      #rbind(
      constr.min.crit(mu, constr_mat, IV_mat, min.crit) #,
      #shape.constr(mu, elas)
      #)
    )
  )
}
eval_restrictions_marg_av <- function(mu, data, pp, tau, theta, sigma, w, st.code, min, max, constr_mat, IV_mat, min.crit = 0, elas = T) {
  
  return(
    as.matrix(
      #rbind(
      constr.min.crit(mu, constr_mat, IV_mat, min.crit) #,
      #shape.constr(mu, elas)
      #)
    )
  )
}

## Function for jacobian
eval_restrictions_j_marg <- function(mu, data, pp, tau, theta, sigma, w, min, max, constr_mat, IV_mat, min.crit = 0, elas = T) {
  
  constr.jac <- NULL
  for (k in 1:K) {
    
    constr.jac <- cbind(
      constr.jac,
      #rbind(
      constr.min.crit(c(rep(0,k-1),1,rep(0,K-k)), constr_mat, rep(0, dim(constr_mat)[1]), 0) #,
      #shape.constr(c(rep(0,k-1),1,rep(0,K-k)), elas)
      #)
    )
    
  }
  return(as.matrix(constr.jac))
}
## Function for constraint: includes the arguments from evaluation function even if not needed so it runs
eval_restrictions_nmarg <- function(mu, data, pp, t0, t1, theta, sigma, w, min, max, constr_mat, IV_mat, min.crit = 0, elas = T) {
  
  return(
    as.matrix(
      #rbind(
      constr.min.crit(mu, constr_mat, IV_mat, min.crit) #,
      #shape.constr(mu, elas)
      #)
    )
  )
}
eval_restrictions_nmarg_av <- function(mu, data, pp, t0, t1, theta, sigma, w, st.code, min, max, constr_mat, IV_mat, min.crit = 0, elas = T) {
  
  return(
    as.matrix(
      #rbind(
      constr.min.crit(mu, constr_mat, IV_mat, min.crit) #,
      #shape.constr(mu, elas)
      #)
    )
  )
}
## Function for jacobian
eval_restrictions_j_nmarg <- function(mu, data, pp, t0, t1, theta, sigma, w, min, max, constr_mat, IV_mat, min.crit = 0, elas = T) {
  
  constr.jac <- NULL
  for (k in 1:K) {
    
    constr.jac <- cbind(
      constr.jac,
      #rbind(
      constr.min.crit(c(rep(0,k-1),1,rep(0,K-k)), constr_mat, rep(0, dim(constr_mat)[1]), 0) #,
      #shape.constr(c(rep(0,k-1),1,rep(0,K-k)), elas)
      #)
    )
    
  }
  return(as.matrix(constr.jac))
}







## Small function to get an initial values for optimization
get.init.val <- function(A, b, min.c, max = 1000) {
  
  init <- as.vector(ginv(A) %*% b)
  if (sum(init> 0) == 0) {
    return(init)
  } 
  else {
    srv <- (sum(init> 0) > 0)
    d <- dim(A)[2]
    kernel <- null(A)
    if (is.null(kernel)) kernel <- t(t(rep(0, length(init)))) # When it has a solution then is going to be null: use only min.criterion
    for (d in 1:dim(kernel)[2]) {
      ker <- as.vector(kernel[,d])
      i <- 0
      while (srv & i < max) {
        i <- i + 1
        s <- sign(ker[which(init == max(init))])
        rat <- abs( ker[which(init == max(init))]/min(init))
        if (min.c == 0) {
          init <- init - s*rat*ker
        }
        else {
          init <- init - s*rat*ker -
            (init > 0)*rep(sum(init < 0)*min.c/(d), length(init)) + 
            (init < 0)*rep(sum(init < 0)*min.c/(d), length(init))          
        }
        if (i < 11 & round(5*i/max) == 5*i/max) {
        } 
        srv <- (sum(init> 0) > 0)
      }
    }
    if (i == max) {
      # Set 1 of them to 0
      m <- dim(A)[2] -1
      if (m > 1){
        A <- A[,1:m]
        init <- c(get.init.val(A, b, min.c, max), 0)
        
      }
      else {
        stop("Algorithm Failed") 
      }
    }
    else {
      print(init)
      return(init)
    }
  }
}




### 13. Estimation of Welfare: hypothetical markets ------------------------


hypot.data <- data.table(p_m = c(-0.1, 0.1, -0.1, 0.1),
                         tau = log1p(c(0.05, 0.05, 0.09, 0.09)),
                         eta_m = 1)
hypot.data[, p_cml := p_m - tau]
hypot.data[, tauno := 0]
hypot.data[, tau5 := log(exp(tau) + 0.05)]
hypot.data[, state := .I]

### Part 2. Produce estimates for Linear case (No extrapolaton)

## Open IV estimates
IVs <- fread(iv.output.salience.results.file)
IVs <- IVs[controls == "division_by_module_by_time"]

sigmas.test <- c(0.25, 0.5, 0.75, 1)

thetas.list <- list()
thetas.list$s25 <- c(0, 0.058897778, 0.564015475)
thetas.list$s50 <- c(0, 0.018501935, 0.202776786)
thetas.list$s75 <- c(0, 0.008007016, 0.102212766)
thetas.list$s100 <- c(0, 0.004861793, 0.066616166)


#
results.marginal <- data.table(NULL)
results.nonmarginal <- data.table(NULL)
# FOR LINEAR Estimates we don't need to normalize!!! The coefficient is directly interpretable (contrary to non-linear, where matrices are normalized)
min <- 0
max <- 1
for (state in 1:nrow(hypot.data)) {
  data.st <- hypot.data[state,]
  i <- 0
  for (sig in sigmas.test) {
    i <- i + 1
    thetas.test <- thetas.list[[i]]
    for (theta in thetas.test) {
      ## Capture min/max and coef in lin case
      lin <- IVs[outcome == "IV" & sigma == sig][["Estimate"]]
      
      ## Marginal Change
      up <- down <- marginal.change(lin, data.st, "p_cml", "tau", theta, sig, "eta_m", min, max, 0, 0) 
      results.marginal<- rbind(results.marginal, data.table(state, down, up, theta, sigma = sig, K = 1, D = 1, s1 = 1, s2 = 1))
      
      
      ## Non Marginal Change
      t0 <- "tauno"
      t1 <- "tau"
      sc <- "No Tax"
      up <- down <- non.marginal.change(lin, data.st, "p_cml", t0, t1, theta, sig, "eta_m", min, max, np = 0, nd = 1, 0, 0)
      results.nonmarginal<- rbind(results.nonmarginal, data.table(state, down, up, sc, theta, sigma = sig, K = 1, D = 1, s1 = 1, s2 = 1, it1 = 0, it2 = 0, ConsChckUp = 1, ConsChckDown = 1))
      
      
      t0 <- "tau"
      t1 <- "tau5"
      sc <- "plus 5 Tax"
      up <- down <- non.marginal.change(lin, data.st, "p_cml", t0, t1, theta, sig, "eta_m", min, max, np = 0, nd = 1, 0, 0)
      results.nonmarginal<- rbind(results.nonmarginal, data.table(state, down, up, sc, theta, sigma = sig, K = 1, D = 1, s1 = 1, s2 = 1, it1 = 0, it2 = 0, ConsChckUp = 1, ConsChckDown = 1))
    }
  }
}
results.nonmarginal <- merge(results.nonmarginal, hypot.data, by = "state")[, -c("tauno", "tau5", "p_cml", "eta_m")]
results.marginal <- merge(results.marginal, hypot.data, by = "state")[, -c("tauno", "tau5", "p_cml", "eta_m")]
fwrite(results.nonmarginal, out.file.hyp.nonmarginal)
fwrite(results.marginal, out.file.hyp.marginal)


### Part 3. Set up Extrapolation (K>1)

## 1. Set up IV estimates for each sigma
# For L = 1
IVs1 <- IVs[outcome == "IV", -c("Std. Error", "controls", "rn", "Cluster s.e.", "t value", "Pr(>|t|)", "outcome")]
# For L > 1
IVs2 <- dcast(IVs[n.groups > 1], n.groups + lev + sigma ~ outcome,  fun=sum, value.var = c("Estimate"))
IVs2[, w.ln_cpricei2 := w.ln_cpricei2_sig0.25 + w.ln_cpricei2_sig0.5 + w.ln_cpricei2_sig0.75 + w.ln_cpricei2_sig1]
IVs2[, Estimate := w.ln_quantity3/w.ln_cpricei2]
IVs2 <- IVs2[, -c(paste0("w.ln_cpricei2_sig", c(0.25, 0.5, 0.75, 1)), "w.ln_quantity3", "w.ln_cpricei2")]
# Merge and Order appropiately
res.ivs <- rbind(IVs1, IVs2)
res.ivs <- res.ivs[order(sigma, n.groups, lev)]
rm(IVs, IVs1, IVs2)


## 2. Open Min - Max files
res.pq <- fread(pq.output.results.file)



## 2. Set up Ks and scenarios (non-marginal) to test
# K.test <- c(7,10)
K.test <- c(2, 8)
scenarios <- c("Original", "No Tax", "plus 5 Tax")

## 3. Set up Optimization Parameters (algorithm for now)
nlo.opts.local.df <- list(
  "algorithm"="NLOPT_LN_COBYLA",
  "maxeval" = 150000,
  "xtol_rel"=1.0e-8
)

# Options for Gurobi's min criterion calculation
params <- list()
params$NumericFocus <- 3
params$ScaleFlag <- 2
params$Method <- 1
params$Presolve <- 0
tolerance <- 1e-6
params$FeasibilityTol <- tolerance


### Part 3. Run Extrapolations (K>1)
mincriteria <- data.table(NULL)


## A. Loop across scenarios
for (sc in scenarios) {
  
  if (sc == "Original") t <- "tau"
  if (sc == "No Tax") {
    t0 <- "tauno"
    t1 <- "tau"
  } 
  if (sc == "plus 5 Tax")  {
    t0 <- "tau"
    t1 <- "tau5"
  }
  ## B. Loop across K
  for (K in K.test) {
    
    ## B.1. Load Matrix of gamma (this extrictly depends on K since the basis change)
    in.file <- paste0(theta.berstein.sal, K,"_bern.csv")
    gamma.full.data <- fread(in.file)
    
    ## C. Loop across sigmas
    i <- 0
    for (sig in sigmas.test) {
      i <- i + 1
      thetas.test <- thetas.list[[i]]
      ## C.1 Extract support to use
      p.min <- res.pq[extrap == sc & sigma == sig][["min.p"]]
      p.max <- res.pq[extrap == sc & sigma == sig][["max.p"]]
      
      ## C.2 Restrict gamma file. Constant across p
      gamma <- gamma.full.data[extrap == sc & n.groups < 3 & sigma == sig][, c(paste0("b", 0:(K-1)), "n.groups"), with = F]             ## For elasticity
      
      ## D Start Loop at number of groups
      for (D in unique(gamma$n.groups)) {
        
        ## D1. Build the constraints matrix 
        constr <- as.matrix(gamma[n.groups == D][, -c("n.groups")])   ## For elasticity
        
        ## D2. Retrieve IVs
        IVs <- res.ivs[n.groups == D  & sigma == sig][["Estimate"]] 
        
        ## D3. Estimate min.criterion for case (note that if there is no value it is 0)
        constr.mono <- Diagonal(ncol(constr))
        RHS.mono <- rep(0, K)
        
        # D3a. Define the problem
        min.crit <- list() 
        min.crit$A <- rbind(cbind(Diagonal(nrow(constr)), constr), 
                            cbind(Diagonal(nrow(constr)), -constr),
                            cbind(matrix(0, nrow(constr.mono), nrow(constr)), constr.mono)
        )
        min.crit$rhs <- c(IVs, -IVs, RHS.mono)
        min.crit$sense <- c( rep('>=', 2*length(IVs)), rep('<=',K))
        min.crit$obj <- c(rep(1, nrow(constr)), rep(0, ncol(constr)))
        min.crit$lb <- c(rep(0, nrow(constr)), rep(-Inf, ncol(constr)))  
        min.crit$modelsense <- 'min'
        
        # D3b. Solve for the minimum criterion
        min.crit.sol <- gurobi(min.crit)
        
        # D3c. Get the minimum criterion estimated and modify the setting of the problem
        mc <- min.crit.sol$objval
        
        # D3d. Export the min creterion for each case to check
        mincriteria <- rbind(mincriteria, data.table(min.criteria = mc, D, K, sigma = sig, extrap = sc))
        fwrite(mincriteria, out.file.mc.welf.hyp)
        
        ## D4. Generate an initial value somewhere in the middle to test algorithms
        init.old<- init.val0 <- get.init.val(constr, IVs, mc)
        print(init.val0)
        
        
        ## E. Loop across thetas
        for (theta in thetas.test) {
          
          ## F Loop across states
          for (state in 1:nrow(hypot.data)) {
            print(state)
            # F1. Subset data
            st.data <- hypot.data[state,]
            
            if (sc == "Original") {
              # F2. Marginal change
              # F2a1. Min calculation
              res0 <- nloptr( x0=init.val0,
                              eval_f= marginal.change,
                              eval_g_ineq = eval_restrictions_marg,
                              opts = nlo.opts.local.df,
                              data = st.data,
                              pp = "p_cml",
                              tau = "tau",
                              theta = theta,
                              sigma = sig,
                              w = "eta_m",
                              min = p.min,
                              max = p.max,
                              constr_mat = constr,
                              IV_mat = IVs,
                              min.crit = mc,
                              elas = T,
                              ub = rep(0, K),
                              lb = rep(min(IVs)/min(constr), K)
              )
              # F2a2 Results extraction
              down <- res0$objective
              s1 <- res0$status
              # F2b1. Max calculation
              res0 <- nloptr( x0=init.val0,
                              eval_f= max.marginal.change,
                              eval_g_ineq = eval_restrictions_marg,
                              opts = nlo.opts.local.df,
                              data = st.data,
                              pp = "p_cml",
                              tau = "tau",
                              theta = theta,
                              sigma = sig,
                              w = "eta_m",
                              min = p.min,
                              max = p.max,
                              constr_mat = constr,
                              IV_mat = IVs,
                              min.crit = mc,
                              elas = T,
                              ub = rep(0, K),
                              lb = rep(min(IVs)/min(constr), K)
              )
              # F2b2 Results extraction
              up <- -res0$objective
              s2 <- res0$status
              
              ## F2c Export
              results.marginal <- rbind(results.marginal, data.table(down, up, state, D , K, sigma = sig, theta, s1, s2))
              fwrite(results.marginal, out.file.hyp.marginal)
            }
            
            
            # F3.NonMarginal
            else {
              # B3 Run minimization: derivative free
              res0 <- nloptr( x0=init.val0,
                              eval_f= non.marginal.change,
                              eval_g_ineq = eval_restrictions_nmarg,
                              opts = nlo.opts.local.df,
                              data = st.data,
                              pp = "p_cml",
                              t0 = t0,
                              t1 = t1,
                              theta = theta,
                              sigma = sig,
                              w = "eta_m",
                              min = p.min,
                              max = p.max,
                              np = 0,
                              nd = 1,
                              constr_mat = constr,
                              IV_mat = IVs,
                              min.crit = mc,
                              elas = T,
                              ub = rep(0, K),
                              lb = rep(min(IVs)/min(constr), K)
              )
              # B4. Extract minimization results
              down <- res0$objective
              s1 <- res0$status
              it1 <- res0$iterations
              
              
              ConsChckDown <- all.equal(sum(abs(constr%*%(as.matrix(res0$solution)) - IVs) - mc), 0)
              # B5. Check constraint is met
              
              
              
              # B5 Run maximization: derivative free
              res0 <- nloptr( x0=init.val0,
                              eval_f= max.non.marginal.change,
                              eval_g_ineq = eval_restrictions_nmarg,
                              opts = nlo.opts.local.df,
                              data = st.data,
                              pp = "p_cml",
                              t0 = t0,
                              t1 = t1,
                              theta = theta,
                              sigma = sig,
                              w = "eta_m",
                              min = p.min,
                              max = p.max,
                              np = 0,
                              nd = 1,
                              constr_mat = constr,
                              IV_mat = IVs,
                              min.crit = mc,
                              elas = T,
                              ub = rep(0, K),
                              lb = rep(min(IVs)/min(constr), K)
              )
              # B6. Extract minimization results
              up<- -res0$objective
              s2 <- res0$status
              it2 <- res0$iterations
              
              ConsChckUp <- all.equal(sum(abs(constr%*%(as.matrix(res0$solution)) - IVs) - mc), 0)
              
              # B7. Compile estimates export
              results.nonmarginal <- rbind(results.nonmarginal, data.table(down, up, state, sc, D , K, sigma = sig, theta, s1, s2, it1, it2, ConsChckDown, ConsChckUp))
              fwrite(results.nonmarginal, out.file.hyp.nonmarginal)
            }
          }
        }
      }    
    }  
  }
}

## Merge state data to compare

results.nonmarginal <- merge(results.nonmarginal, hypot.data, by = "state")[, -c("tauno", "tau5", "p_cml", "eta_m")]
results.marginal <- merge(results.marginal, hypot.data, by = "state")[, -c("tauno", "tau5", "p_cml", "eta_m")]
fwrite(results.nonmarginal, out.file.hyp.nonmarginal)
fwrite(results.marginal, out.file.hyp.marginal)


### 14. Estimation of welfare: bin data to reduce time ------

## Collapse to binned price
# Generate rounded price
all_pi[, p_m := round(dm.ln_cpricei2, 3)]

# collapse for every price x state on taxable goods 
all_pi_p<- all_pi[ln_sales_tax > 0, .(tau = weighted.mean(ln_sales_tax, w = base.sales),
                                      eta_m = sum(base.sales)), by = .(fips_state, p_m)]

# Export
fwrite(all_pi_p, binned.data.price)

## Collapse to binned tax
# Generate rounded tax
all_pi[, tau := round(ln_sales_tax, 3)]

# collapse for every price x state on taxable goods 
all_pi_t<- all_pi[ln_sales_tax > 0, .(p_m = weighted.mean(dm.ln_cpricei2, w = base.sales),
                                      eta_m = sum(base.sales)), by = .(fips_state, tau)]

# Export
fwrite(all_pi_t, binned.data.tax)


### 15. Estimation of welfare: marginal extrapolation on data ------


# 0. Parallelize options
# use the environment variable SLURM_NTASKS_PER_NODE to set the number of cores
registerDoParallel(cores=(Sys.getenv("SLURM_NTASKS_PER_NODE")))

# 1. Open data
data <- fread(binned.data.tax)
data[, p_cml := p_m - tau]
data[, tauno := 0]
data[, tau5 := tau + log(1+0.05)]

# 2. Open IV estimates
IVs <- fread(iv.output.salience.results.file)
IVs <- IVs[controls == "division_by_module_by_time"]

# 3. Values to Tests. In practice, this is done in batches to avoid running all cases and exceed the running time

sigmas.test <- c(0.25, 0.5, 0.75, 1)

thetas.list <- list()
# All
thetas.list$s25 <- c(0, 0.058897778, 0.564015475)
thetas.list$s50 <- c(0, 0.018501935, 0.202776786)
thetas.list$s75 <- c(0, 0.008007016, 0.102212766)
thetas.list$s100 <- c(0, 0.004861793, 0.066616166)
# # Priority
# thetas.list$s25 <- c(0)
# thetas.list$s50 <- c(0)
# thetas.list$s75 <- c(0)
# thetas.list$s100 <- c(0, 0.066616166)
# # Priority 2
# thetas.list$s25 <- c(0.058897778, 0.564015475)
# thetas.list$s50 <- c(0.018501935, 0.202776786)
# thetas.list$s75 <- c(0.008007016, 0.102212766)
# thetas.list$s100 <- c(0.004861793)

states.test <- unique(data$fips_state)

#### Estimates for Linear Case
results.marginal <- data.table(NULL)
# FOR LINEAR Estimates we don't need to normalize!!! The coefficient is directly interpretable (contrary to non-linear, where matrices are normalized)
min <- 0
max <- 1
for (state in states.test) {
  data.st <- data[fips_state == state,]
  i <- 0
  for (sig in sigmas.test) {
    i <- i + 1
    thetas.test <- thetas.list[[i]]
    for (theta in thetas.test) {
      ## Capture min/max and coef in lin case
      lin <- IVs[outcome == "IV" & sigma == sig][["Estimate"]]

      ## Marginal Change
      up <- down <- marginal.change(lin, data.st, "p_cml", "tau", theta, sig, "eta_m", min, max, 0, 0)
      results.marginal<- rbind(results.marginal, data.table(state, down, up, theta, sigma = sig, K = 1, D = 1, s1 = 1, s2 = 1))

    }
  }
}


## 4. Set up IV estimates for each sigma
# For L = 1
IVs1 <- IVs[outcome == "IV", -c("Std. Error", "controls", "rn", "Cluster s.e.", "t value", "Pr(>|t|)", "outcome")]
# For L > 1
IVs2 <- dcast(IVs[n.groups > 1], n.groups + lev + sigma ~ outcome,  fun=sum, value.var = c("Estimate"))
IVs2[, w.ln_cpricei2 := w.ln_cpricei2_sig0.25 + w.ln_cpricei2_sig0.5 + w.ln_cpricei2_sig0.75 + w.ln_cpricei2_sig1]
IVs2[, Estimate := w.ln_quantity3/w.ln_cpricei2]
IVs2 <- IVs2[, -c(paste0("w.ln_cpricei2_sig", c(0.25, 0.5, 0.75, 1)), "w.ln_quantity3", "w.ln_cpricei2")]
# Merge and Order appropiately
res.ivs <- rbind(IVs1, IVs2)
res.ivs <- res.ivs[order(sigma, n.groups, lev)]
rm(IVs, IVs1, IVs2)

## 5. Load min criteria (we already computed them for the hypothetical markets no need to do it again)
min.criteria <- fread(out.file.mc.welf.hyp)
setnames(min.criteria, c("K", "D"), c("Degree", "L"))

## 5. Open Min - Max files
res.pq <- fread(pq.output.salience.results.file)


## 6. Set up Ks
# K.test <- c(7,10)
K.test <- c(2, 8)
#K.test <- 8

## 7. Set up Optimization Parameters (algorithm for now)
nlo.opts.local.df <- list(
  "algorithm"="NLOPT_LN_COBYLA",
  "maxeval" = 150000,
  "xtol_rel"=1.0e-8
)

# Options for Gurobi's min criterion calculation
params <- list()
params$NumericFocus <- 3
params$ScaleFlag <- 2
params$Method <- 1
params$Presolve <- 0
tolerance <- 1e-6
params$FeasibilityTol <- tolerance


## B. Loop across K
for (K in K.test) {
  
  ## B.1. Load Matrix of gamma (this extrictly depends on K since the basis change)
  in.file <- paste0(theta.berstein.sal, K,"_bern.csv")
  gamma.full.data <- fread(in.file)
  
  ## C. Loop across sigmas
  i <- 0
  for (sig in sigmas.test) {
    i <- i + 1
    thetas.test <- thetas.list[[i]]
    ## C.1 Extract support to use
    p.min <- res.pq[extrap == "Original" & sigma == sig][["min.p"]]
    p.max <- res.pq[extrap == "Original" & sigma == sig][["max.p"]]
    
    ## C.2 Restrict gamma file. Constant across p
    gamma <- gamma.full.data[extrap == "Original" & n.groups < 3 & sigma == sig][, c(paste0("b", 0:(K-1)), "n.groups"), with = F]             ## For elasticity
    
    ## D Start Loop at number of groups
    for (D in 2) { #unique(gamma$n.groups)
      
      ## D1. Build the constraints matrix 
      constr <- as.matrix(gamma[n.groups == D][, -c("n.groups")])   ## For elasticity
      
      ## D2. Retrieve IVs
      IVs <- res.ivs[n.groups == D  & sigma == sig][["Estimate"]] 
      
      ## D3. Load min.criterion for case
      mc <- min.criteria[Degree == K & L == D & sigma == sig & extrap == "Original",][["min.criteria"]]
      
      ## D4. Generate an initial value somewhere in the middle to test algorithms
      init.old<- init.val0 <- get.init.val(constr, IVs, mc)
      print(init.val0)
      
      
      ## E. Loop across thetas
      for (theta in thetas.test) {
        
        ## F Loop across states
        ## A4. Loop across states
        welfare.st <- foreach (state= states.test, .combine=rbind) %dopar% {
          
          # F1. Subset data
          st.data <- data[fips_state == state,]
          # F2. Marginal change
          # F2a1. Min calculation
          res0 <- nloptr( x0=init.val0,
                          eval_f= marginal.change,
                          eval_g_ineq = eval_restrictions_marg,
                          opts = nlo.opts.local.df,
                          data = st.data,
                          pp = "p_cml",
                          tau = "tau",
                          theta = theta,
                          sigma = sig,
                          w = "eta_m", 
                          min = p.min, 
                          max = p.max,
                          constr_mat = constr, 
                          IV_mat = IVs, 
                          min.crit = mc,
                          elas = T,
                          ub = rep(0, K),
                          lb = rep(min(IVs)/min(constr), K)
          )
          # F2a2 Results extraction
          down <- res0$objective
          s1 <- res0$status
          # F2b1. Max calculation
          res0 <- nloptr( x0=init.val0,
                          eval_f= max.marginal.change,
                          eval_g_ineq = eval_restrictions_marg,
                          opts = nlo.opts.local.df,
                          data = st.data,
                          pp = "p_cml",
                          tau = "tau",
                          theta = theta,
                          sigma = sig,
                          w = "eta_m", 
                          min = p.min, 
                          max = p.max,
                          constr_mat = constr, 
                          IV_mat = IVs, 
                          min.crit = mc,
                          elas = T,
                          ub = rep(0, K),
                          lb = rep(min(IVs)/min(constr), K)
          )
          # F2b2 Results extraction
          up <- -res0$objective
          s2 <- res0$status
          
          ## F2c Export
          data.table(down, up, state, D , K, sigma = sig, theta, s1, s2)
          
        }
        results.marginal <- rbind(results.marginal, welfare.st)
        fwrite(results.marginal, out.file.marginal)
        
      }
    }    
  }  
}


### 16. Estimation of welfare: nonmarginal extrapolation on data ------


# 0. Parallelize options
# use the environment variable SLURM_NTASKS_PER_NODE to set the number of cores
registerDoParallel(cores=(Sys.getenv("SLURM_NTASKS_PER_NODE")))

# 1. Open data
data <- fread(binned.data.tax)
data[, p_cml := p_m - tau]
data[, tauno := 0]
data[, tau5 := tau + log(1+0.05)]

# 2. Open IV estimates
IVs <- fread(iv.output.salience.results.file)
IVs <- IVs[controls == "division_by_module_by_time"]

# 3. Values to Tests. In practice, this is done in batches to avoid running all cases and exceed the running time

sigmas.test <- c(0.25, 0.5, 0.75, 1)

thetas.list <- list()
# All
thetas.list$s25 <- c(0, 0.058897778, 0.564015475)
thetas.list$s50 <- c(0, 0.018501935, 0.202776786)
thetas.list$s75 <- c(0, 0.008007016, 0.102212766)
thetas.list$s100 <- c(0, 0.004861793, 0.066616166)
# # Priority
# thetas.list$s25 <- c(0)
# thetas.list$s50 <- c(0)
# thetas.list$s75 <- c(0)
# thetas.list$s100 <- c(0, 0.066616166)
# # Priority 2
# thetas.list$s25 <- c(0.058897778, 0.564015475)
# thetas.list$s50 <- c(0.018501935, 0.202776786)
# thetas.list$s75 <- c(0.008007016, 0.102212766)
# thetas.list$s100 <- c(0.004861793)

states.test <- unique(data$fips_state)

#### Estimates for Linear Case
results.nonmarginal <- data.table(NULL)
#FOR LINEAR Estimates we don't need to normalize!!! The coefficient is directly interpretable (contrary to non-linear, where matrices are normalized)
min <- 0
max <- 1
for (state in states.test) {
  data.st <- data[fips_state == state,]
  i <- 0
  for (sig in sigmas.test) {
    i <- i + 1
    thetas.test <- thetas.list[[i]]
    for (theta in thetas.test) {
      ## Capture min/max and coef in lin case
      lin <- IVs[outcome == "IV" & sigma == sig][["Estimate"]]
      
      ## Non Marginal Change
      t0 <- "tauno"
      t1 <- "tau"
      sc <- "No Tax"
      up <- down <- non.marginal.change(lin, data.st, "p_cml", t0, t1, theta, sig, "eta_m", min, max, 0, 0)
      results.nonmarginal<- rbind(results.nonmarginal, data.table(state, down, up, sc, theta, sigma = sig, K = 1, D = 1, s1 = 1, s2 = 1, it1 = 0, it2 = 0, ConsChckUp = 1, ConsChckDown = 1))
      
      
      t0 <- "tau"
      t1 <- "tau5"
      sc <- "plus 5 Tax"
      up <- down <- non.marginal.change(lin, data.st, "p_cml", t0, t1, theta, sig, "eta_m", min, max, 0, 0)
      results.nonmarginal<- rbind(results.nonmarginal, data.table(state, down, up, sc, theta, sigma = sig, K = 1, D = 1, s1 = 1, s2 = 1, it1 = 0, it2 = 0, ConsChckUp = 1, ConsChckDown = 1))
      
    }
  }
}


## 4. Set up IV estimates for each sigma
# For L = 1
IVs1 <- IVs[outcome == "IV", -c("Std. Error", "controls", "rn", "Cluster s.e.", "t value", "Pr(>|t|)", "outcome")]
# For L > 1
IVs2 <- dcast(IVs[n.groups > 1], n.groups + lev + sigma ~ outcome,  fun=sum, value.var = c("Estimate"))
IVs2[, w.ln_cpricei2 := w.ln_cpricei2_sig0.25 + w.ln_cpricei2_sig0.5 + w.ln_cpricei2_sig0.75 + w.ln_cpricei2_sig1]
IVs2[, Estimate := w.ln_quantity3/w.ln_cpricei2]
IVs2 <- IVs2[, -c(paste0("w.ln_cpricei2_sig", c(0.25, 0.5, 0.75, 1)), "w.ln_quantity3", "w.ln_cpricei2")]
# Merge and Order appropiately
res.ivs <- rbind(IVs1, IVs2)
res.ivs <- res.ivs[order(sigma, n.groups, lev)]
rm(IVs, IVs1, IVs2)

## 5. Load min criteria (we already computed them for the hypothetical markets no need to do it again)
min.criteria <- fread(out.file.mc.welf.hyp)
setnames(min.criteria, c("K", "D"), c("Degree", "L"))

## 5. Open Min - Max files
res.pq <- fread(pq.output.salience.results.file)


## 6. Set up Ks
# K.test <- c(7,10)
K.test <- c(2, 8)
#K.test <- 8

## 7. Set up Optimization Parameters (algorithm for now)
nlo.opts.local.df <- list(
  "algorithm"="NLOPT_LN_COBYLA",
  "maxeval" = 150000,
  "xtol_rel"=1.0e-8
)

# Options for Gurobi's min criterion calculation
params <- list()
params$NumericFocus <- 3
params$ScaleFlag <- 2
params$Method <- 1
params$Presolve <- 0
tolerance <- 1e-6
params$FeasibilityTol <- tolerance


for (sc in scenarios) {
  
  if (sc == "No Tax") {
    t0 <- "tauno"
    t1 <- "tau"
  } 
  if (sc == "plus 5 Tax")  {
    t0 <- "tau"
    t1 <- "tau5"
  }
  
  ## B. Loop across K
  for (K in K.test) {
    
    ## B.1. Load Matrix of gamma (this extrictly depends on K since the basis change)
    in.file <- paste0("Data/Demand_gamma_sat_initial_price_semester_salience_K", K,"_bern.csv")
    gamma.full.data <- fread(in.file)
    
    ## C. Loop across sigmas
    i <- 0
    for (sig in sigmas.test) {
      i <- i + 1
      thetas.test <- thetas.list[[i]]
      ## C.1 Extract support to use
      p.min <- res.pq[extrap == sc & sigma == sig][["min.p"]]
      p.max <- res.pq[extrap == sc & sigma == sig][["max.p"]]
      
      ## C.2 Restrict gamma file. Constant across p
      gamma <- gamma.full.data[extrap == sc & n.groups < 3 & sigma == sig][, c(paste0("b", 0:(K-1)), "n.groups"), with = F]             ## For elasticity
      
      ## D Start Loop at number of groups
      for (D in 2) { #unique(gamma$n.groups)
        
        ## D1. Build the constraints matrix 
        constr <- as.matrix(gamma[n.groups == D][, -c("n.groups")])   ## For elasticity
        
        ## D2. Retrieve IVs
        IVs <- res.ivs[n.groups == D  & sigma == sig][["Estimate"]] 
        
        ## D3. Load min.criterion for case (note that if there is no value it is 0)
        mc <- min.criteria[Degree == K & L == D & sigma == sig & extrap == sc,][["min.criteria"]]
        
        ## D4. Generate an initial value somewhere in the middle to test algorithms
        init.old<- init.val0 <- get.init.val(constr, IVs, mc)
        print(init.val0)
        print(constr)
        print(IVs)
        print(mc)
        
        
        ## E. Loop across thetas
        for (theta in thetas.test) {
          
          ## F Loop across states
          ## A4. Loop across states
          welfare.st <- foreach (state= states.test, .combine=rbind) %dopar% {
            
            # F1. Subset data
            st.data <- data[fips_state == state,]
            
            # F2. Non Marginal change
            # B3 Run minimization: derivative free 
            res0 <- nloptr( x0=init.val0,
                            eval_f= non.marginal.change,
                            eval_g_ineq = eval_restrictions_nmarg,
                            opts = nlo.opts.local.df,
                            data = st.data,
                            pp = "p_cml", 
                            t0 = t0, 
                            t1 = t1,
                            theta = theta,
                            sigma = sig,
                            w = "eta_m", 
                            min = p.min, 
                            max = p.max,
                            constr_mat = constr, 
                            IV_mat = IVs, 
                            min.crit = mc,
                            elas = T,
                            ub = rep(0, K),
                            lb = rep(min(IVs)/min(constr), K)
            )       
            # B4. Extract minimization results
            down <- res0$objective
            s1 <- res0$status
            it1 <- res0$iterations
            
            
            ConsChckDown <- all.equal(sum(abs(constr%*%(as.matrix(res0$solution)) - IVs) - mc), 0)
            # B5. Check constraint is met
            
            # B5 Run maximization: derivative free 
            res0 <- nloptr( x0=init.val0,
                            eval_f= max.non.marginal.change,
                            eval_g_ineq = eval_restrictions_nmarg,
                            opts = nlo.opts.local.df,
                            data = st.data,
                            pp = "p_cml", 
                            t0 = t0, 
                            t1 = t1,
                            theta = theta,
                            sigma = sig,
                            w = "eta_m", 
                            min = p.min, 
                            max = p.max,
                            constr_mat = constr, 
                            IV_mat = IVs, 
                            min.crit = mc,
                            elas = T,
                            ub = rep(0, K),
                            lb = rep(min(IVs)/min(constr), K)
            )       
            # B6. Extract minimization results
            up <- sol <- -res0$objective
            s2 <- res0$status
            it2 <- res0$iterations
            ConsChckUp <- all.equal(sum(abs(constr%*%(as.matrix(res0$solution)) - IVs) - mc), 0)
            
            
            
            data.table(down, up, state, sc, D , K, sigma = sig, theta, s1, s2, it1, it2, ConsChckDown, ConsChckUp)
            
          }
          results.nonmarginal <- rbind(results.nonmarginal, welfare.st)
          fwrite(results.nonmarginal, out.file.nonmarginal)
          
        }
      }    
    }  
  }
}

### 17. Estimation of welfare: extrapolation of average ---------


# 0. Parallelize options
# use the environment variable SLURM_NTASKS_PER_NODE to set the number of cores
registerDoParallel(cores=(Sys.getenv("SLURM_NTASKS_PER_NODE")))

# 1. Open data
data <- fread(binned.data.tax)
data[, p_cml := p_m - tau]
data[, tauno := 0]
data[, tau5 := tau + log(1+0.05)]

# 2. Open IV estimates
IVs <- fread(iv.output.salience.results.file)
IVs <- IVs[controls == "division_by_module_by_time"]

# 3. Values to Tests. In practice, this is done in batches to avoid running all cases and exceed the running time

sigmas.test <- c(0.25, 0.5, 0.75, 1)

thetas.list <- list()
# All
thetas.list$s25 <- c(0, 0.058897778, 0.564015475)
thetas.list$s50 <- c(0, 0.018501935, 0.202776786)
thetas.list$s75 <- c(0, 0.008007016, 0.102212766)
thetas.list$s100 <- c(0, 0.004861793, 0.066616166)
# # Priority
# thetas.list$s25 <- c(0)
# thetas.list$s50 <- c(0)
# thetas.list$s75 <- c(0)
# thetas.list$s100 <- c(0, 0.066616166)
# # Priority 2
# thetas.list$s25 <- c(0.058897778, 0.564015475)
# thetas.list$s50 <- c(0.018501935, 0.202776786)
# thetas.list$s75 <- c(0.008007016, 0.102212766)
# thetas.list$s100 <- c(0.004861793)

states.test <- unique(data$fips_state)

## 4. Set up IV estimates for each sigma
# For L = 1
IVs1 <- IVs[outcome == "IV", -c("Std. Error", "controls", "rn", "Cluster s.e.", "t value", "Pr(>|t|)", "outcome")]
# For L > 1
IVs2 <- dcast(IVs[n.groups > 1], n.groups + lev + sigma ~ outcome,  fun=sum, value.var = c("Estimate"))
IVs2[, w.ln_cpricei2 := w.ln_cpricei2_sig0.25 + w.ln_cpricei2_sig0.5 + w.ln_cpricei2_sig0.75 + w.ln_cpricei2_sig1]
IVs2[, Estimate := w.ln_quantity3/w.ln_cpricei2]
IVs2 <- IVs2[, -c(paste0("w.ln_cpricei2_sig", c(0.25, 0.5, 0.75, 1)), "w.ln_quantity3", "w.ln_cpricei2")]
# Merge and Order appropiately
res.ivs <- rbind(IVs1, IVs2)
res.ivs <- res.ivs[order(sigma, n.groups, lev)]
rm(IVs, IVs1, IVs2)

## 5. Load min criteria (we already computed them for the hypothetical markets no need to do it again)
min.criteria <- fread(out.file.mc.welf.hyp)
setnames(min.criteria, c("K", "D"), c("Degree", "L"))

## 5. Open Min - Max files
res.pq <- fread(pq.output.salience.results.file)


## 6. Set up Ks
# K.test <- c(7,10)
K.test <- c(2, 8)



K.test <- 8
scenarios <- c("Original", "No Tax", "plus 5 Tax")
scenarios <- c("No Tax", "plus 5 Tax")

## 7. Set up Optimization Parameters (algorithm for now)
nlo.opts.local.df <- list(
  "algorithm"="NLOPT_LN_COBYLA",
  "maxeval" = 600,
  "xtol_rel"=1.0e-8
)


results <- data.table(NULL)

for (sc in scenarios) {
  
  if (sc == "No Tax") {
    t0 <- "tauno"
    t1 <- "tau"
  } 
  if (sc == "plus 5 Tax")  {
    t0 <- "tau"
    t1 <- "tau5"
  }
  
  for (K in K.test) {
    
    ## B.1. Load Matrix of gamma (this extrictly depends on K since the basis change)
    in.file <- paste0(theta.berstein.sal, K,"_bern.csv")
    gamma.full.data <- fread(in.file)
    
    ## C. Loop across sigmas
    i <- 0
    for (sig in sigmas.test) {
      i <- i + 1
      thetas.test <- thetas.list[[i]]
      ## C.1 Extract support to use
      p.min <- res.pq[extrap == sc & sigma == sig][["min.p"]]
      p.max <- res.pq[extrap == sc & sigma == sig][["max.p"]]
      
      ## C.2 Restrict gamma file. Constant across p
      gamma <- gamma.full.data[extrap == sc & n.groups < 3 & sigma == sig][, c(paste0("b", 0:(K-1)), "n.groups"), with = F]             ## For elasticity
      
      ## D Start Loop at number of groups
      for (D in 2) { #unique(gamma$n.groups)
        
        ## D1. Build the constraints matrix 
        constr <- as.matrix(gamma[n.groups == D][, -c("n.groups")])   ## For elasticity
        
        ## D2. Retrieve IVs
        IVs <- res.ivs[n.groups == D  & sigma == sig][["Estimate"]] 
        
        ## D3. Load min.criterion for case
        mc <- min.criteria[Degree == K & L == D & sigma == sig & extrap == "Original",][["min.criteria"]]
        
        ## D4. Generate an initial value somewhere in the middle to test algorithms
        # init.val0max <- init.val0min <- get.init.val(constr, IVs, mc)
        
        ## E. Loop across thetas
        for (theta in thetas.test) {
          
          case <- data.table(sc, D , K, sigma = sig, theta)
          target <- merge(prev.sol, case, by = c("sc", "sigma", "theta", "K", "D"))
          
          init.val0min <- target[["sol1"]]
          init.val0max <- target[["sol2"]]
          target <- target[, .(it1 = mean(it1), it2 = mean(it2),
                               down = mean(down), up = mean(up),
                               s1 = mean(s1), s2 = mean(s2)), by = .(sc,sigma,theta,K,D)]
          
          
          if (sc == "Original") {
            # F2. Marginal change
            # F2a1. Min calculation
            res0 <- nloptr( x0=init.val0min,
                            eval_f= av.marginal.change,
                            eval_g_ineq = eval_restrictions_marg_av,
                            opts = nlo.opts.local.df,
                            data = data,
                            pp = "p_cml",
                            tau = "tau",
                            theta = theta,
                            sigma = sig,
                            w = "eta_m", 
                            st.code = "fips_state", 
                            min = p.min, 
                            max = p.max,
                            constr_mat = constr, 
                            IV_mat = IVs, 
                            min.crit = mc,
                            elas = T,
                            ub = rep(0, K),
                            lb = rep(min(IVs)/min(constr), K)
            )
            # F2a2 Results extraction
            down <- res0$objective
            s1 <- res0$status
            it1 <- res0$iterations
            sol1 <- res0$solution
            # F2b1. Max calculation
            res0 <- nloptr( x0=init.val0max,
                            eval_f= max.av.marginal.change,
                            eval_g_ineq = eval_restrictions_marg_av,
                            opts = nlo.opts.local.df,
                            data = data,
                            pp = "p_cml",
                            tau = "tau",
                            theta = theta,
                            sigma = sig,
                            w = "eta_m", 
                            st.code = "fips_state", 
                            min = p.min, 
                            max = p.max,
                            constr_mat = constr, 
                            IV_mat = IVs, 
                            min.crit = mc,
                            elas = T,
                            ub = rep(0, K),
                            lb = rep(min(IVs)/min(constr), K)
            )
            # F2b2 Results extraction
            up <- -res0$objective
            s2 <- res0$status
            it2 <- res0$iterations
            sol2 <- res0$solution
            
          }
          else {
            # F2. Non Marginal change
            # B3 Run minimization: derivative free 
            if (target[["it1"]] == 400) {
              res0 <- nloptr( x0=init.val0min,
                              eval_f= av.non.marginal.change.parallel,
                              eval_g_ineq = eval_restrictions_nmarg_av,
                              opts = nlo.opts.local.df,
                              data = data,
                              pp = "p_cml", 
                              t0 = t0, 
                              t1 = t1,
                              theta = theta,
                              sigma = sig,
                              w = "eta_m", 
                              st.code = "fips_state", 
                              min = p.min, 
                              max = p.max,
                              constr_mat = constr, 
                              IV_mat = IVs, 
                              min.crit = mc,
                              elas = T,
                              ub = rep(0, K),
                              lb = rep(min(IVs)/min(constr), K)
              )       
              # B4. Extract minimization results
              down <- res0$objective
              s1 <- res0$status
              it1 <- res0$iterations
              sol1 <- res0$solution
            }
            else {
              down <- target[["down"]]
              s1 <- target[["s1"]]
              it1 <- target[["it1"]]
              sol1 <- init.val0min
            }
            
            
            if (target[["it2"]] == 400) {
              # B5 Run maximization: derivative free 
              res0 <- nloptr( x0=init.val0max,
                              eval_f= max.av.non.marginal.change.parallel,
                              eval_g_ineq = eval_restrictions_nmarg_av,
                              opts = nlo.opts.local.df,
                              data = data,
                              pp = "p_cml", 
                              t0 = t0, 
                              t1 = t1,
                              theta = theta,
                              sigma = sig,
                              w = "eta_m", 
                              st.code = "fips_state", 
                              min = p.min, 
                              max = p.max,
                              constr_mat = constr, 
                              IV_mat = IVs, 
                              min.crit = mc,
                              elas = T,
                              ub = rep(0, K),
                              lb = rep(min(IVs)/min(constr), K)
              )       
              # B6. Extract minimization results
              up <- sol <- -res0$objective
              s2 <- res0$status
              it2 <- res0$iterations
              sol2 <- res0$solution
            }
            else {
              up <- target[["up"]]
              s2 <- target[["s2"]]
              it2 <- target[["it2"]]
              sol2 <- init.val0max
            }
            
          }
          ## F2c Export
          welfare.theta <- data.table(down, up, sc, D , K, sigma = sig, theta, s1, s2, it1, it2, sol1, sol2, R)
          results <- rbind(results, welfare.theta)
          fwrite(results, out.file.average)
          
        }
        
      }
    }    
  }  
}
