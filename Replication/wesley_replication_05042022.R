##### Wesley Janson
#' Sales Taxes
#' Replication File. Updated on 5/4/2022
#' This Code replicates all presented results in the current version of the draft 
#' This test is to ensure that the current version of the paper is accurate

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
output.results.file <- "Data/Replication/LRdiff_semesterly_main.csv"
output.results.file.econ <- "Data/Replication/LRdiff_semesterly_w_econ.csv"
results.file.spillovers <- "Data/Replication/DiD_spillover_estimates_csinitprice_semester.csv"
output.results.file.hh <- "Data/Replication/HH_cross_sectional_design_income_salesweight.csv"
output.results.file.crossec <- "Data/Replication/LRdiff_cross_sectional_design.csv"
output.results.file.TWFE <- "Data/Replication/LR_TWFE_dessign.csv"
iv.output.results.file <- "Data/Replication/Demand_iv_sat_initial_price_semester_boot_r.csv"
theta.output.results.file <- "Data/Replication/Demand_theta_sat_initial_price_semester_boot_r.csv"
theta.berstein <- "Data/Replication/Demand_gamma_sat_initial_price_semester_boot_r_K"
pq.output.results.file <- "Data/Replication/Demand_pq_sat_initial_price_semester_boot_r.csv"
output.table.avelas <- "Data/Replication/summary_elasticity_states.csv"
out.file.elast <- "Data/Replication/elasticity_bounds_table_berns_monot_mincreterion_d.csv"
out.file.mc <-  "Data/Replication/table_berns_monot_mincreteria_d.csv"
iv.output.results.pretax <- "Data/Replication/DID_iv_sat_initial_price_pretax_semester.csv"
theta.output.results.pretax <- "Data/Replication/Demand_theta_sat_initial_price_pretax_semester.csv"
iv.output.salience.results.file <- "Data/Replication/Demand_iv_sat_initial_price_semester_salience.csv"
theta.output.salience.results.file <- "Data/Replication/Demand_theta_sat_initial_price_semester_salience.csv"
conduct.parameter.file <- "Data/Replication/salience_conduct_parameter_at_p.csv"
pq.output.salience.results.file <- "Data/Replication/Demand_pq_sat_initial_price_semester_salience.csv"
theta.berstein.sal <- "Data/Replication/Demand_gamma_sat_initial_price_semester_salience_K"
out.file.hyp.nonmarginal <- "Output/Replication/nonmarginal_hypothetical_def2.csv"
out.file.hyp.marginal <- "Output/Replication/marginal_hypothetical_def2.csv"
out.file.mc.welf.hyp <- "Output/Replication/mincriteria_all.csv"
binned.data.price <- "Data/Replication/extraction_state_binned_price.csv"
binned.data.tax <- "Data/Replication/extraction_state_binned_tax.csv"
out.file.marginal <- "Data/Replication/marginal_extrapoaltion_state_priority.csv"
out.file.nonmarginal <- "Data/Replication/nonmarginal_extrapoaltion_state_priority.csv"
out.file.average <- "Data/Replication/average_extrapolation_state_priority.csv"


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


