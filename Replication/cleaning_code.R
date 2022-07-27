##### Wesley Janson
#' Sales Taxes
#' Replication File. Updated on 5/21/2022
#' Cleaning portion of replication

library(data.table)
library(futile.logger)
library(multcomp)
library(Matrix)
library(zoo)
library(tidyverse)
library(stringr)


setwd("/project2/igaarder")
rm(list = ls())

## input filepaths -----------------------------------------------
data.semester <- "Data/Nielsen/semester_nielsen_data.csv"
#data.semester <- "Data/Nielsen/semester_nielsen_data_old.csv"
data.taxability <- "Data/taxability_state_panel.csv"
zillow_path <- "Data/covariates/zillow_long_by_county_clean.csv"
zillow_state_path <- "Data/covariates/zillow_long_by_state_clean.csv"
unemp.path <- "Data/covariates/county_monthly_unemp_clean.csv"
wage.path <- "Data/covariates/qcew_quarterly_clean.csv"
data.hh <- "Data/Nielsen/Household_panel/cleaning/consumer_panel_q_hh_group_2006-2016.csv"


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
all_pi[, L.ln_pricei2 := ln_pricei2 - D.ln_pricei2]
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
all_pi_cs <- all_pi_cs[, c("year", "semester", "fips_state", "fips_county", "product_module_code", "store_code_uc")]

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

all_pi_spill_econ[, tax_exempt := taxability == 0]
all_pi_spill_econ[, taxable := taxability == 1]
all_pi_spill_econ[, T_tax_exempt := sum(tax_exempt), by = .(store_by_module)]
all_pi_spill_econ[, T_taxable := sum(taxable), by = .(store_by_module)]
all_pi_spill_econ[, T_total := .N, by = .(store_by_module)]
all_pi_spill_econ[, all_taxable:= ifelse(T_taxable == T_total,1,0)]
all_pi_spill_econ[, all_taxexempt:= ifelse(T_tax_exempt == T_total,1,0)]

# Identify statutory tax rate
all_pi_spill[, ln_statutory_tax := max(ln_sales_tax, na.rm = T), by = .(fips_state, fips_county, year, semester)]
all_pi_spill[, ln_statutory_tax := ifelse(taxability == 1, ln_sales_tax, ln_statutory_tax)]

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

# Keeping common support
# Spillovers
all_pi_spill <- merge(all_pi_spill, all_pi_cs, by = c("year", "semester", "fips_state", "fips_county" , "product_module_code","store_code_uc"))
all_pi_spill_econ <- merge(all_pi_spill_econ, all_pi_cs, by = c("year", "semester", "fips_state", "fips_county" , "product_module_code","store_code_uc"))
# Main data w. econ vars
all_pi_econ <- merge(all_pi_econ, all_pi_cs, by = c("year", "semester", "fips_state", "fips_county" , "product_module_code","store_code_uc"))
# Main data set
all_pi <- merge(all_pi, all_pi_cs, by = c("year", "semester", "fips_state", "fips_county" , "product_module_code","store_code_uc"))

# Save Datasets
fwrite(all_pi, "Data/Replication/all_pi.csv")
fwrite(all_pi_spill, "Data/Replication/all_pi_spill.csv")
fwrite(all_pi_spill_econ, "Data/Replication/all_pi_spill_econ.csv")
fwrite(all_pi_econ, "Data/Replication/all_pi_econ.csv")
fwrite(purchases.sample, "Data/Replication/purchases.sample.csv")


