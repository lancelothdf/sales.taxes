##### Produce the main dataset with ``imperfectly salient" prices
##### Wesley Janson and Santiago Lacouture
#' Sales Taxes
#' Replication File. Updated on 04/25/23
#' Cleaning portion of replication

library(data.table)
library(futile.logger)
library(multcomp)
library(Matrix)
library(zoo)
library(tidyverse)
library(stringr)
library(readstata13)
library(geodist)


## In this case, let us not set a directory but change below in the input and (at the end) in the output filepaths
# Because input files are from midway2 and but output is saved on midway3
setwd("/project2/igaarder")
#setwd("/Users/lancelot/Documents/Sales Taxes/Data/")

rm(list = ls())

## input filepaths -----------------------------------------------
data.semester <- "/project2/igaarder/Data/Nielsen/semester_nielsen_data.csv"
#data.semester <- "Data/Nielsen/semester_nielsen_data_old.csv"
data.taxability <- "/project2/igaarder/Data/taxability_state_panel.csv"
zillow_path <- "/project2/igaarder/Data/covariates/zillow_long_by_county_clean.csv"
zillow_state_path <- "/project2/igaarder/Data/covariates/zillow_long_by_state_clean.csv"
unemp.path <- "/project2/igaarder/Data/covariates/county_monthly_unemp_clean.csv"
wage.path <- "/project2/igaarder/Data/covariates/qcew_quarterly_clean.csv"
data.hh <- "/project2/igaarder/Data/Nielsen/Household_panel/cleaning/consumer_panel_y_hh_group_2006-2016.csv"


## Open Data ----------------

# Semesterly data
all_pi <- fread(data.semester)
#all_pi <- fread("training_data_all_pi.csv")


## Open Taxability panel
taxability <- fread(data.taxability)
#taxability <- fread("taxability_state_panel.csv")


# collapse taxability to the semester
taxability[, semester := ceiling(month/6)]
taxability <- taxability[, .(taxability = mean(taxability),
                             reduced_rate = mean(reduced_rate, na.rm = T)), 
                         by = .(product_module_code, semester, year, fips_state)]
taxability[, taxability := ifelse(!is.nan(reduced_rate), 2, taxability)]




## Nielsen Retailer Data Cleaning. Semester -----------------------

## Create 2-year differences
all_pi <- all_pi[order(store_code_uc, product_module_code, year, semester),] ##Sort on store by year-semester (in ascending order) 


all_pi[, DL.ln_cpricei2 := ln_cpricei2 - shift(ln_cpricei2, n=4, type="lag"),
       by = .(store_code_uc, product_module_code)]

all_pi[, DL.ln_pricei2 := ln_pricei2 - shift(ln_pricei2, n=4, type="lag"),
       by = .(store_code_uc, product_module_code)]


all_pi[, DL.ln_quantity3 := ln_quantity3 - shift(ln_quantity3, n=4, type="lag"),
       by = .(store_code_uc, product_module_code)]


all_pi[, DL.ln_sales_tax := ln_sales_tax - shift(ln_sales_tax, n=4, type="lag"),
       by = .(store_code_uc, product_module_code)]


all_pi[, DL.ln_sales := ln_sales - shift(ln_sales, n=4, type="lag"),
       by = .(store_code_uc, product_module_code)]

## We also use a 5-lag difference to define p^{c}_{it-1}
all_pi[, DLL.ln_cpricei2 := ln_cpricei2 - shift(ln_cpricei2, n=5, type="lag"),
       by = .(store_code_uc, product_module_code)]

all_pi[, DLL.ln_pricei2 := ln_pricei2 - shift(ln_pricei2, n=5, type="lag"),
       by = .(store_code_uc, product_module_code)]

all_pi[, DLL.ln_sales_tax := ln_sales_tax - shift(ln_sales_tax, n=5, type="lag"),
       by = .(store_code_uc, product_module_code)]

## Add a lead tax rate so that we can test for pre-trends
all_pi[, FL.ln_sales_tax := shift(ln_sales_tax, n=4, type="lead") - ln_sales_tax,
       by = .(store_code_uc, product_module_code)]



## Remove some variables no longer in use
all_pi <- all_pi[, -c("D.ln_UPC", "D.ln_raw_quant")]


# Create some necesary variables
all_pi[, module_by_time := .GRP, by = .(product_module_code, semester, year)]
all_pi[, L.ln_cpricei2 := ln_cpricei2 - DL.ln_cpricei2]
all_pi[, dm.L.ln_cpricei2 := L.ln_cpricei2 - mean(L.ln_cpricei2, na.rm = T), by = module_by_time]
all_pi[, dm.ln_cpricei2 := ln_cpricei2 - mean(ln_cpricei2, na.rm = T), by = module_by_time]
all_pi[, dm.ln_quantity3 := ln_quantity3 - mean(ln_quantity3, na.rm = T), by = module_by_time]
all_pi[, L.ln_pricei2 := ln_pricei2 - DL.ln_pricei2]
all_pi[, dm.L.ln_pricei2 := L.ln_pricei2 - mean(L.ln_pricei2, na.rm = T), by = module_by_time]
all_pi[, dm.ln_pricei2 := ln_pricei2 - mean(ln_pricei2, na.rm = T), by = module_by_time]



## Create transformed price under imperfect salience for estimations
all_pi[, L.ln_sales_tax := ln_sales_tax - DL.ln_sales_tax]

for (sig in seq(0.25, 1, 0.05)) {
  
  # build p^sigma
  all_pi[, paste0("ln_cpricei2_sig", sig) := ln_pricei2 + sig*ln_sales_tax]
  all_pi[, paste0("dm.ln_cpricei2_sig", sig) := get(paste0("ln_cpricei2_sig", sig)) - mean(get(paste0("ln_cpricei2_sig", sig)), na.rm = T), by = module_by_time]
  
  # Created lagged and de-meaned lagegd for splitting sample
  all_pi[, paste0("D.ln_cpricei2_sig", sig) := D.ln_pricei2 +sig*D.ln_sales_tax]
  all_pi[, paste0("DL.ln_cpricei2_sig", sig) := DL.ln_pricei2 +sig*DL.ln_sales_tax] ## Long Difference
  all_pi[, paste0("DLL.ln_cpricei2_sig", sig) := DLL.ln_pricei2 +sig*DLL.ln_sales_tax] ## Longer Difference
  
  all_pi[, paste0("L.ln_cpricei2_sig", sig) := get(paste0("ln_cpricei2_sig", sig)) - get(paste0("DL.ln_cpricei2_sig", sig))]
  all_pi[, paste0("dm.L.ln_cpricei2_sig", sig) := get(paste0("L.ln_cpricei2_sig", sig)) - mean(get(paste0("L.ln_cpricei2_sig", sig)), na.rm = T), by = module_by_time]
  
  names.rem <- paste0(c("D.ln_cpricei2_sig", "L.ln_cpricei2_sig", "ln_cpricei2_sig", "DLL.ln_cpricei2_sig"), sig)
  all_pi <- all_pi[, (names.rem):= NULL]
}

#names.rem <- c(paste0("dm.L.ln_cpricei2_sig", seq(0.25, 1, 0.05)))
#all_pi <- all_pi[, (names.rem):= NULL]


#### Define Common Support
control <- all_pi[DL.ln_sales_tax == 0,]
treated <- all_pi[DL.ln_sales_tax != 0,]

pct1.control <- quantile(control$dm.L.ln_cpricei2, probs = 0.01, na.rm = T, weight=control$base.sales)
pct1.treated <- quantile(treated$dm.L.ln_cpricei2, probs = 0.01, na.rm = T, weight=treated$base.sales)

pct99.control <- quantile(control$dm.L.ln_cpricei2, probs = 0.99, na.rm = T, weight=control$base.sales)
pct99treated <- quantile(treated$dm.L.ln_cpricei2, probs = 0.99, na.rm = T, weight=treated$base.sales)

all_pi[, cs_price := ifelse(dm.L.ln_cpricei2 > max(pct1.treated, pct1.control) & 
                              dm.L.ln_cpricei2 < min(pct99treated, pct99.control), 1, 0)]
rm(control, treated)
# Make sure missings are 0s
all_pi[, cs_price := ifelse(is.na(dm.L.ln_cpricei2), 0, cs_price)]

## Keep within the common support
all_pi_cs <- all_pi[cs_price == 1,]

## cut the tails (keep between 1st and 99th percentile)
pct1 <- quantile(all_pi$dm.ln_cpricei2, probs = 0.01, na.rm = T, weight=all_pi$base.sales)
pct99 <- quantile(all_pi$dm.ln_cpricei2, probs = 0.99, na.rm = T, weight=all_pi$base.sales)
all_pi_cs <- all_pi_cs[(dm.ln_cpricei2 > pct1 & dm.ln_cpricei2 < pct99),]
all_pi_cs <- all_pi_cs[, c("year", "semester", "fips_state", "fips_county", "product_module_code", "store_code_uc")]


# Define samples
all_pi_cs[(year > 2009 & year < 2015)
          , non_imp_tax_strong := 1]
all_pi_cs <- all_pi_cs[non_imp_tax_strong == 1]



##### Additional set up and CS restrictions

# Keeping common support
all_pi <- merge(all_pi, all_pi_cs, by = c("year", "semester", "fips_state", "fips_county" , "product_module_code","store_code_uc"))



## Drop leads and lags of the tax rate
names.rem <- c("F4.D.ln_sales_tax", "F3.D.ln_sales_tax", "F2.D.ln_sales_tax", "F1.D.ln_sales_tax", "L1.D.ln_sales_tax", "L2.D.ln_sales_tax", "L3.D.ln_sales_tax", "L4.D.ln_sales_tax")
all_pi <- all_pi[, (names.rem):= NULL]


# Save Datasets
fwrite(all_pi, "/project/igaarder/Data/Replication_v4/all_pi_salience.csv", showProgress = T)


