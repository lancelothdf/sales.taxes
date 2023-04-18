##### Wesley Janson and Santiago Lacouture
#' Sales Taxes
#' Replication File. Updated on 04/18/23
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
#setwd("/project2/igaarder")
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



## Remove some variables no longer in use
all_pi <- all_pi[, -c("D.ln_UPC", "D.ln_raw_quant", "D.ln_cpricei", "D.ln_quantity", "D.ln_quantity2")]



## Create 2-year differences
all_pi <- all_pi[order(store_code_uc, product_module_code, year, semester),] ##Sort on store by year-semester (in ascending order) 

all_pi[, DLL.ln_cpricei2 := ln_cpricei2 - shift(ln_cpricei2, n=5, type="lag"),
       by = .(store_code_uc, product_module_code)]

all_pi[, DL.ln_sales_tax := ln_sales_tax - shift(ln_sales_tax, n=4, type="lag"),
       by = .(store_code_uc, product_module_code)]

# Create some necesary variables
all_pi[, module_by_time := .GRP, by = .(product_module_code, semester, year)]
all_pi[, L.ln_cpricei2 := ln_cpricei2 - DLL.ln_cpricei2]
all_pi[, dm.L.ln_cpricei2 := L.ln_cpricei2 - mean(L.ln_cpricei2, na.rm = T), by = module_by_time]
all_pi[, dm.ln_cpricei2 := ln_cpricei2 - mean(ln_cpricei2, na.rm = T), by = module_by_time]



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
all_pi_cs[, all := 1]
all_pi_cs[(year > 2007 & year < 2015) 
          & !(year ==2008 & semester == 1) 
          & !(year ==2014 & semester == 2) 
          , non_imp_tax := 1]
all_pi_cs[(year > 2009 & year < 2015)
          , non_imp_tax_strong := 1]




##### Additional set up and CS restrictions

# Keeping common support
# Main data set
all_pi <- merge(all_pi, all_pi_cs, by = c("year", "semester", "fips_state", "fips_county" , "product_module_code","store_code_uc"))





# Save Datasets
fwrite(all_pi, "/project/igaarder/Data/Replication_v4/all_pi_DL.csv", showProgress = T)

