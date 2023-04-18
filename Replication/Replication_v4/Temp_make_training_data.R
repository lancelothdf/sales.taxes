##### Wesley Janson and Santiago Lacouture
#' Sales Taxes
#' Replication File. Updated on 03/07/23
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

## Open Taxability panel
taxability <- fread(data.taxability)


# collapse taxability to the semester
taxability[, semester := ceiling(month/6)]
taxability <- taxability[, .(taxability = mean(taxability),
                             reduced_rate = mean(reduced_rate, na.rm = T)), 
                         by = .(product_module_code, semester, year, fips_state)]
taxability[, taxability := ifelse(!is.nan(reduced_rate), 2, taxability)]



## Nielsen Retailer Data Cleaning. Semester -----------------------
all_pi <- all_pi[store_by_module <= 10,]



# Save Datasets
fwrite(all_pi, "/project/igaarder/Data/Replication_v2/training_data_all_pi.csv", showProgress = T)


