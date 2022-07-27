### OLD Version: We used to extend 2008 backwards and 2014 onwards
## This file combines the sales tax data with exemption status (+ sales data) to create a panel of tax rate at the module by county-level (by month and by quarter)

library(data.table)
library(lfe)
library(futile.logger)
library(AER)
library(readstata13)

setwd("/project2/igaarder")


## useful filepaths ------------------------------------------------------------
sales_data_path <- "Data/sales_monthly_2006-2016.csv"
tax_rate_data_path <- "Data/county_monthly_tax_rates.csv"
taxability_data_path <- "Data/taxability_state_panel.csv"
#expanded_data_path <- "Data/expanded_state_sales.csv"    #2005-2006 & 2015-2016 Dataset

## output ----
monthly_output_path <- "Data/monthly_tax_rates_old.csv"
quarterly_output_path <- "Data/quarterly_tax_rates_old.csv"


#####################################################
##### Quarterly
# Checing permision is working
file.create(quarterly_output_path)

### Make quarterly tax rates
sales.data <- fread(sales_data_path)
tax.data <- fread(tax_rate_data_path)
taxability.data <- fread(taxability_data_path)

### Keep only relevant variables
sales.data <- sales.data[, c("store_code_uc", "product_module_code", "year", "month", "sales", "nweeks", "fips_state", "fips_county")]
taxability.data <- taxability.data[, c("year", "month", "fips_state", "product_module_code", "taxability", "reduced_rate")]

### Merge Datasets
sales.data <- merge(sales.data, taxability.data, by = c("fips_state", "product_module_code", "year", "month"), all.x = T)
sales.data <- merge(sales.data, tax.data, by = c("fips_state", "fips_county", "year", "month"))

## Create sales weights
sales.data[, sales := sales*nweeks]
sales.data[, quarter := ceiling(month/3)]


## Create tax rates
sales.data[, sales_tax := 1 + sales_tax]
sales.data[, sales_tax := ifelse(taxability == 0, 1, sales_tax)]
sales.data[, sales_tax := ifelse(taxability == 2, NA, sales_tax)]
sales.data[, sales_tax := ifelse(is.na(reduced_rate) == F, reduced_rate, sales_tax)]

## Collapse to Quarterly data
sales.data[, taxability := ifelse(taxability == 2, NA, taxability)]
sales.data <- sales.data[, list(sales_tax = mean(sales_tax), sales_tax_wtd = weighted.mean(sales_tax, w = sales), taxability = mode(taxability)), by = .(store_code_uc, product_module_code, year, quarter)]


### Save CSVs
fwrite(sales.data, quarterly_output_path, showProgress=T)
rm(quarterly_output_path)

### Make monthly tax rates
sales.data <- fread(sales_data_path)
tax.data <- fread(tax_rate_data_path)
taxability.data <- fread(taxability_data_path)


### Keep only relevant variables
sales.data <- sales.data[, c("store_code_uc", "product_module_code", "year", "month", "fips_state", "fips_county")]
taxability.data <- taxability.data[, c("year", "month", "fips_state", "product_module_code", "taxability", "reduced_rate")]
tax.data <- tax.data[, c("year", "month", "fips_state", "fips_county", "sales_tax")]


### Merge Datasets
sales.data <- merge(sales.data, taxability.data, by = c("fips_state", "product_module_code", "year", "month"), all.x = T)
sales.data <- merge(sales.data, tax.data, by = c("fips_state", "fips_county", "year", "month"))


sales.data[, sales_tax := 1 + sales_tax]
sales.data[, sales_tax := ifelse(taxability == 0, 1, sales_tax)]
sales.data[, sales_tax := ifelse(taxability == 2, NA, sales_tax)]
sales.data[, sales_tax := ifelse(is.na(reduced_rate) == F, reduced_rate, sales_tax)]


sales.data <- sales.data[, c("store_code_uc", "product_module_code", "year", "month", "sales_tax", "taxability")]
fwrite(sales.data, monthly_output_path)
rm(monthly_output_path)

