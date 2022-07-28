## This file combines the sales tax data with exemption status (+ sales data) to create a panel of tax rate at the module by county-level (by month and by quarter)
## We will produce both an old version (the closest we could get to the 2020 results) and a new where we use the extended panel

library(data.table)
library(lfe)
library(futile.logger)
library(AER)
library(readstata13)
library(stringr)

setwd("/project2/igaarder")


## useful filepaths ------------------------------------------------------------
sales_data_path <- "Data/sales_monthly_2006-2016.csv"
tax_rate_data_path <- "Data/county_monthly_tax_rates.csv"
taxability_data_path <- "Data/taxability_state_panel.csv"
expanded_data_path <- "Data/expanded_state_sales.csv"    #2005-2006 & 2015-2016 Dataset

## output ----
monthly_output_path <- "Data/monthly_tax_rates.csv"
quarterly_output_path <- "Data/quarterly_tax_rates.csv"
monthly_output_path_old <- "Data/monthly_tax_rates_old.csv"
quarterly_output_path_old <- "Data/quarterly_tax_rates_older.csv"


##### Monthly Data
### Make monthly tax rates
sales.data <- fread(sales_data_path)
tax.data <- fread(tax_rate_data_path)
taxability.data <- fread(taxability_data_path)
expanded.data <- fread(expanded_data_path)


### Make expanded dataset mergeable
expanded.data$year<-as.numeric(str_sub(expanded.data$date, -4, -1)) #Create year variable
expanded.data$month<-rep(seq(1,12,1), nrow(expanded.data)/12)  # Create month variable
expanded.data$state_tax<-expanded.data$sales_tax_rate/100
setnames(expanded.data, "sales_tax_rate", "state_tax")

### Keep only relevant variables
expanded.data <- expanded.data[, c("year", "month", "fips_state", "state_tax")]
sales.data <- sales.data[, c("store_code_uc", "product_module_code", "year", "month", "sales", "nweeks", "fips_state", "fips_county")]
taxability.data <- taxability.data[, c("year", "month", "fips_state", "product_module_code", "taxability", "reduced_rate")]
tax.data <- tax.data[, c("year", "month", "fips_state", "fips_county", "sales_tax", "state_tax")]

# old version
sales.data.old <- copy(sales.data)

### Generate New Tax Variables for 2006/07 & 2015/16 in new version
all.tax <- rbind(tax.data, expanded.data, fill = T)
all.tax <- all.tax[year < 2008, 
                   sales_tax := sales_tax[year == 2008 & month == 1] - 
                     state_tax[year == 2008 & month == 1] + state_tax,
                   by = c("fips_state", "fips_county", "year", "month")]

all.tax <- all.tax[year > 2014, 
                   sales_tax := sales_tax[year == 2014 & month == 12] - 
                     state_tax[year == 2014 & month == 12] + state_tax,
                   by = c("fips_state", "fips_county", "year", "month")]


### Merge Datasets
sales.data <- merge(sales.data, taxability.data, by = c("fips_state", "product_module_code", "year", "month"), all.x = T)
sales.data <- merge(sales.data, all.tax, by = c("fips_state", "fips_county", "year", "month"))

## Create tax rates
sales.data[, sales_tax := 1 + sales_tax]
sales.data[, reduced_rate := 1 + reduced_rate]
sales.data[, sales_tax := ifelse(taxability == 0, 1, sales_tax)]
sales.data[, sales_tax := ifelse(taxability == 2, NA, sales_tax)]
sales.data[, sales_tax := ifelse(is.na(reduced_rate) == F, reduced_rate, sales_tax)]



# old version
sales.data.old <- merge(sales.data.old, taxability.data, by = c("fips_state", "product_module_code", "year", "month"), all.x = T)
sales.data.old <- merge(sales.data.old, tax.data, by = c("fips_state", "fips_county", "year", "month"))


## Create tax rates
sales.data.old[, sales_tax := 1 + sales_tax]
sales.data.old[, reduced_rate := 1 + reduced_rate]
sales.data.old[, sales_tax := ifelse(taxability == 0, 1, sales_tax)]
sales.data.old[, sales_tax := ifelse(taxability == 2, NA, sales_tax)]
sales.data.old[, sales_tax := ifelse(is.na(reduced_rate) == F, reduced_rate, sales_tax)]

## Create sales weights
sales.data[, sales := sales*nweeks]
sales.data[, quarter := ceiling(month/3)]
sales.data.old[, sales := sales*nweeks]
sales.data.old[, quarter := ceiling(month/3)]


sales.data.t <- sales.data[, c("store_code_uc", "product_module_code", "year", "month", "sales_tax", "taxability")]
sales.data.old.t <- sales.data.old[, c("store_code_uc", "product_module_code", "year", "month", "sales_tax", "taxability")]
fwrite(sales.data.t, monthly_output_path)
fwrite(sales.data.old.t, monthly_output_path_old)
rm(sales.data.t, sales.data.old.t, monthly_output_path, monthly_output_path_old)

## Collapse to Quarterly data
sales.data[, taxability := ifelse(taxability == 2, NA, taxability)]
sales.data <- sales.data[, list(sales_tax = mean(sales_tax), sales_tax_wtd = weighted.mean(sales_tax, w = sales), taxability = mode(taxability)), by = .(store_code_uc, product_module_code, year, quarter)]

sales.data.old <- sales.data.old[, list(sales_tax = mean(sales_tax), sales_tax_wtd = weighted.mean(sales_tax, w = sales), taxability = mode(taxability)), by = .(store_code_uc, product_module_code, year, quarter)]
sales.data.old[, taxability := ifelse(taxability == 2, NA, taxability)]


### Save CSVs
fwrite(sales.data, quarterly_output_path)
fwrite(sales.data.old, quarterly_output_path_old)
rm(quarterly_output_path)
