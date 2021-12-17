## This file combines the sales tax data with exemption status (+ sales data) to create a panel of tax rate at the module by county-level (by month and by quarter)

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

### Keep only relevant variables
expanded.data <- expanded.data[, c("year", "month", "fips_state", "state_tax")]
sales.data <- sales.data[, c("store_code_uc", "product_module_code", "year", "month", "fips_state", "fips_county")]
taxability.data <- taxability.data[, c("year", "month", "fips_state", "product_module_code", "taxability", "reduced_rate")]
tax.data <- tax.data[, c("year", "month", "fips_state", "fips_county", "sales_tax", "state_tax")]


### Merge Datasets
sales.data <- merge(sales.data, taxability.data, by = c("fips_state", "product_module_code", "year", "month"), all.x = T)
sales.data <- merge(sales.data, tax.data, by = c("fips_state", "fips_county", "year", "month"), all = T)
sales.data <- merge(sales.data, expanded.data, by = c("fips_state", "year", "month"), all=T)
sales.data[, state_tax := ifelse(year==2006 | year==2007 | year==2015 | year==2016, state_tax.y, state_tax.x)]
sales.data <- sales.data[,-c("state_tax.x", "state_tax.y")]


sales.data[, sales_tax := 1 + sales_tax]
sales.data[, reduced_rate := 1 + reduced_rate]
sales.data[, sales_tax := ifelse(taxability == 0, 1, sales_tax)]
sales.data[, sales_tax := ifelse(taxability == 2, NA, sales_tax)]
sales.data[, sales_tax := ifelse(is.na(reduced_rate) == F, reduced_rate, sales_tax)]


### Generate New Tax Variables for 2006/07 & 2015/16
data_2008m1<-sales.data[sales.data$year==2008 & sales.data$month==1,c("year","month","store_code_uc","sales_tax")]
data_2008m1$total_tax_2008m1<-data_2008m1$sales_tax
data_2008m1 <- data_2008m1[, list(sales_tax = mean(sales_tax), total_tax_2008m1=mean(total_tax_2008m1)), by = .(store_code_uc, year, month)] # Collapse by utc/year/month
data_2008m1<-data_2008m1[,-c("year","month","sales_tax")]
sales.data <- merge(sales.data, data_2008m1, by=c("store_code_uc"), all.x=T)
sales.data[, loc2008m1 := ifelse(year==2006 | year==2007, total_tax_2008m1-state_tax, NA)]
sales.data[, sales_tax := ifelse(year==2006 | year==2007, loc2008m1+state_tax, sales_tax)]

data_2014m12 <- sales.data[sales.data$year==2014 & sales.data$month==12,c("year","month","store_code_uc","sales_tax")]
data_2014m12$total_tax_2014m12<-data_2014m12$sales_tax
data_2014m12 <- data_2014m12[, list(sales_tax = mean(sales_tax), total_tax_2014m12=mean(total_tax_2014m12)), by = .(store_code_uc, year, month)]
data_2014m12<-data_2014m12[,-c("year","month","sales_tax")]
sales.data<-merge(sales.data, data_2014m12, by=c("store_code_uc"), all.x=T)
sales.data[, loc2014m12 := ifelse(year==2015 | year==2016, total_tax_2014m12-state_tax, NA)]
sales.data[, sales_tax := ifelse(year==2015 | year==2016, loc2014m12+state_tax, sales_tax)]

### Drop Observations with no sales tax reading
sales.data <- sales.data[is.na(sales.data$sales_tax)==F,]

sales.data <- sales.data[, c("store_code_uc", "product_module_code", "year", "month", "sales_tax", "taxability")]
fwrite(sales.data, monthly_output_path)
rm(monthly_output_path)


#####################################################
##### Quarterly
### Make quarterly tax rates
sales.data <- fread(sales_data_path)
tax.data <- fread(tax_rate_data_path)
taxability.data <- fread(taxability_data_path)
expanded.data <- fread(expanded_data_path)

### Make expanded dataset mergeable
expanded.data$year<-as.numeric(str_sub(expanded.data$date, -4, -1)) #Create year variable
expanded.data$month<-rep(seq(1,12,1), nrow(expanded.data)/12)  # Create month variable
expanded.data$state_tax<-expanded.data$sales_tax_rate/100

### Keep only relevant variables
sales.data <- sales.data[, c("store_code_uc", "product_module_code", "year", "month", "sales", "nweeks", "fips_state", "fips_county")]
taxability.data <- taxability.data[, c("year", "month", "fips_state", "product_module_code", "taxability", "reduced_rate")]
expanded.data <- expanded.data[, c("year", "month", "fips_state", "state_tax")]
tax.data <- tax.data[, c("year", "month", "fips_state", "fips_county", "sales_tax", "state_tax")]

### Merge Datasets
sales.data <- merge(sales.data, taxability.data, by = c("fips_state", "product_module_code", "year", "month"), all.x = T)
sales.data <- merge(sales.data, tax.data, by = c("fips_state", "fips_county", "year", "month"), all = T)
sales.data <- merge(sales.data, expanded.data, by = c("fips_state", "year", "month"), all = T)
sales.data[, state_tax := ifelse(year==2006 | year==2007 | year==2015 | year==2016, state_tax.y, state_tax.x)]
sales.data <- sales.data[,-c("state_tax.x", "state_tax.y")]

## Create sales weights
sales.data[, sales := sales*nweeks]
sales.data[, quarter := ceiling(month/3)]

### Generate New Tax Variables for 2006/07 & 2015/16
data_2008q1<-sales.data[sales.data$year==2008 & sales.data$quarter==1,c("year","quarter","store_code_uc","sales_tax")]
data_2008q1$total_tax_2008q1<-data_2008q1$sales_tax
data_2008q1 <- data_2008q1[, list(sales_tax = mean(sales_tax), total_tax_2008q1=mean(total_tax_2008q1)), by = .(store_code_uc, year, quarter)] # Collapse by utc/year/quarter
data_2008q1<-data_2008q1[,-c("year","quarter","sales_tax")]
sales.data <- merge(sales.data, data_2008q1, by=c("store_code_uc"), all.x=T)
sales.data[, loc2008q1 := ifelse(year==2006 | year==2007, total_tax_2008q1-state_tax, NA)]
sales.data[, sales_tax := ifelse(year==2006 | year==2007, loc2008q1+state_tax, sales_tax)]

data_2014q4 <- sales.data[sales.data$year==2014 & sales.data$quarter==4,c("year","quarter","store_code_uc","sales_tax")]
data_2014q4$total_tax_2014q4<-data_2014q4$sales_tax
data_2014q4 <- data_2014q4[, list(sales_tax = mean(sales_tax), total_tax_2014q4=mean(total_tax_2014q4)), by = .(store_code_uc, year, quarter)]
data_2014q4<-data_2014q4[,-c("year","quarter","sales_tax")]
sales.data<-merge(sales.data, data_2014q4, by=c("store_code_uc"), all.x=T)
sales.data[, loc2014q4 := ifelse(year==2015 | year==2016, total_tax_2014q4-state_tax, NA)]
sales.data[, sales_tax := ifelse(year==2015 | year==2016, loc2014q4+state_tax, sales_tax)]

## Create tax rates
sales.data[, sales_tax := 1 + sales_tax]
sales.data[, reduced_rate := 1 + reduced_rate]
sales.data[, sales_tax := ifelse(taxability == 0, 1, sales_tax)]
sales.data[, sales_tax := ifelse(taxability == 2, NA, sales_tax)]
sales.data[, sales_tax := ifelse(is.na(reduced_rate) == F, reduced_rate, sales_tax)]

### Drop Observations with no sales tax reading
sales.data <- sales.data[is.na(sales.data$sales_tax)==F,]

# Create mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

## Collapse to Quarterly data
sales.data <- sales.data[, list(sales_tax = mean(sales_tax), sales_tax_wtd = weighted.mean(sales_tax, w = sales), taxability = getmode(taxability)), by = .(store_code_uc, product_module_code, year, quarter)]
sales.data[, taxability := ifelse(taxability == 2, NA, taxability)]


### Save CSVs
fwrite(sales.data, quarterly_output_path)
rm(quarterly_output_path)
