### In this R-file we create a quarterly and a yearly file with all Nielsen data and tax rate information

library(data.table)
library(lfe)
library(futile.logger)
library(AER)
library(readstata13)


setwd("/project2/igaarder")

## useful filepaths ------------------------------------------------------------
sales_data_path <- "Data/sales_quarterly_2006-2016.csv"
quarterly_tax_path <- "Data/quarterly_tax_rates.csv"
all_goods_pi_path <- "Data/all_nielsen_data_2006_2016_quarterly.csv"
all_goods_pi_path_Q1 <- "Data/all_nielsen_data_2006_2016_Q1only.csv"
FE_pindex_path <- "Data/Nielsen/Pindex_FE_yearly_all_years.csv"
output_yearly <- "Data/Nielsen/yearly_nielsen_data.csv"
monthly_tax_path <- "Data/Nielsen/sales_tax_rate_monthly_product_level.dta"
old_pi_path <- "Data/Nielsen/Quarterly_old_pi.csv"
#taxable_pi_path <- "Data/Nielsen/price_quantity_indices_taxableitems_2006-2016.csv"
#eventstudy_tr_path <- "Data/event_study_tr_groups_comprehensive_firstonly_no2012q4_2013q1q2.csv"
#tr_groups_path <- "Data/tr_groups_comprehensive_firstonly_no2012q4_2013q1q2.csv"


##Load quarterly data
all_pi <- fread(all_goods_pi_path)


## First balance the dataset to start from same sample as yearly data
all_pi <- all_pi[year %in% 2006:2016 & !is.na(cpricei)]


## balance on store-module level (only keep observations that are in every quarter)
keep_store_modules <- all_pi[, list(n = .N),
                             by = .(store_code_uc, product_module_code)]
keep_store_modules <- keep_store_modules[n == (2016 - 2005) * 4]

setkey(all_pi, store_code_uc, product_module_code)
setkey(keep_store_modules, store_code_uc, product_module_code)

all_pi <- all_pi[keep_store_modules]
setkey(all_pi, fips_county, fips_state)



## Keep only Q1 observations
all_pi <- all_pi[quarter == 1,]


all_pi <- all_pi[, c("year", "fips_state", "fips_county", "store_code_uc", "product_module_code", "pricei", "cpricei", "sales", "sales_tax")]
names(all_pi) <- c("year", "fips_state", "fips_county", "store_code_uc", "product_module_code", "pricei_Q1", "cpricei_Q1", "sales_Q1", "sales_tax_Q1")



## Load and merge to "old price indices"
old_pi <- fread(old_pi_path)
all_pi$quarter <- 1
all_pi <- merge(all_pi, old_pi, by = c("fips_state", "fips_county", "store_code_uc", "year", "quarter", "product_module_code"), all.x = T)

all_pi <- all_pi[, c("year", "fips_state", "fips_county", "store_code_uc", "product_module_code", "pricei_Q1", "cpricei_Q1", "sales_Q1", "sales_tax_Q1", "ln_pricei2")]
names(all_pi) <- c("year", "fips_state", "fips_county", "store_code_uc", "product_module_code", "pricei_Q1", "cpricei_Q1", "sales_Q1", "sales_tax_Q1", "ln_pricei2_Q1")


## Impute sales_tax for 2006-2007 and 2015-2016 (+ define cpricei for these years)
all_pi[, base.tax := sales_tax_Q1[year == 2008],
            by = .(store_code_uc, product_module_code)]
all_pi[year <= 2007,]$sales_tax_Q1 <- all_pi[year <= 2007,]$base.tax

all_pi[, base.tax := sales_tax_Q1[year == 2014], by = .(store_code_uc, product_module_code)]
all_pi[year >= 2015,]$sales_tax_Q1 <- all_pi[year >= 2015,]$base.tax


all_pi[, ln_pricei_Q1 := log(pricei_Q1)]
all_pi[, ln_cpricei_Q1 := log(cpricei_Q1)]
all_pi[, ln_sales_Q1 := log(sales_Q1)]
all_pi[, ln_quantity_Q1 := ln_sales_Q1 - ln_pricei_Q1]
all_pi[, ln_sales_tax_Q1 := log(sales_tax_Q1)] ## sales_tax_Q1 is already equal to 1+tax rate
all_pi[, ln_cpricei2_Q1 := ln_pricei2_Q1 + ln_sales_tax_Q1]
all_pi[, ln_quantity2_Q1 := ln_sales_Q1 - ln_pricei2_Q1]

all_pi <- all_pi[, c("year", "fips_state", "fips_county", "store_code_uc", "product_module_code", "ln_cpricei_Q1", "ln_quantity_Q1", "ln_cpricei2_Q1", "ln_quantity2_Q1", "ln_sales_tax_Q1")]

fwrite(all_pi, all_goods_pi_path_Q1)
rm(all_pi)

