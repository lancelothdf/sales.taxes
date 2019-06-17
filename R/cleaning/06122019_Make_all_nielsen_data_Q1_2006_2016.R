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
all_goods_pi_path <- "Data/all_nielsen_data_2006_2016_Q1only.csv"
FE_pindex_path <- "Data/Nielsen/Pindex_FE_yearly_all_years.csv"
output_yearly <- "Data/Nielsen/yearly_nielsen_data.csv"
monthly_tax_path <- "Data/Nielsen/sales_tax_rate_monthly_product_level.dta"
old_pi_path <- "Data/Nielsen/Quarterly_old_pi.csv"
#taxable_pi_path <- "Data/Nielsen/price_quantity_indices_taxableitems_2006-2016.csv"
#eventstudy_tr_path <- "Data/event_study_tr_groups_comprehensive_firstonly_no2012q4_2013q1q2.csv"
#tr_groups_path <- "Data/tr_groups_comprehensive_firstonly_no2012q4_2013q1q2.csv"


#### First Step: Make quarterly data (all_nielsen_data_2006_2016_quarterly.csv)
nonfood_pi <- read.dta13("Data/Nielsen/Price_quantity_indices_nonfood.dta")
nonfood_pi <- as.data.table(nonfood_pi)

food_pi <- fread("Data/Nielsen/price_quantity_indices_food.csv")
food_pi[, c("fips_state", "fips_county") := NULL]

all_pi <- rbind(food_pi, nonfood_pi)
all_pi <- all_pi[year <= 2016]
rm(nonfood_pi, food_pi)
gc()


### attach county and state FIPS codes, sales ----------------------------------
sales_data <- fread(sales_data_path)
sales_data <- sales_data[, .(store_code_uc, product_module_code, fips_county,
                             fips_state, quarter, year, sales)]
sales_data <- sales_data[year <= 2016]

all_pi <- merge(all_pi, sales_data, by = c("store_code_uc", "quarter", "year",
                                           "product_module_code" ))
rm(sales_data)
gc()

all.tax <- fread(quarterly_tax_path)
all_pi <- merge(all_pi, all.tax, by = c("store_code_uc", "product_module_code",
                                        "year", "quarter", "product_group_code"),
                all.x = T)
rm(all.tax)

#all_pi <- fread(quarterly_tax_path)
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
all_pi[, ln_sales_tax_Q1 := log(1 + sales_tax_Q1)]
all_pi[, ln_cpricei2_Q1 := ln_pricei2_Q1 + ln_sales_tax_Q1]
all_pi[, ln_quantity2_Q1 := ln_sales_Q1 - ln_pricei2_Q1]

all_pi <- all_pi[, c("year", "fips_state", "fips_county", "store_code_uc", "product_module_code", "ln_cpricei_Q1", "ln_quantity_Q1", "ln_cpricei2_Q1", "ln_quantity2_Q1", "ln_sales_tax_Q1")]

fwrite(all_pi, all_goods_pi_path)
rm(all_pi)

