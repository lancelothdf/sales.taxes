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
FE_pindex_path <- "Data/Nielsen/Pindex_FE_yearly_all_years.csv"
output_yearly <- "Data/Nielsen/yearly_nielsen_data.csv"
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

fwrite(all_pi, all_goods_pi_path)
rm(all_pi)


 ### Prepare the data
 all_pi <- fread(all_goods_pi_path)
 all_pi <- all_pi[year %in% 2006:2016 & !is.na(cpricei)]  ## Apparently, we are currently missing 2015-2016 - need to correct this
 #all_pi <- all_pi[year %in% 2006:2016 & !is.na(cpricei) & !is.na(sales_tax)]


 ## balance on store-module level (only keep observations that are in every quarter)
 keep_store_modules <- all_pi[, list(n = .N),
                              by = .(store_code_uc, product_module_code)]
 keep_store_modules <- keep_store_modules[n == (2016 - 2005) * 4]

 setkey(all_pi, store_code_uc, product_module_code)
 setkey(keep_store_modules, store_code_uc, product_module_code)

 all_pi <- all_pi[keep_store_modules]
 setkey(all_pi, fips_county, fips_state)


 ## Generate yearly variables
 #Note: all variables are quarterly averages
 yearly_data <- all_pi[, list(pricei = mean(pricei), quantityi = mean(quantityi), cpricei = mean(cpricei), sales = mean(sales), sales_tax = weighted.mean(sales_tax, w = sales)), by = .(store_code_uc, product_module_code, product_group_code, fips_state, fips_county, year)]
 rm(all_pi)


 ## Impute sales_tax for 2006-2007 and 2015-2016 (+ define cpricei for these years)
 yearly_data[, base.tax := sales_tax[year == 2008],
             by = .(store_code_uc, product_module_code)]
 yearly_data[year <= 2007,]$sales_tax <- yearly_data[year <= 2007,]$base.tax

 yearly_data[, base.tax := sales_tax[year == 2014], by = .(store_code_uc, product_module_code)]
 yearly_data[year >= 2015,]$sales_tax <- yearly_data[year >= 2015,]$base.tax


 ## take logs
 yearly_data[, ln_pricei := log(pricei)]
 yearly_data[, ln_cpricei := log(cpricei)]
 yearly_data[, ln_sales_tax := log(sales_tax)]
 yearly_data[, ln_quantity := log(sales) - log(pricei)]
 yearly_data[, ln_sales := log(sales)]


 ##Import the "FE" price indices
 FE_pi <- fread(FE_pindex_path)
 FE_pi <- FE_pi[,c("store_code_uc", "year", "FE_store", "constant", "product_module_code")]

 ## Merge with "FE" price index
 yearly_data <- merge(yearly_data, FE_pi, by = c("store_code_uc", "year", "product_module_code"))
 rm(FE_pi)


 ##Re-balance the sample
 keep_store_modules <- yearly_data[, list(n = .N),
                              by = .(store_code_uc, product_module_code)]
 keep_store_modules <- keep_store_modules[n == (2016 - 2005)]

 setkey(yearly_data, store_code_uc, product_module_code)
 setkey(keep_store_modules, store_code_uc, product_module_code)

 yearly_data <- yearly_data[keep_store_modules]


 ## get sales weights
 yearly_data[, base.sales := sales[year == 2008],
         by = .(store_code_uc, product_module_code)]

 yearly_data <- yearly_data[!is.na(base.sales) & !is.na(sales) & !is.na(ln_cpricei) &
                    !is.na(ln_sales_tax) & !is.na(ln_quantity) & !is.na(FE_store)]

 ## Create alternative measures of price and quantity
 yearly_data[, ln_cpricei2 := FE_store + ln_sales_tax]
 yearly_data[, ln_quantity2 := log(sales) - FE_store]


 ## calculate expenditure shares
 yearly_data[, total_sales := sum(sales), by = .(store_code_uc, year)]
 yearly_data[, expend_share := sales / total_sales]
 yearly_data[, ln_expend_share := log(expend_share)]


 ## prep some variables for the regression (FE, cluster variables)
 yearly_data[, yr := .GRP, by = .(year)]
 yearly_data[, store_module := .GRP, by = .(store_code_uc, product_module_code)]
 yearly_data[, state_by_module := .GRP, by = .(fips_state, product_module_code)]
 yearly_data[, linear_time := year - 2006]
 yearly_data[, module_by_time := .GRP, by = .(year, product_module_code)]
 yearly_data[, store_by_time := .GRP, by = .(year, store_code_uc)]
 yearly_data[, county_by_module := .GRP, by = .(fips_state, fips_county, product_module_code)]

 ###
 fwrite(yearly_data, output_yearly)

