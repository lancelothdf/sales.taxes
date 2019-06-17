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
output_semester <- "Data/Nielsen/semester_nielsen_data.csv"
#taxable_pi_path <- "Data/Nielsen/price_quantity_indices_taxableitems_2006-2016.csv"
#eventstudy_tr_path <- "Data/event_study_tr_groups_comprehensive_firstonly_no2012q4_2013q1q2.csv"
#tr_groups_path <- "Data/tr_groups_comprehensive_firstonly_no2012q4_2013q1q2.csv"


#### First Step: Make quarterly data (all_nielsen_data_2006_2016_quarterly.csv)
## See Make_all_nielsen_data_yearandquarter_2006_2016.R
## NOTE: for now, we do not use price index (2) because we need to estimate these price indices

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


 ## Generate a semester indicator
 all_pi[, semester := 1 + (quarter >= 3 & quarter <= 4)*1 ]


 ## Generate semester variables
 #Note: all semesters are averages over quarters
 semester_data <- all_pi[, sales_tax_Q2 := ifelse(semester == 1, sales_tax[quarter == 2], sales_tax[quarter == 4]), by = .(store_code_uc, product_module_code, year)]
 semester_data <- all_pi[, pricei_Q2 := ifelse(semester == 1, pricei[quarter == 2], pricei[quarter == 4]), by = .(store_code_uc, product_module_code, year)]
 semester_data <- all_pi[, cpricei_Q2 := ifelse(semester == 1, cpricei[quarter == 2], cpricei[quarter == 4]), by = .(store_code_uc, product_module_code, year)]
 semester_data <- all_pi[, sales_Q2 := ifelse(semester == 1, sales[quarter == 2], sales[quarter == 4]), by = .(store_code_uc, product_module_code, year)]

 semester_data <- all_pi[, list(pricei = mean(pricei), quantityi = mean(quantityi), cpricei = mean(cpricei), sales = mean(sales), sales_tax = weighted.mean(sales_tax, w = sales), sales_tax_Q2 = mean(sales_tax_Q2), pricei_Q2 = mean(pricei_Q2), cpricei_Q2 = mean(cpricei_Q2), sales_Q2 = mean(sales_Q2)), by = .(store_code_uc, product_module_code, product_group_code, fips_state, fips_county, year, semester)]
 # Note: sales_tax_Q2, pricei_Q2, cpricei_Q2 and sales_Q2 are constant within semester so when we take the mean we just get this constant
 rm(all_pi)


 ## Impute sales_tax for 2006-2007 and 2015-2016 (+ define cpricei for these years)
 semester_data[, base.tax := sales_tax[year == 2008 & semester == 1],
             by = .(store_code_uc, product_module_code)]
 semester_data[year <= 2007,]$sales_tax <- semester_data[year <= 2007,]$base.tax

 semester_data[, base.tax := sales_tax[year == 2014 & semester == 2], by = .(store_code_uc, product_module_code)]
 semester_data[year >= 2015,]$sales_tax <- semester_data[year >= 2015,]$base.tax

 semester_data[, base.tax.Q2 := sales_tax_Q2[year == 2008 & semester == 1],
               by = .(store_code_uc, product_module_code)]
 semester_data[year <= 2007,]$sales_tax_Q2 <- semester_data[year <= 2007,]$base.tax.Q2

 semester_data[, base.tax.Q2 := sales_tax_Q2[year == 2014 & semester == 2],
               by = .(store_code_uc, product_module_code)]
 semester_data[year >= 2014,]$sales_tax_Q2 <- semester_data[year >= 2014,]$base.tax.Q2


 ## take logs
 semester_data[, ln_pricei := log(pricei)]
 semester_data[, ln_pricei_Q2 := log(pricei_Q2)]
 semester_data[, ln_cpricei := log(cpricei)]
 semester_data[, ln_cpricei_Q2 := log(cpricei_Q2)]
 semester_data[, ln_sales_tax := log(sales_tax)]
 semester_data[, ln_sales_tax_Q2 := log(sales_tax_Q2)]
 semester_data[, ln_quantity := log(sales) - log(pricei)]
 semester_data[, ln_quantity_Q2 := log(sales_Q2) - log(pricei_Q2)]
 semester_data[, ln_sales := log(sales)]
 semester_data[, ln_sales_Q2 := log(sales_Q2)]


 ##Import the "FE" price indices --> Need to add the second price indices at semester level
 #FE_pi <- fread()

 ## Merge with "FE" price index
 #semester_data <- merge(semester_data, FE_pi, by = c("store_code_uc", "year", "product_module_code"))
 #rm(FE_pi)


 ##Re-balance the sample
 keep_store_modules <- semester_data[, list(n = .N),
                              by = .(store_code_uc, product_module_code)]
 keep_store_modules <- keep_store_modules[n == (2016 - 2005)*2]

 setkey(semester_data, store_code_uc, product_module_code)
 setkey(keep_store_modules, store_code_uc, product_module_code)

 semester_data <- semester_data[keep_store_modules]


 ## get sales weights
semester_data  <- semester_data[ , base.sales := sum(sales), by = .(store_code_uc, product_module_code, year)]


 semester_data[, base.sales := base.sales[year == 2008 & semester == 1],
         by = .(store_code_uc, product_module_code)]


 #semester_data <- semester_data[!is.na(base.sales) & !is.na(sales) & !is.na(ln_cpricei) &
 #                  !is.na(ln_sales_tax) & !is.na(ln_quantity) & !is.na(FE_store)]
 semester_data <- semester_data[!is.na(base.sales) & !is.na(sales) & !is.na(ln_cpricei) &
                   !is.na(ln_sales_tax) & !is.na(ln_quantity)]

 ## Create alternative measures of price and quantity
 #yearly_data[, ln_cpricei2 := FE_store + ln_sales_tax]
 #yearly_data[, ln_quantity2 := log(sales) - FE_store]


 ## calculate expenditure shares --> Decided to skip this


 ## prep some variables for the regression (FE, cluster variables)
 #semester_data[, sem := .GRP, by = .(year, semester)]
 #semester_data[, store_module := .GRP, by = .(store_code_uc, product_module_code)]
 #semester_data[, state_by_module := .GRP, by = .(fips_state, product_module_code)]
 #semester_data[, module_by_time := .GRP, by = .(year, semester, product_module_code)]
 #semester_data[, store_by_time := .GRP, by = .(year, semester, store_code_uc)]
 #semester_data[, county_by_module := .GRP, by = .(fips_state, fips_county, product_module_code)]

 ###
 fwrite(semester_data, output_semester)

