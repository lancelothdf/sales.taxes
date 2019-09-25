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
input_old_pi_path <- "Data/Nielsen/Quarterly_old_pi.csv"
quantity_index_path <- "Data/Nielsen/Quarterly_quantity_quality_indices.csv"

output_yearly <- "Data/Nielsen/yearly_nielsen_data.csv"
#taxable_pi_path <- "Data/Nielsen/price_quantity_indices_taxableitems_2006-2016.csv"
#eventstudy_tr_path <- "Data/event_study_tr_groups_comprehensive_firstonly_no2012q4_2013q1q2.csv"
#tr_groups_path <- "Data/tr_groups_comprehensive_firstonly_no2012q4_2013q1q2.csv"

## Output: Master quarterly file
master_quarterly_path <- "Data/Nielsen/master_data_quarterly.csv"


#### First Step: Make quarterly data (all_nielsen_data_2006_2016_quarterly.csv)
nonfood_pi <- read.dta13("Data/Nielsen/Price_quantity_indices_nonfood.dta")
nonfood_pi <- as.data.table(nonfood_pi)

food_pi <- fread("Data/Nielsen/price_quantity_indices_food.csv")
food_pi[, c("fips_state", "fips_county") := NULL]

all_pi <- rbind(food_pi, nonfood_pi)
all_pi <- all_pi[year <= 2016]
rm(nonfood_pi, food_pi)
gc()

### Attach the old price indices
old_pi <- fread(input_old_pi_path)
all_pi <- merge(all_pi, old_pi, by = c("store_code_uc", "product_module_code", "year", "quarter"))
rm(old_pi)


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
                                        "year", "quarter"),
                all.x = T)
rm(all.tax)

## Impute sales_tax for 2006-2007 and 2015-2016 (+ define cpricei for these years)
all_pi[, base.tax := sales_tax_wtd[year == 2008 & quarter == 1],
            by = .(store_code_uc, product_module_code)]
all_pi[year <= 2007,]$sales_tax_wtd <- all_pi[year <= 2007,]$base.tax

all_pi[, base.tax := sales_tax_wtd[year == 2014 & quarter == 4], by = .(store_code_uc, product_module_code)]
all_pi[year >= 2015,]$sales_tax_wtd <- all_pi[year >= 2015,]$base.tax


### Create consumer price index 2
# We could include
all_pi <- all_pi[, ln_cpricei2 := ln_pricei2 + log(sales_tax_wtd)]

## Also need to create consumer price with pricei - (replace the one in the original file because imputed tax rate is not correct)
all_pi <- all_pi[, cpricei := pricei*sales_tax_wtd]

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
 yearly_data <- all_pi[, list(pricei = mean(pricei), quantityi = mean(quantityi), cpricei = mean(cpricei), sales = mean(sales), sales_tax = weighted.mean(sales_tax_wtd, w = sales)), by = .(store_code_uc, product_module_code, product_group_code, fips_state, fips_county, year)]
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
 
 
 ##Import the quarterly quantity indices, collapse at the yearly-level
 # !! Need to do this differently - Need to compute some yearly indices then take logs.
 quant_pi <- fread(quantity_index_path)
 quant_pi <- quant_pi[, list(ln_quantity3 = mean(ln_quantity3), ln_UPC = mean(ln_UPC), ln_raw_quant = mean(ln_raw_quant)), by = .(store_code_uc, product_module_code, year)]
 yearly_data <- merge(yearly_data, quant_pi, by = c("store_code_uc", "product_module_code", "year"))
 rm(quant_pi)


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
                    !is.na(ln_sales_tax) & !is.na(ln_quantity) & !is.na(FE_store) & !is.na(ln_quantity3)]

 ## Create alternative measures of price and quantity
 yearly_data[, ln_cpricei2 := FE_store + ln_sales_tax]
 yearly_data[, ln_pricei2 := FE_store]
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


 ##############
 ############## Add some more variables, measures,... and difference the data to get a "master file"
 ### Prepare the data
 all_pi <- fread(all_goods_pi_path)
 all_pi <- all_pi[year %in% 2006:2016 & !is.na(cpricei)]


 ### Choose the sales weighted tax rate as main measure of taxes
 all_pi[, sales_tax2 := sales_tax]
 all_pi[, sales_tax := sales_tax_wtd]


 ###
 # Merge to additional measures (FE quantity and quality indices)
 quant_pi <- fread(quantity_index_path)
 all_pi <- merge(all_pi, quant_pi, by = c("store_code_uc", "product_module_code", "year", "quarter"))
 rm(quant_pi)


 ## prep Census region/division data ------------------------------
 geo_dt <- structure(list(
   fips_state = c(1L, 2L, 4L, 5L, 6L, 8L, 9L, 10L, 12L, 13L, 15L, 16L, 17L, 18L,
                  19L, 20L, 21L, 22L, 23L, 24L, 25L, 26L, 27L, 28L, 29L, 30L,
                  31L, 32L, 33L, 34L, 35L, 36L, 37L, 38L, 39L, 40L, 41L, 42L,
                  44L, 45L, 46L, 47L, 48L, 49L, 50L, 51L, 53L, 54L, 55L, 56L),
   region = c(3L, 4L, 4L, 3L, 4L, 4L, 1L, 3L, 3L, 3L, 4L, 4L, 2L, 2L, 2L, 2L, 3L,
              3L, 1L, 3L, 1L, 2L, 2L, 3L, 2L, 4L, 2L, 4L, 1L, 1L, 4L, 1L, 3L, 2L,
              2L, 3L, 4L, 1L, 1L, 3L, 2L, 3L, 3L, 4L, 1L, 3L, 4L, 3L, 2L, 4L),
   division = c(6L, 9L, 8L,  7L, 9L, 8L, 1L, 5L, 5L, 5L, 9L, 8L, 3L, 3L, 4L, 4L,
                6L, 7L, 1L, 5L, 1L, 3L, 4L, 6L, 4L, 8L, 4L, 8L, 1L, 2L, 8L, 2L,
                5L, 4L, 3L,  7L, 9L, 2L, 1L, 5L, 4L, 6L, 7L, 8L, 1L, 5L, 9L, 5L, 3L, 8L)),
   class = "data.frame", row.names = c(NA, -50L))
 setDT(geo_dt)


 ####
 ## merge on the census region/division info
 all_pi <- merge(all_pi, geo_dt, by = "fips_state")

 # impute tax rates prior to 2008 and after 2014
 all_pi[, sales_tax := ifelse(year < 2008, sales_tax[year == 2008 & quarter == 1], sales_tax),
        by = .(store_code_uc, product_module_code)]
 all_pi[, sales_tax := ifelse(year > 2014, sales_tax[year == 2014 & quarter == 4], sales_tax),
        by = .(store_code_uc, product_module_code)]

 # create necessary variables
 all_pi[, ln_cpricei := log(cpricei)]
 all_pi[, ln_pricei := log(pricei)]
 all_pi[, ln_sales_tax := log(sales_tax)]
 all_pi[, ln_sales_tax2 := log(sales_tax2)]
 all_pi[, ln_quantity := log(sales) - log(pricei)]
 all_pi[, ln_quantity2 := log(quantityi)]
 all_pi[, ln_sales := log(sales)]
 all_pi[, store_by_module := .GRP, by = .(store_code_uc, product_module_code)]
 all_pi[, cal_time := 4 * year + quarter]
 all_pi[, module_by_time := .GRP, by = .(product_module_code, cal_time)]
 all_pi[, module_by_state := .GRP, by = .(product_module_code, fips_state)]
 all_pi[, region_by_module_by_time := .GRP, by = .(region, product_module_code, cal_time)]
 all_pi[, division_by_module_by_time := .GRP, by = .(division, product_module_code, cal_time)]
 
 all_pi[, store_sales := sum(sales), by = .(store_code_uc, cal_time)]
 all_pi[, ln_sales_share := log(sales/store_sales)]

 ## get sales weights
 all_pi[, base.sales := sales[year == 2008 & quarter == 1],
        by = .(store_code_uc, product_module_code)]

 all_pi <- all_pi[!is.na(base.sales) & !is.na(sales) & !is.na(ln_cpricei) & !is.na(ln_pricei2) &
                    !is.na(ln_sales_tax) & !is.na(ln_quantity) & !is.na(ln_quantity2) & !is.na(ln_cpricei2)
                  & !is.na(ln_quantity3)]

 ## balance on store-module level
 keep_store_modules <- all_pi[, list(n = .N),
                              by = .(store_code_uc, product_module_code)]
 keep_store_modules <- keep_store_modules[n == (2016 - 2005) * 4]

 setkey(all_pi, store_code_uc, product_module_code)
 setkey(keep_store_modules, store_code_uc, product_module_code)

 all_pi <- all_pi[keep_store_modules]
 setkey(all_pi, store_code_uc, product_module_code, year, quarter)


 #############################################################
 ## Delete some variables to save memory
 all_pi <- all_pi[, c("fips_state", "fips_county", "year", "quarter", "store_code_uc", "product_module_code", "ln_cpricei", "ln_pricei", "ln_cpricei2", "ln_pricei2", "ln_sales","ln_quantity", "ln_quantity2", "ln_quantity3", "ln_UPC", "ln_raw_quant", "ln_sales_share", "ln_sales_tax", "base.sales", "sales", "store_by_module", "cal_time", "module_by_time", "module_by_state", "region_by_module_by_time", "division_by_module_by_time")]
 ## Here we do not carry over ln_sales_tax2 because do not want to save all leads and lags - But we should try and see if results are similar using ln_sales_tax2
  ## Also we do not carry over taxability

 #######################################################
 ## take first differences of outcomes and treatment
 all_pi <- all_pi[order(store_code_uc, product_module_code, cal_time),] ##Sort on store by year-quarter (in ascending order)


 all_pi[, D.ln_cpricei := ln_cpricei - shift(ln_cpricei, n=1, type="lag"),
        by = .(store_code_uc, product_module_code)]
 
 all_pi[, D.ln_pricei := ln_pricei - shift(ln_pricei, n=1, type="lag"),
        by = .(store_code_uc, product_module_code)]

 all_pi[, D.ln_cpricei2 := ln_cpricei2 - shift(ln_cpricei2, n=1, type="lag"),
        by = .(store_code_uc, product_module_code)]
 
 all_pi[, D.ln_pricei2 := ln_pricei2 - shift(ln_pricei2, n=1, type="lag"),
        by = .(store_code_uc, product_module_code)]

 all_pi[, D.ln_quantity := ln_quantity - shift(ln_quantity, n=1, type="lag"),
        by = .(store_code_uc, product_module_code)]

 all_pi[, D.ln_quantity2 := ln_quantity2 - shift(ln_quantity2, n=1, type="lag"),
        by = .(store_code_uc, product_module_code)]

 all_pi[, D.ln_quantity3 := ln_quantity3 - shift(ln_quantity3, n=1, type="lag"),
        by = .(store_code_uc, product_module_code)]

 all_pi[, D.ln_sales := ln_sales - shift(ln_sales, n=1, type="lag"),
        by = .(store_code_uc, product_module_code)]

 all_pi[, D.ln_sales_tax := ln_sales_tax - shift(ln_sales_tax, n=1, type="lag"),
        by = .(store_code_uc, product_module_code)]

 all_pi[, D.ln_UPC := ln_UPC - shift(ln_UPC, n=1, type="lag"),
        by = .(store_code_uc, product_module_code)]

 all_pi[, D.ln_raw_quant := ln_raw_quant - shift(ln_raw_quant, n=1, type="lag"),
        by = .(store_code_uc, product_module_code)]
 
 all_pi[, D.ln_sales_share := ln_sales_share - shift(ln_sales_share, n=1, type = "lag"),
       by = .(store_code_uc, product_module_code)]



 ## generate lags and leads of ln_sales_tax
 for (lag.val in 1:8) {
   lag.X <- paste0("L", lag.val, ".D.ln_sales_tax")
   all_pi[, (lag.X) := shift(D.ln_sales_tax, n=lag.val, type="lag"),
          by = .(store_code_uc, product_module_code)]

   lead.X <- paste0("F", lag.val, ".D.ln_sales_tax")
   all_pi[, (lead.X) := shift(D.ln_sales_tax, n=lag.val, type="lead"),
          by = .(store_code_uc, product_module_code)]
 }



### Keep only relevant years ---------------------------------------------------
 all_pi <- all_pi[between(year, 2008, 2014)]
 all_pi <- all_pi[ year >= 2009 | (year == 2008 & quarter >= 2)] ## First quarter of 2008, the difference was imputed not real data - so we drop it


### Finally keep only the variables that we may need (mostly the differenced variables)
 all_pi <- all_pi[, c("fips_state", "fips_county", "year", "quarter", "store_code_uc", "product_module_code", "D.ln_cpricei", "D.ln_pricei", "ln_cpricei2", "D.ln_cpricei2", "D.ln_pricei2", "D.ln_sales","D.ln_quantity", "D.ln_quantity2", "D.ln_quantity3", "D.ln_UPC", "D.ln_raw_quant", "D.ln_sales_share", "D.ln_sales_tax", "base.sales", "sales", "store_by_module", "cal_time", "module_by_time", "module_by_state", "region_by_module_by_time", "division_by_module_by_time", "F8.D.ln_sales_tax", "F7.D.ln_sales_tax", "F6.D.ln_sales_tax", "F5.D.ln_sales_tax", "F4.D.ln_sales_tax", "F3.D.ln_sales_tax", "F2.D.ln_sales_tax", "F1.D.ln_sales_tax", "L1.D.ln_sales_tax", "L2.D.ln_sales_tax", "L3.D.ln_sales_tax", "L4.D.ln_sales_tax", "L5.D.ln_sales_tax", "L6.D.ln_sales_tax", "L7.D.ln_sales_tax", "L8.D.ln_sales_tax")]
## We do not carry over taxability

 fwrite(all_pi, master_quarterly_path)

