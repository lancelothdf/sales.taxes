###
### We create semesterly data.  We start from the quarterly data and take averages of consecutive quarters in the same semester.

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
input_old_pi_path <- "Data/Nielsen/Quarterly_old_pi.csv"
quantity_index_path <- "Data/Nielsen/Quarterly_quantity_quality_indices.csv"
#taxable_pi_path <- "Data/Nielsen/price_quantity_indices_taxableitems_2006-2016.csv"
#eventstudy_tr_path <- "Data/event_study_tr_groups_comprehensive_firstonly_no2012q4_2013q1q2.csv"
#tr_groups_path <- "Data/tr_groups_comprehensive_firstonly_no2012q4_2013q1q2.csv"


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


## get sales weights
all_pi[, base.sales := sales[year == 2008 & quarter == 1],
       by = .(store_code_uc, product_module_code)]

all_pi <- all_pi[!is.na(base.sales) & !is.na(sales) & !is.na(ln_cpricei) & !is.na(ln_pricei) &
                   !is.na(ln_sales_tax) & !is.na(ln_quantity) & !is.na(ln_quantity2) & !is.na(ln_cpricei2) & !is.na(ln_pricei2)
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
all_pi <- all_pi[, c("fips_state", "fips_county", "year", "quarter", "store_code_uc", "product_module_code", "ln_cpricei", "ln_pricei", "ln_cpricei2", "ln_pricei2", "ln_sales","ln_quantity", "ln_quantity2", "ln_quantity3", "ln_UPC", "ln_raw_quant", "ln_sales_tax", "base.sales", "sales")]
## Here we do not carry over ln_sales_tax2 because do not want to save all leads and lags - But we should try and see if results are similar using ln_sales_tax2
## Here we do not carry over "taxability"

## Generate a semester indicator
all_pi[, semester := 1 + (quarter >= 3 & quarter <= 4)*1 ]

## Collapse at semester-leve
#all_pi <- all_pi[, list(ln_cpricei = mean(ln_cpricei), ln_cpricei2 = mean(ln_cpricei2), ln_sales = mean(ln_sales), ln_quantity = mean(ln_quantity), ln_quantity2 = mean(ln_quantity2), ln_quantity3 = mean(ln_quantity3), ln_UPC = mean(ln_UPC), ln_raw_quant = mean(ln_raw_quant), ln_sales_tax = mean(ln_sales_tax), taxability = mean(taxability), base.sales = mean(base.sales), sales = mean(sales)), by = .(fips_state, fips_county, store_code_uc, product_module_code, year, semester)]
all_pi <- all_pi[, list(ln_cpricei = mean(ln_cpricei), ln_pricei = mean(ln_pricei), ln_cpricei2 = mean(ln_cpricei2), ln_pricei2 = mean(ln_pricei2), ln_sales = mean(ln_sales), ln_quantity = mean(ln_quantity), ln_quantity2 = mean(ln_quantity2), ln_quantity3 = mean(ln_quantity3), ln_UPC = mean(ln_UPC), ln_raw_quant = mean(ln_raw_quant), ln_sales_tax = mean(ln_sales_tax), base.sales = mean(base.sales), sales = mean(sales)), by = .(fips_state, fips_county, store_code_uc, product_module_code, year, semester)]
### Excludes taxability because there was sthg wrong with that variable

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


all_pi[, store_by_module := .GRP, by = .(store_code_uc, product_module_code)]
all_pi[, cal_time := 2 * year + semester]
all_pi[, module_by_time := .GRP, by = .(product_module_code, cal_time)]
all_pi[, module_by_state := .GRP, by = .(product_module_code, fips_state)]
all_pi[, region_by_module_by_time := .GRP, by = .(region, product_module_code, cal_time)]
all_pi[, division_by_module_by_time := .GRP, by = .(division, product_module_code, cal_time)]

all_pi[, store_sales := sum(sales), by = .(store_code_uc, cal_time)]
all_pi[, ln_sales_share := log(sales/store_sales)]


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
for (lag.val in 1:4) {
  lag.X <- paste0("L", lag.val, ".D.ln_sales_tax")
  all_pi[, (lag.X) := shift(D.ln_sales_tax, n=lag.val, type="lag"),
         by = .(store_code_uc, product_module_code)]

  lead.X <- paste0("F", lag.val, ".D.ln_sales_tax")
  all_pi[, (lead.X) := shift(D.ln_sales_tax, n=lag.val, type="lead"),
         by = .(store_code_uc, product_module_code)]
}



### Keep only relevant years ---------------------------------------------------
all_pi <- all_pi[between(year, 2008, 2014)]
all_pi <- all_pi[ year >= 2009 | (year == 2008 & semester >= 2)] ## First semester of 2008, the difference was imputed not real data - so we drop it


### Finally keep only the variables that we may need (mostly the differenced variables)
#all_pi <- all_pi[, c("fips_state", "fips_county", "year", "semester", "store_code_uc", "product_module_code", "D.ln_cpricei", "ln_cpricei2", "D.ln_cpricei2", "D.ln_sales","D.ln_quantity", "D.ln_quantity2", "D.ln_quantity3", "D.ln_UPC", "D.ln_raw_quant", "D.ln_sales_tax", "taxability", "base.sales", "sales", "store_by_module", "cal_time", "module_by_time", "module_by_state", "region_by_module_by_time", "division_by_module_by_time", "F4.D.ln_sales_tax", "F3.D.ln_sales_tax", "F2.D.ln_sales_tax", "F1.D.ln_sales_tax", "L1.D.ln_sales_tax", "L2.D.ln_sales_tax", "L3.D.ln_sales_tax", "L4.D.ln_sales_tax")]
all_pi <- all_pi[, c("fips_state", "fips_county", "year", "semester", "store_code_uc", "product_module_code", "D.ln_cpricei", "D.ln_pricei", "ln_cpricei2", "ln_pricei2", "D.ln_pricei2", "D.ln_cpricei2", "D.ln_sales", "ln_sales", "D.ln_quantity", "D.ln_quantity2", "D.ln_quantity3", "ln_quantity3", "D.ln_UPC", "D.ln_raw_quant", "D.ln_sales_share", "D.ln_sales_tax", "ln_sales_tax" , "base.sales", "sales", "store_by_module", "cal_time", "module_by_time", "module_by_state", "region_by_module_by_time", "division_by_module_by_time", "F4.D.ln_sales_tax", "F3.D.ln_sales_tax", "F2.D.ln_sales_tax", "F1.D.ln_sales_tax", "L1.D.ln_sales_tax", "L2.D.ln_sales_tax", "L3.D.ln_sales_tax", "L4.D.ln_sales_tax")]
## Taxability not on the list


 ###
 fwrite(all_pi, output_semester)

