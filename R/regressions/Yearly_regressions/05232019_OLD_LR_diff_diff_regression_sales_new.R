### In this R-file we explore a specification meant to estimate long-run pass-through and sales/quantity response to tax changes

library(data.table)
library(lfe)
library(futile.logger)
library(AER)


setwd("/project2/igaarder")

## useful filepaths ------------------------------------------------------------
all_goods_pi_path <- "Data/Nielsen/price_quantity_indices_allitems_2006-2016_notaxinfo.csv"
taxable_pi_path <- "Data/Nielsen/price_quantity_indices_taxableitems_2006-2016.csv"
eventstudy_tr_path <- "Data/event_study_tr_groups_comprehensive_firstonly_no2012q4_2013q1q2.csv"
tr_groups_path <- "Data/tr_groups_comprehensive_firstonly_no2012q4_2013q1q2.csv"


### Prepare the data
all_pi <- fread(all_goods_pi_path)
all_pi <- all_pi[year %in% 2006:2014 & !is.na(cpricei) & !is.na(sales_tax)]

# limit it to taxable goods
#all_pi <- all_pi[sales_tax > 1 | (year < 2008 & is.na(sales_tax))]

## take logs
all_pi[, ln_pricei := log(pricei)]
all_pi[, sales_tax := log(sales_tax)]
all_pi[, ln_quantity := log(sales) - log(pricei)]


## get sales weights
all_pi[, base.sales := sales[year == 2008 & quarter == 1],
       by = .(store_code_uc, product_module_code)]

all_pi <- all_pi[!is.na(base.sales) & !is.na(sales) & !is.na(ln_cpricei) &
                   !is.na(ln_sales_tax) & !is.na(ln_quantity)]
all_pi <- all_pi[, .(
  store_code_uc, product_module_code, fips_state, fips_county, year, quarter,
  sales, ln_cpricei, ln_sales_tax, ln_quantity, base.sales
)]

## balance on store-module level
keep_store_modules <- all_pi[, list(n = .N),
                             by = .(store_code_uc, product_module_code)]
keep_store_modules <- keep_store_modules[n == (2014 - 2007) * 4]

setkey(all_pi, store_code_uc, product_module_code)
setkey(keep_store_modules, store_code_uc, product_module_code)

all_pi <- all_pi[keep_store_modules]
setkey(all_pi, fips_county, fips_state)

## calculate expenditure shares
all_pi[, total_sales := sum(sales), by = .(store_code_uc, quarter, year)]
all_pi[, expend_share := sales / total_sales]
all_pi[, ln_expend_share := log(expend_share)]

## prep some variables for the regression (FE, cluster variables)
all_pi[, yr_quarter := .GRP, by = .(year, quarter)]
all_pi[, store_module := .GRP, by = .(store_code_uc, product_module_code)]
all_pi[, state_by_module := .GRP, by = .(fips_state, product_module_code)]
all_pi[, linear_time := year * 4 + quarter]
all_pi[, module_by_time := .GRP, by = .(year, quarter, product_module_code)]


###

