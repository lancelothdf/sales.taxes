##' Sales Taxes Project
##' Extract state average, median, p25 and p75 demeaned prices (pooling time)
##' We weight modules by sales using hh data
##' 

library(data.table)
library(futile.logger)

setwd("/project2/igaarder")


## input filepaths -----------------------------------------------
#' This data is the same as all_goods_pi_path, except it has 2015-2016 data as well.
data.semester <- "Data/Nielsen/semester_nielsen_data.csv"
hhsales.semester <- "Data/Nielsen/Household_panel/cleaning/consumer_sales_semester_state_2006-2016.csv"

## output filepaths ----------------------------------------------
output.results.file <- "Data/prices_state_hhsalesweighted.csv"

### Set up Semester Data ---------------------------------
all_pi <- fread(data.semester)
all_pi[, w.ln_sales_tax := ln_sales_tax - mean(ln_sales_tax), by = .(store_by_module)]
all_pi[, w.ln_cpricei2 := ln_cpricei2 - mean(ln_cpricei2), by = .(store_by_module)]
all_pi[, w.ln_quantity3 := ln_quantity3 - mean(ln_quantity3), by = .(store_by_module)]

# Need to demean
all_pi[, module_by_time := .GRP, by = .(product_module_code, semester, year)]
all_pi[, L.ln_cpricei2 := ln_cpricei2 - D.ln_cpricei2]
all_pi[, dm.L.ln_cpricei2 := L.ln_cpricei2 - mean(L.ln_cpricei2, na.rm = T), by = module_by_time]
all_pi[, dm.ln_cpricei2 := ln_cpricei2 - mean(ln_cpricei2, na.rm = T), by = module_by_time]
all_pi[, dm.ln_quantity3 := ln_quantity3 - mean(ln_quantity3, na.rm = T), by = module_by_time]


# Defining common support
control <- all_pi[D.ln_sales_tax == 0,]
treated <- all_pi[D.ln_sales_tax != 0,]

# Price 
pct1.control <- quantile(control$dm.L.ln_cpricei2, probs = 0.01, na.rm = T, weight=control$base.sales)
pct1.treated <- quantile(treated$dm.L.ln_cpricei2, probs = 0.01, na.rm = T, weight=treated$base.sales)

pct99.control <- quantile(control$dm.L.ln_cpricei2, probs = 0.99, na.rm = T, weight=control$base.sales)
pct99treated <- quantile(treated$dm.L.ln_cpricei2, probs = 0.99, na.rm = T, weight=treated$base.sales)

all_pi[, cs_price := ifelse(dm.L.ln_cpricei2 > max(pct1.treated, pct1.control) & 
                              dm.L.ln_cpricei2 < min(pct99treated, pct99.control), 1, 0)]
# Make sure missings are 0s
all_pi[, cs_price := ifelse(is.na(dm.L.ln_cpricei2), 0, cs_price)]

## Keep within the common support
all_pi <- all_pi[cs_price == 1,]

### Calculate measures of interest --------------------

## Merge sales by module
hh.sales <- fread(hhsales.semester)
## Rename
setnames(hh.sales, old =c("fips_state_code"), new = c("fips_state"))

## Merge retailer data with sales from hh data, keeping only the observations in the best selling modules
all_pi<- merge(all_pi, hh.sales, all.x = T, by = c("year", "semester", "product_module_code", "fips_state"))

## Compute and extract the interesting data we want to plot
state.prices <- all_pi[, .(av.dm.ln_cpricei2 = mean(dm.ln_cpricei2, weights = sum_total_exp_month),
                          md.dm.ln_cpricei2 = median(dm.ln_cpricei2, weights = sum_total_exp_month),
                          p25.dm.ln_cpricei2 = quantile(dm.ln_cpricei2, weights = sum_total_exp_month, probs = 0.25),
                          p75.dm.ln_cpricei2 = quantile(dm.ln_cpricei2, weights = sum_total_exp_month, probs = 0.75)), by = .(fips_state)]

## Export that data
fwrite(state.prices, output.results.file)



