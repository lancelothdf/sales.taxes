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
rurality <- "Data/PctUrbanRural_County.csv"

## output filepaths ----------------------------------------------
output.results.file <- "Data/prices_state_hhsalesweighted_2014.csv"
output.results.file.tax <- "Data/prices_state_hhsalesweighted_taxable_2014.csv"

### Set up Semester Data ---------------------------------
all_pi <- fread(data.semester)

# Need to demean
all_pi[, module_by_time := .GRP, by = .(product_module_code, semester, year)]
all_pi[, L.ln_cpricei2 := ln_cpricei2 - D.ln_cpricei2]
all_pi[, dm.L.ln_cpricei2 := L.ln_cpricei2 - mean(L.ln_cpricei2, na.rm = T), by = module_by_time]
all_pi[, dm.ln_cpricei2 := ln_cpricei2 - mean(ln_cpricei2, na.rm = T), by = module_by_time]


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

## Add rurality
rural.data <- fread(rurality)
setnames(rural.data, old = c("STATE", "COUNTY"), new = c("fips_state", "fips_county") )
rural.data[, md.urb.pop := median(POPPCT_URBAN)]
rural.data[, urban_md := POPPCT_URBAN >= md.urb.pop , by = .(fips_state)]
rural.data <- rural.data[, c("fips_state", "fips_county" , "urban_md", "md.urb.pop")]
## Merge this data to the store
all_pi<- merge(all_pi, rural.data, all.x = T, by = c("fips_state", "fips_county"))


## Merge sales by module
hh.sales <- fread(hhsales.semester)
## Rename
setnames(hh.sales, old =c("fips_state_code"), new = c("fips_state"))

## Merge retailer data with sales from hh data, keeping only the observations in the best selling modules
all_pi<- merge(all_pi, hh.sales, all.x = T, by = c("year", "semester", "product_module_code", "fips_state"))

## Compute and extract the interesting data we want to plot

## First average across stores
states.prices <- all_pi[year ==  2014 & semester == 1, .(av.total.tax = mean(exp(ln_sales_tax)-1),
                                         av.dm.ln_cpricei2 = mean(dm.ln_cpricei2),
                                         av.dm.ln_cpricei2.urb = weighted.mean(dm.ln_cpricei2, w = urban_md),
                                         av.dm.ln_cpricei2.rur = weighted.mean(dm.ln_cpricei2, w = (1-urban_md)),
                                         md.por.urb = mean(urban_md),
                                         sales = sum(sales)
                                         ), by = .(fips_state, product_module_code, total_sales, md.urb.pop)]

state.prices.av <- states.prices[, .(av.total.tax = weighted.mean(av.total.tax, w = sales, na.rm = T),
                                     av.total.tax.home = weighted.mean(av.total.tax, w = total_sales, na.rm = T),
                                     av.dm.ln_cpricei2 = weighted.mean(av.dm.ln_cpricei2, w = sales, na.rm = T),
                                     av.dm.ln_cpricei2.home = weighted.mean(av.dm.ln_cpricei2, w = total_sales, na.rm = T),
                                     av.dm.ln_cpricei2.urb = weighted.mean(av.dm.ln_cpricei2.urb, w = sales, na.rm = T),
                                     av.dm.ln_cpricei2.urb.home = weighted.mean(av.dm.ln_cpricei2.urb, w = total_sales, na.rm = T),
                                     av.dm.ln_cpricei2.rur = weighted.mean(av.dm.ln_cpricei2.rur, w = sales, na.rm = T),
                                     av.dm.ln_cpricei2.rur.home = weighted.mean(av.dm.ln_cpricei2.rur, w = total_sales, na.rm = T),
                                     md.por.urb = mean(md.por.urb)
                                     ), by = .(fips_state, md.urb.pop )]

## Repeat using only taxable items
states.prices <- all_pi[year ==  2014 & semester == 1 & ln_sales_tax > 1, .(av.total.tax = mean(exp(ln_sales_tax)-1),
                                                         av.dm.ln_cpricei2 = mean(dm.ln_cpricei2),
                                                         av.dm.ln_cpricei2.urb = weighted.mean(dm.ln_cpricei2, w = urban_md),
                                                         av.dm.ln_cpricei2.rur = weighted.mean(dm.ln_cpricei2, w = (1-urban_md)),
                                                         md.por.urb = mean(urban_md),
                                                         sales = sum(sales)
), by = .(fips_state, product_module_code, total_sales, md.urb.pop)]

state.prices.av.tax <- states.prices[, .(av.total.tax = weighted.mean(av.total.tax, w = sales, na.rm = T),
                                     av.total.tax.home = weighted.mean(av.total.tax, w = total_sales, na.rm = T),
                                     av.dm.ln_cpricei2 = weighted.mean(av.dm.ln_cpricei2, w = sales, na.rm = T),
                                     av.dm.ln_cpricei2.home = weighted.mean(av.dm.ln_cpricei2, w = total_sales, na.rm = T),
                                     av.dm.ln_cpricei2.urb = weighted.mean(av.dm.ln_cpricei2.urb, w = sales, na.rm = T),
                                     av.dm.ln_cpricei2.urb.home = weighted.mean(av.dm.ln_cpricei2.urb, w = total_sales, na.rm = T),
                                     av.dm.ln_cpricei2.rur = weighted.mean(av.dm.ln_cpricei2.rur, w = sales, na.rm = T),
                                     av.dm.ln_cpricei2.rur.home = weighted.mean(av.dm.ln_cpricei2.rur, w = total_sales, na.rm = T),
                                     md.por.urb = mean(md.por.urb)
), by = .(fips_state, md.urb.pop )]

## Export that data
fwrite(state.prices.av, output.results.file)
fwrite(state.prices.av.tax, output.results.file.tax)


