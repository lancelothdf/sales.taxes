## Sales taxes Project. Household Panel
# Exploring Data Set Unbalance
# This code runs basic statistics to test for unbalance
# Author: Santiago Lacouture


library(data.table)
library(lfe)
library(futile.logger)
library(AER)
library(multcomp)
library(psych)
library(ggplot2)

setwd("/project2/igaarder/Data/Nielsen/Household_panel")

## Filepaths
# Exporting results to my folder in the server

purchases.full <- fread("cleaning/consumer_panel_2006-2016_ids.csv")

### Identify households missing in each quarter

## Step 1. Keep data actually used (sales tax)
# Drop "magnet" purchases: 
purchases.full[, sum(is.na(projection_factor))]
# 2016154 obs
purchases.nomagnet <- purchases.full[!is.na(projection_factor)]

# Drop purchases without sales tax data
purchases.full[, sum(is.na(sales_tax))]
# More than 70%: 181247984 obs
purchases.retail <- purchases.nomagnet[!is.na(sales_tax)]

## Step 2. "balance" the data
purchases.retail$quarter_of_year <- factor(with(purchases.retail, 
                                             interaction(year, quarter)))


setkey(purchases.retail, household_by_store_by_module, quarter_of_year)
flog.info("Expanding data")
purchases.retail <- purchases.retail[, list(quarter_of_year = seq.int(quarter_of_year[1L], quarter_of_year[.N])),
                                 by = list(household_by_store_by_module, product_module_code, household_code,
                                           product_group_code, store_code_uc, channel_code, fips_county,
                                           fips_state)]
purchases.retail <- purchases.retail[setkey(purchases.retail)]

## Now I have to drop real missings: households that actually do not appear in a quarter
flog.info("Drop 'Real' Missings")
purchases.retail <- purchases.retail[, missing := mean(!is.na(total_expenditures)), 
                                  by = .(household_code, quarter_of_year) ]
purchases.retail <- purchases.retail[missing == 1]
## Now I build a new variable for attition of module x store by transform expenses
flog.info("Building 'Attrition' variable")
purchases.retail <- purchases.retail[, purchased := !is.na(total_expenditures)]

## Extrapolate sales tax for new observations
flog.info("Extrapolating missings")
purchases.retail <- purchases.retail[, .( ln_sales_tax := mean(ln_sales_tax, na.rm = T), 
                                          module_by_time := mean(module_by_time, na.rm = T) 
                                          ), by = .(product_module_code, quarter_of_year)]
## Extrapolate projection factor for new observations
purchases.retail <- purchases.retail[, projection_factor := mean(projection_factor, na.rm = T), 
                                          , by = .(household_by_store_by_module, quarter_of_year)]


## Estimate desired especification on this new variable

# Log Share of Expenditure
formula0 <- as.formula(paste0(
  "purchased ~ ln_sales_tax | module_by_time + household_by_store_by_module"
))

flog.info("Estimating Balance")
res0 <- felm(data = purchases.retail,
             formula = formula0,
             weights = purchases.retail$projection_factor,
             na.omit)

flog.info("Writing results...")
res0.dt <- data.table(coef(summary(res0)), keep.rownames=T)
res0.dt[, outcome := "purchased"]
res0.dt[, Rsq := summary(res0)$r.squared]
res0.dt[, adj.Rsq := summary(res0)$adj.r.squared]
res0.dt[, specification := "Basic"]
LRdiff_res <- res0.dt ### Create table LRdiff_res in which we store all results (we start with the results we had just stored in res1.dt)
LRdiff_res$N <- sum((!is.na(purchases.retail$purchased)))

fwrite(LRdiff_res, "../../../../../home/slacouture/HMS/Basic_balance_Results.csv")