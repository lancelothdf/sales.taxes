## Sales taxes Project. Household Panel
# Further cleaning panel data
# This code further cleans panel: creates and cleans interest variables and ids

library(data.table)
library(futile.logger)
library(AER)
library(multcomp)
library(psych)


setwd("/project2/igaarder/Data/Nielsen/Household_panel")

## Filepaths

## Load clean panel
purchases.full <- fread("cleaning/consumer_panel_2006-2016.csv")


## New Variables ----------

# Convert variables in factor to create indexes
purchases.full$household_code <- factor(purchases.full$household_code)
purchases.full$store_code_uc <- factor(purchases.full$store_code_uc)
purchases.full$product_module_code <- factor(purchases.full$product_module_code)
purchases.full$quarter <- factor(purchases.full$quarter)
purchases.full$year <- factor(purchases.full$year)


## Create Indexes
purchases.full$module_by_time <- factor(with(purchases.full, 
                                             interaction(year, quarter, product_module_code )))

purchases.full <- within(purchases.full,{household_by_store_by_module<-as.numeric(
  factor(paste0(product_module_code,
                store_code_uc,
                household_code)))}) 

## Create outcome and interest variables
# -Inf values are converted to NAs

# Total expenditure per quarter 
purchases.full <- purchases.full[, sum_total_exp_quarter := sum(total_expenditures),
               by = .(household_code, year, quarter)]

# Log sales tax
purchases.full <- purchases.full[, ln_sales_tax := log(sales_tax) ]
purchases.full$ln_sales_tax[is.infinite(purchases.full$ln_sales_tax)] <- NA
# Log price
purchases.full <- purchases.full[, ln_cpricei := log(cpricei) ]
purchases.full$ln_cpricei[is.infinite(purchases.full$ln_cpricei)] <- NA
# Log expenditures
purchases.full <- purchases.full[, ln_expenditures := log(total_expenditures) ]
purchases.full$ln_expenditures[is.infinite(purchases.full$ln_expenditures)] <- NA
# Log quantities
purchases.full <- purchases.full[, ln_quantity := ln_expenditures - log(pricei) ]
purchases.full$ln_quantity[is.infinite(purchases.full$ln_quantity)] <- NA

# Share of expenditure on a given module X store
purchases.full <- purchases.full[, share_expend := total_expenditures/sum_total_exp_quarter ]
purchases.full <- purchases.full[, ln_share_expend := log(share_expend) ]
purchases.full$ln_share_expend[is.infinite(purchases.full$ln_share_expend)] <- NA

purchases.full <- purchases.full[, ln_share_expend_100 := log(100*share_expend)]
purchases.full$ln_share_expend_100[is.infinite(purchases.full$ln_share_expend_100)] <- NA

fwrite(purchases.full, "cleaning/consumer_panel_2006-2016_ids.csv")