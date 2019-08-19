## Sales taxes Project. Household Panel
# Running Basic Cross-Sectional design on the household x group x year on best selling modules (identified tax rates)
# Author: Santiago Lacouture


library(data.table)
library(lfe)
library(futile.logger)
library(AER)
library(multcomp)
library(psych)
library(ggplot2)

setwd("/project2/igaarder/Data/Nielsen/Household_panel")

## Open Data
purchases.full <- fread("cleaning/consumer_panel_y_hh_group_2006-2016.csv")

## Constraining Data set for estimations ------------ 
# Drop "magnet" households: 
purchases.full[, sum(is.na(projection_factor))]
purchases.nomagnet <- purchases.full[!is.na(projection_factor)]

# Drop households without sales tax data
purchases.full[, sum(is.na(sales_tax))]
purchases.sample <- purchases.nomagnet[!is.na(sales_tax)]

# FE
purchases.sample[, group_by_time := .GRP, by = .(product_group_code, year)]
purchases.sample[, household_by_time := .GRP, by = .(year, household_code)]


## Estimations: Expenditure on type of module --------

output.results.file <- "../../../../../home/slacouture/HMS/HH_group_year_cross_sectonal_design.csv"

outcomes <- c("ln_expenditures", "ln_expenditures_taxable", "ln_expenditures_non_taxable", 
              "ln_share", "ln_share_taxable",  "ln_share_non_taxable")

LRdiff_res <- data.table(NULL)
for (Y in outcomes) {
  formula1 <- as.formula(paste0(
    Y, "~ ln_sales_tax | household_by_time + group_by_time | 0 | household_code"
  ))
  flog.info("Estimating with %s as outcome", Y)
  res1 <- felm(formula = formula1, data = purchases.sample,
               weights = purchases.sample$projection_factor)
  flog.info("Finished estimating with %s as outcome", Y)
  
  ## attach results
  flog.info("Writing results...")
  res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
  res1.dt[, outcome := Y]
  res1.dt[, Rsq := summary(res1)$r.squared]
  res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
  res1.dt[, N.obs := nrow(purchases.sample[!is.na(get(Y))])]
  res1.dt[, N_hholds := uniqueN(purchases.sample[!is.na(get(Y))], by = c("household_code"))]
  res1.dt[, N_groups := uniqueN(purchases.sample[!is.na(get(Y))], by = c("product_group_code"))]
  LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
  fwrite(LRdiff_res, output.results.file)

}



## summary values --------------------------------------------------------------
LRdiff_res$N_years <- uniqueN(purchases.sample, by = c("year"))

fwrite(LRdiff_res, output.results.file)
