## Sales taxes Project. Household Panel
# Running first Basic Estimations. Clustered S.E.
# Runs basic identification without covariates and without time lags
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
purchases.full <- fread("cleaning/consumer_panel_2006-2016_ids.csv")


## Constraining Data set for estimations ------------ 
# Drop "magnet" purchases: 
purchases.full[, sum(is.na(projection_factor))]
# 2016154 obs
purchases.nomagnet <- purchases.full[!is.na(projection_factor)]

# Drop purchases without sales tax data
purchases.full[, sum(is.na(sales_tax))]
# More than 70%: 181247984 obs
purchases.retail <- purchases.nomagnet[!is.na(sales_tax)]


## Building weights as in retailer data
# Approach 1. Use factor projection x share of expenditure in module X store X quarter for each household (among items in data)
purchases.retail <- purchases.retail[, expend_retail_quarter := sum(total_expenditures), by = .(household_code, quarter, year) ]
purchases.retail <- purchases.retail[, weight_quarterly := projection_factor * (total_expenditures / expend_retail_quarter) ]

#TODO: Approach 2. Same as above but using only the shares in 2008 

## Basic Specifications ----------

# Log Share of Expenditure
formula0 <- as.formula(paste0(
  "ln_share_expend ~ ln_sales_tax | module_by_time + household_by_store_by_module "
))

flog.info("Estimating Log Share")
res0 <- felm(data = purchases.retail,
             formula = formula0,
             weights = purchases.retail$weight_quarterly,
             na.omit)

flog.info("Writing results...")
res0.dt <- data.table(coef(summary(res0)), keep.rownames=T)
res0.dt[, outcome := "ln_share_expend"]
res0.dt[, Rsq := summary(res0)$r.squared]
res0.dt[, adj.Rsq := summary(res0)$adj.r.squared]
res0.dt[, specification := "Weight: PF x Share"]
res0.dt[, N_obs := sum((!is.na(purchases.retail$ln_share_expend)))]
LRdiff_res <- res0.dt ### Create table LRdiff_res in which we store all results (we start with the results we had just stored in res1.dt)
fwrite(LRdiff_res, "../../../../../home/slacouture/HMS/Basic_Results_weights.csv")


# Log Price
formula2 <- as.formula(paste0(
  "ln_cpricei ~ ln_sales_tax | module_by_time + household_by_store_by_module "
))
flog.info("Estimating Log Price")
res2 <- felm(data = purchases.retail,
             formula = formula2,
             weights = purchases.retail$weight_quarterly,
             na.omit)

flog.info("Writing results...")
res0.dt <- data.table(coef(summary(res2)), keep.rownames=T)
res0.dt[, outcome := "ln_cpricei"]
res0.dt[, Rsq := summary(res2)$r.squared]
res0.dt[, adj.Rsq := summary(res2)$adj.r.squared]
res0.dt[, specification := "Weight: PF x Share"]
res0.dt[, N_obs := sum((!is.na(purchases.retail$ln_cpricei)))]
LRdiff_res <- rbind(LRdiff_res,res0.dt) ### Append 
fwrite(LRdiff_res, "../../../../../home/slacouture/HMS/Basic_Results_weights.csv")

# Log Quantity
formula2 <- as.formula(paste0(
  "ln_quantity ~ ln_sales_tax | module_by_time + household_by_store_by_module "
))
flog.info("Estimating Log Price")
res2 <- felm(data = purchases.retail,
             formula = formula2,
             weights = purchases.retail$weight_quarterly,
             na.omit)

flog.info("Writing results...")
res0.dt <- data.table(coef(summary(res2)), keep.rownames=T)
res0.dt[, outcome := "ln_quantity"]
res0.dt[, Rsq := summary(res2)$r.squared]
res0.dt[, adj.Rsq := summary(res2)$adj.r.squared]
res0.dt[, specification := "Weight: PF x Share"]
res0.dt[, N_obs := sum((!is.na(purchases.retail$ln_quantity)))]
LRdiff_res <- rbind(LRdiff_res,res0.dt) ### Append 
fwrite(LRdiff_res, "../../../../../home/slacouture/HMS/Basic_Results_weights.csv")


## Basic Specifications ----------

# Log Share of Expenditure
formula0 <- as.formula(paste0(
  "ln_share_expend ~ ln_sales_tax | module_by_time + household_by_store_by_module | 0 | household_code"
))

flog.info("Estimating Log Share")
res0 <- felm(data = purchases.retail,
             formula = formula0,
             weights = purchases.retail$weight_quarterly,
             na.omit)

flog.info("Writing results...")
res0.dt <- data.table(coef(summary(res0)), keep.rownames=T)
res0.dt[, outcome := "ln_share_expend"]
res0.dt[, Rsq := summary(res0)$r.squared]
res0.dt[, adj.Rsq := summary(res0)$adj.r.squared]
res0.dt[, specification := "Weight: PF x Share"]
res0.dt[, N_obs := sum((!is.na(purchases.retail$ln_share_expend)))]
LRdiff_res <- res0.dt ### Create table LRdiff_res in which we store all results (we start with the results we had just stored in res1.dt)
fwrite(LRdiff_res, "../../../../../home/slacouture/HMS/Basic_Results_weights.csv")


# Log Price
formula2 <- as.formula(paste0(
  "ln_cpricei ~ ln_sales_tax | module_by_time + household_by_store_by_module | 0 | household_code"
))
flog.info("Estimating Log Price")
res2 <- felm(data = purchases.retail,
             formula = formula2,
             weights = purchases.retail$weight_quarterly,
             na.omit)

flog.info("Writing results...")
res0.dt <- data.table(coef(summary(res2)), keep.rownames=T)
res0.dt[, outcome := "ln_cpricei"]
res0.dt[, Rsq := summary(res2)$r.squared]
res0.dt[, adj.Rsq := summary(res2)$adj.r.squared]
res0.dt[, specification := "Weight: PF x Share"]
res0.dt[, N_obs := sum((!is.na(purchases.retail$ln_cpricei)))]
LRdiff_res <- rbind(LRdiff_res,res0.dt) ### Append 
fwrite(LRdiff_res, "../../../../../home/slacouture/HMS/Basic_Results_weights.csv")

# Log Quantity
formula2 <- as.formula(paste0(
  "ln_quantity ~ ln_sales_tax | module_by_time + household_by_store_by_module | 0 | household_code"
))
flog.info("Estimating Log Price")
res2 <- felm(data = purchases.retail,
             formula = formula2,
             weights = purchases.retail$weight_quarterly,
             na.omit)

flog.info("Writing results...")
res0.dt <- data.table(coef(summary(res2)), keep.rownames=T)
res0.dt[, outcome := "ln_quantity"]
res0.dt[, Rsq := summary(res2)$r.squared]
res0.dt[, adj.Rsq := summary(res2)$adj.r.squared]
res0.dt[, specification := "Weight: PF x Share"]
res0.dt[, N_obs := sum((!is.na(purchases.retail$ln_quantity)))]
LRdiff_res <- rbind(LRdiff_res,res0.dt) ### Append 
fwrite(LRdiff_res, "../../../../../home/slacouture/HMS/Basic_Results_weights.csv")



## summary values --------------------------------------------------------------
LRdiff_res$N_hholds <- length(unique(purchases.retail$household_code))
LRdiff_res$N_modules <- length(unique(purchases.retail$product_module_code))
LRdiff_res$N_stores <- length(unique(purchases.retail$store_code_uc))
LRdiff_res$N_counties <- uniqueN(purchases.retail, by = c("fips_state", "fips_county"))
LRdiff_res$N_years <- uniqueN(purchases.retail, by = c("year"))
LRdiff_res$N_county_modules <- uniqueN(purchases.retail, by = c("fips_state", "fips_county",
                                                                "product_module_code"))
LRdiff_res$N_store_modules <- uniqueN(purchases.retail, by = c("store_code_uc", "product_module_code"))
LRdiff_res$N_state_modules <- uniqueN(purchases.retail, by = c("fips_state", "product_module_code"))
LRdiff_res$N_hholds_modules <- uniqueN(purchases.retail, by = c("household_code", "product_module_code"))
LRdiff_res$N_hholds_stores <- uniqueN(purchases.retail, by = c("household_code", "store_code_uc"))
LRdiff_res$N_hholds_modules_stores <- length(purchases.retail$household_by_store_by_module)
LRdiff_res$N_module_time <- length(purchases.retail$module_by_time)

fwrite(LRdiff_res, "../../../../../home/slacouture/HMS/Basic_Results_weights.csv")