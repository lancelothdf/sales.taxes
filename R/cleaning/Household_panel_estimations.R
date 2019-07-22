## Sales taxes Project. Household Panel
# Running first Basic Estimations
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

## Filepaths
# Exporting results to my folder in the server
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





## Basic Specifications ----------

# Log Share of Expenditure
formula0 <- as.formula(paste0(
  "ln_share_expend ~ ln_sales_tax | module_by_time + household_by_store_by_module"
))

flog.info("Estimating Log Share")
res0 <- felm(data = purchases.retail,
             formula = formula0,
             weights = purchases.retail$projection_factor,
             na.omit)

flog.info("Writing results...")
res0.dt <- data.table(coef(summary(res0)), keep.rownames=T)
res0.dt[, outcome := "ln_share_expend"]
res0.dt[, Rsq := summary(res0)$r.squared]
res0.dt[, adj.Rsq := summary(res0)$adj.r.squared]
res0.dt[, specification := "Basic"]
LRdiff_res <- res0.dt ### Create table LRdiff_res in which we store all results (we start with the results we had just stored in res1.dt)
N_obs <- sum((!is.na(purchases.retail$ln_share_expend)))

# Share of Expenditure
formula1 <- as.formula(paste0(
  "share_expend ~ ln_sales_tax | module_by_time + household_by_store_by_module"
))

flog.info("Estimating Share")
res1 <- felm(data = purchases.retail,
             formula = formula1,
             weights = purchases.retail$projection_factor,
             na.omit)
flog.info("Writing results...")
res0.dt <- data.table(coef(summary(res1)), keep.rownames=T)
res0.dt[, outcome := "share_expend"]
res0.dt[, Rsq := summary(res1)$r.squared]
res0.dt[, adj.Rsq := summary(res1)$adj.r.squared]
res0.dt[, specification := "Basic"]
LRdiff_res <- rbind(LRdiff_res,res0.dt) ### Append 
N_obs[2] <- sum((!is.na(purchases.retail$share_expend)))

# Log Price
formula2 <- as.formula(paste0(
  "ln_cpricei ~ ln_sales_tax | module_by_time + household_by_store_by_module"
))
flog.info("Estimating Log Price")
res2 <- felm(data = purchases.retail,
             formula = formula2,
             weights = purchases.retail$projection_factor,
             na.omit)

flog.info("Writing results...")
res0.dt <- data.table(coef(summary(res2)), keep.rownames=T)
res0.dt[, outcome := "ln_cpricei"]
res0.dt[, Rsq := summary(res2)$r.squared]
res0.dt[, adj.Rsq := summary(res2)$adj.r.squared]
res0.dt[, specification := "Basic"]
LRdiff_res <- rbind(LRdiff_res,res0.dt) ### Append 
N_obs[3] <- sum((!is.na(purchases.retail$ln_cpricei)))

# Log Quantity
formula2 <- as.formula(paste0(
  "ln_quantity ~ ln_sales_tax | module_by_time + household_by_store_by_module"
))
flog.info("Estimating Log Price")
res2 <- felm(data = purchases.retail,
             formula = formula2,
             weights = purchases.retail$projection_factor,
             na.omit)

flog.info("Writing results...")
res0.dt <- data.table(coef(summary(res2)), keep.rownames=T)
res0.dt[, outcome := "ln_quantity"]
res0.dt[, Rsq := summary(res2)$r.squared]
res0.dt[, adj.Rsq := summary(res2)$adj.r.squared]
res0.dt[, specification := "Basic"]
LRdiff_res <- rbind(LRdiff_res,res0.dt) ### Append 
N_obs[4] <- sum((!is.na(purchases.retail$ln_quantity)))

# Add Ns
LRdiff_res$N <- N_obs


fwrite(LRdiff_res, "../../../../../home/slacouture/HMS/Basic_Results.csv")
