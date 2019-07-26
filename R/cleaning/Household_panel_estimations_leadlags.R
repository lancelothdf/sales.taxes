## Sales taxes Project. Household Panel
# Running Basic Estimations without covariates but with time lags
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

## Generate lags and leads of sales tax
# Need to drop it in existing because of conflict
purchases.retail <- purchases.retail[, -c("ln_sales_tax", "sales_tax")]

# Need to retrieve the stores x module data: panel is not balanced
all_goods_pi_path <- "../../all_nielsen_data_2006_2016_quarterly.csv"
all_pi <- fread(all_goods_pi_path)
all_pi <- all_pi[, .(store_code_uc, product_module_code,
                     year, quarter, pricei, cpricei, sales_tax)]
all_pi <- all_pi[, ln_sales_tax := log(sales_tax)]
all_pi <- all_pi[, ln_cpricei := log(cpricei)]

setkeyv(all_pi, c("year","quarter"))
# Differences
all_pi <- all_pi[, d_ln_sales_tax := c(NA, diff(ln_sales_tax)), by=.(store_code_uc, product_module_code)]
all_pi <- all_pi[, d_ln_cpricei := c(NA, diff(ln_cpricei)), by=.(store_code_uc, product_module_code)]


# Lags of difference:
all_pi <- all_pi[, lag1.ln_sales_tax := shift(d_ln_sales_tax, 1), by=.(store_code_uc, product_module_code)]
all_pi <- all_pi[, lag2.ln_sales_tax := shift(d_ln_sales_tax, 2), by=.(store_code_uc, product_module_code)]
all_pi <- all_pi[, lag3.ln_sales_tax := shift(d_ln_sales_tax, 3), by=.(store_code_uc, product_module_code)]
all_pi <- all_pi[, lag4.ln_sales_tax := shift(d_ln_sales_tax, 4), by=.(store_code_uc, product_module_code)]
all_pi <- all_pi[, lag5.ln_sales_tax := shift(d_ln_sales_tax, 5), by=.(store_code_uc, product_module_code)]
all_pi <- all_pi[, lag6.ln_sales_tax := shift(d_ln_sales_tax, 6), by=.(store_code_uc, product_module_code)]
all_pi <- all_pi[, lag7.ln_sales_tax := shift(d_ln_sales_tax, 7), by=.(store_code_uc, product_module_code)]
all_pi <- all_pi[, lag8.ln_sales_tax := shift(d_ln_sales_tax, 8), by=.(store_code_uc, product_module_code)]

# Leads: 

all_pi <- all_pi[, lea1.ln_sales_tax := shift(d_ln_sales_tax, 1, type='lead'), by=.(store_code_uc, product_module_code)]
all_pi <- all_pi[, lea2.ln_sales_tax := shift(d_ln_sales_tax, 2, type='lead'), by=.(store_code_uc, product_module_code)]
all_pi <- all_pi[, lea3.ln_sales_tax := shift(d_ln_sales_tax, 3, type='lead'), by=.(store_code_uc, product_module_code)]
all_pi <- all_pi[, lea4.ln_sales_tax := shift(d_ln_sales_tax, 4, type='lead'), by=.(store_code_uc, product_module_code)]
all_pi <- all_pi[, lea5.ln_sales_tax := shift(d_ln_sales_tax, 5, type='lead'), by=.(store_code_uc, product_module_code)]
all_pi <- all_pi[, lea6.ln_sales_tax := shift(d_ln_sales_tax, 6, type='lead'), by=.(store_code_uc, product_module_code)]
all_pi <- all_pi[, lea7.ln_sales_tax := shift(d_ln_sales_tax, 7, type='lead'), by=.(store_code_uc, product_module_code)]
all_pi <- all_pi[, lea8.ln_sales_tax := shift(d_ln_sales_tax, 8, type='lead'), by=.(store_code_uc, product_module_code)]

## Merge with existing data
purchases.retail <- merge(
  purchases.retail, all_pi,
  by = c("store_code_uc", "product_module_code", "year", "quarter"),
  all.x = T
)

## Almost there: now I have to get the lag of the log share of expenditure. To be clear: we are going to lose many observations
setkeyv(purchases.retail, c("year","quarter"))
purchases.retail <- purchases.retail[, d_ln_share_expend := c(NA, diff(ln_share_expend)), by=.(household_code, store_code_uc, product_module_code)]
purchases.retail <- purchases.retail[, d_ln_quantity := c(NA, diff(ln_quantity)), by=.(household_code, store_code_uc, product_module_code)]

## Finally I will restrict to data having all leads and lags, which is (should be) equal to drop first 2 and last 2 years
purchases.retail <- purchases.retail[year < 2013 & year > 2009]

## Basic Specifications ----------

# Log Share of Expenditure
formula0 <- as.formula(paste0(
  "d_ln_share_expend ~ d_ln_sales_tax + lag8.ln_sales_tax + lag7.ln_sales_tax + lag6.ln_sales_tax + lag5.ln_sales_tax + lag4.ln_sales_tax + lag3.ln_sales_tax + lag2.ln_sales_tax + lag1.ln_sales_tax + lea8.ln_sales_tax + lea7.ln_sales_tax + lea6.ln_sales_tax + lea5.ln_sales_tax + lea4.ln_sales_tax + lea3.ln_sales_tax + lea2.ln_sales_tax + lea1.ln_sales_tax | module_by_time "
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
res0.dt[, N_obs := sum((!is.na(purchases.retail$ln_share_expend)))]
LRdiff_res <- res0.dt ### Create table LRdiff_res in which we store all results (we start with the results we had just stored in res1.dt)
fwrite(LRdiff_res, "../../../../../home/slacouture/HMS/Leads_Lags_Results.csv")


# Log Price
formula2 <- as.formula(paste0(
  "d_ln_cpricei ~ d_ln_sales_tax + lag8.ln_sales_tax + lag7.ln_sales_tax + lag6.ln_sales_tax + lag5.ln_sales_tax + lag4.ln_sales_tax + lag3.ln_sales_tax + lag2.ln_sales_tax + lag1.ln_sales_tax + lea8.ln_sales_tax + lea7.ln_sales_tax + lea6.ln_sales_tax + lea5.ln_sales_tax + lea4.ln_sales_tax + lea3.ln_sales_tax + lea2.ln_sales_tax + lea1.ln_sales_tax | module_by_time "
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
res0.dt[, N_obs := sum((!is.na(purchases.retail$ln_cpricei)))]
LRdiff_res <- rbind(LRdiff_res,res0.dt) ### Append 
fwrite(LRdiff_res, "../../../../../home/slacouture/HMS/Leads_Lags_Results.csv")

# Log Quantity
formula2 <- as.formula(paste0(
  "d_ln_quantity ~ d_ln_sales_tax + lag8.ln_sales_tax + lag7.ln_sales_tax + lag6.ln_sales_tax + lag5.ln_sales_tax + lag4.ln_sales_tax + lag3.ln_sales_tax + lag2.ln_sales_tax + lag1.ln_sales_tax + lea8.ln_sales_tax + lea7.ln_sales_tax + lea6.ln_sales_tax + lea5.ln_sales_tax + lea4.ln_sales_tax + lea3.ln_sales_tax + lea2.ln_sales_tax + lea1.ln_sales_tax | module_by_time "
))
flog.info("Estimating Log Quantity")
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
res0.dt[, N_obs := sum((!is.na(purchases.retail$ln_quantity)))]
LRdiff_res <- rbind(LRdiff_res,res0.dt) ### Append 
fwrite(LRdiff_res, "../../../../../home/slacouture/HMS/Leads_Lags_Results.csv")


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
LRdiff_res$N_hholds_modules_stores <- length(unique(purchases.retail$household_by_store_by_module))
LRdiff_res$N_module_time <- length(unique(purchases.retail$module_by_time))

fwrite(LRdiff_res, "../../../../../home/slacouture/HMS/Leads_Lags_Results.csv")
