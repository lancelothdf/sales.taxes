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
describe(all_pi$d_ln_sales_tax)
describe(all_pi$d_ln_cpricei)

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
# to be sure 
purchases.retail <- purchases.retail[!is.na(d_ln_sales_tax)]


### Estimation ---------

## Preparing Estimation
output.results.file <- "../../../../../home/slacouture/HMS/Leads_Lags_Results.csv"
outcomes <- c("d_ln_share_expend", "d_ln_cpricei", "d_ln_quantity")
formula_RHS <- "d_ln_sales_tax + lag8.ln_sales_tax + lag7.ln_sales_tax + lag6.ln_sales_tax + lag5.ln_sales_tax + lag4.ln_sales_tax + lag3.ln_sales_tax + lag2.ln_sales_tax + lag1.ln_sales_tax + lea8.ln_sales_tax + lea7.ln_sales_tax + lea6.ln_sales_tax + lea5.ln_sales_tax + lea4.ln_sales_tax + lea3.ln_sales_tax + lea2.ln_sales_tax + lea1.ln_sales_tax"


## for linear hypothesis tests
lead.vars <- paste(paste0("lea", 8:1, ".ln_sales_tax"), collapse = " + ")
lag.vars <- paste(paste0("lag", 8:1, ".ln_sales_tax"), collapse = " + ")
lead.lp.restr <- paste(lead.vars, "= 0")
lag.lp.restr <- paste(lag.vars, "+ d_ln_sales_tax = 0")
total.lp.restr <- paste(lag.vars, "+", lead.vars, "+ d_ln_sales_tax = 0")


LRdiff_res <- data.table(NULL)
for (Y in outcomes) {

  formula1 <- as.formula(paste0(
    Y, "~", formula_RHS, "| module_by_time"
  ))
  flog.info("Estimating with %s as outcome with %s FE.", Y, FE)
  res1 <- felm(formula = formula1, data = purchases.retail,
               weights = purchases.retail$projection_factor)
  flog.info("Finished estimating with %s as outcome with %s FE.", Y, FE)
  
  
  ## attach results
  flog.info("Writing results...")
  res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
  res1.dt[, outcome := Y]
  res1.dt[, parametric := "No"]
  res1.dt[, Rsq := summary(res1)$r.squared]
  res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
  LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
  fwrite(LRdiff_res, output.results.file)
  
  ## sum leads
  flog.info("Summing leads...")
  lead.test <- glht(res1, linfct = lead.lp.restr)
  lead.test.est <- coef(summary(lead.test))[[1]]
  lead.test.se <- sqrt(vcov(summary(lead.test)))[[1]]
  lead.test.pval <- 2*(1 - pnorm(abs(lead.test.est/lead.test.se)))
  
  ## sum lags
  flog.info("Summing lags...")
  lag.test <- glht(res1, linfct = lag.lp.restr)
  lag.test.est <- coef(summary(lag.test))[[1]]
  lag.test.se <- sqrt(vcov(summary(lag.test)))[[1]]
  lag.test.pval <- 2*(1 - pnorm(abs(lag.test.est/lag.test.se)))
  
  ## sum all
  flog.info("Summing all...")
  total.test <- glht(res1, linfct = total.lp.restr)
  total.test.est <- coef(summary(total.test))[[1]]
  total.test.se <- sqrt(vcov(summary(total.test)))[[1]]
  total.test.pval <- 2*(1 - pnorm(abs(total.test.est/total.test.se)))
  
  ## linear hypothesis results
  lp.dt <- data.table(
    rn = c("Pre.D.ln_sales_tax", "Post.D.ln_sales_tax", "All.D.ln_sales_tax"),
    Estimate = c(lead.test.est, lag.test.est, total.test.est),
    `Cluster s.e.` = c(lead.test.se, lag.test.se, total.test.se),
    `Pr(>|t|)` = c(lead.test.pval, lag.test.pval, total.test.pval),
    outcome = Y,
    parametric = "No",
    Rsq = summary(res1)$r.squared,
    adj.Rsq = summary(res1)$adj.r.squared)
  LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T)
  fwrite(LRdiff_res, output.results.file)
}



## summary values --------------------------------------------------------------
LRdiff_res$N_obs <- nrow(purchases.retail)
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

fwrite(LRdiff_res, output.results.file)
