## Sales taxes Project. Household Panel
# Running Basic Estimations on the new panel (household aggregate x month)
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
purchases.full <- fread("cleaning/consumer_panel_m_hh_2006-2016.csv")

purchases.full$time <- factor(with(purchases.full, interaction(year, month)))


## Constraining Data set for estimations ------------ 
# Drop "magnet" households: 
purchases.full[, sum(is.na(projection_factor))]
# 2016154 obs
purchases.nomagnet <- purchases.full[!is.na(projection_factor)]

# Drop households without sales tax data
purchases.full[, sum(is.na(sales_tax))]
# More than 70%: 181247984 obs
purchases.sample <- purchases.nomagnet[!is.na(sales_tax)]

## Estimations: Expenditure on type of module --------
output.results.file <- "../../../../../home/slacouture/HMS/HH_month_basic_results.csv"
outcomes <- c("expenditure_taxable", "expenditure_non_taxable", "expenditure_unknown")


LRdiff_res <- data.table(NULL)
for (Y in outcomes) {
  
  formula1 <- as.formula(paste0(
    Y, "~ ln_sales_tax | time + household_code"
  ))
  flog.info("Estimating with %s as outcome", Y)
  res1 <- felm(formula = formula1, data = purchases.sample,
               weights = purchases.sample$projection_factor)
  flog.info("Finished estimating with %s as outcome.", Y)
  
  
  ## attach results
  flog.info("Writing results...")
  res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
  res1.dt[, outcome := Y]
  res1.dt[, Rsq := summary(res1)$r.squared]
  res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
  LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
  fwrite(LRdiff_res, output.results.file)

  # Now the log
  log.Y <- paste0("ln_", Y)
  purchases.sample[, (log.Y) := log(Y)]
  # make sure 0s are now missings
  purchases.full$log.Y[is.infinite(purchases.full$log.Y)] <- NA

  formula1 <- as.formula(paste0(
    (log.Y), "~ ln_sales_tax | time + household_code "
  ))
  flog.info("Estimating with log %s as outcome", Y)
  res1 <- felm(formula = formula1, data = purchases.sample,
               weights = purchases.sample$projection_factor)
  flog.info("Finished estimating with log %s as outcome.", Y)
  
  
  ## attach results
  flog.info("Writing results...")
  res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
  res1.dt[, outcome := (log.Y)]
  res1.dt[, Rsq := summary(res1)$r.squared]
  res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
  LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
  fwrite(LRdiff_res, output.results.file)
}



## summary values --------------------------------------------------------------
LRdiff_res$N_obs <- nrow(purchases.sample)
LRdiff_res$N_hholds <- length(unique(purchases.sample$household_code))
LRdiff_res$N_counties <- uniqueN(purchases.sample, by = c("fips_state_code", "fips_county_code"))
LRdiff_res$N_years <- uniqueN(purchases.sample, by = c("year"))

fwrite(LRdiff_res, output.results.file)
