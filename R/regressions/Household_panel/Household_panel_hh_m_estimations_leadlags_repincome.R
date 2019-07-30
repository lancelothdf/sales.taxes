## Sales taxes Project. Household Panel
# Running distributed lag regression in differences on the new panel (household aggregate x month)
# New: control for income: reported income (using midpoint of interval)
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

## Create Necessary variables -----------------------
head(purchases.sample)
# Reported income
purchases.sample$household_income_c[household_income == 3] <- 2500
purchases.sample$household_income_c[household_income == 4] <- 6500
purchases.sample$household_income_c[household_income == 6] <- 9000
purchases.sample$household_income_c[household_income == 8] <- 11000
purchases.sample$household_income_c[household_income == 10] <- 13000
purchases.sample$household_income_c[household_income == 11] <- 17500
purchases.sample$household_income_c[household_income == 13] <- 22500
purchases.sample$household_income_c[household_income == 15] <- 27500
purchases.sample$household_income_c[household_income == 16] <- 32500
purchases.sample$household_income_c[household_income == 17] <- 37500
purchases.sample$household_income_c[household_income == 18] <- 42500
purchases.sample$household_income_c[household_income == 19] <- 47500
purchases.sample$household_income_c[household_income == 21] <- 55000
purchases.sample$household_income_c[household_income == 23] <- 65000
purchases.sample$household_income_c[household_income == 26] <- 80000
purchases.sample$household_income_c[household_income == 27] <- 112500
purchases.sample$household_income_c[household_income == 28] <- 137500
purchases.sample$household_income_c[household_income == 29] <- 175000
purchases.sample$household_income_c[household_income == 30] <- 212500

# Try also shares
purchases.sample[, share_taxable := expenditure_taxable/sum_total_exp_month]
purchases.sample[, share_non_taxable := expenditure_non_taxable/sum_total_exp_month]
purchases.sample[, share_unknown := expenditure_unknown/sum_total_exp_month]
purchases.sample[, share_same3 := expenditure_same3/sum_total_exp_month]
purchases.sample[, share_diff3 := expenditure_diff3/sum_total_exp_month]

# Logarithms of variables
purchases.sample[, ln_hh_income := log(household_income_c)]
purchases.sample$ln_hh_income[is.infinite(purchases.sample$ln_hh_income)] <- NA

purchases.sample <- purchases.sample[, ln_expenditure_taxable := log(expenditure_taxable)]
purchases.sample$ln_expenditure_taxable[is.infinite(purchases.sample$ln_expenditure_taxable)] <- NA
purchases.sample <- purchases.sample[, ln_expenditure_non_taxable := log(expenditure_non_taxable)]
purchases.sample$ln_expenditure_non_taxable[is.infinite(purchases.sample$ln_expenditure_non_taxable)] <- NA
purchases.sample <- purchases.sample[, ln_expenditure_unknown := log(expenditure_unknown)]
purchases.sample$ln_expenditure_unknown[is.infinite(purchases.sample$ln_expenditure_unknown)] <- NA
purchases.sample <- purchases.sample[, ln_expenditure_same3 := log(expenditure_same3)]
purchases.sample$ln_expenditure_same3[is.infinite(purchases.sample$ln_expenditure_same3)] <- NA
purchases.sample <- purchases.sample[, ln_expenditure_diff3 := log(expenditure_diff3)]
purchases.sample$ln_expenditure_diff3[is.infinite(purchases.sample$ln_expenditure_diff3)] <- NA

purchases.sample <- purchases.sample[, ln_share_taxable := log(share_taxable)]
purchases.sample$ln_share_taxable[is.infinite(purchases.sample$ln_share_taxable)] <- NA
purchases.sample <- purchases.sample[, ln_share_non_taxable := log(share_non_taxable)]
purchases.sample$ln_share_non_taxable[is.infinite(purchases.sample$ln_share_non_taxable)] <- NA
purchases.sample <- purchases.sample[, ln_share_unknown := log(share_unknown)]
purchases.sample$ln_share_unknown[is.infinite(purchases.sample$ln_share_unknown)] <- NA
purchases.sample <- purchases.sample[, ln_share_same3 := log(share_same3)]
purchases.sample$ln_share_same3[is.infinite(purchases.sample$ln_share_same3)] <- NA
purchases.sample <- purchases.sample[, ln_share_diff3 := log(share_diff3)]
purchases.sample$ln_share_diff3[is.infinite(purchases.sample$ln_share_diff3)] <- NA


# Time
purchases.sample[, cal_time := 12 * year + month]

# impute tax rates prior to 2008 and after 2014
purchases.sample[, ln_sales_tax := ifelse(year < 2008, ln_sales_tax[year == 2008 & month == 1], ln_sales_tax),
                 by = .(household_code)]
purchases.sample[, ln_sales_tax := ifelse(year > 2014, ln_sales_tax[year == 2014 & month == 12], ln_sales_tax),
                 by = .(household_code)]

## take first differences of outcomes and treatment
setkey(purchases.sample, household_code, year, month)
purchases.sample <- purchases.sample[order(household_code, cal_time),] ##Sort on hh by year-quarter (in ascending order)

purchases.sample[, D.ln_sales_tax := ln_sales_tax - shift(ln_sales_tax, n=1, type="lag"),
                 by = .(household_code)]

purchases.sample[, D.ln_hh_income := ln_hh_income - shift(ln_hh_income, n=1, type="lag"),
                 by = .(household_code)]

purchases.sample[, D.ln_expenditure_taxable := ln_expenditure_taxable - shift(ln_expenditure_taxable, n=1, type="lag"),
                 by = .(household_code)]
purchases.sample[, D.ln_expenditure_non_taxable := ln_expenditure_non_taxable - shift(ln_expenditure_non_taxable, n=1, type="lag"),
                 by = .(household_code)]
purchases.sample[, D.ln_expenditure_unknown := ln_expenditure_unknown - shift(ln_expenditure_unknown, n=1, type="lag"),
                 by = .(household_code)]
purchases.sample[, D.ln_expenditure_diff3 := ln_expenditure_diff3 - shift(ln_expenditure_diff3, n=1, type="lag"),
                 by = .(household_code)]
purchases.sample[, D.ln_expenditure_same3 := ln_expenditure_same3 - shift(ln_expenditure_same3, n=1, type="lag"),
                 by = .(household_code)]

purchases.sample[, D.ln_share_taxable := ln_share_taxable - shift(ln_share_taxable, n=1, type="lag"),
                 by = .(household_code)]
purchases.sample[, D.ln_share_non_taxable := ln_share_non_taxable - shift(ln_share_non_taxable, n=1, type="lag"),
                 by = .(household_code)]
purchases.sample[, D.ln_share_unknown := ln_share_unknown - shift(ln_share_unknown, n=1, type="lag"),
                 by = .(household_code)]
purchases.sample[, D.ln_share_diff3 := ln_share_diff3 - shift(ln_share_diff3, n=1, type="lag"),
                 by = .(household_code)]
purchases.sample[, D.ln_share_same3 := ln_share_same3 - shift(ln_share_same3, n=1, type="lag"),
                 by = .(household_code)]



## generate lags and leads of ln_sales_tax
for (lag.val in 1:24) {
  lag.X <- paste0("L", lag.val, ".D.ln_sales_tax")
  purchases.sample[, (lag.X) := shift(D.ln_sales_tax, n=lag.val, type="lag"),
                   by = .(household_code)]
  
  lead.X <- paste0("F", lag.val, ".D.ln_sales_tax")
  purchases.sample[, (lead.X) := shift(D.ln_sales_tax, n=lag.val, type="lead"),
                   by = .(household_code)]
}
# Restrict data to interest window
purchases.sample <- purchases.sample[between(year, 2008, 2014)]
purchases.sample <- purchases.sample[ year >= 2009 | (year == 2008 & month >= 2)] ## First month of 2008, the difference was imputed not real data - so we drop it


## Estimations: Expenditure on type of module --------
output.results.file <- "../../../../../home/slacouture/HMS/HH_month_leadslags_cumulative_repincome.csv"
outcomes <- c("D.ln_hh_income", "D.ln_expenditure_taxable", "D.ln_expenditure_non_taxable", "D.ln_expenditure_unknown",
              "D.ln_expenditure_diff3", "D.ln_expenditure_same3", "D.ln_share_taxable",
              "D.ln_share_non_taxable", "D.ln_share_unknown", "D.ln_share_same3", "D.ln_share_diff3")


formula_lags <- paste0("L", 1:24, ".D.ln_sales_tax", collapse = "+")
formula_leads <- paste0("F", 1:24, ".D.ln_sales_tax", collapse = "+")
formula_RHS <- paste0("D.ln_sales_tax + ", formula_lags, "+", formula_leads)

## for linear hypothesis tests
lead.vars <- paste(paste0("F", 24:1, ".D.ln_sales_tax"), collapse = " + ")
lag.vars <- paste(paste0("L", 24:1, ".D.ln_sales_tax"), collapse = " + ")
lead.lp.restr <- paste(lead.vars, "= 0")
lag.lp.restr <- paste(lag.vars, "+ D.ln_sales_tax = 0")
total.lp.restr <- paste(lag.vars, "+", lead.vars, "+ D.ln_sales_tax = 0")


LRdiff_res <- data.table(NULL)
for (Y in outcomes) {
  
  formula1 <- as.formula(paste0(
    Y, "~ D.ln_hh_income +", formula_RHS, "| time"
  ))
  if (Y == "D.ln_hh_income") {
    formula1 <- as.formula(paste0(
      Y, "~ ", formula_RHS, "| time"
    ))
  }
  flog.info("Estimating with %s as outcome.", Y)
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
    `Std. Error` = c(lead.test.se, lag.test.se, total.test.se),
    `Pr(>|t|)` = c(lead.test.pval, lag.test.pval, total.test.pval),
    outcome = Y,
    Rsq = summary(res1)$r.squared,
    adj.Rsq = summary(res1)$adj.r.squared)
  LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T)
  fwrite(LRdiff_res, output.results.file)
  
  
  ##### Add the cumulative effect at each lead/lag (relative to -1)
  cumul.lead1.est <- 0
  cumul.lead1.se <- NA
  cumul.lead1.pval <- NA
  
  #cumul.lead2.est is just equal to minus the change between -2 and -1
  cumul.lead2.est <- - coef(summary(res1))[ "F1.D.ln_sales_tax", "Estimate"]
  cumul.lead2.se <- coef(summary(res1))[ "F1.D.ln_sales_tax", "Std. Error"]
  cumul.lead2.pval <- coef(summary(res1))[ "F1.D.ln_sales_tax", "Pr(>|t|)"]
  
  ##LEADS
  for(j in 3:25) {
    
    ## Create a name for estimate, se and pval of each lead
    cumul.test.est.name <- paste("cumul.lead", j, ".est", sep = "")
    cumul.test.se.name <- paste("cumul.lead", j, ".se", sep = "")
    cumul.test.pval.name <- paste("cumul.lead", j, ".pval", sep = "")
    
    ## Create the formula to compute cumulative estimate at each lead/lag
    cumul.test.form <- paste0("-", paste(paste0("F", (j-1):1, ".D.ln_sales_tax"), collapse = " - "))
    cumul.test.form <- paste(cumul.test.form, " = 0")
    
    ## Compute estimate and store in variables names
    cumul.test <- glht(res1, linfct = cumul.test.form)
    
    assign(cumul.test.est.name, coef(summary(cumul.test))[[1]])
    assign(cumul.test.se.name, sqrt(vcov(summary(cumul.test)))[[1]])
    assign(cumul.test.pval.name, 2*(1 - pnorm(abs(coef(summary(cumul.test))[[1]]/sqrt(vcov(summary(cumul.test)))[[1]]))))
  }
  
  
  ##LAGS
  ## On Impact --> Effect = coefficient on D.ln_sales_tax
  cumul.lag0.est <- coef(summary(res1))[ "D.ln_sales_tax", "Estimate"]
  cumul.lag0.se <- coef(summary(res1))[ "D.ln_sales_tax", "Std. Error"]
  cumul.lag0.pval <- coef(summary(res1))[ "D.ln_sales_tax", "Pr(>|t|)"]
  
  for(j in 1:24) {
    
    ## Create a name for estimate, se and pval of each lead
    cumul.test.est.name <- paste("cumul.lag", j, ".est", sep = "")
    cumul.test.se.name <- paste("cumul.lag", j, ".se", sep = "")
    cumul.test.pval.name <- paste("cumul.lag", j, ".pval", sep = "")
    
    ## Create the formula to compute cumulative estimate at each lead/lag
    cumul.test.form <- paste("D.ln_sales_tax + ", paste(paste0("L", 1:j, ".D.ln_sales_tax"), collapse = " + "), sep = "")
    cumul.test.form <- paste(cumul.test.form, " = 0")
    
    ## Compute estimate and store in variables names
    cumul.test <- glht(res1, linfct = cumul.test.form)
    
    assign(cumul.test.est.name, coef(summary(cumul.test))[[1]])
    assign(cumul.test.se.name, sqrt(vcov(summary(cumul.test)))[[1]])
    assign(cumul.test.pval.name, 2*(1 - pnorm(abs(coef(summary(cumul.test))[[1]]/sqrt(vcov(summary(cumul.test)))[[1]]))))
  }
  
  
  ## linear hypothesis results
  lp.dt <- data.table(
    rn = c("cumul.lead24.D.ln_sales_tax", "cumul.lead23.D.ln_sales_tax", "cumul.lead22.D.ln_sales_tax", "cumul.lead21.D.ln_sales_tax", "cumul.lead20.D.ln_sales_tax", "cumul.lead19.D.ln_sales_tax", "cumul.lead18.D.ln_sales_tax", "cumul.lead17.D.ln_sales_tax", 
           "cumul.lead16.D.ln_sales_tax", "cumul.lead15.D.ln_sales_tax", "cumul.lead14.D.ln_sales_tax", "cumul.lead13.D.ln_sales_tax", "cumul.lead12.D.ln_sales_tax", "cumul.lead11.D.ln_sales_tax", "cumul.lead10.D.ln_sales_tax", "cumul.lead9.D.ln_sales_tax", 
           "cumul.lead8.D.ln_sales_tax", "cumul.lead7.D.ln_sales_tax", "cumul.lead6.D.ln_sales_tax", "cumul.lead5.D.ln_sales_tax", "cumul.lead4.D.ln_sales_tax", "cumul.lead3.D.ln_sales_tax", "cumul.lead2.D.ln_sales_tax", "cumul.lead1.D.ln_sales_tax", 
           "cumul.lag0.D.ln_sales_tax", "cumul.lag1.D.ln_sales_tax", "cumul.lag2.D.ln_sales_tax", "cumul.lag3.D.ln_sales_tax", "cumul.lag4.D.ln_sales_tax", "cumul.lag5.D.ln_sales_tax", "cumul.lag6.D.ln_sales_tax", "cumul.lag7.D.ln_sales_tax", "cumul.lag8.D.ln_sales_tax",
           "cumul.lag9.D.ln_sales_tax", "cumul.lag10.D.ln_sales_tax", "cumul.lag11.D.ln_sales_tax", "cumul.lag12.D.ln_sales_tax", "cumul.lag13.D.ln_sales_tax", "cumul.lag14.D.ln_sales_tax", "cumul.lag15.D.ln_sales_tax", "cumul.lag16.D.ln_sales_tax",
           "cumul.lag17.D.ln_sales_tax", "cumul.lag18.D.ln_sales_tax", "cumul.lag19.D.ln_sales_tax", "cumul.lag20.D.ln_sales_tax", "cumul.lag21.D.ln_sales_tax", "cumul.lag22.D.ln_sales_tax", "cumul.lag23.D.ln_sales_tax", "cumul.lag24.D.ln_sales_tax"),
    Estimate = c(cumul.lead24.est, cumul.lead23.est, cumul.lead22.est, cumul.lead21.est, cumul.lead20.est, cumul.lead19.est, cumul.lead18.est, cumul.lead17.est,
                 cumul.lead16.est, cumul.lead15.est, cumul.lead14.est, cumul.lead13.est, cumul.lead12.est, cumul.lead11.est, cumul.lead10.est, cumul.lead9.est,
                 cumul.lead8.est, cumul.lead7.est, cumul.lead6.est, cumul.lead5.est, cumul.lead4.est, cumul.lead3.est, cumul.lead2.est, cumul.lead1.est, 
                 cumul.lag0.est, cumul.lag1.est, cumul.lag2.est, cumul.lag3.est, cumul.lag4.est, cumul.lag5.est, cumul.lag6.est, cumul.lag7.est, cumul.lag8.est,
                 cumul.lag9.est, cumul.lag10.est, cumul.lag11.est, cumul.lag12.est, cumul.lag13.est, cumul.lag14.est, cumul.lag15.est, cumul.lag16.est, 
                 cumul.lag17.est, cumul.lag18.est, cumul.lag19.est, cumul.lag20.est, cumul.lag21.est, cumul.lag22.est, cumul.lag23.est, cumul.lag24.est),
    `Std. Error` = c(cumul.lead24.se, cumul.lead23.se, cumul.lead22.se, cumul.lead21.se, cumul.lead20.se, cumul.lead19.se, cumul.lead18.se, cumul.lead17.se,
                     cumul.lead16.se, cumul.lead15.se, cumul.lead14.se, cumul.lead13.se, cumul.lead12.se, cumul.lead11.se, cumul.lead10.se, cumul.lead9.se,
                     cumul.lead8.se, cumul.lead7.se, cumul.lead6.se, cumul.lead5.se, cumul.lead4.se, cumul.lead3.se, cumul.lead2.se, cumul.lead1.se, 
                     cumul.lag0.se, cumul.lag1.se, cumul.lag2.se, cumul.lag3.se, cumul.lag4.se, cumul.lag5.se, cumul.lag6.se, cumul.lag7.se, cumul.lag8.se,
                     cumul.lag9.se, cumul.lag10.se, cumul.lag11.se, cumul.lag12.se, cumul.lag13.se, cumul.lag14.se, cumul.lag15.se, cumul.lag16.se, 
                     cumul.lag17.se, cumul.lag18.se, cumul.lag19.se, cumul.lag20.se, cumul.lag21.se, cumul.lag22.se, cumul.lag23.se, cumul.lag24.se),
    `Pr(>|t|)` = c(cumul.lead24.pval, cumul.lead23.pval, cumul.lead22.pval, cumul.lead21.pval, cumul.lead20.pval, cumul.lead19.pval, cumul.lead18.pval, cumul.lead17.pval,
                   cumul.lead16.pval, cumul.lead15.pval, cumul.lead14.pval, cumul.lead13.pval, cumul.lead12.pval, cumul.lead11.pval, cumul.lead10.pval, cumul.lead9.pval,
                   cumul.lead8.pval, cumul.lead7.pval, cumul.lead6.pval, cumul.lead5.pval, cumul.lead4.pval, cumul.lead3.pval, cumul.lead2.pval, cumul.lead1.pval, 
                   cumul.lag0.pval, cumul.lag1.pval, cumul.lag2.pval, cumul.lag3.pval, cumul.lag4.pval, cumul.lag5.pval, cumul.lag6.pval, cumul.lag7.pval, cumul.lag8.pval,
                   cumul.lag9.pval, cumul.lag10.pval, cumul.lag11.pval, cumul.lag12.pval, cumul.lag13.pval, cumul.lag14.pval, cumul.lag15.pval, cumul.lag16.pval, 
                   cumul.lag17.pval, cumul.lag18.pval, cumul.lag19.pval, cumul.lag20.pval, cumul.lag21.pval, cumul.lag22.pval, cumul.lag23.pval, cumul.lag24.pval),
    outcome = Y,
    Rsq = summary(res1)$r.squared,
    adj.Rsq = summary(res1)$adj.r.squared)
  LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T)
  fwrite(LRdiff_res, output.results.file)
  
}


## summary values --------------------------------------------------------------
LRdiff_res$N_obs <- nrow(purchases.sample)
LRdiff_res$N_hholds <- length(unique(purchases.sample$household_code))
LRdiff_res$N_counties <- uniqueN(purchases.sample, by = c("fips_state_code", "fips_county_code"))
LRdiff_res$N_years <- uniqueN(purchases.sample, by = c("year"))

fwrite(LRdiff_res, output.results.file)