##### Wesley Janson and Santiago Lacouture
#' Sales Taxes
#' Replication File. Updated on 8/2/2022
#' Step 4A: Cross-sectional estimates using retailer data portion of replication

library(data.table)
library(futile.logger)
library(lfe)
library(multcomp)
library(Matrix)
library(zoo)
library(tidyverse)
library(stringr)
library(nloptr)
library(doParallel)
library(MASS)
library(pracma)

setwd("/project2/igaarder")
rm(list = ls())

## input filepath ----------------------------------------------
all_pi <- fread("Data/Replication/all_pi.csv")

## output filepath ----------------------------------------------
output.results.file.crossec <- "Data/Replication/LRdiff_cross_sectional_design.csv"


### Clean data ----
colnames(all_pi)
## Collapse to yearly data
yearly_data <- all_pi[, list(ln_cpricei2 = log(mean(exp(ln_cpricei2))), 
                             ln_quantity3 = log(mean(exp(ln_quantity3))), 
                             base.sales = sum(base.sales), 
                             sales = sum(sales), 
                             ln_sales_tax = log(weighted.mean(exp(ln_sales_tax), w = sales))), 
                      by = .(store_code_uc, product_module_code,  fips_state, 
                             fips_county, year, module_by_state, module_by_time)]
rm(all_pi)

yearly_data[, store_by_time := .GRP, by = .(store_code_uc, year)]


# Restrict to relevant tax data span
yearly_data <- yearly_data[year >= 2008 & year <= 2014]
yearly_data$year <- factor(yearly_data$year) ##Convert the indicator for year to a factor variable (needed for interaction in the regression between ln_sales_tax and dummy for year)

##Construct weights to average across cohorts/years.  Equal weights
cohort.weights <- rep(1, 7) 
cohort.weights <- cohort.weights/sum(cohort.weights)


### Cross-Sectional Estimates Retailer Data -----------------------


# outcomes of interest
outcomes <- c("ln_cpricei2", "ln_quantity3")

for (Y in outcomes) {
  
  # Formula
  formula0 <- as.formula(paste0(
    Y, " ~ ln_sales_tax:year | module_by_time + store_by_time | 0 | module_by_state "
  ))
  
  # Regression
  res0 <- felm(data = yearly_data,
               formula = formula0,
               weights = yearly_data$base.sales)
  
  
  ## attach results
  flog.info("Writing results...")
  res1.dt <- data.table(coef(summary(res0)), keep.rownames=T)
  res1.dt[, outcome := Y]
  res1.dt[, Rsq := summary(res0)$r.squared]
  res1.dt[, adj.Rsq := summary(res0)$adj.r.squared]
  LRdiff_res <- res1.dt ### Create table LRdiff_res in which we store all results (we start with the results we had just stored in res1.dt)
  fwrite(LRdiff_res, output.results.file.crossec)  ## Write results to a csv file 
  
  
  ### Take linear combinations of coefficients and attach results (this is the coefficient of interest)
  lc.lr0 <- paste0(cohort.weights[1], "*ln_sales_tax:year2008 + ", cohort.weights[2], "*ln_sales_tax:year2009 + ", cohort.weights[3], "*ln_sales_tax:year2010 + ", cohort.weights[4], "*ln_sales_tax:year2011 + ", cohort.weights[5], "*ln_sales_tax:year2012 + ", cohort.weights[6], "*ln_sales_tax:year2013 + ", cohort.weights[7], "*ln_sales_tax:year2014", sep = "")
  lc.formula0 <- paste0(lc.lr0, " = 0", sep = "")
  lc.test0 <- glht(res0, linfct = c(lc.formula0))
  
  # Calculate the p-value
  pval <- 2*(1 - pnorm(abs(coef(summary(lc.test0))[[1]]/sqrt(vcov(summary(lc.test0)))[[1]])))
  
  
  lp.dt <- data.table(
    rn = "avg.ln_sales_tax",
    Estimate = coef(summary(lc.test0))[[1]],
    `Cluster s.e.` = sqrt(vcov(summary(lc.test0)))[[1]],
    `Pr(>|t|)` = pval,
    outcome = Y,
    Rsq = summary(res0)$r.squared,
    adj.Rsq = summary(res0)$adj.r.squared)
  LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T) ## Merge results to LRdiff_res
  fwrite(LRdiff_res, output.results.file.crossec) ## Write resulting file to a csv file
  
  
}

LRdiff_res$N_obs <- nrow(yearly_data)
LRdiff_res$N_modules <- length(unique(yearly_data$product_module_code))
LRdiff_res$N_stores <- length(unique(yearly_data$store_code_uc))
LRdiff_res$N_counties <- uniqueN(yearly_data, by = c("fips_state", "fips_county"))
LRdiff_res$N_years <- uniqueN(yearly_data, by = c("year")) 
LRdiff_res$N_county_modules <- uniqueN(yearly_data, by = c("fips_state", "fips_county",
                                                           "product_module_code"))
LRdiff_res$N_store_modules <- uniqueN(yearly_data, by = c("store_code_uc", "product_module_code"))
LRdiff_res$N_state_modules <- uniqueN(yearly_data, by = c("fips_state", "product_module_code"))

fwrite(LRdiff_res, output.results.file.crossec)
