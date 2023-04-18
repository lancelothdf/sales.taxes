##### Wesley Janson and Santiago Lacouture
#' Sales Taxes
#' Replication File. Updated on 8/2/2022
#' Step 4B: Cross-sectional estimates using consumer panel data portion of replication

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

setwd("/project/igaarder")
rm(list = ls())

## input filepath ----------------------------------------------
purchases.sample <-  fread("Data/Replication_v4/purchases.sample.csv")

## output filepath ----------------------------------------------
output.results.file <- "Data/Replication_v4/LRdiff_cross_sectional_design_hh.csv"


### 4B. Cross-Sectional Estimates from Consumer Panel ------------------
purchases.sample <- purchases.sample[year >= 2010]
purchases.sample$year <- factor(purchases.sample$year) ##Convert the indicator for year to a factor variable (needed for interaction in the regression between ln_sales_tax and dummy for year)

##Construct weights to average across cohorts/years.  Equal weights
cohort.weights <- rep(1, 5) 
cohort.weights <- cohort.weights/sum(cohort.weights)

FE_opts <- c("income_by_group_by_time", "group_by_time")

LRdiff_res <- data.table(NULL)

for (FE in FE_opts) {
  data.est <- purchases.sample[!is.na(get(FE))]
  
  formula1 <- as.formula(paste0(
    "ln_expenditures ~ ln_sales_tax:year | household_by_time + ", FE, " | 0 | household_code"
  ))
  flog.info("Estimating with %s as FE", FE)
  res1 <- felm(formula = formula1, data = data.est,
               weights = data.est$projection_factor)
  flog.info("Finished estimating with %s as FE", FE)
  
  ## attach results
  flog.info("Writing results...")
  res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
  res1.dt[, outcome := "ln_expenditures"]
  res1.dt[, Rsq := summary(res1)$r.squared]
  res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
  res1.dt[, N.obs := nrow(data.est[!is.na(ln_expenditures)])]
  res1.dt[, N_hholds := uniqueN(data.est[!is.na(ln_expenditures)], by = c("household_code"))]
  res1.dt[, N_groups := uniqueN(data.est[!is.na(ln_expenditures)], by = c("product_group_code"))]
  res1.dt[, FE_d := FE]
  LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
  fwrite(LRdiff_res, output.results.file)
  
  ### Take linear combinations of coefficients and attach results (this is the coefficient of interest)
  lc.lr0 <- paste0(cohort.weights[1], "*ln_sales_tax:year2010 + ", 
                   cohort.weights[2], "*ln_sales_tax:year2011 + ", cohort.weights[3], 
                   "*ln_sales_tax:year2012 + ", cohort.weights[4], "*ln_sales_tax:year2013 + ", 
                   cohort.weights[5], "*ln_sales_tax:year2014", sep = "")
  lc.formula0 <- paste0(lc.lr0, " = 0", sep = "")
  lc.test0 <- glht(res1, linfct = c(lc.formula0))
  
  # Calculate the p-value
  pval <- 2*(1 - pnorm(abs(coef(summary(lc.test0))[[1]]/sqrt(vcov(summary(lc.test0)))[[1]])))
  
  
  lp.dt <- data.table(
    rn = "avg.ln_sales_tax",
    Estimate = coef(summary(lc.test0))[[1]],
    `Cluster s.e.` = sqrt(vcov(summary(lc.test0)))[[1]],
    `Pr(>|t|)` = pval,
    outcome = "ln_expenditures",
    Rsq = summary(res1)$r.squared,
    adj.Rsq = summary(res1)$adj.r.squared,
    N.obs = nrow(data.est),
    N_hholds = uniqueN(data.est[!is.na(ln_expenditures)], by = c("household_code")),
    N_groups = uniqueN(data.est[!is.na(ln_expenditures)], by = c("product_group_code")),
    FE_d = FE
  )
  LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T) ## Merge results to LRdiff_res
  fwrite(LRdiff_res, output.results.file) ## Write resulting file to a csv file
  
}