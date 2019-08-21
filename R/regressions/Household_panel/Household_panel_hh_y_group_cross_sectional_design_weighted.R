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
purchases.sample <- fread("cleaning/consumer_panel_y_hh_group_2006-2016.csv")

## Constraining Data set for estimations ------------ 

# FE
purchases.sample[, group_by_time := .GRP, by = .(product_group_code, year)]
purchases.sample[, household_by_time := .GRP, by = .(year, household_code)]

### Drop observations for which the sales tax rate is imputed
purchases.sample <- purchases.sample[year >= 2008 & year <= 2014]
purchases.sample$year <- factor(purchases.sample$year) ##Convert the indicator for year to a factor variable (needed for interaction in the regression between ln_sales_tax and dummy for year)

# Compute sales weight: by quarter, how large are purchases in each group in this sample?
purchases.sample[, sales.weight := sum(expenditures, na.rm = T), by = .(product_group_code, quarter, year)]
purchases.sample[, sales.weight := sales.weight / sum(sales.weight, na.rm = T), by = .(quarter, year)]

# Build new weight as the prdocut of both household and group weights
purchases.sample[, projection_factor := projection_factor*sales.weight]

# Drop observations without weights at the end
purchases.sample <- purchases.sample[!is.na(projection_factor)]


cohort.weights <- rep(1, 7) ##Construct weights to average across cohorts/years.  Start with equal weights
cohort.weights <- cohort.weights/sum(cohort.weights)

## Estimations: Expenditure on type of module --------

output.results.file <- "../../../../../home/slacouture/HMS/HH_group_year_cross_sectonal_design_salesweight.csv"

outcomes <- c("ln_expenditures", "ln_expenditures_taxable", "ln_expenditures_non_taxable", 
              "ln_share", "ln_share_taxable",  "ln_share_non_taxable")

LRdiff_res <- data.table(NULL)
for (Y in outcomes) {
  formula1 <- as.formula(paste0(
    Y, "~ ln_sales_tax:year | household_by_time + group_by_time | 0 | household_code"
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
  
  ### Take linear combinations of coefficients and attach results (this is the coefficient of interest)
  lc.lr0 <- paste0(cohort.weights[1], "*ln_sales_tax:year2008 + ", cohort.weights[2], "*ln_sales_tax:year2009 + ", cohort.weights[3], "*ln_sales_tax:year2010 + ", cohort.weights[4], "*ln_sales_tax:year2011 + ", cohort.weights[5], "*ln_sales_tax:year2012 + ", cohort.weights[6], "*ln_sales_tax:year2013 + ", cohort.weights[7], "*ln_sales_tax:year2014", sep = "")
  lc.formula0 <- paste0(lc.lr0, " = 0", sep = "")
  lc.test0 <- glht(res1, linfct = c(lc.formula0))
  
  # Calculate the p-value
  pval <- 2*(1 - pnorm(abs(coef(summary(lc.test0))[[1]]/sqrt(vcov(summary(lc.test0)))[[1]])))
  
  
  lp.dt <- data.table(
    rn = "avg.ln_sales_tax",
    Estimate = coef(summary(lc.test0))[[1]],
    `Cluster s.e.` = sqrt(vcov(summary(lc.test0)))[[1]],
    `Pr(>|t|)` = pval,
    outcome = Y,
    Rsq = summary(res1)$r.squared,
    adj.Rsq = summary(res1)$adj.r.squared,
    N.obs = nrow(purchases.sample[!is.na(get(Y))]),
    N_hholds = uniqueN(purchases.sample[!is.na(get(Y))], by = c("household_code")),
    N_groups = uniqueN(purchases.sample[!is.na(get(Y))], by = c("product_group_code"))
  )
  LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T) ## Merge results to LRdiff_res
  fwrite(LRdiff_res, output.results.file) ## Write resulting file to a csv file
  
  
}



## summary values --------------------------------------------------------------
LRdiff_res$N_years <- uniqueN(purchases.sample, by = c("year"))

fwrite(LRdiff_res, output.results.file)
