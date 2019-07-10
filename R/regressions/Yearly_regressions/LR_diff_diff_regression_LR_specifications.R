###
### In this do-file we explore the "very long-run specification": Diff-in-diff over 1) product and 2) stores

library(data.table)
library(lfe)
library(futile.logger)
library(AER)
library(multcomp)


setwd("/project2/igaarder")

## useful filepaths ------------------------------------------------------------
all_goods_pi_path <- "Data/Nielsen/price_quantity_indices_allitems_2006-2016_notaxinfo.csv"
FE_pindex_path <- "Data/Nielsen/Pindex_FE_yearly_all_years.csv"
output_yearly <- "Data/Nielsen/yearly_nielsen_data.csv"
pre_trend_data_path <- "Data/Nielsen/pre_trend_data_yearly.csv"
#taxable_pi_path <- "Data/Nielsen/price_quantity_indices_taxableitems_2006-2016.csv"
#eventstudy_tr_path <- "Data/event_study_tr_groups_comprehensive_firstonly_no2012q4_2013q1q2.csv"
#tr_groups_path <- "Data/tr_groups_comprehensive_firstonly_no2012q4_2013q1q2.csv"


## Where to save results
output.results.file <- "Data/LRdiff_results_LRspec.csv"


# ### Prepare the data
## See LR_diff_diff_regression_sales_FE_specification.R -- the file output_yearly is prepared in that file
yearly_data <- fread(output_yearly)


### Drop observations for which the sales tax rate is imputed
yearly_data <- yearly_data[year >= 2008 & year <= 2014]
yearly_data$year <- factor(yearly_data$year) ##Convert the indicator for year to a factor variable (needed for interaction in the regression between ln_sales_tax and dummy for year)

cohort.weights <- rep(1, 7) ##Construct weights to average across cohorts/years.  Start with equal weights
cohort.weights <- cohort.weights/sum(cohort.weights)


### Price
formula0 <- as.formula(paste0(
  "ln_cpricei2 ~ ln_sales_tax:year | module_by_time + store_by_time | 0 | state_by_module "
))

res0 <- felm(data = yearly_data,
             formula = formula0,
             weights = yearly_data$base.sales)



## attach results
flog.info("Writing results...")
res1.dt <- data.table(coef(summary(res0)), keep.rownames=T)
res1.dt[, outcome := "ln_cpricei2"]
res1.dt[, Rsq := summary(res0)$r.squared]
res1.dt[, adj.Rsq := summary(res0)$adj.r.squared]
res1.dt[, specification := "LR"]
LRdiff_res <- res1.dt ### Create table LRdiff_res in which we store all results (we start with the results we had just stored in res1.dt)
fwrite(LRdiff_res, output.results.file)  ## Write results to a csv file 



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
  outcome = "ln_cpricei2",
  Rsq = summary(res0)$r.squared,
  adj.Rsq = summary(res0)$adj.r.squared,
  specification = "LR")
LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T) ## Merge results to LRdiff_res
fwrite(LRdiff_res, output.results.file) ## Write resulting file to a csv file




### Quantity
formula1 <- as.formula(paste0(
  "ln_quantity2 ~ ln_sales_tax:year | module_by_time + store_by_time | 0 | state_by_module "
))

res1 <- felm(data = yearly_data,
             formula = formula1,
             weights = yearly_data$base.sales)


## attach results
flog.info("Writing results...")
res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
res1.dt[, outcome := "ln_quantity2"]
res1.dt[, Rsq := summary(res1)$r.squared]
res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
res1.dt[, specification := "LR"]
LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T) ## Merge results to LRdiff_res
fwrite(LRdiff_res, output.results.file)  ## Write results to a csv file 


# Take linear combinations of coefficients
lc.formula1 <- paste0(cohort.weights[1], "*ln_sales_tax:year2008 + ", cohort.weights[2], "*ln_sales_tax:year2009 + ", cohort.weights[3], "*ln_sales_tax:year2010 + ", cohort.weights[4], "*ln_sales_tax:year2011 + ", cohort.weights[5], "*ln_sales_tax:year2012 + ", cohort.weights[6], "*ln_sales_tax:year2013 + ", cohort.weights[7], "*ln_sales_tax:year2014 = 0", sep = "")
lc.test1 <- glht(res1, linfct = c(lc.formula1))

# Calculate the p-value
pval <- 2*(1 - pnorm(abs(coef(summary(lc.test1))[[1]]/sqrt(vcov(summary(lc.test1)))[[1]])))



lp.dt <- data.table(
  rn = "avg.ln_sales_tax",
  Estimate = coef(summary(lc.test1))[[1]],
  `Cluster s.e.` = sqrt(vcov(summary(lc.test1)))[[1]],
  `Pr(>|t|)` = pval,
  outcome = "ln_quantity2",
  Rsq = summary(res1)$r.squared,
  adj.Rsq = summary(res1)$adj.r.squared,
  specification = "LR")
LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T) ## Merge results to LRdiff_res
fwrite(LRdiff_res, output.results.file) ## Write resulting file to a csv file



## summary values --------------------------------------------------------------
LRdiff_res$N_obs <- nrow(yearly_data)
LRdiff_res$N_modules <- length(unique(yearly_data$product_module_code))
LRdiff_res$N_stores <- length(unique(yearly_data$store_code_uc))
LRdiff_res$N_counties <- uniqueN(yearly_data, by = c("fips_state", "fips_county"))
LRdiff_res$N_years <- uniqueN(yearly_data, by = c("year")) 
LRdiff_res$N_county_modules <- uniqueN(yearly_data, by = c("fips_state", "fips_county",
                                                           "product_module_code"))
LRdiff_res$N_store_modules <- uniqueN(yearly_data, by = c("store_code_uc", "product_module_code"))
LRdiff_res$N_state_modules <- uniqueN(yearly_data, by = c("fips_state", "product_module_code"))

fwrite(LRdiff_res, "Data/LRdiff_results_LRspec.csv")


