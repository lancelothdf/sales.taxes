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

# Take linear combinations of coefficients
lc.lr0 <- paste0(cohort.weights[1], "*ln_sales_tax:year2008 + ", cohort.weights[2], "*ln_sales_tax:year2009 + ", cohort.weights[3], "*ln_sales_tax:year2010 + ", cohort.weights[4], "*ln_sales_tax:year2011 + ", cohort.weights[5], "*ln_sales_tax:year2012 + ", cohort.weights[6], "*ln_sales_tax:year2013 + ", cohort.weights[7], "*ln_sales_tax:year2014", sep = "")
lc.formula0 <- paste0(lc.lr0, " = 0", sep = "")
lc.test0 <- glht(res0, linfct = c(lc.formula0))


## Save results
LRdiff_res <- data.table(
    outcome = "ln_cpricei2",
    estimate = coef(summary(lc.test0))[[1]],
    se = sqrt(vcov(summary(lc.test0)))[[1]],
    Rsq = summary(res0)$r.squared,
    adj.Rsq = summary(res0)$adj.r.squared,
    specification = "LR"
)


### Quantity
formula1 <- as.formula(paste0(
  "ln_quantity2 ~ ln_sales_tax:year | module_by_time + store_by_time | 0 | state_by_module "
))

res1 <- felm(data = yearly_data,
             formula = formula1,
             weights = yearly_data$base.sales)

# Take linear combinations of coefficients
lc.formula1 <- paste0(cohort.weights[1], "*ln_sales_tax:year2008 + ", cohort.weights[2], "*ln_sales_tax:year2009 + ", cohort.weights[3], "*ln_sales_tax:year2010 + ", cohort.weights[4], "*ln_sales_tax:year2011 + ", cohort.weights[5], "*ln_sales_tax:year2012 + ", cohort.weights[6], "*ln_sales_tax:year2013 + ", cohort.weights[7], "*ln_sales_tax:year2014 = 0", sep = "")
lc.test1 <- glht(res1, linfct = c(lc.formula))


## Save Results
LRdiff_res <- rbind(
  LRdiff_res,
  data.table(
    outcome = "ln_quantity2",
    estimate = coef(summary(lc.test1))[[1]],
    se = sqrt(vcov(summary(lc.test1)))[[1]],
    Rsq = summary(res1)$r.squared,
    adj.Rsq = summary(res1)$adj.r.squared,
    specification = "LR"
  )
)


### Expenditure Share
formula2 <- as.formula(paste0(
  "expend_share ~ ln_sales_tax:year | module_by_time + store_by_time | 0 | state_by_module "
))

res2 <- felm(data = yearly_data,
             formula = formula2,
             weights = yearly_data$base.sales)

# Take linear combinations of coefficients
lc.formula2 <- paste0(cohort.weights[1], "*ln_sales_tax:year2008 + ", cohort.weights[2], "*ln_sales_tax:year2009 + ", cohort.weights[3], "*ln_sales_tax:year2010 + ", cohort.weights[4], "*ln_sales_tax:year2011 + ", cohort.weights[5], "*ln_sales_tax:year2012 + ", cohort.weights[6], "*ln_sales_tax:year2013 + ", cohort.weights[7], "*ln_sales_tax:year2014 = 0", sep = "")
lc.test2 <- glht(res2, linfct = c(lc.formula))

## Save Results
LRdiff_res <- rbind(
  LRdiff_res,
  data.table(
    outcome = "expend_share",
    estimate = coef(summary(lc.test2))[[1]],
    se = sqrt(vcov(summary(lc.test2)))[[1]],
    Rsq = summary(res2)$r.squared,
    adj.Rsq = summary(res2)$adj.r.squared,
    specification = "LR"
  )
)


### Ln_expend_share
formula3 <- as.formula(paste0(
  "ln_expend_share ~ ln_sales_tax:year | module_by_time + store_by_time | 0 | state_by_module "
))

res3 <- felm(data = yearly_data,
             formula = formula3,
             weights = yearly_data$base.sales)

# Take linear combinations of coefficients
lc.formula3 <- paste0(cohort.weights[1], "*ln_sales_tax:year2008 + ", cohort.weights[2], "*ln_sales_tax:year2009 + ", cohort.weights[3], "*ln_sales_tax:year2010 + ", cohort.weights[4], "*ln_sales_tax:year2011 + ", cohort.weights[5], "*ln_sales_tax:year2012 + ", cohort.weights[6], "*ln_sales_tax:year2013 + ", cohort.weights[7], "*ln_sales_tax:year2014 = 0", sep = "")
lc.test3 <- glht(res3, linfct = c(lc.formula))


## Save Results
LRdiff_res <- rbind(
  LRdiff_res,
  data.table(
    outcome = "ln_expend_share",
    estimate = coef(summary(lc.test3))[[1]],
    se = sqrt(vcov(summary(lc.test3)))[[1]],
    Rsq = summary(res3)$r.squared,
    adj.Rsq = summary(res3)$adj.r.squared,
    specification = "LR"
  )
)




## summary values --------------------------------------------------------------
LRdiff_res$N_obs <- nrow(yearly_data)
LRdiff_res$N_modules <- length(unique(yearly_data$product_module_code))
LRdiff_res$N_stores <- length(unique(yearly_data$store_code_uc))
LRdiff_res$N_counties <- uniqueN(yearly_data, by = c("fips_state", "fips_county"))
LRdiff_res$N_years <- uniqueN(yearly_data, by = c("year")) # should be 6 (we lose one because we difference)
LRdiff_res$N_county_modules <- uniqueN(yearly_data, by = c("fips_state", "fips_county",
                                                           "product_module_code"))

fwrite(LRdiff_res, "Data/LRdiff_results_LRspec.csv")


### Long-run IV specification
