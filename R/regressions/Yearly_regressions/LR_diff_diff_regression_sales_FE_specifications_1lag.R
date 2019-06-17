### In this R-file we explore a specification meant to estimate long-run pass-through and sales/quantity response to tax changes

library(data.table)
library(lfe)
library(futile.logger)
library(AER)


setwd("/project2/igaarder")

## useful filepaths ------------------------------------------------------------
FE_pindex_path <- "Data/Nielsen/Pindex_FE_yearly_all_years.csv"
output_yearly <- "Data/Nielsen/yearly_nielsen_data.csv"
#taxable_pi_path <- "Data/Nielsen/price_quantity_indices_taxableitems_2006-2016.csv"
#eventstudy_tr_path <- "Data/event_study_tr_groups_comprehensive_firstonly_no2012q4_2013q1q2.csv"
#tr_groups_path <- "Data/tr_groups_comprehensive_firstonly_no2012q4_2013q1q2.csv"

covariates.nhgis.path <- "Data/covariates/nhgis_county_clean.csv"
covariates.qcew.path <- "Data/covariates/qcew_clean.csv"
census.regions.path <- "Data/covariates/census_regions.csv"



######### Regression analysis
## run the analysis on price ---------------------------------------------------
yearly_data <- fread(output_yearly)


################################
## Include some covariates
#List of unique counties in the sales data
list.counties <- data.frame(unique(yearly_data[,c('fips_state','fips_county')]))

covariates.nhgis <- fread(covariates.nhgis.path)
census.regions <- fread(census.regions.path)
census.regions <- merge(list.counties, census.regions, by = c("fips_state"),
                        all.x = T)
census.regions$Division <- census.regions$Region*10 + census.regions$Division


setkey(covariates.nhgis, year)
covariates.nhgis <- covariates.nhgis[list(2000),]

setnames(covariates.nhgis, c('statefp','countyfp'), c('fips_state', 'fips_county'))
covariates.nhgis <- covariates.nhgis[,c('fips_state', 'fips_county', 'pct_pop_urban')] #For now, let's just use "urban"
covariates.nhgis$urban <- as.integer(covariates.nhgis$pct_pop_urban >= 0.5)

covariates.nhgis <- merge(census.regions, covariates.nhgis, by = c("fips_state", "fips_county"),
                          all.x = T)


###
yearly_data <- yearly_data[order(fips_state, fips_county, store_code_uc, product_module_code, year), lag1 := shift(.SD, 1, type = "lag"), .SDcols = "ln_sales_tax", by = c("fips_state", "fips_county", "store_code_uc", "product_module_code")]
yearly_data <- yearly_data[year >= 2009 & year <= 2015,]
##NOTE: Make sure that we only include 2009-2015 in the regressions!! - These are the years for which the lagged tax rate is not imputed

### Change variable names to make rest of the code easier (now ln_sales_tax is the lagged tax rate)
yearly_data[, current := ln_sales_tax]
yearly_data[, ln_sales_tax := lag1]

###
yearly_data <- merge(yearly_data, covariates.nhgis, by = c("fips_state", "fips_county"), all.x = T)

yearly_data[, region_by_time := .GRP, by = .(Region, year)]
yearly_data[, division_by_time := .GRP, by = .(Division, year)]
yearly_data[, urban_by_time := .GRP, by = .(urban, year)]
yearly_data[, regXurban_by_time := .GRP, by = .(Region, urban, year)]
yearly_data[, divXurban_by_time := .GRP, by = .(Division, urban, year)]
yearly_data[, reg_by_module_by_time := .GRP, by = .(Region, product_module_code, year)]
yearly_data[, div_by_module_by_time := .GRP, by = .(Division, product_module_code, year)]


##Make sure that some of these variables are treated as factor variables
#yearly_data$product_module_code <- factor(yearly_data$product_module_code)


#############################################
 ## no FE (only store_module and time) - First measure of prices
 ## First price index
 price_formula1 <- as.formula(paste0(
   "ln_cpricei ~ ln_sales_tax | store_module + yr | 0 | state_by_module "
 ))

 price_res1 <- felm(data = yearly_data,
                   formula = price_formula1,
                   weights = yearly_data$base.sales)

 LRdiff_res <-
   data.table(
     outcome = "ln_cpricei",
     estimate = coef(summary(price_res1))["ln_sales_tax", "Estimate"],
     se = coef(summary(price_res1))["ln_sales_tax", "Cluster s.e."],
     pval = coef(summary(price_res1))["ln_sales_tax", "Pr(>|t|)"],
     Rsq = summary(price_res1)$r.squared,
     adj.Rsq = summary(price_res1)$adj.r.squared,
     time_controls = "time FE"
   )


## with module-time FE - First measure of prices
## First price index
price_formula <- as.formula(paste0(
  "ln_cpricei ~ ln_sales_tax | store_module + module_by_time | 0 | state_by_module "
))

price_res <- felm(data = yearly_data,
                   formula = price_formula,
                   weights = yearly_data$base.sales)

LRdiff_res <-rbind(
  LRdiff_res,
  data.table(
    outcome = "ln_cpricei",
    estimate = coef(summary(price_res))["ln_sales_tax", "Estimate"],
    se = coef(summary(price_res))["ln_sales_tax", "Cluster s.e."],
    pval = coef(summary(price_res))["ln_sales_tax", "Pr(>|t|)"],
    Rsq = summary(price_res)$r.squared,
    adj.Rsq = summary(price_res)$adj.r.squared,
    time_controls = "module-time FE"
  )
)


## with store-time FE (instead of module-time) - First measure of prices
price_formula2 <- as.formula(paste0(
  "ln_cpricei ~ ln_sales_tax | store_module + store_by_time | 0 | state_by_module "
))

price_res2 <- felm(data = yearly_data,
                  formula = price_formula2,
                  weights = yearly_data$base.sales)


LRdiff_res <-rbind(
  LRdiff_res,
  data.table(
    outcome = "ln_cpricei",
    estimate = coef(summary(price_res2))["ln_sales_tax", "Estimate"],
    se = coef(summary(price_res2))["ln_sales_tax", "Cluster s.e."],
    pval = coef(summary(price_res2))["ln_sales_tax", "Pr(>|t|)"],
    Rsq = summary(price_res2)$r.squared,
    adj.Rsq = summary(price_res2)$adj.r.squared,
    time_controls = "store-time FE"
  )
)


## with store-time FE + module-time FE - First measure of prices
price_formula3 <- as.formula(paste0(
  "ln_cpricei ~ ln_sales_tax | store_module + store_by_time + module_by_time | 0 | state_by_module "
))

price_res3 <- felm(data = yearly_data,
                   formula = price_formula3,
                   weights = yearly_data$base.sales)


LRdiff_res <-rbind(
  LRdiff_res,
  data.table(
    outcome = "ln_cpricei",
    estimate = coef(summary(price_res3))["ln_sales_tax", "Estimate"],
    se = coef(summary(price_res3))["ln_sales_tax", "Cluster s.e."],
    pval = coef(summary(price_res3))["ln_sales_tax", "Pr(>|t|)"],
    Rsq = summary(price_res3)$r.squared,
    adj.Rsq = summary(price_res3)$adj.r.squared,
    time_controls = "store-time FE + module-time FE"
  )
)



## no FE (only store_module and time) - Second measure of prices
price_formula7 <- as.formula(paste0(
  "ln_cpricei2 ~ ln_sales_tax | store_module + yr | 0 | state_by_module "
))

price2_res1 <- felm(data = yearly_data,
                   formula = price_formula7,
                   weights = yearly_data$base.sales)

LRdiff_res <-rbind(
  LRdiff_res,
  data.table(
    outcome = "ln_cpricei2",
    estimate = coef(summary(price2_res1))["ln_sales_tax", "Estimate"],
    se = coef(summary(price2_res1))["ln_sales_tax", "Cluster s.e."],
    pval = coef(summary(price2_res1))["ln_sales_tax", "Pr(>|t|)"],
    Rsq = summary(price2_res1)$r.squared,
    adj.Rsq = summary(price2_res1)$adj.r.squared,
    time_controls = "time FE"
  )
)



## with module-time FE - Second measure of prices
price_formula4 <- as.formula(paste0(
  "ln_cpricei2 ~ ln_sales_tax | store_module + module_by_time | 0 | state_by_module "
))

price2_res <- felm(data = yearly_data,
                  formula = price_formula4,
                  weights = yearly_data$base.sales)

LRdiff_res <-rbind(
  LRdiff_res,
  data.table(
    outcome = "ln_cpricei2",
    estimate = coef(summary(price2_res))["ln_sales_tax", "Estimate"],
    se = coef(summary(price2_res))["ln_sales_tax", "Cluster s.e."],
    pval = coef(summary(price2_res))["ln_sales_tax", "Pr(>|t|)"],
    Rsq = summary(price2_res)$r.squared,
    adj.Rsq = summary(price2_res)$adj.r.squared,
    time_controls = "module-time FE"
  )
)


## with store-time FE (instead of module-time) - Second measure of prices
price_formula5 <- as.formula(paste0(
  "ln_cpricei2 ~ ln_sales_tax | store_module + store_by_time | 0 | state_by_module "
))

price2_res2 <- felm(data = yearly_data,
                   formula = price_formula5,
                   weights = yearly_data$base.sales)


LRdiff_res <-rbind(
  LRdiff_res,
  data.table(
    outcome = "ln_cpricei2",
    estimate = coef(summary(price2_res2))["ln_sales_tax", "Estimate"],
    se = coef(summary(price2_res2))["ln_sales_tax", "Cluster s.e."],
    pval = coef(summary(price2_res2))["ln_sales_tax", "Pr(>|t|)"],
    Rsq = summary(price2_res2)$r.squared,
    adj.Rsq = summary(price2_res2)$adj.r.squared,
    time_controls = "store-time FE"
  )
)


## with store-time FE + module-time FE - Second measure of prices
price_formula6 <- as.formula(paste0(
  "ln_cpricei2 ~ ln_sales_tax | store_module + store_by_time + module_by_time | 0 | state_by_module "
))

price2_res3 <- felm(data = yearly_data,
                   formula = price_formula6,
                   weights = yearly_data$base.sales)


LRdiff_res <-rbind(
  LRdiff_res,
  data.table(
    outcome = "ln_cpricei2",
    estimate = coef(summary(price2_res3))["ln_sales_tax", "Estimate"],
    se = coef(summary(price2_res3))["ln_sales_tax", "Cluster s.e."],
    pval = coef(summary(price2_res3))["ln_sales_tax", "Pr(>|t|)"],
    Rsq = summary(price2_res3)$r.squared,
    adj.Rsq = summary(price2_res3)$adj.r.squared,
    time_controls = "store-time FE + module-time FE"
  )
)



## run the analysis on quantity ------------------------------------------------

## no FE (store_module and time FE only) - First measure of quantity
quantity_formula1 <- as.formula(paste0(
  "ln_quantity ~ ln_sales_tax | store_module + yr | 0 | state_by_module "
))

quantity_res1 <- felm(data = yearly_data,
                     formula = quantity_formula1,
                     weights = yearly_data$base.sales)

LRdiff_res <- rbind(
  LRdiff_res,
  data.table(
    outcome = "ln_quantity",
    estimate = coef(summary(quantity_res1))["ln_sales_tax", "Estimate"],
    se = coef(summary(quantity_res1))["ln_sales_tax", "Cluster s.e."],
    pval = coef(summary(quantity_res1))["ln_sales_tax", "Pr(>|t|)"],
    Rsq = summary(quantity_res1)$r.squared,
    adj.Rsq = summary(quantity_res1)$adj.r.squared,
    time_controls = "time FE"
  )
)


## with module-time FE - First measure of quantity
quantity_formula <- as.formula(paste0(
  "ln_quantity ~ ln_sales_tax | store_module + module_by_time | 0 | state_by_module "
))

quantity_res <- felm(data = yearly_data,
                      formula = quantity_formula,
                      weights = yearly_data$base.sales)

LRdiff_res <- rbind(
  LRdiff_res,
  data.table(
    outcome = "ln_quantity",
    estimate = coef(summary(quantity_res))["ln_sales_tax", "Estimate"],
    se = coef(summary(quantity_res))["ln_sales_tax", "Cluster s.e."],
    pval = coef(summary(quantity_res))["ln_sales_tax", "Pr(>|t|)"],
    Rsq = summary(quantity_res)$r.squared,
    adj.Rsq = summary(quantity_res)$adj.r.squared,
    time_controls = "module-time FE"
  )
)


## with store-time FE (instead of module-time) - First measure of quantity
quantity_formula2 <- as.formula(paste0(
  "ln_quantity ~ ln_sales_tax | store_module + store_by_time | 0 | state_by_module "
))

quantity_res2 <- felm(data = yearly_data,
                   formula = quantity_formula2,
                   weights = yearly_data$base.sales)


LRdiff_res <-rbind(
  LRdiff_res,
  data.table(
    outcome = "ln_quantity",
    estimate = coef(summary(quantity_res2))["ln_sales_tax", "Estimate"],
    se = coef(summary(quantity_res2))["ln_sales_tax", "Cluster s.e."],
    pval = coef(summary(quantity_res2))["ln_sales_tax", "Pr(>|t|)"],
    Rsq = summary(quantity_res2)$r.squared,
    adj.Rsq = summary(quantity_res2)$adj.r.squared,
    time_controls = "store-time FE"
  )
)


## with store-time FE + module-time FE - First measure of quantity
quantity_formula3 <- as.formula(paste0(
  "ln_quantity ~ ln_sales_tax | store_module + store_by_time + module_by_time | 0 | state_by_module "
))

quantity_res3 <- felm(data = yearly_data,
                   formula = quantity_formula3,
                   weights = yearly_data$base.sales)


LRdiff_res <-rbind(
  LRdiff_res,
  data.table(
    outcome = "ln_quantity",
    estimate = coef(summary(quantity_res3))["ln_sales_tax", "Estimate"],
    se = coef(summary(quantity_res3))["ln_sales_tax", "Cluster s.e."],
    pval = coef(summary(quantity_res3))["ln_sales_tax", "Pr(>|t|)"],
    Rsq = summary(quantity_res3)$r.squared,
    adj.Rsq = summary(quantity_res3)$adj.r.squared,
    time_controls = "store-time FE + module-time FE"
  )
)


## no FE (only store_module and time FE) - Second measure of quantity
quantity_formula7 <- as.formula(paste0(
  "ln_quantity ~ ln_sales_tax | store_module + yr | 0 | state_by_module "
))

quantity2_res1 <- felm(data = yearly_data,
                      formula = quantity_formula7,
                      weights = yearly_data$base.sales)

LRdiff_res <- rbind(
  LRdiff_res,
  data.table(
    outcome = "ln_quantity2",
    estimate = coef(summary(quantity2_res1))["ln_sales_tax", "Estimate"],
    se = coef(summary(quantity2_res1))["ln_sales_tax", "Cluster s.e."],
    pval = coef(summary(quantity2_res1))["ln_sales_tax", "Pr(>|t|)"],
    Rsq = summary(quantity2_res1)$r.squared,
    adj.Rsq = summary(quantity2_res1)$adj.r.squared,
    time_controls = "time FE"
  )
)



## with module-time FE - Second measure of quantity
quantity_formula4 <- as.formula(paste0(
  "ln_quantity ~ ln_sales_tax | store_module + module_by_time | 0 | state_by_module "
))

quantity2_res <- felm(data = yearly_data,
                     formula = quantity_formula4,
                     weights = yearly_data$base.sales)

LRdiff_res <- rbind(
  LRdiff_res,
  data.table(
    outcome = "ln_quantity2",
    estimate = coef(summary(quantity2_res))["ln_sales_tax", "Estimate"],
    se = coef(summary(quantity2_res))["ln_sales_tax", "Cluster s.e."],
    pval = coef(summary(quantity2_res))["ln_sales_tax", "Pr(>|t|)"],
    Rsq = summary(quantity2_res)$r.squared,
    adj.Rsq = summary(quantity2_res)$adj.r.squared,
    time_controls = "module-time FE"
  )
)


## with store-time FE (instead of module-time) - second measure of quantity
quantity_formula5 <- as.formula(paste0(
  "ln_quantity2 ~ ln_sales_tax | store_module + store_by_time | 0 | state_by_module "
))

quantity2_res2 <- felm(data = yearly_data,
                      formula = quantity_formula5,
                      weights = yearly_data$base.sales)


LRdiff_res <-rbind(
  LRdiff_res,
  data.table(
    outcome = "ln_quantity2",
    estimate = coef(summary(quantity2_res2))["ln_sales_tax", "Estimate"],
    se = coef(summary(quantity2_res2))["ln_sales_tax", "Cluster s.e."],
    pval = coef(summary(quantity2_res2))["ln_sales_tax", "Pr(>|t|)"],
    Rsq = summary(quantity2_res2)$r.squared,
    adj.Rsq = summary(quantity2_res2)$adj.r.squared,
    time_controls = "store-time FE"
  )
)


## with store-time FE + module-time FE - First measure of quantity
quantity_formula6 <- as.formula(paste0(
  "ln_quantity2 ~ ln_sales_tax | store_module + store_by_time + module_by_time | 0 | state_by_module "
))

quantity2_res3 <- felm(data = yearly_data,
                      formula = quantity_formula6,
                      weights = yearly_data$base.sales)


LRdiff_res <-rbind(
  LRdiff_res,
  data.table(
    outcome = "ln_quantity2",
    estimate = coef(summary(quantity2_res3))["ln_sales_tax", "Estimate"],
    se = coef(summary(quantity2_res3))["ln_sales_tax", "Cluster s.e."],
    pval = coef(summary(quantity2_res3))["ln_sales_tax", "Pr(>|t|)"],
    Rsq = summary(quantity2_res3)$r.squared,
    adj.Rsq = summary(quantity2_res3)$adj.r.squared,
    time_controls = "store-time FE + module-time FE"
  )
)


## run the analysis on expenditure share ---------------------------------------

## no FE (only module_store and time FE)
expenditure_formula1 <- as.formula(paste0(
  "expend_share ~ ln_sales_tax | store_module + yr | 0 | state_by_module "
))

expenditure_res1 <- felm(data = yearly_data,
                        formula = expenditure_formula1,
                        weights = yearly_data$base.sales)

LRdiff_res <- rbind(
  LRdiff_res,
  data.table(
    outcome = "expend_share",
    estimate = coef(summary(expenditure_res1))["ln_sales_tax", "Estimate"],
    se = coef(summary(expenditure_res1))["ln_sales_tax", "Cluster s.e."],
    pval = coef(summary(expenditure_res1))["ln_sales_tax", "Pr(>|t|)"],
    Rsq = summary(expenditure_res1)$r.squared,
    adj.Rsq = summary(expenditure_res1)$adj.r.squared,
    time_controls = "time FE"
  )
)



## with module-time FE
expenditure_formula <- as.formula(paste0(
  "expend_share ~ ln_sales_tax | store_module + module_by_time | 0 | state_by_module "
))

expenditure_res <- felm(data = yearly_data,
                           formula = expenditure_formula,
                           weights = yearly_data$base.sales)

LRdiff_res <- rbind(
  LRdiff_res,
  data.table(
    outcome = "expend_share",
    estimate = coef(summary(expenditure_res))["ln_sales_tax", "Estimate"],
    se = coef(summary(expenditure_res))["ln_sales_tax", "Cluster s.e."],
    pval = coef(summary(expenditure_res))["ln_sales_tax", "Pr(>|t|)"],
    Rsq = summary(expenditure_res)$r.squared,
    adj.Rsq = summary(expenditure_res)$adj.r.squared,
    time_controls = "module-time FE"
  )
)

## with store-time FE (instead of module-time)
expenditure_formula2 <- as.formula(paste0(
  "expend_share ~ ln_sales_tax | store_module + store_by_time | 0 | state_by_module "
))

expenditure_res2 <- felm(data = yearly_data,
                            formula = expenditure_formula2,
                            weights = yearly_data$base.sales)


LRdiff_res <-rbind(
  LRdiff_res,
  data.table(
    outcome = "expend_share",
    estimate = coef(summary(expenditure_res2))["ln_sales_tax", "Estimate"],
    se = coef(summary(expenditure_res2))["ln_sales_tax", "Cluster s.e."],
    pval = coef(summary(expenditure_res2))["ln_sales_tax", "Pr(>|t|)"],
    Rsq = summary(expenditure_res2)$r.squared,
    adj.Rsq = summary(expenditure_res2)$adj.r.squared,
    time_controls = "store-time FE"
  )
)


## with store-time FE + module-time FE
expenditure_formula3 <- as.formula(paste0(
  "expend_share ~ ln_sales_tax | store_module + store_by_time + module_by_time | 0 | state_by_module "
))

expenditure_res3 <- felm(data = yearly_data,
                            formula = expenditure_formula3,
                            weights = yearly_data$base.sales)


LRdiff_res <-rbind(
  LRdiff_res,
  data.table(
    outcome = "expend_share",
    estimate = coef(summary(expenditure_res3))["ln_sales_tax", "Estimate"],
    se = coef(summary(expenditure_res3))["ln_sales_tax", "Cluster s.e."],
    pval = coef(summary(expenditure_res3))["ln_sales_tax", "Pr(>|t|)"],
    Rsq = summary(expenditure_res3)$r.squared,
    adj.Rsq = summary(expenditure_res3)$adj.r.squared,
    time_controls = "store-time FE + module-time FE"
  )
)



## run the analysis on log of expenditure share ---------------------------------------


## no FE (module_store and time FE only)
ln_expenditure_formula1 <- as.formula(paste0(
  "ln_expend_share ~ ln_sales_tax | store_module + yr | 0 | state_by_module "
))

ln_expenditure_res1 <- felm(data = yearly_data,
                           formula = ln_expenditure_formula1,
                           weights = yearly_data$base.sales)

LRdiff_res <- rbind(
  LRdiff_res,
  data.table(
    outcome = "ln_expend_share",
    estimate = coef(summary(ln_expenditure_res1))["ln_sales_tax", "Estimate"],
    se = coef(summary(ln_expenditure_res1))["ln_sales_tax", "Cluster s.e."],
    pval = coef(summary(ln_expenditure_res1))["ln_sales_tax", "Pr(>|t|)"],
    Rsq = summary(ln_expenditure_res1)$r.squared,
    adj.Rsq = summary(ln_expenditure_res1)$adj.r.squared,
    time_controls = "time FE"
  )
)


## with module-time FE
ln_expenditure_formula <- as.formula(paste0(
  "ln_expend_share ~ ln_sales_tax | store_module + module_by_time | 0 | state_by_module "
))

ln_expenditure_res <- felm(data = yearly_data,
                         formula = ln_expenditure_formula,
                         weights = yearly_data$base.sales)

LRdiff_res <- rbind(
  LRdiff_res,
  data.table(
    outcome = "ln_expend_share",
    estimate = coef(summary(ln_expenditure_res))["ln_sales_tax", "Estimate"],
    se = coef(summary(ln_expenditure_res))["ln_sales_tax", "Cluster s.e."],
    pval = coef(summary(ln_expenditure_res))["ln_sales_tax", "Pr(>|t|)"],
    Rsq = summary(ln_expenditure_res)$r.squared,
    adj.Rsq = summary(ln_expenditure_res)$adj.r.squared,
    time_controls = "module-time FE"
  )
)

## with store-time FE (instead of module-time)
ln_expenditure_formula2 <- as.formula(paste0(
  "ln_expend_share ~ ln_sales_tax | store_module + store_by_time | 0 | state_by_module "
))

ln_expenditure_res2 <- felm(data = yearly_data,
                      formula = ln_expenditure_formula2,
                      weights = yearly_data$base.sales)


LRdiff_res <-rbind(
  LRdiff_res,
  data.table(
    outcome = "ln_expend_share",
    estimate = coef(summary(ln_expenditure_res2))["ln_sales_tax", "Estimate"],
    se = coef(summary(ln_expenditure_res2))["ln_sales_tax", "Cluster s.e."],
    pval = coef(summary(ln_expenditure_res2))["ln_sales_tax", "Pr(>|t|)"],
    Rsq = summary(ln_expenditure_res2)$r.squared,
    adj.Rsq = summary(ln_expenditure_res2)$adj.r.squared,
    time_controls = "store-time FE"
  )
)


## with store-time FE + module-time FE
ln_expenditure_formula3 <- as.formula(paste0(
  "ln_expend_share ~ ln_sales_tax | store_module + store_by_time + module_by_time | 0 | state_by_module "
))

ln_expenditure_res3 <- felm(data = yearly_data,
                      formula = ln_expenditure_formula3,
                      weights = yearly_data$base.sales)


LRdiff_res <-rbind(
  LRdiff_res,
  data.table(
    outcome = "ln_expend_share",
    estimate = coef(summary(ln_expenditure_res3))["ln_sales_tax", "Estimate"],
    se = coef(summary(ln_expenditure_res3))["ln_sales_tax", "Cluster s.e."],
    pval = coef(summary(ln_expenditure_res3))["ln_sales_tax", "Pr(>|t|)"],
    Rsq = summary(ln_expenditure_res3)$r.squared,
    adj.Rsq = summary(ln_expenditure_res3)$adj.r.squared,
    time_controls = "store-time FE + module-time FE"
  )
)



##################
################## ROBUSTNESS: Try to add more covariates
## with Region_by_time + Module_by_time FE - First measure of prices
## First price index
price_formula8 <- as.formula(paste0(
  "ln_cpricei ~ ln_sales_tax | store_module + module_by_time + region_by_time | 0 | state_by_module "
))

price_res4 <- felm(data = yearly_data,
                   formula = price_formula8,
                   weights = yearly_data$base.sales)

LRdiff_res <-rbind(
  LRdiff_res,
  data.table(
    outcome = "ln_cpricei",
    estimate = coef(summary(price_res4))["ln_sales_tax", "Estimate"],
    se = coef(summary(price_res4))["ln_sales_tax", "Cluster s.e."],
    pval = coef(summary(price_res4))["ln_sales_tax", "Pr(>|t|)"],
    Rsq = summary(price_res4)$r.squared,
    adj.Rsq = summary(price_res4)$adj.r.squared,
    time_controls = "module-time FE + region-time FE"
  )
)


## with Division_by_time + Module_by_time FE - First measure of prices
## First price index
price_formula8 <- as.formula(paste0(
  "ln_cpricei ~ ln_sales_tax | store_module + module_by_time + division_by_time | 0 | state_by_module "
))

price_res4 <- felm(data = yearly_data,
                   formula = price_formula8,
                   weights = yearly_data$base.sales)

LRdiff_res <-rbind(
  LRdiff_res,
  data.table(
    outcome = "ln_cpricei",
    estimate = coef(summary(price_res4))["ln_sales_tax", "Estimate"],
    se = coef(summary(price_res4))["ln_sales_tax", "Cluster s.e."],
    pval = coef(summary(price_res4))["ln_sales_tax", "Pr(>|t|)"],
    Rsq = summary(price_res4)$r.squared,
    adj.Rsq = summary(price_res4)$adj.r.squared,
    time_controls = "module-time FE + division-time FE"
  )
)

## with Module_by_Region_by_time FE - First measure of prices
## First price index
price_formula8 <- as.formula(paste0(
  "ln_cpricei ~ ln_sales_tax | store_module + module_by_time + reg_by_module_by_time | 0 | state_by_module "
))

price_res4 <- felm(data = yearly_data,
                   formula = price_formula8,
                   weights = yearly_data$base.sales)

LRdiff_res <-rbind(
  LRdiff_res,
  data.table(
    outcome = "ln_cpricei",
    estimate = coef(summary(price_res4))["ln_sales_tax", "Estimate"],
    se = coef(summary(price_res4))["ln_sales_tax", "Cluster s.e."],
    pval = coef(summary(price_res4))["ln_sales_tax", "Pr(>|t|)"],
    Rsq = summary(price_res4)$r.squared,
    adj.Rsq = summary(price_res4)$adj.r.squared,
    time_controls = "module-region-time FE"
  )
)


## with Module_by_Division_by_time FE - First measure of prices
## First price index
price_formula8 <- as.formula(paste0(
  "ln_cpricei ~ ln_sales_tax | store_module + module_by_time + div_by_module_by_time | 0 | state_by_module "
))

price_res4 <- felm(data = yearly_data,
                   formula = price_formula8,
                   weights = yearly_data$base.sales)

LRdiff_res <-rbind(
  LRdiff_res,
  data.table(
    outcome = "ln_cpricei",
    estimate = coef(summary(price_res4))["ln_sales_tax", "Estimate"],
    se = coef(summary(price_res4))["ln_sales_tax", "Cluster s.e."],
    pval = coef(summary(price_res4))["ln_sales_tax", "Pr(>|t|)"],
    Rsq = summary(price_res4)$r.squared,
    adj.Rsq = summary(price_res4)$adj.r.squared,
    time_controls = "module-division-time FE"
  )
)


## with Region_by_time + Module_by_time FE - Second measure of prices
## Second price index
price_formula8 <- as.formula(paste0(
  "ln_cpricei2 ~ ln_sales_tax | store_module + module_by_time + region_by_time | 0 | state_by_module "
))

price_res4 <- felm(data = yearly_data,
                   formula = price_formula8,
                   weights = yearly_data$base.sales)

LRdiff_res <-rbind(
  LRdiff_res,
  data.table(
    outcome = "ln_cpricei2",
    estimate = coef(summary(price_res4))["ln_sales_tax", "Estimate"],
    se = coef(summary(price_res4))["ln_sales_tax", "Cluster s.e."],
    pval = coef(summary(price_res4))["ln_sales_tax", "Pr(>|t|)"],
    Rsq = summary(price_res4)$r.squared,
    adj.Rsq = summary(price_res4)$adj.r.squared,
    time_controls = "module-time FE + region-time FE"
  )
)


## with Division_by_time + Module_by_time FE - Second measure of prices
## Second price index
price_formula8 <- as.formula(paste0(
  "ln_cpricei2 ~ ln_sales_tax | store_module + module_by_time + division_by_time | 0 | state_by_module "
))

price_res4 <- felm(data = yearly_data,
                   formula = price_formula8,
                   weights = yearly_data$base.sales)

LRdiff_res <-rbind(
  LRdiff_res,
  data.table(
    outcome = "ln_cpricei2",
    estimate = coef(summary(price_res4))["ln_sales_tax", "Estimate"],
    se = coef(summary(price_res4))["ln_sales_tax", "Cluster s.e."],
    pval = coef(summary(price_res4))["ln_sales_tax", "Pr(>|t|)"],
    Rsq = summary(price_res4)$r.squared,
    adj.Rsq = summary(price_res4)$adj.r.squared,
    time_controls = "module-time FE + division-time FE"
  )
)



## with Module_by_Region_by_time FE - Second measure of prices
## Second price index
price_formula8 <- as.formula(paste0(
  "ln_cpricei2 ~ ln_sales_tax | store_module + module_by_time + reg_by_module_by_time | 0 | state_by_module "
))

price_res4 <- felm(data = yearly_data,
                   formula = price_formula8,
                   weights = yearly_data$base.sales)

LRdiff_res <-rbind(
  LRdiff_res,
  data.table(
    outcome = "ln_cpricei2",
    estimate = coef(summary(price_res4))["ln_sales_tax", "Estimate"],
    se = coef(summary(price_res4))["ln_sales_tax", "Cluster s.e."],
    pval = coef(summary(price_res4))["ln_sales_tax", "Pr(>|t|)"],
    Rsq = summary(price_res4)$r.squared,
    adj.Rsq = summary(price_res4)$adj.r.squared,
    time_controls = "module-region-time FE"
  )
)


## with Module_by_Division_by_time FE - Second measure of prices
## Second price index
price_formula8 <- as.formula(paste0(
  "ln_cpricei2 ~ ln_sales_tax | store_module + module_by_time + div_by_module_by_time | 0 | state_by_module "
))

price_res4 <- felm(data = yearly_data,
                   formula = price_formula8,
                   weights = yearly_data$base.sales)

LRdiff_res <-rbind(
  LRdiff_res,
  data.table(
    outcome = "ln_cpricei2",
    estimate = coef(summary(price_res4))["ln_sales_tax", "Estimate"],
    se = coef(summary(price_res4))["ln_sales_tax", "Cluster s.e."],
    pval = coef(summary(price_res4))["ln_sales_tax", "Pr(>|t|)"],
    Rsq = summary(price_res4)$r.squared,
    adj.Rsq = summary(price_res4)$adj.r.squared,
    time_controls = "module-division-time FE"
  )
)



#### Analysis on quantity (robustness)
## with Region_by_time + Module_by_time FE - First measure of quantity
price_formula8 <- as.formula(paste0(
  "ln_quantity ~ ln_sales_tax | store_module + module_by_time + region_by_time | 0 | state_by_module "
))

price_res4 <- felm(data = yearly_data,
                   formula = price_formula8,
                   weights = yearly_data$base.sales)

LRdiff_res <-rbind(
  LRdiff_res,
  data.table(
    outcome = "ln_quantity",
    estimate = coef(summary(price_res4))["ln_sales_tax", "Estimate"],
    se = coef(summary(price_res4))["ln_sales_tax", "Cluster s.e."],
    pval = coef(summary(price_res4))["ln_sales_tax", "Pr(>|t|)"],
    Rsq = summary(price_res4)$r.squared,
    adj.Rsq = summary(price_res4)$adj.r.squared,
    time_controls = "module-time FE + region-time FE"
  )
)


## with Division_by_time + Module_by_time FE - First measure of quantity
price_formula8 <- as.formula(paste0(
  "ln_quantity ~ ln_sales_tax | store_module + module_by_time + division_by_time | 0 | state_by_module "
))

price_res4 <- felm(data = yearly_data,
                   formula = price_formula8,
                   weights = yearly_data$base.sales)

LRdiff_res <-rbind(
  LRdiff_res,
  data.table(
    outcome = "ln_quantity",
    estimate = coef(summary(price_res4))["ln_sales_tax", "Estimate"],
    se = coef(summary(price_res4))["ln_sales_tax", "Cluster s.e."],
    pval = coef(summary(price_res4))["ln_sales_tax", "Pr(>|t|)"],
    Rsq = summary(price_res4)$r.squared,
    adj.Rsq = summary(price_res4)$adj.r.squared,
    time_controls = "module-time FE + division-time FE"
  )
)



## with Module_by_Region_by_time FE - First measure of quantity
price_formula8 <- as.formula(paste0(
  "ln_quantity ~ ln_sales_tax | store_module + module_by_time + reg_by_module_by_time | 0 | state_by_module "
))

price_res4 <- felm(data = yearly_data,
                   formula = price_formula8,
                   weights = yearly_data$base.sales)

LRdiff_res <-rbind(
  LRdiff_res,
  data.table(
    outcome = "ln_quantity",
    estimate = coef(summary(price_res4))["ln_sales_tax", "Estimate"],
    se = coef(summary(price_res4))["ln_sales_tax", "Cluster s.e."],
    pval = coef(summary(price_res4))["ln_sales_tax", "Pr(>|t|)"],
    Rsq = summary(price_res4)$r.squared,
    adj.Rsq = summary(price_res4)$adj.r.squared,
    time_controls = "module-region-time FE"
  )
)


## with Module_by_Division_by_time FE - First measure of quantity
price_formula8 <- as.formula(paste0(
  "ln_quantity ~ ln_sales_tax | store_module + module_by_time + div_by_module_by_time | 0 | state_by_module "
))

price_res4 <- felm(data = yearly_data,
                   formula = price_formula8,
                   weights = yearly_data$base.sales)

LRdiff_res <-rbind(
  LRdiff_res,
  data.table(
    outcome = "ln_quantity",
    estimate = coef(summary(price_res4))["ln_sales_tax", "Estimate"],
    se = coef(summary(price_res4))["ln_sales_tax", "Cluster s.e."],
    pval = coef(summary(price_res4))["ln_sales_tax", "Pr(>|t|)"],
    Rsq = summary(price_res4)$r.squared,
    adj.Rsq = summary(price_res4)$adj.r.squared,
    time_controls = "module-division-time FE"
  )
)


## with Region_by_time + Module_by_time FE - Second measure of quantity
price_formula8 <- as.formula(paste0(
  "ln_quantity2 ~ ln_sales_tax | store_module + module_by_time + region_by_time | 0 | state_by_module "
))

price_res4 <- felm(data = yearly_data,
                   formula = price_formula8,
                   weights = yearly_data$base.sales)

LRdiff_res <-rbind(
  LRdiff_res,
  data.table(
    outcome = "ln_quantity2",
    estimate = coef(summary(price_res4))["ln_sales_tax", "Estimate"],
    se = coef(summary(price_res4))["ln_sales_tax", "Cluster s.e."],
    pval = coef(summary(price_res4))["ln_sales_tax", "Pr(>|t|)"],
    Rsq = summary(price_res4)$r.squared,
    adj.Rsq = summary(price_res4)$adj.r.squared,
    time_controls = "module-time FE + region-time FE"
  )
)


## with Division_by_time + Module_by_time FE - Second measure of quantity
price_formula8 <- as.formula(paste0(
  "ln_quantity2 ~ ln_sales_tax | store_module + module_by_time + division_by_time | 0 | state_by_module "
))

price_res4 <- felm(data = yearly_data,
                   formula = price_formula8,
                   weights = yearly_data$base.sales)

LRdiff_res <-rbind(
  LRdiff_res,
  data.table(
    outcome = "ln_quantity2",
    estimate = coef(summary(price_res4))["ln_sales_tax", "Estimate"],
    se = coef(summary(price_res4))["ln_sales_tax", "Cluster s.e."],
    pval = coef(summary(price_res4))["ln_sales_tax", "Pr(>|t|)"],
    Rsq = summary(price_res4)$r.squared,
    adj.Rsq = summary(price_res4)$adj.r.squared,
    time_controls = "module-time FE + division-time FE"
  )
)



## with Module_by_Region_by_time FE - Second measure of quantity
price_formula8 <- as.formula(paste0(
  "ln_quantity2 ~ ln_sales_tax | store_module + module_by_time + reg_by_module_by_time | 0 | state_by_module "
))

price_res4 <- felm(data = yearly_data,
                   formula = price_formula8,
                   weights = yearly_data$base.sales)

LRdiff_res <-rbind(
  LRdiff_res,
  data.table(
    outcome = "ln_quantity2",
    estimate = coef(summary(price_res4))["ln_sales_tax", "Estimate"],
    se = coef(summary(price_res4))["ln_sales_tax", "Cluster s.e."],
    pval = coef(summary(price_res4))["ln_sales_tax", "Pr(>|t|)"],
    Rsq = summary(price_res4)$r.squared,
    adj.Rsq = summary(price_res4)$adj.r.squared,
    time_controls = "module-region-time FE"
  )
)


## with Module_by_Division_by_time FE - Second measure of quantity
price_formula8 <- as.formula(paste0(
  "ln_quantity2 ~ ln_sales_tax | store_module + module_by_time + div_by_module_by_time | 0 | state_by_module "
))

price_res4 <- felm(data = yearly_data,
                   formula = price_formula8,
                   weights = yearly_data$base.sales)

LRdiff_res <-rbind(
  LRdiff_res,
  data.table(
    outcome = "ln_quantity2",
    estimate = coef(summary(price_res4))["ln_sales_tax", "Estimate"],
    se = coef(summary(price_res4))["ln_sales_tax", "Cluster s.e."],
    pval = coef(summary(price_res4))["ln_sales_tax", "Pr(>|t|)"],
    Rsq = summary(price_res4)$r.squared,
    adj.Rsq = summary(price_res4)$adj.r.squared,
    time_controls = "module-division-time FE"
  )
)



## summary values --------------------------------------------------------------
LRdiff_res$N_obs <- nrow(yearly_data)
LRdiff_res$N_modules <- length(unique(yearly_data$product_module_code))
LRdiff_res$N_stores <- length(unique(yearly_data$store_code_uc))
LRdiff_res$N_counties <- uniqueN(yearly_data, by = c("fips_state", "fips_county"))
LRdiff_res$N_years <- uniqueN(yearly_data, by = c("year")) # should be 7
LRdiff_res$N_county_modules <- uniqueN(yearly_data, by = c("fips_state", "fips_county",
                                                    "product_module_code"))

fwrite(LRdiff_res, "Data/LRdiff_results_1lag.csv")


