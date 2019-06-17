### In this R-file we explore a specification meant to estimate long-run pass-through and sales/quantity response to tax changes

library(data.table)
library(lfe)
library(futile.logger)
library(AER)


setwd("/project2/igaarder")

## useful filepaths ------------------------------------------------------------
all_goods_pi_path <- "Data/Nielsen/price_quantity_indices_allitems_2006-2016_notaxinfo.csv"
FE_pindex_path <- "Data/Nielsen/Pindex_FE_yearly_all_years.csv"
output_yearly <- "Data/Nielsen/yearly_nielsen_data.csv"
#taxable_pi_path <- "Data/Nielsen/price_quantity_indices_taxableitems_2006-2016.csv"
#eventstudy_tr_path <- "Data/event_study_tr_groups_comprehensive_firstonly_no2012q4_2013q1q2.csv"
#tr_groups_path <- "Data/tr_groups_comprehensive_firstonly_no2012q4_2013q1q2.csv"


# ### Prepare the data
# all_pi <- fread(all_goods_pi_path)
# all_pi <- all_pi[year %in% 2006:2016 & !is.na(cpricei)]  ## Apparently, we are currently missing 2015-2016 - need to correct this
# #all_pi <- all_pi[year %in% 2006:2016 & !is.na(cpricei) & !is.na(sales_tax)]


# ## balance on store-module level (only keep observations that are in every quarter)
# keep_store_modules <- all_pi[, list(n = .N),
#                              by = .(store_code_uc, product_module_code)]
# keep_store_modules <- keep_store_modules[n == (2014 - 2005) * 4]

# setkey(all_pi, store_code_uc, product_module_code)
# setkey(keep_store_modules, store_code_uc, product_module_code)

# all_pi <- all_pi[keep_store_modules]
# setkey(all_pi, fips_county, fips_state)


# ## Generate yearly variables
# #Note: all variables are quarterly averages
# yearly_data <- all_pi[, list(pricei = mean(pricei), quantityi = mean(quantityi), cpricei = mean(cpricei), sales = mean(sales), sales_tax = weighted.mean(sales_tax, w = sales)), by = .(store_code_uc, product_module_code, product_group_code, fips_state, fips_county, year)]
# rm(all_pi)


# ## Impute sales_tax for 2006-2007 and 2015-2016 (+ define cpricei for these years) ##NOTE: for now 2015-2016 are missing - need to be added
# yearly_data[, base.tax := sales_tax[year == 2008],
#             by = .(store_code_uc, product_module_code)]
# yearly_data[year <= 2007,]$sales_tax <- yearly_data[year <= 2007,]$base.tax


# ## take logs
# yearly_data[, ln_pricei := log(pricei)]
# yearly_data[, ln_cpricei := log(cpricei)]
# yearly_data[, ln_sales_tax := log(sales_tax)]
# yearly_data[, ln_quantity := log(sales) - log(pricei)]
# yearly_data[, ln_sales := log(sales)]


# ##Import the "FE" price indices
# FE_pi <- fread(FE_pindex_path)
# FE_pi <- FE_pi[,c("store_code_uc", "year", "FE_store", "constant", "product_module_code")]

# ## Merge with "FE" price index
# yearly_data <- merge(yearly_data, FE_pi, by = c("store_code_uc", "year", "product_module_code"))
# rm(FE_pi)


# ##Re-balance the sample
# keep_store_modules <- yearly_data[, list(n = .N),
#                              by = .(store_code_uc, product_module_code)]
# keep_store_modules <- keep_store_modules[n == (2014 - 2005)]

# setkey(yearly_data, store_code_uc, product_module_code)
# setkey(keep_store_modules, store_code_uc, product_module_code)

# yearly_data <- yearly_data[keep_store_modules]


# ## get sales weights
# yearly_data[, base.sales := sales[year == 2008],
#         by = .(store_code_uc, product_module_code)]

# yearly_data <- yearly_data[!is.na(base.sales) & !is.na(sales) & !is.na(ln_cpricei) &
#                    !is.na(ln_sales_tax) & !is.na(ln_quantity) & !is.na(FE_store)]

# ## Create alternative measures of price and quantity
# yearly_data[, ln_cpricei2 := FE_store + ln_sales_tax]
# yearly_data[, ln_quantity2 := log(sales) - FE_store]


# ## calculate expenditure shares
# yearly_data[, total_sales := sum(sales), by = .(store_code_uc, year)]
# yearly_data[, expend_share := sales / total_sales]
# yearly_data[, ln_expend_share := log(expend_share)]


# ## prep some variables for the regression (FE, cluster variables)
# yearly_data[, yr := .GRP, by = .(year)]
# yearly_data[, store_module := .GRP, by = .(store_code_uc, product_module_code)]
# yearly_data[, state_by_module := .GRP, by = .(fips_state, product_module_code)]
# yearly_data[, linear_time := year - 2008]
# yearly_data[, module_by_time := .GRP, by = .(year, product_module_code)]
# yearly_data[, store_by_time := .GRP, by = .(year, store_code_uc)]
# yearly_data[, county_by_module := .GRP, by = .(fips_state, fips_county, product_module_code)]

# ###
# fwrite(yearly_data, output_yearly)




######### Regression analysis ##NOTE: Make sure that we only include 2008-2014 in the regressions!!
## run the analysis on price ---------------------------------------------------
yearly_data <- fread(output_yearly)
yearly_data <- yearly_data[year >= 2008 & year <= 2014,]

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



## summary values --------------------------------------------------------------
LRdiff_res$N_obs <- nrow(yearly_data)
LRdiff_res$N_modules <- length(unique(yearly_data$product_module_code))
LRdiff_res$N_stores <- length(unique(yearly_data$store_code_uc))
LRdiff_res$N_counties <- uniqueN(yearly_data, by = c("fips_state", "fips_county"))
LRdiff_res$N_years <- uniqueN(yearly_data, by = c("year")) # should be 7
LRdiff_res$N_county_modules <- uniqueN(yearly_data, by = c("fips_state", "fips_county",
                                                    "product_module_code"))

fwrite(LRdiff_res, "Data/LRdiff_results.csv")


