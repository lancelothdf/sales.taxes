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
# all_pi <- all_pi[year %in% 2008:2014 & !is.na(cpricei) & !is.na(sales_tax)]


# ## balance on store-module level (only keep observations that are in every quarter)
# keep_store_modules <- all_pi[, list(n = .N),
#                              by = .(store_code_uc, product_module_code)]
# keep_store_modules <- keep_store_modules[n == (2014 - 2007) * 4]

# setkey(all_pi, store_code_uc, product_module_code)
# setkey(keep_store_modules, store_code_uc, product_module_code)

# all_pi <- all_pi[keep_store_modules]
# setkey(all_pi, fips_county, fips_state)


# ## Generate yearly variables
# #Note: all variables are quarterly averages
# yearly_data <- all_pi[, list(pricei = mean(pricei), quantityi = mean(quantityi), cpricei = mean(cpricei), sales = mean(sales), sales_tax = weighted.mean(sales_tax, w = sales)), by = .(store_code_uc, product_module_code, product_group_code, fips_state, fips_county, year)]
# rm(all_pi)

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
# keep_store_modules <- keep_store_modules[n == (2014 - 2007)]

# setkey(yearly_data, store_code_uc, product_module_code)
# setkey(keep_store_modules, store_code_uc, product_module_code)

# yearly_data <- yearly_data[keep_store_modules]


# ## get sales weights
# yearly_data[, base.sales := sales[year == 2008],
#         by = .(store_code_uc, product_module_code)]

# yearly_data <- yearly_data[!is.na(base.sales) & !is.na(sales) & !is.na(ln_cpricei) &
#                    !is.na(ln_sales_tax) & !is.na(ln_quantity) & !is.na(FE_store)]



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
yearly_data <- fread(output_yearly)


##### create the module specific time trends
varnames_old <- names(yearly_data)
for (P in unique(yearly_data$product_module_code)) {
  flog.info("Creating trend for product %s", P)
  P_vname <- paste0("ttrend_", P)

  yearly_data[, (P_vname) := as.integer(product_module_code == P) * linear_time]
}
ttrend_vars <- setdiff(names(yearly_data), varnames_old)
ttrend_formula <- paste0(ttrend_vars, collapse = "+")

######### Regression analysis
## run the analysis on price ---------------------------------------------------

yearly_data$product_module_code <- as.factor(yearly_data$product_module_code)
#price_formula <- as.formula(paste0(
#  "ln_cpricei ~ ln_sales_tax +", ttrend_formula,
#  "| yr + store_module | 0 | state_by_module "
#))
price_formula <- as.formula(paste0(
  "ln_cpricei ~ ln_sales_tax +", "product_module_code:linear_time",
  "| yr + store_module | 0 | state_by_module "
))


price_res <- felm(data = yearly_data,
                  formula = price_formula,
                  weights = yearly_data$base.sales,
                  keepX = F, keepCX = F, keepModel = F)

LRdiff_res <- data.table(
  outcome = "ln_cpricei",
  estimate = coef(summary(price_res))["ln_sales_tax", "Estimate"],
  se = coef(summary(price_res))["ln_sales_tax", "Cluster s.e."],
  pval = coef(summary(price_res))["ln_sales_tax", "Pr(>|t|)"],
  Rsq = summary(price_res)$r.squared,
  adj.Rsq = summary(price_res)$adj.r.squared,
  time_controls = "module specific time trend"
)

## replacing module-specific time trend with module-time FE
price_formula2 <- as.formula(paste0(
  "ln_cpricei ~ ln_sales_tax | store_module + module_by_time | 0 | state_by_module "
))

price2_res <- felm(data = yearly_data,
                   formula = price_formula2,
                   weights = yearly_data$base.sales)

LRdiff_res <- rbind(
  LRdiff_res,
  data.table(
    outcome = "ln_cpricei",
    estimate = coef(summary(price2_res))["ln_sales_tax", "Estimate"],
    se = coef(summary(price2_res))["ln_sales_tax", "Cluster s.e."],
    pval = coef(summary(price2_res))["ln_sales_tax", "Pr(>|t|)"],
    Rsq = summary(price2_res)$r.squared,
    adj.Rsq = summary(price2_res)$adj.r.squared,
    time_controls = "module-time FE"
  )
)

## run the analysis on quantity ------------------------------------------------
quantity_formula <- as.formula(paste0(
  "ln_quantity ~ ln_sales_tax +", ttrend_formula,
  "| yr + store_module | 0 | state_by_module "
))

quantity_res <- felm(data = yearly_data,
                     formula = quantity_formula,
                     weights = yearly_data$base.sales)

LRdiff_res <- data.table(
  outcome = "ln_quantity",
  estimate = coef(summary(quantity_res))["ln_sales_tax", "Estimate"],
  se = coef(summary(quantity_res))["ln_sales_tax", "Cluster s.e."],
  pval = coef(summary(quantity_res))["ln_sales_tax", "Pr(>|t|)"],
  Rsq = summary(quantity_res)$r.squared,
  adj.Rsq = summary(quantity_res)$adj.r.squared,
  time_controls = "module specific time trend"
)

## replacing module-specific time trend with module-time FE
quantity_formula2 <- as.formula(paste0(
  "ln_quantity ~ ln_sales_tax | store_module + module_by_time | 0 | state_by_module "
))

quantity2_res <- felm(data = yearly_data,
                      formula = quantity_formula,
                      weights = yearly_data$base.sales)

LRdiff_res <- rbind(
  LRdiff_res,
  data.table(
    outcome = "ln_quantity",
    estimate = coef(summary(quantity2_res))["ln_sales_tax", "Estimate"],
    se = coef(summary(quantity2_res))["ln_sales_tax", "Cluster s.e."],
    pval = coef(summary(quantity2_res))["ln_sales_tax", "Pr(>|t|)"],
    Rsq = summary(quantity2_res)$r.squared,
    adj.Rsq = summary(quantity2_res)$adj.r.squared,
    time_controls = "module-time FE"
  )
)

## run the analysis on expenditure share ---------------------------------------
expenditure_formula <- as.formula(paste0(
  "ln_expend_share ~ ln_sales_tax +", ttrend_formula,
  "| yr + store_module | 0 | state_by_module "
))

expenditure_res <- felm(data = yearly_data,
                        formula = expenditure_formula,
                        weights = yearly_data$base.sales)

LRdiff_res <- data.table(
  outcome = "ln_expend_share",
  estimate = coef(summary(expenditure_res))["ln_sales_tax", "Estimate"],
  se = coef(summary(expenditure_res))["ln_sales_tax", "Cluster s.e."],
  pval = coef(summary(expenditure_res))["ln_sales_tax", "Pr(>|t|)"],
  Rsq = summary(expenditure_res)$r.squared,
  adj.Rsq = summary(expenditure_res)$adj.r.squared,
  time_controls = "module specific time trend"
)

## replacing module-specific time trend with module-time FE
expenditure_formula2 <- as.formula(paste0(
  "ln_expend_share ~ ln_sales_tax | store_module + module_by_time | 0 | state_by_module "
))

expenditure2_res <- felm(data = yearly_data,
                         formula = expenditure_formula,
                         weights = yearly_data$base.sales)

LRdiff_res <- rbind(
  LRdiff_res,
  data.table(
    outcome = "ln_expend_share",
    estimate = coef(summary(expenditure2_res))["ln_sales_tax", "Estimate"],
    se = coef(summary(expenditure2_res))["ln_sales_tax", "Cluster s.e."],
    pval = coef(summary(expenditure2_res))["ln_sales_tax", "Pr(>|t|)"],
    Rsq = summary(expenditure2_res)$r.squared,
    adj.Rsq = summary(expenditure2_res)$adj.r.squared,
    time_controls = "module-time FE"
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

