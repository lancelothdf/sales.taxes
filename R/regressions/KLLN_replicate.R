#' Replicate analysis in Table 4 of Kroft, Laliberte, Leal-Vizcaino, and
#'   Notowidigdo (2017). Specifically, replicate the analysis in Panel A,
#'   but instead of estimating with a "variety" outcome, estimate on
#'   quantity (log(sales) - log(price)).

library(data.table)
library(lfe)
library(futile.logger)

setwd("/project2/igaarder")

## useful filepaths ------------------------------------------------------------
all_goods_pi_path <- "Data/Nielsen/price_quantity_indices_allitems_2006-2016_notaxinfo.csv"

## prep the data ---------------------------------------------------------------
all_pi <- fread(all_goods_pi_path)
all_pi <- all_pi[year %in% 2008:2014 & !is.na(cpricei) & !is.na(sales_tax)]

## take logs
all_pi[, ln_cpricei := log(cpricei)]
all_pi[, ln_sales_tax := log(sales_tax)]
all_pi[, ln_quantity := log(sales) - log(pricei)]

## get sales weights
all_pi[, base.sales := sales[year == 2008 & quarter == 1],
       by = .(store_code_uc, product_module_code)]

all_pi <- all_pi[!is.na(base.sales) & !is.na(sales) & !is.na(ln_cpricei) &
                   !is.na(ln_sales_tax) & !is.na(ln_quantity)]
all_pi <- all_pi[, .(
  store_code_uc, product_module_code, fips_state, fips_county, year, quarter,
  sales, ln_cpricei, ln_sales_tax, ln_quantity, base.sales
)]

## balance on store-module level
keep_store_modules <- all_pi[, list(n = .N),
                             by = .(store_code_uc, product_module_code)]
keep_store_modules <- keep_store_modules[n == (2014 - 2007) * 4]

setkey(all_pi, store_code_uc, product_module_code)
setkey(keep_store_modules, store_code_uc, product_module_code)

all_pi <- all_pi[keep_store_modules]
setkey(all_pi, fips_county, fips_state)

## calculate expenditure shares
all_pi[, total_sales := sum(sales), by = .(store_code_uc, quarter, year)]
all_pi[, expend_share := sales / total_sales]
all_pi[, ln_expend_share := log(expend_share)]

## prep some variables for the regression (FE, cluster variables)
all_pi[, yr_quarter := .GRP, by = .(year, quarter)]
all_pi[, store_module := .GRP, by = .(store_code_uc, product_module_code)]
all_pi[, state_by_module := .GRP, by = .(fips_state, product_module_code)]
all_pi[, linear_time := year * 4 + quarter]
all_pi[, module_by_time := .GRP, by = .(year, quarter, product_module_code)]

## run the analysis on price ---------------------------------------------------
print(head(all_pi))
test_reg <- felm(data = all_pi, formula = ln_cpricei ~ ln_sales_tax)
print(summary(test_reg))
test_reg_w <- felm(data = all_pi, formula = ln_cpricei ~ ln_sales_tax,
                   weights = all_pi$base.sales)
print(summary(test_reg_w))

price_formula <- as.formula(paste0(
  "ln_cpricei ~ ln_sales_tax + factor(product_module_code) * linear_time ",
  "| yr_quarter + store_module | 0 | state_by_module "
))

price_res <- felm(data = all_pi,
                  formula = price_formula,
                  weights = all_pi$base.sales)

KLLN_res <- data.table(
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

price2_res <- felm(data = all_pi,
                  formula = price_formula,
                  weights = all_pi$base.sales)

KLLN_res <- rbind(
  KLLN_res,
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
  "ln_quantity ~ ln_sales_tax + factor(product_module_code) * linear_time ",
  "| yr_quarter + store_module | 0 | state_by_module "
))

quantity_res <- felm(data = all_pi,
                  formula = quantity_formula,
                  weights = all_pi$base.sales)

KLLN_res <- data.table(
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

quantity2_res <- felm(data = all_pi,
                   formula = quantity_formula,
                   weights = all_pi$base.sales)

KLLN_res <- rbind(
  KLLN_res,
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
  "ln_expend_share ~ ln_sales_tax + factor(product_module_code) * linear_time ",
  "| yr_quarter + store_module | 0 | state_by_module "
))

expenditure_res <- felm(data = all_pi,
                     formula = expenditure_formula,
                     weights = all_pi$base.sales)

KLLN_res <- data.table(
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

expenditure2_res <- felm(data = all_pi,
                      formula = expenditure_formula,
                      weights = all_pi$base.sales)

KLLN_res <- rbind(
  KLLN_res,
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
KLLN_res$N_obs <- nrow(all_pi)
KLLN_res$N_modules <- length(unique(all_pi$product_module_code))
KLLN_res$N_stores <- length(unique(all_pi$store_code_uc))
KLLN_res$N_counties <- uniqueN(all_pi, by = c("fips_state", "fips_county"))
KLLN_res$N_quarters <- uniqueN(all_pi, by = c("quarter", "year")) # should be 28
KLLN_res$N_county_modules <- uniqueN(all_pi, by = c("fips_state", "fips_county",
                                                    "product_module_code"))

fwrite(KLLN_res, "Data/KLLN_replication.csv")
