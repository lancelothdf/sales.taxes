#' Author: John Bonney
#'
#' This script runs "event-study" regressions that are backed out of the
#' corresponding dynamic lag model. These are regressions of logs of
#' price, quantity on (binned) log *changes* in sales tax rates.
#'
#' It uses quarterly data, with two years of leads and two years of lags.
#' In some specifications, the tax rate pre-2008/post-2014 is imputed (changes
#' are assumed to be 0). In others, those years are dropped from the analysis.
#'
#' Specifications are run both without controlling for covariates, controlling
#' for covariates linearly, and controlling for covariates using the method
#' proposed by Freyaldenhoven, Hansen, and Shapiro (2019).

library(data.table)
library(futile.logger)
library(lfe)
library(multcomp)

setwd("/project2/igaarder")
prep_dt <- T

### Useful filepaths ----------------------------------------------
# quarterly Laspeyres indices, sales, and sales tax rates from 2006-2014
all_goods_pi_path <- "Data/Nielsen/price_quantity_indices_allitems_2006-2016_notaxinfo.csv"
# same as all_goods_pi_path, except it has 2015-2016 data as well
data.full.path <- "Data/all_nielsen_data_2006_2016_quarterly.csv"

## covariate filepaths
zillow_path <- "Data/covariates/zillow_long_by_county_clean.csv"
zillow_state_path <- "Data/covariates/zillow_long_by_state_clean.csv"
unemp_path <- "Data/covariates/county_monthly_unemp_clean.csv"

## output filepath --
temp.outfile <- "Data/price_indices_wX_temp.csv"
reg.outfile <- "Data/quarterly_pi_output_FHS.csv"

if (prep_dt) {

## prep Census region/division data ------------------------------
geo_dt <- structure(list(
  fips_state = c(1L, 2L, 4L, 5L, 6L, 8L, 9L, 10L, 12L, 13L, 15L, 16L, 17L, 18L,
                 19L, 20L, 21L, 22L, 23L, 24L, 25L, 26L, 27L, 28L, 29L, 30L,
                 31L, 32L, 33L, 34L, 35L, 36L, 37L, 38L, 39L, 40L, 41L, 42L,
                 44L, 45L, 46L, 47L, 48L, 49L, 50L, 51L, 53L, 54L, 55L, 56L),
  region = c(3L, 4L, 4L, 3L, 4L, 4L, 1L, 3L, 3L, 3L, 4L, 4L, 2L, 2L, 2L, 2L, 3L,
             3L, 1L, 3L, 1L, 2L, 2L, 3L, 2L, 4L, 2L, 4L, 1L, 1L, 4L, 1L, 3L, 2L,
             2L, 3L, 4L, 1L, 1L, 3L, 2L, 3L, 3L, 4L, 1L, 3L, 4L, 3L, 2L, 4L),
  division = c(6L, 9L, 8L,  7L, 9L, 8L, 1L, 5L, 5L, 5L, 9L, 8L, 3L, 3L, 4L, 4L,
               6L, 7L, 1L, 5L, 1L, 3L, 4L, 6L, 4L, 8L, 4L, 8L, 1L, 2L, 8L, 2L,
               5L, 4L, 3L,  7L, 9L, 2L, 1L, 5L, 4L, 6L, 7L, 8L, 1L, 5L, 9L, 5L, 3L, 8L)),
  class = "data.frame", row.names = c(NA, -50L))
setDT(geo_dt)

## prep unemployment data ------------------------------
unemp_dt <- fread(unemp_path)
unemp_dt <- unemp_dt[between(year, 2008, 2014)]
unemp_dt[, quarter := ceiling(month / 3)]
unemp_dt <- unemp_dt[, list(unemp_rate = mean(rate)),
                     by = .(year, quarter, fips_state, fips_county)]
# balance unemp data to ensure balanced FHS analysis
flog.info("%s rows of unemp_dt are NA", nrow(unemp_dt[is.na(unemp_rate)]))
unemp_dt <- unemp_dt[!is.na(unemp_rate)]
unemp_dt[, county.count := .N, by = .(fips_state, fips_county)]
unemp_dt <- unemp_dt[county.count == (2014 - 2007) * 4]
unemp_dt[, county.count := NULL]

## prep house price data -------------------------------
all_pi <- fread(data.full.path)
# build a frame to make sure we can assign every county a home price
all_counties <- unique(all_pi[, .(fips_state, fips_county)])
all_counties <- all_counties[!is.na(fips_state) & !is.na(fips_county)]
county_skeleton <- data.table(NULL)
for (X in 2008:2014) {
  for (Y in 1:12) {
    all_counties[, year := X]
    all_counties[, month := Y]
    county_skeleton <- rbind(county_skeleton, all_counties)
  }
}
fwrite(county_skeleton, "Data/county_skeleton_test.csv")

zillow_dt <- fread(zillow_path)
zillow_dt <- zillow_dt[between(year, 2008, 2014)]
zillow_dt <- zillow_dt[, .(fips_state, fips_county, median_home_price, year, month)]
zillow_dt <- merge(county_skeleton, zillow_dt, all.x = T,
                   by = c("fips_state", "fips_county", "year", "month"))

## prep state-level house prices (for when county-level is missing)
zillow_state_dt <- fread(zillow_state_path)
zillow_state_dt <- zillow_state_dt[between(year, 2008, 2014)]
zillow_state_dt <- zillow_state_dt[, .(fips_state, median_home_price, year, month)]
setnames(zillow_state_dt, "median_home_price", "state_median_home_price")
zillow_state_dt$month <- as.integer(round(zillow_state_dt$month))

zillow_dt <- merge(zillow_dt, zillow_state_dt, all.x = T,
                   by = c("fips_state", "year", "month"))
zillow_dt[is.na(median_home_price), median_home_price := state_median_home_price]
zillow_dt[, state_median_home_price := NULL]

## collapse to quarters
zillow_dt[, quarter := ceiling(month / 3)]
zillow_dt <- zillow_dt[, list(ln_home_price = mean(log(median_home_price))),
                       by = .(year, quarter, fips_state, fips_county)]

# balance zillow data to ensure balanced FHS analysis
flog.info("%s rows of zillow_dt are NA", nrow(zillow_dt[is.na(ln_home_price)]))
zillow_dt <- zillow_dt[!is.na(ln_home_price)]
zillow_dt[, county.count := .N, by = .(fips_state, fips_county)]
zillow_dt <- zillow_dt[county.count == (2014 - 2007) * 4]
zillow_dt[, county.count := NULL]

## prep the 2006-2016 data ---------------------------------------
## merge on the census region/division info
all_pi <- merge(all_pi, geo_dt, by = "fips_state")

## merge on unemployment (m:1 merge)
all_pi <- merge(all_pi, unemp_dt,
                by = c("fips_county", "fips_state", "quarter", "year"),
                all.x = T)
## merge on house prices (m:1 merge)
all_pi <- merge(all_pi, zillow_dt,
                by = c("fips_county", "fips_state", "quarter", "year"),
                all.x = T)

# impute tax rates prior to 2008 and after 2014
all_pi[, sales_tax := ifelse(year < 2008, sales_tax[year == 2008 & quarter == 1], sales_tax),
       by = .(store_code_uc, product_module_code)]
all_pi[, sales_tax := ifelse(year > 2014, sales_tax[year == 2014 & quarter == 4], sales_tax),
       by = .(store_code_uc, product_module_code)]

# create necessary variables
all_pi[, ln_cpricei := log(cpricei)]
all_pi[, ln_sales_tax := log(sales_tax)]
all_pi[, ln_quantity := log(sales) - log(pricei)]
all_pi[, store_by_module := .GRP, by = .(store_code_uc, product_module_code)]
all_pi[, cal_time := 4 * year + quarter]
all_pi[, module_by_time := .GRP, by = .(product_module_code, cal_time)]
all_pi[, module_by_state := .GRP, by = .(product_module_code, fips_state)]
all_pi[, region_by_module_by_time := .GRP, by = .(region, product_module_code, cal_time)]
all_pi[, division_by_module_by_time := .GRP, by = .(division, product_module_code, cal_time)]

## get sales weights
all_pi[, base.sales := sales[year == 2008 & quarter == 1], by = store_by_module]

all_pi <- all_pi[!is.na(base.sales) & !is.na(sales) & !is.na(ln_cpricei) &
                   !is.na(ln_sales_tax) & !is.na(ln_quantity)]

## balance on store-module level from 2006-2016
keep_store_modules <- all_pi[, list(n = .N), by = store_by_module]
keep_store_modules <- keep_store_modules[n == (2016 - 2005) * 4]

setkey(all_pi, store_by_module)
setkey(keep_store_modules, store_by_module)

all_pi <- all_pi[keep_store_modules]
setkey(all_pi, store_by_module, year, quarter)

## take contemporaneous first difference of ln_sales_tax variables
all_pi[, D.ln_sales_tax := ln_sales_tax - shift(ln_sales_tax, n=1, type="lag"),
       by = store_by_module]

## generate lags and leads of ln_sales_tax (imputed and not imputed)
for (lag.val in 1:7) {

  lead.X <- paste0("F", lag.val, ".D.ln_sales_tax")
  all_pi[, (lead.X) := shift(D.ln_sales_tax, n=lag.val, type="lead"),
         by = store_by_module]

  if (lag.val == 7) break # we will bin endpoints for the 7th lag (and 8th lead)

  lag.X <- paste0("L", lag.val, ".D.ln_sales_tax")
  all_pi[, (lag.X) := shift(D.ln_sales_tax, n=lag.val, type="lag"),
         by = store_by_module]

}

## bin the endpoints
all_pi$F8.D.ln_sales_tax <- as.double(NA)
all_pi$L7.D.ln_sales_tax <- as.double(NA)
for (yr in 2008:2014) {
  for (qtr in 1:4) {
    ct <- yr * 4 + qtr

    ## sum over all leads of treatment 8+ periods in the future
    all_pi[, F8.D.ln_sales_tax := ifelse(
          ct == cal_time,
          sum(D.ln_sales_tax[cal_time >= ct + 8], na.rm = T),
          F8.D.ln_sales_tax
          ), by = store_by_module]

    ## sum over all lags of treatment 7+ periods in the past
    all_pi[, L7.D.ln_sales_tax := ifelse(
          ct == cal_time,
          sum(D.ln_sales_tax[cal_time <= ct - 7], na.rm = T),
          L7.D.ln_sales_tax
          ), by = store_by_module]
  }
}

## identify different samples for estimation
all_pi[, sample.not.imputed := as.integer(between(year, 2010, 2012))]
all_pi[, sample.imputed := as.integer(between(year, 2008, 2014))]
all_pi[, sample.all.X := as.integer(!is.na(unemp_rate) & !is.na(ln_home_price))]
all_pi[, sample.unemp := as.integer(!is.na(unemp_rate))]
all_pi[, sample.houseprice := as.integer(!is.na(ln_home_price))]

### Estimation ---------------------------------------------------
all_pi <- all_pi[between(year, 2008, 2014)]
fwrite(all_pi, temp.outfile)
stop("Intended")
}

formula_lags <- paste0("L", 1:7, ".D.ln_sales_tax", collapse = "+")
formula_leads <- paste0("F", c(1, 3:8), ".D.ln_sales_tax", collapse = "+")
formula_leads.FHS <- paste0("F", 3:8, ".D.ln_sales_tax", collapse = "+")
formula_RHS <- paste0("D.ln_sales_tax + ", formula_lags, "+", formula_leads)
formula_RHS.FHS <- paste0("D.ln_sales_tax + ", formula_lags, "+", formula_leads.FHS)

outcomes <- c("ln_cpricei", "ln_quantity")
FE_opts <- c("cal_time", "module_by_time",
             "region_by_module_by_time",
             "division_by_module_by_time")

## subset the data
temp.all_pi.imputed <- all_pi[sample.imputed == 1]
temp.all_pi.not.imputed <- all_pi[sample.not.imputed == 1]
rm(all_pi)

## Demean on store-module level to reduce dimensionality
all_vars <- c(
  paste0("L", 1:7, ".D.ln_sales_tax"), "D.ln_sales_tax",
  paste0("F", c(1, 3:8), ".D.ln_sales_tax"),
  "ln_cpricei", "ln_quantity", "unemp_rate",  "ln_home_price"
)

## Need to demean separately for different subsamples
# for (V in all_vars) {
#   all_pi.imputed[, (V) := get(V) - mean(get(V), na.rm = T),
#                  by = store_by_module]
#
#   all_pi.not.imputed[, (V) := get(V) - mean(get(V), na.rm = T),
#                      by = store_by_module]
# }

res.table <- data.table(NULL)
for (FE in FE_opts) {
  ## Demean by the fixed effects
  all_pi.imputed <- copy(temp.all_pi.imputed)
  all_pi.not.imputed <- copy(temp.all_pi.not.imputed)

  for (V in all_vars) {
    all_pi.imputed[, (V) := get(V) - mean(get(V), na.rm = T),
                   by = FE]

    all_pi.not.imputed[, (V) := get(V) - mean(get(V), na.rm = T),
                       by = FE]
  }

  for (Y in outcomes) {
    model.FE <- "store_by_module"
    # FE <- paste(FE, "+ store_by_module") # include a unit FE

    ## Estimation without accounting for covariates, not imputing
    flog.info("Estimating without accounting for covariates...")

    formula1 <- as.formula(paste0(
      Y, "~", formula_RHS, "| ", model.FE, " | 0 | module_by_state"
    ))
    flog.info("Estimating with %s as outcome with %s FE (not imputing).", Y, FE)
    res1 <- felm(formula = formula1,
                 data    = all_pi.not.imputed,
                 weights = all_pi.not.imputed$base.sales)
    flog.info("Finished estimating with %s as outcome with %s FE (not imputing).", Y, FE)

    res.dt <- as.data.table(coef(summary(res1)), keep.rownames = T)
    res.dt[, `:=` (outcome = Y, controls = FE, imputed = F, spec = "no_X")]
    res.table <- rbind(res.table, res.dt, fill = T)
    fwrite(res.table, reg.outfile)

    ## Estimation without accounting for covariates, imputing

    flog.info("Estimating with %s as outcome with %s FE (imputing).", Y, FE)
    res1.imp <- felm(formula = formula1,
                     data    = all_pi.imputed,
                     weights = all_pi.imputed$base.sales)
    flog.info("Finished estimating with %s as outcome with %s FE (imputing).", Y, FE)

    res.dt <- as.data.table(coef(summary(res1.imp)), keep.rownames = T)
    res.dt[, `:=` (outcome = Y, controls = FE, imputed = T, spec = "no_X")]
    res.table <- rbind(res.table, res.dt, fill = T)
    fwrite(res.table, reg.outfile)

    ## Estimation controlling for unemployment via FHS, not imputing
    flog.info("Controlling for unemployment via FHS...")

    formula2 <- as.formula(paste0(
      Y, "~", formula_RHS.FHS, " | ", model.FE,
      " | (unemp_rate~F1.D.ln_sales_tax) | module_by_state"
    ))
    flog.info("Estimating with %s as outcome with %s FE (not imputing).", Y, FE)
    res2 <- felm(formula = formula2,
                 data    = all_pi.not.imputed[sample.unemp == 1],
                 weights = all_pi.not.imputed[sample.unemp == 1]$base.sales)
    flog.info("Finished estimating with %s as outcome with %s FE (not imputing).", Y, FE)

    res.dt <- as.data.table(coef(summary(res2)), keep.rownames = T)
    res.dt[, `:=` (outcome = Y, controls = FE, imputed = F, spec = "unemp_FHS")]
    res.table <- rbind(res.table, res.dt, fill = T)
    fwrite(res.table, reg.outfile)

    ## Estimation controlling for unemployment via FHS, imputing

    flog.info("Estimating with %s as outcome with %s FE (imputing).", Y, FE)
    res2.imp <- felm(formula = formula2,
                 data    = all_pi.imputed[sample.unemp == 1],
                 weights = all_pi.imputed[sample.unemp == 1]$base.sales)
    flog.info("Finished estimating with %s as outcome with %s FE (imputing).", Y, FE)

    res.dt <- as.data.table(coef(summary(res2.imp)), keep.rownames = T)
    res.dt[, `:=` (outcome = Y, controls = FE, imputed = T, spec = "unemp_FHS")]
    res.table <- rbind(res.table, res.dt, fill = T)
    res.table$unit_FE <- "included"
    res.table$FE_det <- "FE demeaned"
    fwrite(res.table, reg.outfile)

    ## Estimation controlling for house prices via FHS, not imputing
    # flog.info("Controlling for house prices via FHS...")
    #
    # formula3 <- as.formula(paste0(
    #   Y, "~", formula_RHS.FHS, " | ", FE,
    #   " | (ln_home_price~F1.D.ln_sales_tax) | module_by_state"
    # ))
    # flog.info("Number of valid rows for house price: %s", nrow(all_pi[sample.houseprice==1]))
    # flog.info("Estimating with %s as outcome with %s FE (not imputing).", Y, FE)
    # res3 <- try(felm(formula = formula3,
    #              data    = all_pi.not.imputed[sample.houseprice == 1],
    #              weights = all_pi.not.imputed[sample.houseprice == 1]$base.sales))
    # flog.info("Finished estimating with %s as outcome with %s FE (not imputing).", Y, FE)
    #
    # if (class(res3) == "try-error") res.dt <- data.table(rn = NA)
    # else res.dt <- as.data.table(coef(summary(res3)), keep.rownames = T)
    # res.dt[, `:=` (outcome = Y, controls = FE, imputed = F, spec = "houseprice_FHS")]
    # res.table <- rbind(res.table, res.dt, fill = T)
    # fwrite(res.table, reg.outfile)


    ## Estimation controlling for house prices via FHS, imputing

    # flog.info("Estimating with %s as outcome with %s FE (imputing).", Y, FE)
    # res3.imp <- try(felm(formula = formula3,
    #                  data    = all_pi.imputed[sample.houseprice == 1],
    #                  weights = all_pi.imputed[sample.houseprice == 1]$base.sales))
    # flog.info("Finished estimating with %s as outcome with %s FE (imputing).", Y, FE)
    #
    # if (class(res3.imp) == "try-error") res.dt <- data.table(rn = NA)
    # else res.dt <- as.data.table(coef(summary(res3.imp)), keep.rownames = T)
    # res.dt[, `:=` (outcome = Y, controls = FE, imputed = T, spec = "houseprice_FHS")]
    # res.table <- rbind(res.table, res.dt, fill = T)
    # fwrite(res.table, reg.outfile)

  }
}
