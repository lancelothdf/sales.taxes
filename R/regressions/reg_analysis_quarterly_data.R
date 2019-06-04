#' Author: John Bonney
#'
#' Address to-do items sent to me from Lance on June 4, 2019.

library(data.table)
library(futile.logger)
library(lfe)

setwd("/project2/igaarder")

## input filepaths ----------------------------------------------------
#' This data set contains quarterly Laspeyres indices and sales from 2006 to
#' 2014. It also contains sales tax rates from 2008-2014.
all_goods_pi_path <- "Data/Nielsen/price_quantity_indices_allitems_2006-2016_notaxinfo.csv"
#' This data set contains an old price index that Lance constructed, from
old_pi_path <- "Data/Nielsen/Quarterly_old_pi.csv"
#' This data is the same as all_goods_pi_path, except it has 2015-2016 data as well.
data.full.path <- "Data/all_nielsen_data_2006_2016_quarterly.csv"
## output filepaths ---------------------------------------------------
reg.outfile <- "Data/quarterly_pi_output.csv"
reg.lag.outfile <- "Data/quarterly_pi_lagged_output.csv"

## Load in data, restrict to 2008-2014, combine ------------------------
all_pi <- fread(all_goods_pi_path)
old_pi <- fread(old_pi_path)

all_pi <- all_pi[year %in% 2008:2014 & !is.na(cpricei) & !is.na(sales_tax)]
old_pi <- old_pi[year %in% 2008:2014 & !is.na(ln_pricei2)]

all_pi <- merge(all_pi, old_pi,
                by = c("fips_state", "fips_county", "store_code_uc",
                       "product_module_code", "year", "quarter"), all = T)
rm(old_pi)

## balance the data + add useful variables -----------------------------

## Form 4 variables (in quarterly data):
  # 1) ln_cpricei (as before)
  all_pi[, ln_cpricei := log(cpricei)]
  # 2) ln_cpricei2 (consumer price index 2) [add log(1+tax) to obtain ln_cpricei2]
  all_pi[, ln_sales_tax := log(sales_tax)]
  all_pi[, ln_cpricei2 := ln_pricei2 + ln_sales_tax]
  # 3) ln_quantity (= ln(sales) - ln(pricei))
  all_pi[, ln_quantity := log(sales) - log(pricei)]
  # 4) ln_quantity2 (=ln(sales) - ln(pricei2)).
  all_pi[, ln_quantity2 := log(sales) - ln_pricei2]

## get sales weights
  all_pi[, base.sales := sales[year == 2008 & quarter == 1],
         by = .(store_code_uc, product_module_code)]

  all_pi <- all_pi[!is.na(base.sales) & !is.na(sales) & !is.na(ln_cpricei) &
                     !is.na(ln_sales_tax) & !is.na(ln_quantity) &
                     !is.na(ln_quantity2) & !is.na(ln_cpricei2)]

## balance on store-module level
  keep_store_modules <- all_pi[, list(n = .N),
                               by = .(store_code_uc, product_module_code)]
  keep_store_modules <- keep_store_modules[n == (2014 - 2007) * 4]

  setkey(all_pi, store_code_uc, product_module_code)
  setkey(keep_store_modules, store_code_uc, product_module_code)

  all_pi <- all_pi[keep_store_modules]
  setkey(all_pi, fips_county, fips_state)

## prep some fixed effects for later
  all_pi[, store_by_module := .GRP, by = .(store_code_uc, product_module_code)]
  all_pi[, module_by_year := .GRP, by = .(product_module_code, year)]
  all_pi[, module_by_state := .GRP, by = .(product_module_code, fips_state)]

## Run for each of the four outcomes, two specifications: -------------
outcomes <- c("ln_cpricei", "ln_cpricei2", "ln_quantity", "ln_quantity2")

res.table <- data.table(NULL)
for (Y in outcomes) {
  # outcome 1: ln_sales tax rate after taking store_by_module FE +
  # **year FE** (cluster standard errors at the module-by-state).
  formula1 <- as.formula(paste0(
    Y, "~ ln_sales_tax | store_by_module + year | 0 | module_by_state"
  ))
  flog.info("Estimating with %s as outcome with year FE.", Y)
  res1 <- felm(formula = formula1, data = all_pi, weights = all_pi$base.sales)
  flog.info("Finished estimating with %s as outcome with year FE.", Y)
  res.table <- rbind(
    res.table,
    data.table(
      outcome = Y,
      estimate = coef(summary(res1))["ln_sales_tax", "Estimate"],
      se = coef(summary(res1))["ln_sales_tax", "Cluster s.e."],
      pval = coef(summary(res1))["ln_sales_tax", "Pr(>|t|)"],
      Rsq = summary(res1)$r.squared,
      adj.Rsq = summary(res1)$adj.r.squared,
      time_controls = "year FE"
      )
  )

  # outcome 2: ln_sales tax rate after taking store_by_module FE +
  # **module_by_year FE** (cluster standard errors at the module_by_state).
  formula2 <- as.formula(paste0(
    Y, "~ ln_sales_tax | store_by_module + module_by_year | 0 | module_by_state"
  ))
  flog.info("Estimating with %s as outcome with module-by-year FE.", Y)
  res2 <- felm(formula = formula2, data = all_pi, weights = all_pi$base.sales)
  flog.info("Finished estimating with %s as outcome with module-by-year FE.", Y)
  res.table <- rbind(
    res.table,
    data.table(
      outcome = Y,
      estimate = coef(summary(res2))["ln_sales_tax", "Estimate"],
      se = coef(summary(res2))["ln_sales_tax", "Cluster s.e."],
      pval = coef(summary(res2))["ln_sales_tax", "Pr(>|t|)"],
      Rsq = summary(res2)$r.squared,
      adj.Rsq = summary(res2)$adj.r.squared,
      time_controls = "module-by-year FE"
    )
  )
  flog.info("Writing output.")
  fwrite(res.table, reg.outfile)
}

rm(all_pi)
# Do 2) again but with lagged- and lead- outcome (like you guys do for the
# Belgium project) and plot the coefficents (to test for pre-trends). You can
# do 6 quarters of lags and leads.

## prep the 2006-2016 data ----------------------
all_pi_full <- fread(data.full.path)

# impute tax rates prior to 2008 and after 2014
all_pi[, sales_tax := ifelse(year < 2008, sales_tax[year == 2008 & quarter == 1], sales_tax),
       by = .(store_code_uc, product_module_code)]
all_pi[, sales_tax := ifelse(year > 2014, sales_tax[year == 2014 & quarter == 4], sales_tax),
       by = .(store_code_uc, product_module_code)]

# recreate variables from all_pi
all_pi_full[, ln_cpricei := log(cpricei)]
all_pi_full[, ln_sales_tax := log(sales_tax)]
all_pi_full[, ln_cpricei2 := ln_pricei2 + ln_sales_tax]
all_pi_full[, ln_quantity := log(sales) - log(pricei)]
all_pi_full[, ln_quantity2 := log(sales) - ln_pricei2]
all_pi_full[, store_by_module := .GRP, by = .(store_code_uc, product_module_code)]
all_pi_full[, module_by_year := .GRP, by = .(product_module_code, year)]
all_pi_full[, module_by_state := .GRP, by = .(product_module_code, fips_state)]

## get sales weights
all_pi[, base.sales := sales[year == 2008 & quarter == 1],
       by = .(store_code_uc, product_module_code)]

all_pi <- all_pi[!is.na(base.sales) & !is.na(sales) & !is.na(ln_cpricei) &
                   !is.na(ln_sales_tax) & !is.na(ln_quantity) &
                   !is.na(ln_quantity2) & !is.na(ln_cpricei2)]

## balance on store-module level
keep_store_modules <- all_pi_full[, list(n = .N),
                                  by = .(store_code_uc, product_module_code)]
keep_store_modules <- keep_store_modules[n == (2016 - 2005) * 4]

setkey(all_pi, store_code_uc, product_module_code)
setkey(keep_store_modules, store_code_uc, product_module_code)

all_pi <- all_pi[keep_store_modules]
setkey(all_pi, store_code_uc, product_module_code, year, quarter)

## produce results using lags and leads of Y
lag.res.table <- data.table(NULL)
for (Y in outcomes) {
  for (l.type in c("lag", "lead")) {
    for (lag.val in 1:6) {
      # generate lagged outcome
      lag.Y <- paste0(Y, ".", l.type, lag.val)
      all_pi_full[, (lag.Y) := shift(get(Y), n=lag.val, type=l.type),
                  by = .(store_code_uc, product_module_code)]

      # run on specification with year FE
      formula3 <- as.formula(paste0(
        lag.Y, "~ ln_sales_tax | store_by_module + year | 0 | module_by_state"
      ))
      flog.info("Estimating with %s as outcome with year FE.", lag.Y)
      res3 <- felm(formula = formula3,
                   data = all_pi_full[between(year, 2008, 2014)],
                   weights = all_pi_full[between(year, 2008, 2014)]$base.sales)
      flog.info("Finished estimating with %s as outcome with year FE.", lag.Y)

      lag.res.table <- rbind(
        lag.res.table,
        data.table(
          outcome = Y,
          period = ifelse(l.type == "lag", -1*lag.val, lag.val),
          estimate = coef(summary(res3))["ln_sales_tax", "Estimate"],
          se = coef(summary(res3))["ln_sales_tax", "Cluster s.e."],
          pval = coef(summary(res3))["ln_sales_tax", "Pr(>|t|)"],
          Rsq = summary(res3)$r.squared,
          adj.Rsq = summary(res3)$adj.r.squared,
          time_controls = "year FE"
        )
      )

      # run on specification with module-by-year FE
      formula4 <- as.formula(paste0(
        lag.Y, "~ ln_sales_tax | store_by_module + module_by_year | 0 | module_by_state"
      ))
      flog.info("Estimating with %s as outcome with module-by-year FE.", lag.Y)
      res4 <- felm(formula = formula4,
                   data = all_pi_full[between(year, 2008, 2014)],
                   weights = all_pi_full[between(year, 2008, 2014)]$base.sales)
      flog.info("Finished estimating with %s as outcome with module-by-year FE.", lag.Y)

      lag.res.table <- rbind(
        lag.res.table,
        data.table(
          outcome = Y,
          estimate = coef(summary(res4))["ln_sales_tax", "Estimate"],
          se = coef(summary(res4))["ln_sales_tax", "Cluster s.e."],
          pval = coef(summary(res4))["ln_sales_tax", "Pr(>|t|)"],
          Rsq = summary(res4)$r.squared,
          adj.Rsq = summary(res4)$adj.r.squared,
          time_controls = "module-by-year FE"
        )
      )

      all_pi_full[, (lag.Y) := NULL]
      flog.info("Writing output.")
      fwrite(lag.res.table, reg.lag.outfile)

    }
  }
}

