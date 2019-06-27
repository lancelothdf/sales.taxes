#' Author: John Bonney (modified by Lance)
#'
#' Goal: regress change in price/quantity on change in tax rate (including leads and lags)
#' NOTE: tax rate in a given quarter is measured as tax rate in third month of that quarter
#' NOTE 2: Also do "placebos tests" in the same file (regress unemployment and house prices on sales tax rates)
#' NOTE 3: We also consider specifications that control for economic conditions
#' NOTE 4: In this version, we only use one type of price indices (the other ones are under construction)

library(data.table)
library(futile.logger)
library(lfe)
library(multcomp)

setwd("/project2/igaarder")


## input filepaths -----------------------------------------------
#' This data set contains quarterly Laspeyres indices and sales from 2006 to
#' 2014. It also contains sales tax rates from 2008-2014.
all_goods_pi_path <- "Data/all_nielsen_data_2006_2016_quarterly_taxM3.csv"


#' This data set contains an old price index that Lance constructed, from
#old_pi_path <- "Data/Nielsen/Quarterly_old_pi.csv"
#' This data is the same as all_goods_pi_path, except it has 2015-2016 data as well.
#data.full.path <- "Data/all_nielsen_data_2006_2016_quarterly.csv"

## House price and unemployment data
zillow_path <- "Data/covariates/zillow_long_by_county_clean.csv"
zillow_state_path <- "Data/covariates/zillow_long_by_state_clean.csv"
unemp.path <- "Data/covariates/county_monthly_unemp_clean.csv"


## output filepaths ----------------------------------------------
reg.outfile <- "Data/LRdiff_quarterly_results_main_unemp.csv"


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


##### Load the quarterly price index and sales data (prep/clean them later)
all_pi <- fread(all_goods_pi_path)



#### Prep the unemployment and house price data --------------------------
### Start with house prices
# First build a frame to make sure we can assign every county a home price
all_counties <- unique(all_pi[, .(fips_state, fips_county)])
county_skeleton <- data.table(NULL)
for (X in 2006:2016) {
  for (Y in 1:12) {
    all_counties[, year := X]
    all_counties[, month := Y]
    county_skeleton <- rbind(county_skeleton, all_counties)
  }
}

## prep house price data
zillow_dt <- fread(zillow_path)
zillow_dt <- zillow_dt[between(year, 2006, 2016)]
zillow_dt <- zillow_dt[, .(fips_state, fips_county, median_home_price, year, month)]
zillow_dt <- merge(county_skeleton, zillow_dt, all.x = T,
                   by = c("fips_state", "fips_county", "year", "month"))

## prep state-level house prices (for when county-level is missing)
zillow_state_dt <- fread(zillow_state_path)
zillow_state_dt <- zillow_state_dt[between(year, 2006, 2016)]
zillow_state_dt <- zillow_state_dt[, .(fips_state, median_home_price, year, month)]
setnames(zillow_state_dt, "median_home_price", "state_median_home_price")
zillow_state_dt$month <- as.integer(round(zillow_state_dt$month))

zillow_dt <- merge(zillow_dt, zillow_state_dt, all.x = T,
                   by = c("fips_state", "year", "month"))
zillow_dt[is.na(median_home_price), median_home_price := state_median_home_price]
zillow_dt[, state_median_home_price := NULL]


## collapse to quarters
zillow_dt <- zillow_dt[, quarter := ceiling((month/12)*4)]
zillow_dt <- zillow_dt[, list(ln_home_price = log(mean(median_home_price))),
                       by = .(year, quarter, fips_state, fips_county)]


### Unemployment data
unemp.data <- fread(unemp.path)
unemp.data <- unemp.data[, c("fips_state", "fips_county", "year", "month", "rate")]
unemp.data <- unemp.data[, quarter := ceiling((month/12)*4)]
unemp.data <- unemp.data[, list(unemp = mean(rate)), by = .(year, quarter, fips_state, fips_county)]
unemp.data <- unemp.data[year >= 2006 & year <= 2016,]
unemp.data <- unemp.data[, ln_unemp := log(unemp)]



## prep the 2006-2016 data --------------------------------------- ##NOTE: in this version we do not merge to "old price indices" because they are under construction
#old_pi <- fread(old_pi_path)

## merge on the old price indices
#all_pi <- merge(all_pi, old_pi,
#                     by = c("fips_state", "fips_county", "store_code_uc",
#                            "product_module_code", "year", "quarter"), all = T)
#rm(old_pi)


## merge on the census region/division info
all_pi <- merge(all_pi, geo_dt, by = "fips_state")


#### create necessary variables

## First create cpricei
all_pi[, sales_tax := 1 + sales_tax]
all_pi[, cpricei := sales_tax*pricei]

all_pi[, ln_cpricei := log(cpricei)]
all_pi[, ln_sales_tax := log(sales_tax)]
#all_pi[, ln_cpricei2 := ln_pricei2 + ln_sales_tax]
all_pi[, ln_quantity := log(sales) - log(pricei)]
#all_pi[, ln_quantity2 := log(sales) - ln_pricei2]
all_pi[, store_by_module := .GRP, by = .(store_code_uc, product_module_code)]
all_pi[, cal_time := 4 * year + quarter]
all_pi[, module_by_time := .GRP, by = .(product_module_code, cal_time)]
all_pi[, module_by_state := .GRP, by = .(product_module_code, fips_state)]
all_pi[, region_by_module_by_time := .GRP, by = .(region, product_module_code, cal_time)]
all_pi[, division_by_module_by_time := .GRP, by = .(division, product_module_code, cal_time)]

## get sales weights
all_pi[, base.sales := sales[year == 2008 & quarter == 1],
            by = .(store_code_uc, product_module_code)]


##### Merge to econ covariates
##
all_pi <- merge(all_pi, zillow_dt, by = c("fips_state", "fips_county", "year", "quarter"), all.x = T)
rm(zillow_dt)

##
all_pi <- merge(all_pi, unemp.data, by = c("fips_state", "fips_county", "year", "quarter"), all.x = T)
rm(unemp.data)


### Keep only a balanced sample (on price, quantity and econ covariates)
#all_pi <- all_pi[!is.na(base.sales) & !is.na(sales) & !is.na(ln_cpricei) &
#                             !is.na(ln_sales_tax) & !is.na(ln_quantity) &
#                             !is.na(ln_quantity2) & !is.na(ln_cpricei2)]
all_pi <- all_pi[!is.na(base.sales) & !is.na(sales) & !is.na(ln_cpricei) &
                             !is.na(ln_sales_tax) & !is.na(ln_quantity) &
                              !is.na(ln_unemp) & !is.na(ln_home_price)]


## balance on store-module level
keep_store_modules <- all_pi[, list(n = .N),
                                  by = .(store_code_uc, product_module_code)]
keep_store_modules <- keep_store_modules[n == (2016 - 2005) * 4]

setkey(all_pi, store_code_uc, product_module_code)
setkey(keep_store_modules, store_code_uc, product_module_code)

all_pi <- all_pi[keep_store_modules]
setkey(all_pi, store_code_uc, product_module_code, year, quarter)


## Delete some variables to save memory
all_pi <- all_pi[, c("fips_state", "fips_county", "year", "quarter", "product_module_code", "store_code_uc", "ln_cpricei", "ln_sales_tax", "ln_quantity", "store_by_module", "cal_time", "module_by_time", "module_by_state", "region_by_module_by_time", "division_by_module_by_time", "base.sales", "ln_home_price", "ln_unemp")]


## take first differences of outcomes and treatment
all_pi[, D.ln_cpricei := ln_cpricei - shift(ln_cpricei, n=1, type="lag"),
       by = .(store_code_uc, product_module_code)]

#all_pi[, D.ln_cpricei2 := ln_cpricei2 - shift(ln_cpricei2, n=1, type="lag"),
#       by = .(store_code_uc, product_module_code)]

all_pi[, D.ln_quantity := ln_quantity - shift(ln_quantity, n=1, type="lag"),
       by = .(store_code_uc, product_module_code)]

#all_pi[, D.ln_quantity2 := ln_quantity2 - shift(ln_quantity2, n=1, type="lag"),
#       by = .(store_code_uc, product_module_code)]

all_pi[, D.ln_sales_tax := ln_sales_tax - shift(ln_sales_tax, n=1, type="lag"),
       by = .(store_code_uc, product_module_code)]


## Econ covariates
all_pi[, D.ln_unemp := ln_unemp - shift(ln_unemp, n=1, type = "lag"),
       by = .(store_code_uc, product_module_code)]

all_pi[, D.ln_home_price := ln_home_price - shift(ln_home_price, n=1, type = "lag"),
       by = .(store_code_uc, product_module_code)]



## generate lags and leads of ln_sales_tax
for (lag.val in 1:6) {
  lag.X <- paste0("L", lag.val, ".D.ln_sales_tax")
  all_pi[, (lag.X) := shift(D.ln_sales_tax, n=lag.val, type="lag"),
         by = .(store_code_uc, product_module_code)]

  lead.X <- paste0("F", lag.val, ".D.ln_sales_tax")
  all_pi[, (lead.X) := shift(D.ln_sales_tax, n=lag.val, type="lead"),
         by = .(store_code_uc, product_module_code)]
}


### Estimation ---------------------------------------------------

formula_lags <- paste0("L", 1:6, ".D.ln_sales_tax", collapse = "+")
formula_leads <- paste0("F", 1:6, ".D.ln_sales_tax", collapse = "+")
formula_RHS <- paste0("D.ln_sales_tax + ", formula_lags, "+", formula_leads)

#outcomes <- c("D.ln_cpricei", "D.ln_cpricei2", "D.ln_quantity", "D.ln_quantity2")
outcomes <- c("D.ln_cpricei", "D.ln_quantity")
econ.outcomes <- c("D.ln_unemp", "D.ln_home_price")
FE_opts <- c("cal_time", "module_by_time", "region_by_module_by_time", "division_by_module_by_time")


## for linear hypothesis tests
lead.vars <- paste(paste0("F", 6:1, ".D.ln_sales_tax"), collapse = " + ")
lag.vars <- paste(paste0("L", 6:1, ".D.ln_sales_tax"), collapse = " + ")
lag5.vars <- paste(paste0("L", 5:1, ".D.ln_sales_tax"), collapse = " + ")
lead.lp.restr <- paste(lead.vars, "= 0")
lag.lp.restr <- paste(lag.vars, "+ D.ln_sales_tax = 0")
lag5.lp.restr <- paste(lag5.vars, "+ D.ln_sales_tax = 0")
total.lp.restr <- paste(lag.vars, "+", lead.vars, "+ D.ln_sales_tax = 0")


## generate lags and leads of ln_unemp
for (lag.val in 1:6) {
  lag.X <- paste0("L", lag.val, ".D.ln_unemp")
  all_pi[, (lag.X) := shift(D.ln_unemp, n=lag.val, type="lag"),
         by = .(store_code_uc, product_module_code)]

  lead.X <- paste0("F", lag.val, ".D.ln_unemp")
  all_pi[, (lead.X) := shift(D.ln_unemp, n=lag.val, type="lead"),
         by = .(store_code_uc, product_module_code)]
}

form_unemp_lags <- paste0("L", 1:6, ".D.ln_unemp", collapse = "+")
form_unemp_leads <- paste0("F", 1:6, ".D.ln_unemp", collapse = "+")
form_unemp_RHS <- paste0("D.ln_unemp + ", form_unemp_lags, "+", form_unemp_leads)


##### Now control for Econ conditions including leads and lags
print(" Now include leads and lags of unemployment")
all_pi <- all_pi[between(year, 2008, 2014)]

res.table <- data.table(NULL)
for (Y in c(outcomes)) {
  for (FE in FE_opts) {

      total_formula <- paste(formula_RHS, " + ", form_unemp_RHS, sep = "")

      formula1 <- as.formula(paste0(
        Y, "~", total_formula, "| ", FE, " | 0 | module_by_state"
      ))
      flog.info("Estimating with %s as outcome with %s FE.", Y, FE)
      res1 <- felm(formula = formula1, data = all_pi,
                   weights = all_pi$base.sales)
      flog.info("Finished estimating with %s as outcome with %s FE.", Y, FE)

      ## sum leads
      flog.info("Summing leads...")
      lead.test <- glht(res1, linfct = lead.lp.restr)
      lead.test.est <- coef(summary(lead.test))[[1]]
      lead.test.se <- sqrt(vcov(summary(lead.test)))[[1]]
      lead.test.pval <- 2*(1 - pnorm(abs(lead.test.est/lead.test.se)))

      ## sum lags
      flog.info("Summing lags...")
      lag.test <- glht(res1, linfct = lag.lp.restr)
      lag.test.est <- coef(summary(lag.test))[[1]]
      lag.test.se <- sqrt(vcov(summary(lag.test)))[[1]]
      lag.test.pval <- 2*(1 - pnorm(abs(lag.test.est/lag.test.se)))

      ## sum lags - include only 5 lags (in case it looks like including the last lag or not may influence the results)
      flog.info("Summing lags5...")
      lag5.test <- glht(res1, linfct = lag5.lp.restr)
      lag5.test.est <- coef(summary(lag5.test))[[1]]
      lag5.test.se <- sqrt(vcov(summary(lag5.test)))[[1]]
      lag5.test.pval <- 2*(1 - pnorm(abs(lag5.test.est/lag5.test.se)))

      ## sum all
      flog.info("Summing all...")
      total.test <- glht(res1, linfct = total.lp.restr)
      total.test.est <- coef(summary(total.test))[[1]]
      total.test.se <- sqrt(vcov(summary(total.test)))[[1]]
      total.test.pval <- 2*(1 - pnorm(abs(total.test.est/total.test.se)))

      ## linear hypothesis results
      lp.dt <- data.table(
        rn = c("Pre.D.ln_sales_tax", "Post.D.ln_sales_tax", "Post5.D.ln_sales_tax", "All.D.ln_sales_tax"),
        Estimate = c(lead.test.est, lag.test.est, lag5.test.est, total.test.est),
        `Cluster s.e.` = c(lead.test.se, lag.test.se, lag5.test.se, total.test.se),
        `Pr(>|t|)` = c(lead.test.pval, lag.test.pval, lag5.test.pval, total.test.pval),
        outcome = Y,
        controls = paste(FE, " + lead-lag unemp", sep = "")
      )

      ## attach results
      flog.info("Writing results...")
      res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
      res1.dt[, outcome := Y]
      res1.dt[, controls := paste(FE, " + lead-lag unemp", sep = "")]
      res.table <- rbind(res.table, res1.dt, fill = T)
      res.table <- rbind(res.table, lp.dt, fill = T)
      fwrite(res.table, reg.outfile)
    }
  }

