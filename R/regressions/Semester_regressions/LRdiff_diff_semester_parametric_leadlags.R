####  In this file, we estimate leads and lags of effect of change in sales tax rate and impose a functional form (polynomial) on the pattern followed by dynamic effects
#' Authors: John Bonney and Lancelot Henry de Frahan


library(data.table)
library(futile.logger)
library(lfe)
library(multcomp)

setwd("/project2/igaarder")


## input filepaths -----------------------------------------------
#' This data set contains quarterly Laspeyres indices and sales from 2006 to
#' 2014. It also contains sales tax rates from 2008-2014.
all_goods_pi_path <- "Data/Nielsen/price_quantity_indices_allitems_2006-2016_notaxinfo.csv"
#' This data set contains an old price index that Lance constructed, from
old_pi_path <- "Data/Nielsen/Quarterly_old_pi.csv"
#' This data is the same as all_goods_pi_path, except it has 2015-2016 data as well.
data.full.path <- "Data/Nielsen/semester_nielsen_data.csv"


## OUTPUT filepaths ----------------------------------------------
output.results.file <- "Data/LRdiff_results_semester_param_leadlags.csv"


zillow_path <- "Data/covariates/zillow_long_by_county_clean.csv"
zillow_state_path <- "Data/covariates/zillow_long_by_state_clean.csv"
unemp.path <- "Data/covariates/county_monthly_unemp_clean.csv"


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

## prep the 2006-2016 data ---------------------------------------
all_pi <- fread(data.full.path)
#old_pi <- fread(old_pi_path)

## merge on the old price indices
#all_pi <- merge(all_pi, old_pi,
#                     by = c("fips_state", "fips_county", "store_code_uc",
#                            "product_module_code", "year", "quarter"), all = T)
#rm(old_pi)

## merge on the census region/division info
all_pi <- merge(all_pi, geo_dt, by = "fips_state")


# create necessary variables
all_pi[, store_by_module := .GRP, by = .(store_code_uc, product_module_code)]
all_pi[, cal_time := 2 * year + semester]
all_pi[, module_by_time := .GRP, by = .(product_module_code, cal_time)]
all_pi[, module_by_state := .GRP, by = .(product_module_code, fips_state)]
all_pi[, region_by_module_by_time := .GRP, by = .(region, product_module_code, cal_time)]
all_pi[, division_by_module_by_time := .GRP, by = .(division, product_module_code, cal_time)]

## Balance the sample
all_pi <- all_pi[!is.na(base.sales) & !is.na(sales) & !is.na(ln_cpricei) &
                             !is.na(ln_sales_tax) & !is.na(ln_quantity) &
                             !is.na(ln_sales_tax_Q2) & !is.na(ln_cpricei_Q2) & !is.na(ln_quantity_Q2)]

## balance on store-module level
keep_store_modules <- all_pi[, list(n = .N),
                                  by = .(store_code_uc, product_module_code)]
keep_store_modules <- keep_store_modules[n == (2016 - 2005) * 2]

setkey(all_pi, store_code_uc, product_module_code)
setkey(keep_store_modules, store_code_uc, product_module_code)

all_pi <- all_pi[keep_store_modules]
setkey(all_pi, store_code_uc, product_module_code, year, semester)


######## Import and prep house price and unemployment data

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


## collapse to semesters
zillow_dt <- zillow_dt[, semester := ifelse(between(month, 1, 6), 1, 2)]
zillow_dt <- zillow_dt[, list(ln_home_price = log(mean(median_home_price))),
                       by = .(year, semester, fips_state, fips_county)]

##
all_pi <- merge(all_pi, zillow_dt, by = c("fips_state", "fips_county", "year", "semester"), all.x = T)


### Unemployment data
unemp.data <- fread(unemp.path)
unemp.data <- unemp.data[, c("fips_state", "fips_county", "year", "month", "rate")]
unemp.data <- unemp.data[, semester := ifelse(between(month, 1, 6), 1, 2)]
unemp.data <- unemp.data[, list(unemp = mean(rate)), by = .(year, semester, fips_state, fips_county)]
unemp.data <- unemp.data[year >= 2006 & year <= 2016,]
unemp.data <- unemp.data[, ln_unemp := log(unemp)]

##
all_pi <- merge(all_pi, unemp.data, by = c("fips_state", "fips_county", "year", "semester"), all.x = T)


## Delete some variables to save memory
all_pi <- all_pi[, c("fips_state", "fips_county", "year", "semester", "store_code_uc", "product_module_code", "ln_cpricei", "ln_sales_tax", "ln_quantity", "base.sales", "store_by_module", "cal_time", "module_by_time", "module_by_state", "region_by_module_by_time", "division_by_module_by_time", "ln_home_price", "ln_unemp")]

###########################################################################
## take first differences of outcomes and treatment
all_pi <- all_pi[order(store_code_uc, product_module_code, year, semester),] ##Sort on store by year (year in ascending order)


## Difference the outcomes, treatment and econ covariates
all_pi[, D.ln_cpricei := ln_cpricei - shift(ln_cpricei, n=1, type="lag"),
            by = .(store_code_uc, product_module_code)]

all_pi[, D.ln_quantity := ln_quantity - shift(ln_quantity, n=1, type="lag"),
            by = .(store_code_uc, product_module_code)]

all_pi[, D.ln_sales_tax := ln_sales_tax - shift(ln_sales_tax, n=1, type="lag"),
            by = .(store_code_uc, product_module_code)]

all_pi[, D.ln_unemp := ln_unemp - shift(ln_unemp, n=1, type = "lag"),
            by = .(store_code_uc, product_module_code)]

all_pi[, D.ln_home_price := ln_home_price - shift(ln_home_price, n=1, type = "lag"),
            by = .(store_code_uc, product_module_code)]


## Create 2 - year differences (= 4 semesters)
all_pi[, D2.ln_unemp := ln_unemp - shift(ln_unemp, n=4, type = "lag"),
            by = .(store_code_uc, product_module_code)]

all_pi[, D2.ln_home_price := ln_home_price - shift(ln_home_price, n=4, type = "lag"),
            by = .(store_code_uc, product_module_code)]



## generate lags and leads of ln_sales_tax
for (lag.val in 1:4) {
  lag.X <- paste0("L", lag.val, ".D.ln_sales_tax")
  all_pi[, (lag.X) := shift(D.ln_sales_tax, n=lag.val, type="lag"),
         by = .(store_code_uc, product_module_code)]

  lead.X <- paste0("F", lag.val, ".D.ln_sales_tax")
  all_pi[, (lead.X) := shift(D.ln_sales_tax, n=lag.val, type="lead"),
         by = .(store_code_uc, product_module_code)]

}


##Create a lead and lag of the 2-year difference in unemployment and home price
all_pi <- all_pi[order(fips_state, fips_county, store_code_uc, product_module_code, year, semester), c("F4.D2.ln_unemp") := shift(.SD, 4, type = "lead"), .SDcols = "D2.ln_unemp", by = c("fips_state", "fips_county", "store_code_uc", "product_module_code")]
all_pi <- all_pi[order(fips_state, fips_county, store_code_uc, product_module_code, year, semester), c("L1.D2.ln_unemp") := shift(.SD, 1, type = "lag"), .SDcols = "D2.ln_unemp", by = c("fips_state", "fips_county", "store_code_uc", "product_module_code")]
all_pi <- all_pi[order(fips_state, fips_county, store_code_uc, product_module_code, year, semester), c("F4.D2.ln_home_price") := shift(.SD, 4, type = "lead"), .SDcols = "D2.ln_home_price", by = c("fips_state", "fips_county", "store_code_uc", "product_module_code")]
all_pi <- all_pi[order(fips_state, fips_county, store_code_uc, product_module_code, year, semester), c("L1.D2.ln_home_price") := shift(.SD, 1, type = "lag"), .SDcols = "D2.ln_home_price", by = c("fips_state", "fips_county", "store_code_uc", "product_module_code")]


#### Create sum of lag/lead tax rates interacted with lag/lead
all_pi[, lag.poly0 := D.ln_sales_tax + L1.D.ln_sales_tax + L2.D.ln_sales_tax + L3.D.ln_sales_tax + L4.D.ln_sales_tax]
all_pi[, lead.poly0 := F4.D.ln_sales_tax + F3.D.ln_sales_tax + F2.D.ln_sales_tax + F1.D.ln_sales_tax]
all_pi[, lag.poly1 := L1.D.ln_sales_tax + 2*L2.D.ln_sales_tax + 3*L3.D.ln_sales_tax + 4*L4.D.ln_sales_tax]
all_pi[, lead.poly1 := 4*F4.D.ln_sales_tax + 3*F3.D.ln_sales_tax + 2*F2.D.ln_sales_tax + F1.D.ln_sales_tax]
all_pi[, lag.poly2 := L1.D.ln_sales_tax + 4*L2.D.ln_sales_tax + 9*L3.D.ln_sales_tax + 16*L4.D.ln_sales_tax]
all_pi[, lead.poly2 := 16*F4.D.ln_sales_tax + 9*F3.D.ln_sales_tax + 4*F2.D.ln_sales_tax + F1.D.ln_sales_tax]
all_pi[, lag.poly3 := L1.D.ln_sales_tax + 8*L2.D.ln_sales_tax + 27*L3.D.ln_sales_tax + 64*L4.D.ln_sales_tax]
all_pi[, lead.poly3 := 64*F4.D.ln_sales_tax + 27*F3.D.ln_sales_tax + 8*F2.D.ln_sales_tax + F1.D.ln_sales_tax]


### Estimation ---------------------------------------------------
all_pi <- all_pi[between(year, 2008, 2014)]
all_pi <- all_pi[ year >= 2009 | (year == 2008 & semester == 2)] ## First semester of 2008, the difference was imputed not real data - so we drop it



outcomes <- c("D.ln_cpricei", "D.ln_quantity")
FE_opts <- c("cal_time", "module_by_time", "region_by_module_by_time", "division_by_module_by_time")
Econ_opts <- c("D.ln_unemp", "D.ln_home_price", "D.ln_unemp + D.ln_home_price")


## Create a matrix with controls for econ conditions that include leads and lags - also store indicators that will be used in final matrix with results
Econ_w_lags <- c("D.ln_unemp", "D.ln_unemp", "D.ln_unemp + D.ln_home_price", "D.ln_unemp + D.ln_home_price")
Econ_w_lags <- rbind(Econ_w_lags, c("Yes", "Yes", "Yes", "Yes"))
Econ_w_lags <- rbind(Econ_w_lags, c("No", "Yes", "No", "Yes"))
Econ_w_lags <- rbind(Econ_w_lags, c("L1.D2.ln_unemp + D.ln_unemp", "F4.D2.ln_unemp + D.ln_unemp + L1.D2.ln_unemp", "L1.D2.ln_unemp + D.ln_unemp + L1.D2.ln_home_price + D.ln_home_price", "F4.D2.ln_unemp + D.ln_unemp + L1.D2.ln_unemp + F4.D2.ln_home_price + D.ln_home_price + L1.D2.ln_home_price"))




LRdiff_res <- data.table(NULL)
for (Y in c(outcomes)) {
  for (FE in FE_opts) {

    ## First: 2nd degree polynomial for leads and lags
    formula1 <- as.formula(paste0(
      Y, "~ lead.poly0 + lead.poly1 + lead.poly2 + lag.poly0 + lag.poly1 + lag.poly2 |", FE, " | 0 | module_by_state"
    ))
    flog.info("Estimating with %s as outcome with %s FE.", Y, FE)
    res1 <- felm(formula = formula1, data = all_pi,
                 weights = all_pi$base.sales)
    flog.info("Finished estimating with %s as outcome with %s FE.", Y, FE)


    ## attach results
    flog.info("Writing results...")
    res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
    res1.dt[, outcome := Y]
    res1.dt[, controls := FE]
    res1.dt[, econ := "none"]
    res1.dt[, lag.econ := NA]
    res1.dt[, lead.econ := NA]
    res1.dt[, poly := 2]
    res1.dt[, Rsq := summary(res1)$r.squared]
    res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)

    ## 4th lead
    lead4.test <- glht(res1, linfct = "lead.poly0 + lead.poly1*4 + lead.poly2*16 = 0")
    lead4.test.est <- coef(summary(lead4.test))[[1]]
    lead4.test.se <- sqrt(vcov(summary(lead4.test)))[[1]]
    lead4.test.pval <- 2*(1 - pnorm(abs(lead4.test.est/lead4.test.se)))

    ## 3rd lead
    lead3.test <- glht(res1, linfct = "lead.poly0 + lead.poly1*3 + lead.poly2*9 = 0")
    lead3.test.est <- coef(summary(lead3.test))[[1]]
    lead3.test.se <- sqrt(vcov(summary(lead3.test)))[[1]]
    lead3.test.pval <- 2*(1 - pnorm(abs(lead3.test.est/lead3.test.se)))

    ## 2nd lead
    lead2.test <- glht(res1, linfct = "lead.poly0 + lead.poly1*2 + lead.poly2*4 = 0")
    lead2.test.est <- coef(summary(lead2.test))[[1]]
    lead2.test.se <- sqrt(vcov(summary(lead2.test)))[[1]]
    lead2.test.pval <- 2*(1 - pnorm(abs(lead2.test.est/lead2.test.se)))

    ## 1st lead
    lead1.test <- glht(res1, linfct = "lead.poly0 + lead.poly1 + lead.poly2 = 0")
    lead1.test.est <- coef(summary(lead1.test))[[1]]
    lead1.test.se <- sqrt(vcov(summary(lead1.test)))[[1]]
    lead1.test.pval <- 2*(1 - pnorm(abs(lead1.test.est/lead1.test.se)))


    ## sum leads
    flog.info("Summing leads...")
    lead.test <- glht(res1, linfct = "4*lead.poly0 + 10*lead.poly1 + 30*lead.poly2 = 0")
    lead.test.est <- coef(summary(lead.test))[[1]]
    lead.test.se <- sqrt(vcov(summary(lead.test)))[[1]]
    lead.test.pval <- 2*(1 - pnorm(abs(lead.test.est/lead.test.se)))

    ## On Impact --> Effect = coefficient on lag.poly0
    lag0.test.est <- coef(summary(res1))[ "lag.poly0", "Estimate"]
    lag0.test.se <- coef(summary(res1))[ "lag.poly0", "Cluster s.e."]
    lag0.test.pval <- coef(summary(res1))[ "lag.poly0", "Pr(>|t|)"]

    ## 1st lag
    lag1.test <- glht(res1, linfct = "lag.poly0 + lag.poly1 + lag.poly2 = 0")
    lag1.test.est <- coef(summary(lag1.test))[[1]]
    lag1.test.se <- sqrt(vcov(summary(lag1.test)))[[1]]
    lag1.test.pval <- 2*(1 - pnorm(abs(lag1.test.est/lag1.test.se)))

    ## 2nd lag
    lag2.test <- glht(res1, linfct = "lag.poly0 + 2*lag.poly1 + 4*lag.poly2 = 0")
    lag2.test.est <- coef(summary(lag2.test))[[1]]
    lag2.test.se <- sqrt(vcov(summary(lag2.test)))[[1]]
    lag2.test.pval <- 2*(1 - pnorm(abs(lag2.test.est/lag2.test.se)))

    ## 3rd lag
    lag3.test <- glht(res1, linfct = "lag.poly0 + 3*lag.poly1 + 9*lag.poly2 = 0")
    lag3.test.est <- coef(summary(lag3.test))[[1]]
    lag3.test.se <- sqrt(vcov(summary(lag3.test)))[[1]]
    lag3.test.pval <- 2*(1 - pnorm(abs(lag3.test.est/lag3.test.se)))

    ## 4th lag
    lag4.test <- glht(res1, linfct = "lag.poly0 + 4*lag.poly1 + 16*lag.poly2 = 0")
    lag4.test.est <- coef(summary(lag4.test))[[1]]
    lag4.test.se <- sqrt(vcov(summary(lag4.test)))[[1]]
    lag4.test.pval <- 2*(1 - pnorm(abs(lag4.test.est/lag4.test.se)))


    ## sum lags
    flog.info("Summing lags...")
    lag.test <- glht(res1, linfct = "5*lag.poly0 + 10*lag.poly1 + 30*lag.poly2 = 0")
    lag.test.est <- coef(summary(lag.test))[[1]]
    lag.test.se <- sqrt(vcov(summary(lag.test)))[[1]]
    lag.test.pval <- 2*(1 - pnorm(abs(lag.test.est/lag.test.se)))


    ## linear hypothesis results
    lp.dt <- data.table(
      rn = c("lead4.D.ln_sales_tax", "lead3.D.ln_sales_tax", "lead2.D.ln_sales_tax", "lead1.D.ln_sales_tax", "Pre.D.ln_sales_tax", "lag0.D.ln_sales_tax", "lag1.D.ln_sales_tax", "lag2.D.ln_sales_tax", "lag3.D.ln_sales_tax", "lag4.D.ln_sales_tax", "Post.D.ln_sales_tax"),
      Estimate = c(lead4.test.est, lead3.test.est, lead2.test.est, lead1.test.est, lead.test.est, lag0.test.est, lag1.test.est, lag2.test.est, lag3.test.est, lag4.test.est, lag.test.est),
      `Cluster s.e.` = c(lead4.test.se, lead3.test.se, lead2.test.se, lead1.test.se, lead.test.se, lag0.test.se, lag1.test.se, lag2.test.se, lag3.test.se, lag4.test.se, lag.test.se),
      `Pr(>|t|)` = c(lead4.test.pval, lead3.test.pval, lead2.test.pval, lead1.test.pval, lead.test.pval, lag0.test.pval, lag1.test.pval, lag2.test.pval, lag3.test.pval, lag4.test.pval, lag.test.pval),
      outcome = Y,
      controls = FE,
      econ = "none",
      lag.econ = NA,
      lead.econ = NA,
      poly = 2,
      Rsq = summary(res1)$r.squared,
      adj.Rsq = summary(res1)$adj.r.squared)
    LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)


    ## Second: 2nd degree polynomial for leads and 3rd degree polynomial for lags
    formula1 <- as.formula(paste0(
      Y, "~ lead.poly0 + lead.poly1 + lead.poly2 + lag.poly0 + lag.poly1 + lag.poly2 + lag.poly3 |", FE, " | 0 | module_by_state"
    ))
    flog.info("Estimating with %s as outcome with %s FE.", Y, FE)
    res1 <- felm(formula = formula1, data = all_pi,
                 weights = all_pi$base.sales)
    flog.info("Finished estimating with %s as outcome with %s FE.", Y, FE)


    ## attach results
    flog.info("Writing results...")
    res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
    res1.dt[, outcome := Y]
    res1.dt[, controls := FE]
    res1.dt[, econ := "none"]
    res1.dt[, lag.econ := NA]
    res1.dt[, lead.econ := NA]
    res1.dt[, poly := 3]
    res1.dt[, Rsq := summary(res1)$r.squared]
    res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)

    ## 4th lead
    lead4.test <- glht(res1, linfct = "lead.poly0 + lead.poly1*4 + lead.poly2*16 = 0")
    lead4.test.est <- coef(summary(lead4.test))[[1]]
    lead4.test.se <- sqrt(vcov(summary(lead4.test)))[[1]]
    lead4.test.pval <- 2*(1 - pnorm(abs(lead4.test.est/lead4.test.se)))

    ## 3rd lead
    lead3.test <- glht(res1, linfct = "lead.poly0 + lead.poly1*3 + lead.poly2*9 = 0")
    lead3.test.est <- coef(summary(lead3.test))[[1]]
    lead3.test.se <- sqrt(vcov(summary(lead3.test)))[[1]]
    lead3.test.pval <- 2*(1 - pnorm(abs(lead3.test.est/lead3.test.se)))

    ## 2nd lead
    lead2.test <- glht(res1, linfct = "lead.poly0 + lead.poly1*2 + lead.poly2*4 = 0")
    lead2.test.est <- coef(summary(lead2.test))[[1]]
    lead2.test.se <- sqrt(vcov(summary(lead2.test)))[[1]]
    lead2.test.pval <- 2*(1 - pnorm(abs(lead2.test.est/lead2.test.se)))

    ## 1st lead
    lead1.test <- glht(res1, linfct = "lead.poly0 + lead.poly1 + lead.poly2 = 0")
    lead1.test.est <- coef(summary(lead1.test))[[1]]
    lead1.test.se <- sqrt(vcov(summary(lead1.test)))[[1]]
    lead1.test.pval <- 2*(1 - pnorm(abs(lead1.test.est/lead1.test.se)))


    ## sum leads
    flog.info("Summing leads...")
    lead.test <- glht(res1, linfct = "4*lead.poly0 + 10*lead.poly1 + 30*lead.poly2 = 0")
    lead.test.est <- coef(summary(lead.test))[[1]]
    lead.test.se <- sqrt(vcov(summary(lead.test)))[[1]]
    lead.test.pval <- 2*(1 - pnorm(abs(lead.test.est/lead.test.se)))

    ## On Impact --> Effect = coefficient on lag.poly0
    lag0.test.est <- coef(summary(res1))[ "lag.poly0", "Estimate"]
    lag0.test.se <- coef(summary(res1))[ "lag.poly0", "Cluster s.e."]
    lag0.test.pval <- coef(summary(res1))[ "lag.poly0", "Pr(>|t|)"]

    ## 1st lag
    lag1.test <- glht(res1, linfct = "lag.poly0 + lag.poly1 + lag.poly2 + lag.poly3 = 0")
    lag1.test.est <- coef(summary(lag1.test))[[1]]
    lag1.test.se <- sqrt(vcov(summary(lag1.test)))[[1]]
    lag1.test.pval <- 2*(1 - pnorm(abs(lag1.test.est/lag1.test.se)))

    ## 2nd lag
    lag2.test <- glht(res1, linfct = "lag.poly0 + 2*lag.poly1 + 4*lag.poly2 + 8*lag.poly3 = 0")
    lag2.test.est <- coef(summary(lag2.test))[[1]]
    lag2.test.se <- sqrt(vcov(summary(lag2.test)))[[1]]
    lag2.test.pval <- 2*(1 - pnorm(abs(lag2.test.est/lag2.test.se)))

    ## 3rd lag
    lag3.test <- glht(res1, linfct = "lag.poly0 + 3*lag.poly1 + 9*lag.poly2 + 27*lag.poly3 = 0")
    lag3.test.est <- coef(summary(lag3.test))[[1]]
    lag3.test.se <- sqrt(vcov(summary(lag3.test)))[[1]]
    lag3.test.pval <- 2*(1 - pnorm(abs(lag3.test.est/lag3.test.se)))

    ## 4th lag
    lag4.test <- glht(res1, linfct = "lag.poly0 + 4*lag.poly1 + 16*lag.poly2 + 64*lag.poly3 = 0")
    lag4.test.est <- coef(summary(lag4.test))[[1]]
    lag4.test.se <- sqrt(vcov(summary(lag4.test)))[[1]]
    lag4.test.pval <- 2*(1 - pnorm(abs(lag4.test.est/lag4.test.se)))


    ## sum lags
    flog.info("Summing lags...")
    lag.test <- glht(res1, linfct = "5*lag.poly0 + 10*lag.poly1 + 30*lag.poly2 + 100*lag.poly3 = 0")
    lag.test.est <- coef(summary(lag.test))[[1]]
    lag.test.se <- sqrt(vcov(summary(lag.test)))[[1]]
    lag.test.pval <- 2*(1 - pnorm(abs(lag.test.est/lag.test.se)))


    ## linear hypothesis results
    lp.dt <- data.table(
      rn = c("lead4.D.ln_sales_tax", "lead3.D.ln_sales_tax", "lead2.D.ln_sales_tax", "lead1.D.ln_sales_tax", "Pre.D.ln_sales_tax", "lag0.D.ln_sales_tax", "lag1.D.ln_sales_tax", "lag2.D.ln_sales_tax", "lag3.D.ln_sales_tax", "lag4.D.ln_sales_tax", "Post.D.ln_sales_tax"),
      Estimate = c(lead4.test.est, lead3.test.est, lead2.test.est, lead1.test.est, lead.test.est, lag0.test.est, lag1.test.est, lag2.test.est, lag3.test.est, lag4.test.est, lag.test.est),
      `Cluster s.e.` = c(lead4.test.se, lead3.test.se, lead2.test.se, lead1.test.se, lead.test.se, lag0.test.se, lag1.test.se, lag2.test.se, lag3.test.se, lag4.test.se, lag.test.se),
      `Pr(>|t|)` = c(lead4.test.pval, lead3.test.pval, lead2.test.pval, lead1.test.pval, lead.test.pval, lag0.test.pval, lag1.test.pval, lag2.test.pval, lag3.test.pval, lag4.test.pval, lag.test.pval),
      outcome = Y,
      controls = FE,
      econ = "none",
      lag.econ = NA,
      lead.econ = NA,
      poly = 3,
      Rsq = summary(res1)$r.squared,
      adj.Rsq = summary(res1)$adj.r.squared)
    LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)


    ##
    for(EC in Econ_opts) {

      ## First: 2nd degree polynomial for leads and lags
      formula1 <- as.formula(paste0(
        Y, "~ lead.poly0 + lead.poly1 + lead.poly2 + lag.poly0 + lag.poly1 + lag.poly2 + ", EC, " |", FE, " | 0 | module_by_state"
      ))
      flog.info("Estimating with %s as outcome with %s FE.", Y, FE)
      res1 <- felm(formula = formula1, data = all_pi,
                   weights = all_pi$base.sales)
      flog.info("Finished estimating with %s as outcome with %s FE.", Y, FE)


      ## attach results
      flog.info("Writing results...")
      res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
      res1.dt[, outcome := Y]
      res1.dt[, controls := FE]
      res1.dt[, econ := EC]
      res1.dt[, lag.econ := "No"]
      res1.dt[, lead.econ := "No"]
      res1.dt[, poly := 2]
      res1.dt[, Rsq := summary(res1)$r.squared]
      res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
      LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
      fwrite(LRdiff_res, output.results.file)

      ## 4th lead
      lead4.test <- glht(res1, linfct = "lead.poly0 + lead.poly1*4 + lead.poly2*16 = 0")
      lead4.test.est <- coef(summary(lead4.test))[[1]]
      lead4.test.se <- sqrt(vcov(summary(lead4.test)))[[1]]
      lead4.test.pval <- 2*(1 - pnorm(abs(lead4.test.est/lead4.test.se)))

      ## 3rd lead
      lead3.test <- glht(res1, linfct = "lead.poly0 + lead.poly1*3 + lead.poly2*9 = 0")
      lead3.test.est <- coef(summary(lead3.test))[[1]]
      lead3.test.se <- sqrt(vcov(summary(lead3.test)))[[1]]
      lead3.test.pval <- 2*(1 - pnorm(abs(lead3.test.est/lead3.test.se)))

      ## 2nd lead
      lead2.test <- glht(res1, linfct = "lead.poly0 + lead.poly1*2 + lead.poly2*4 = 0")
      lead2.test.est <- coef(summary(lead2.test))[[1]]
      lead2.test.se <- sqrt(vcov(summary(lead2.test)))[[1]]
      lead2.test.pval <- 2*(1 - pnorm(abs(lead2.test.est/lead2.test.se)))

      ## 1st lead
      lead1.test <- glht(res1, linfct = "lead.poly0 + lead.poly1 + lead.poly2 = 0")
      lead1.test.est <- coef(summary(lead1.test))[[1]]
      lead1.test.se <- sqrt(vcov(summary(lead1.test)))[[1]]
      lead1.test.pval <- 2*(1 - pnorm(abs(lead1.test.est/lead1.test.se)))


      ## sum leads
      flog.info("Summing leads...")
      lead.test <- glht(res1, linfct = "4*lead.poly0 + 10*lead.poly1 + 30*lead.poly2 = 0")
      lead.test.est <- coef(summary(lead.test))[[1]]
      lead.test.se <- sqrt(vcov(summary(lead.test)))[[1]]
      lead.test.pval <- 2*(1 - pnorm(abs(lead.test.est/lead.test.se)))

      ## On Impact --> Effect = coefficient on lag.poly0
      lag0.test.est <- coef(summary(res1))[ "lag.poly0", "Estimate"]
      lag0.test.se <- coef(summary(res1))[ "lag.poly0", "Cluster s.e."]
      lag0.test.pval <- coef(summary(res1))[ "lag.poly0", "Pr(>|t|)"]

      ## 1st lag
      lag1.test <- glht(res1, linfct = "lag.poly0 + lag.poly1 + lag.poly2 = 0")
      lag1.test.est <- coef(summary(lag1.test))[[1]]
      lag1.test.se <- sqrt(vcov(summary(lag1.test)))[[1]]
      lag1.test.pval <- 2*(1 - pnorm(abs(lag1.test.est/lag1.test.se)))

      ## 2nd lag
      lag2.test <- glht(res1, linfct = "lag.poly0 + 2*lag.poly1 + 4*lag.poly2 = 0")
      lag2.test.est <- coef(summary(lag2.test))[[1]]
      lag2.test.se <- sqrt(vcov(summary(lag2.test)))[[1]]
      lag2.test.pval <- 2*(1 - pnorm(abs(lag2.test.est/lag2.test.se)))

      ## 3rd lag
      lag3.test <- glht(res1, linfct = "lag.poly0 + 3*lag.poly1 + 9*lag.poly2 = 0")
      lag3.test.est <- coef(summary(lag3.test))[[1]]
      lag3.test.se <- sqrt(vcov(summary(lag3.test)))[[1]]
      lag3.test.pval <- 2*(1 - pnorm(abs(lag3.test.est/lag3.test.se)))

      ## 4th lag
      lag4.test <- glht(res1, linfct = "lag.poly0 + 4*lag.poly1 + 16*lag.poly2 = 0")
      lag4.test.est <- coef(summary(lag4.test))[[1]]
      lag4.test.se <- sqrt(vcov(summary(lag4.test)))[[1]]
      lag4.test.pval <- 2*(1 - pnorm(abs(lag4.test.est/lag4.test.se)))


      ## sum lags
      flog.info("Summing lags...")
      lag.test <- glht(res1, linfct = "5*lag.poly0 + 10*lag.poly1 + 30*lag.poly2 = 0")
      lag.test.est <- coef(summary(lag.test))[[1]]
      lag.test.se <- sqrt(vcov(summary(lag.test)))[[1]]
      lag.test.pval <- 2*(1 - pnorm(abs(lag.test.est/lag.test.se)))


      ## linear hypothesis results
      lp.dt <- data.table(
        rn = c("lead4.D.ln_sales_tax", "lead3.D.ln_sales_tax", "lead2.D.ln_sales_tax", "lead1.D.ln_sales_tax", "Pre.D.ln_sales_tax", "lag0.D.ln_sales_tax", "lag1.D.ln_sales_tax", "lag2.D.ln_sales_tax", "lag3.D.ln_sales_tax", "lag4.D.ln_sales_tax", "Post.D.ln_sales_tax"),
        Estimate = c(lead4.test.est, lead3.test.est, lead2.test.est, lead1.test.est, lead.test.est, lag0.test.est, lag1.test.est, lag2.test.est, lag3.test.est, lag4.test.est, lag.test.est),
        `Cluster s.e.` = c(lead4.test.se, lead3.test.se, lead2.test.se, lead1.test.se, lead.test.se, lag0.test.se, lag1.test.se, lag2.test.se, lag3.test.se, lag4.test.se, lag.test.se),
        `Pr(>|t|)` = c(lead4.test.pval, lead3.test.pval, lead2.test.pval, lead1.test.pval, lead.test.pval, lag0.test.pval, lag1.test.pval, lag2.test.pval, lag3.test.pval, lag4.test.pval, lag.test.pval),
        outcome = Y,
        controls = FE,
        econ = EC,
        lag.econ = "No",
        lead.econ = "No",
        poly = 2,
        Rsq = summary(res1)$r.squared,
        adj.Rsq = summary(res1)$adj.r.squared)
      LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T)
      fwrite(LRdiff_res, output.results.file)


      ## Second: 2nd degree polynomial for leads and 3rd degree polynomial for lags
      formula1 <- as.formula(paste0(
        Y, "~ lead.poly0 + lead.poly1 + lead.poly2 + lag.poly0 + lag.poly1 + lag.poly2 + lag.poly3 + ", EC, " |", FE, " | 0 | module_by_state"
      ))
      flog.info("Estimating with %s as outcome with %s FE.", Y, FE)
      res1 <- felm(formula = formula1, data = all_pi,
                   weights = all_pi$base.sales)
      flog.info("Finished estimating with %s as outcome with %s FE.", Y, FE)


      ## attach results
      flog.info("Writing results...")
      res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
      res1.dt[, outcome := Y]
      res1.dt[, controls := FE]
      res1.dt[, econ := EC]
      res1.dt[, lag.econ := "No"]
      res1.dt[, lead.econ := "No"]
      res1.dt[, poly := 3]
      res1.dt[, Rsq := summary(res1)$r.squared]
      res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
      LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
      fwrite(LRdiff_res, output.results.file)

      ## 4th lead
      lead4.test <- glht(res1, linfct = "lead.poly0 + lead.poly1*4 + lead.poly2*16 = 0")
      lead4.test.est <- coef(summary(lead4.test))[[1]]
      lead4.test.se <- sqrt(vcov(summary(lead4.test)))[[1]]
      lead4.test.pval <- 2*(1 - pnorm(abs(lead4.test.est/lead4.test.se)))

      ## 3rd lead
      lead3.test <- glht(res1, linfct = "lead.poly0 + lead.poly1*3 + lead.poly2*9 = 0")
      lead3.test.est <- coef(summary(lead3.test))[[1]]
      lead3.test.se <- sqrt(vcov(summary(lead3.test)))[[1]]
      lead3.test.pval <- 2*(1 - pnorm(abs(lead3.test.est/lead3.test.se)))

      ## 2nd lead
      lead2.test <- glht(res1, linfct = "lead.poly0 + lead.poly1*2 + lead.poly2*4 = 0")
      lead2.test.est <- coef(summary(lead2.test))[[1]]
      lead2.test.se <- sqrt(vcov(summary(lead2.test)))[[1]]
      lead2.test.pval <- 2*(1 - pnorm(abs(lead2.test.est/lead2.test.se)))

      ## 1st lead
      lead1.test <- glht(res1, linfct = "lead.poly0 + lead.poly1 + lead.poly2 = 0")
      lead1.test.est <- coef(summary(lead1.test))[[1]]
      lead1.test.se <- sqrt(vcov(summary(lead1.test)))[[1]]
      lead1.test.pval <- 2*(1 - pnorm(abs(lead1.test.est/lead1.test.se)))


      ## sum leads
      flog.info("Summing leads...")
      lead.test <- glht(res1, linfct = "4*lead.poly0 + 10*lead.poly1 + 30*lead.poly2 = 0")
      lead.test.est <- coef(summary(lead.test))[[1]]
      lead.test.se <- sqrt(vcov(summary(lead.test)))[[1]]
      lead.test.pval <- 2*(1 - pnorm(abs(lead.test.est/lead.test.se)))

      ## On Impact --> Effect = coefficient on lag.poly0
      lag0.test.est <- coef(summary(res1))[ "lag.poly0", "Estimate"]
      lag0.test.se <- coef(summary(res1))[ "lag.poly0", "Cluster s.e."]
      lag0.test.pval <- coef(summary(res1))[ "lag.poly0", "Pr(>|t|)"]

      ## 1st lag
      lag1.test <- glht(res1, linfct = "lag.poly0 + lag.poly1 + lag.poly2 + lag.poly3 = 0")
      lag1.test.est <- coef(summary(lag1.test))[[1]]
      lag1.test.se <- sqrt(vcov(summary(lag1.test)))[[1]]
      lag1.test.pval <- 2*(1 - pnorm(abs(lag1.test.est/lag1.test.se)))

      ## 2nd lag
      lag2.test <- glht(res1, linfct = "lag.poly0 + 2*lag.poly1 + 4*lag.poly2 + 8*lag.poly3 = 0")
      lag2.test.est <- coef(summary(lag2.test))[[1]]
      lag2.test.se <- sqrt(vcov(summary(lag2.test)))[[1]]
      lag2.test.pval <- 2*(1 - pnorm(abs(lag2.test.est/lag2.test.se)))

      ## 3rd lag
      lag3.test <- glht(res1, linfct = "lag.poly0 + 3*lag.poly1 + 9*lag.poly2 + 27*lag.poly3 = 0")
      lag3.test.est <- coef(summary(lag3.test))[[1]]
      lag3.test.se <- sqrt(vcov(summary(lag3.test)))[[1]]
      lag3.test.pval <- 2*(1 - pnorm(abs(lag3.test.est/lag3.test.se)))

      ## 4th lag
      lag4.test <- glht(res1, linfct = "lag.poly0 + 4*lag.poly1 + 16*lag.poly2 + 64*lag.poly3 = 0")
      lag4.test.est <- coef(summary(lag4.test))[[1]]
      lag4.test.se <- sqrt(vcov(summary(lag4.test)))[[1]]
      lag4.test.pval <- 2*(1 - pnorm(abs(lag4.test.est/lag4.test.se)))


      ## sum lags
      flog.info("Summing lags...")
      lag.test <- glht(res1, linfct = "5*lag.poly0 + 10*lag.poly1 + 30*lag.poly2 + 100*lag.poly3 = 0")
      lag.test.est <- coef(summary(lag.test))[[1]]
      lag.test.se <- sqrt(vcov(summary(lag.test)))[[1]]
      lag.test.pval <- 2*(1 - pnorm(abs(lag.test.est/lag.test.se)))


      ## linear hypothesis results
      lp.dt <- data.table(
        rn = c("lead4.D.ln_sales_tax", "lead3.D.ln_sales_tax", "lead2.D.ln_sales_tax", "lead1.D.ln_sales_tax", "Pre.D.ln_sales_tax", "lag0.D.ln_sales_tax", "lag1.D.ln_sales_tax", "lag2.D.ln_sales_tax", "lag3.D.ln_sales_tax", "lag4.D.ln_sales_tax", "Post.D.ln_sales_tax"),
        Estimate = c(lead4.test.est, lead3.test.est, lead2.test.est, lead1.test.est, lead.test.est, lag0.test.est, lag1.test.est, lag2.test.est, lag3.test.est, lag4.test.est, lag.test.est),
        `Cluster s.e.` = c(lead4.test.se, lead3.test.se, lead2.test.se, lead1.test.se, lead.test.se, lag0.test.se, lag1.test.se, lag2.test.se, lag3.test.se, lag4.test.se, lag.test.se),
        `Pr(>|t|)` = c(lead4.test.pval, lead3.test.pval, lead2.test.pval, lead1.test.pval, lead.test.pval, lag0.test.pval, lag1.test.pval, lag2.test.pval, lag3.test.pval, lag4.test.pval, lag.test.pval),
        outcome = Y,
        controls = FE,
        econ = EC,
        lag.econ = "No",
        lead.econ = "No",
        poly = 3,
        Rsq = summary(res1)$r.squared,
        adj.Rsq = summary(res1)$adj.r.squared)
      LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T)
      fwrite(LRdiff_res, output.results.file)

    }


    ##
    for(i in 1:dim(Econ_w_lags)[2]) {


      EC <- Econ_w_lags[4, i]


      ## First: 2nd degree polynomial for leads and lags
      formula1 <- as.formula(paste0(
        Y, "~ lead.poly0 + lead.poly1 + lead.poly2 + lag.poly0 + lag.poly1 + lag.poly2 + ", EC, " |", FE, " | 0 | module_by_state"
      ))
      flog.info("Estimating with %s as outcome with %s FE.", Y, FE)
      res1 <- felm(formula = formula1, data = all_pi,
                   weights = all_pi$base.sales)
      flog.info("Finished estimating with %s as outcome with %s FE.", Y, FE)


      ## attach results
      flog.info("Writing results...")
      res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
      res1.dt[, outcome := Y]
      res1.dt[, controls := FE]
      res1.dt[, econ := Econ_w_lags[1, i]]
      res1.dt[, lag.econ := Econ_w_lags[2, i]]
      res1.dt[, lead.econ := Econ_w_lags[3,i]]
      res1.dt[, poly := 2]
      res1.dt[, Rsq := summary(res1)$r.squared]
      res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
      LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
      fwrite(LRdiff_res, output.results.file)

      ## 4th lead
      lead4.test <- glht(res1, linfct = "lead.poly0 + lead.poly1*4 + lead.poly2*16 = 0")
      lead4.test.est <- coef(summary(lead4.test))[[1]]
      lead4.test.se <- sqrt(vcov(summary(lead4.test)))[[1]]
      lead4.test.pval <- 2*(1 - pnorm(abs(lead4.test.est/lead4.test.se)))

      ## 3rd lead
      lead3.test <- glht(res1, linfct = "lead.poly0 + lead.poly1*3 + lead.poly2*9 = 0")
      lead3.test.est <- coef(summary(lead3.test))[[1]]
      lead3.test.se <- sqrt(vcov(summary(lead3.test)))[[1]]
      lead3.test.pval <- 2*(1 - pnorm(abs(lead3.test.est/lead3.test.se)))

      ## 2nd lead
      lead2.test <- glht(res1, linfct = "lead.poly0 + lead.poly1*2 + lead.poly2*4 = 0")
      lead2.test.est <- coef(summary(lead2.test))[[1]]
      lead2.test.se <- sqrt(vcov(summary(lead2.test)))[[1]]
      lead2.test.pval <- 2*(1 - pnorm(abs(lead2.test.est/lead2.test.se)))

      ## 1st lead
      lead1.test <- glht(res1, linfct = "lead.poly0 + lead.poly1 + lead.poly2 = 0")
      lead1.test.est <- coef(summary(lead1.test))[[1]]
      lead1.test.se <- sqrt(vcov(summary(lead1.test)))[[1]]
      lead1.test.pval <- 2*(1 - pnorm(abs(lead1.test.est/lead1.test.se)))


      ## sum leads
      flog.info("Summing leads...")
      lead.test <- glht(res1, linfct = "4*lead.poly0 + 10*lead.poly1 + 30*lead.poly2 = 0")
      lead.test.est <- coef(summary(lead.test))[[1]]
      lead.test.se <- sqrt(vcov(summary(lead.test)))[[1]]
      lead.test.pval <- 2*(1 - pnorm(abs(lead.test.est/lead.test.se)))

      ## On Impact --> Effect = coefficient on lag.poly0
      lag0.test.est <- coef(summary(res1))[ "lag.poly0", "Estimate"]
      lag0.test.se <- coef(summary(res1))[ "lag.poly0", "Cluster s.e."]
      lag0.test.pval <- coef(summary(res1))[ "lag.poly0", "Pr(>|t|)"]

      ## 1st lag
      lag1.test <- glht(res1, linfct = "lag.poly0 + lag.poly1 + lag.poly2 = 0")
      lag1.test.est <- coef(summary(lag1.test))[[1]]
      lag1.test.se <- sqrt(vcov(summary(lag1.test)))[[1]]
      lag1.test.pval <- 2*(1 - pnorm(abs(lag1.test.est/lag1.test.se)))

      ## 2nd lag
      lag2.test <- glht(res1, linfct = "lag.poly0 + 2*lag.poly1 + 4*lag.poly2 = 0")
      lag2.test.est <- coef(summary(lag2.test))[[1]]
      lag2.test.se <- sqrt(vcov(summary(lag2.test)))[[1]]
      lag2.test.pval <- 2*(1 - pnorm(abs(lag2.test.est/lag2.test.se)))

      ## 3rd lag
      lag3.test <- glht(res1, linfct = "lag.poly0 + 3*lag.poly1 + 9*lag.poly2 = 0")
      lag3.test.est <- coef(summary(lag3.test))[[1]]
      lag3.test.se <- sqrt(vcov(summary(lag3.test)))[[1]]
      lag3.test.pval <- 2*(1 - pnorm(abs(lag3.test.est/lag3.test.se)))

      ## 4th lag
      lag4.test <- glht(res1, linfct = "lag.poly0 + 4*lag.poly1 + 16*lag.poly2 = 0")
      lag4.test.est <- coef(summary(lag4.test))[[1]]
      lag4.test.se <- sqrt(vcov(summary(lag4.test)))[[1]]
      lag4.test.pval <- 2*(1 - pnorm(abs(lag4.test.est/lag4.test.se)))


      ## sum lags
      flog.info("Summing lags...")
      lag.test <- glht(res1, linfct = "5*lag.poly0 + 10*lag.poly1 + 30*lag.poly2 = 0")
      lag.test.est <- coef(summary(lag.test))[[1]]
      lag.test.se <- sqrt(vcov(summary(lag.test)))[[1]]
      lag.test.pval <- 2*(1 - pnorm(abs(lag.test.est/lag.test.se)))


      ## linear hypothesis results
      lp.dt <- data.table(
        rn = c("lead4.D.ln_sales_tax", "lead3.D.ln_sales_tax", "lead2.D.ln_sales_tax", "lead1.D.ln_sales_tax", "Pre.D.ln_sales_tax", "lag0.D.ln_sales_tax", "lag1.D.ln_sales_tax", "lag2.D.ln_sales_tax", "lag3.D.ln_sales_tax", "lag4.D.ln_sales_tax", "Post.D.ln_sales_tax"),
        Estimate = c(lead4.test.est, lead3.test.est, lead2.test.est, lead1.test.est, lead.test.est, lag0.test.est, lag1.test.est, lag2.test.est, lag3.test.est, lag4.test.est, lag.test.est),
        `Cluster s.e.` = c(lead4.test.se, lead3.test.se, lead2.test.se, lead1.test.se, lead.test.se, lag0.test.se, lag1.test.se, lag2.test.se, lag3.test.se, lag4.test.se, lag.test.se),
        `Pr(>|t|)` = c(lead4.test.pval, lead3.test.pval, lead2.test.pval, lead1.test.pval, lead.test.pval, lag0.test.pval, lag1.test.pval, lag2.test.pval, lag3.test.pval, lag4.test.pval, lag.test.pval),
        outcome = Y,
        controls = FE,
        econ = Econ_w_lags[1, i],
        lag.econ = Econ_w_lags[2, i],
        lead.econ = Econ_w_lags[3,i],
        poly = 2,
        Rsq = summary(res1)$r.squared,
        adj.Rsq = summary(res1)$adj.r.squared)
      LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T)
      fwrite(LRdiff_res, output.results.file)


      ## Second: 2nd degree polynomial for leads and 3rd degree polynomial for lags
      formula1 <- as.formula(paste0(
        Y, "~ lead.poly0 + lead.poly1 + lead.poly2 + lag.poly0 + lag.poly1 + lag.poly2 + lag.poly3 + ", EC, " |", FE, " | 0 | module_by_state"
      ))
      flog.info("Estimating with %s as outcome with %s FE.", Y, FE)
      res1 <- felm(formula = formula1, data = all_pi,
                   weights = all_pi$base.sales)
      flog.info("Finished estimating with %s as outcome with %s FE.", Y, FE)


      ## attach results
      flog.info("Writing results...")
      res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
      res1.dt[, outcome := Y]
      res1.dt[, controls := FE]
      res1.dt[, econ := Econ_w_lags[1, i]]
      res1.dt[, lag.econ := Econ_w_lags[2, i]]
      res1.dt[, lead.econ := Econ_w_lags[3,i]]
      res1.dt[, poly := 3]
      res1.dt[, Rsq := summary(res1)$r.squared]
      res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
      LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
      fwrite(LRdiff_res, output.results.file)

      ## 4th lead
      lead4.test <- glht(res1, linfct = "lead.poly0 + lead.poly1*4 + lead.poly2*16 = 0")
      lead4.test.est <- coef(summary(lead4.test))[[1]]
      lead4.test.se <- sqrt(vcov(summary(lead4.test)))[[1]]
      lead4.test.pval <- 2*(1 - pnorm(abs(lead4.test.est/lead4.test.se)))

      ## 3rd lead
      lead3.test <- glht(res1, linfct = "lead.poly0 + lead.poly1*3 + lead.poly2*9 = 0")
      lead3.test.est <- coef(summary(lead3.test))[[1]]
      lead3.test.se <- sqrt(vcov(summary(lead3.test)))[[1]]
      lead3.test.pval <- 2*(1 - pnorm(abs(lead3.test.est/lead3.test.se)))

      ## 2nd lead
      lead2.test <- glht(res1, linfct = "lead.poly0 + lead.poly1*2 + lead.poly2*4 = 0")
      lead2.test.est <- coef(summary(lead2.test))[[1]]
      lead2.test.se <- sqrt(vcov(summary(lead2.test)))[[1]]
      lead2.test.pval <- 2*(1 - pnorm(abs(lead2.test.est/lead2.test.se)))

      ## 1st lead
      lead1.test <- glht(res1, linfct = "lead.poly0 + lead.poly1 + lead.poly2 = 0")
      lead1.test.est <- coef(summary(lead1.test))[[1]]
      lead1.test.se <- sqrt(vcov(summary(lead1.test)))[[1]]
      lead1.test.pval <- 2*(1 - pnorm(abs(lead1.test.est/lead1.test.se)))


      ## sum leads
      flog.info("Summing leads...")
      lead.test <- glht(res1, linfct = "4*lead.poly0 + 10*lead.poly1 + 30*lead.poly2 = 0")
      lead.test.est <- coef(summary(lead.test))[[1]]
      lead.test.se <- sqrt(vcov(summary(lead.test)))[[1]]
      lead.test.pval <- 2*(1 - pnorm(abs(lead.test.est/lead.test.se)))

      ## On Impact --> Effect = coefficient on lag.poly0
      lag0.test.est <- coef(summary(res1))[ "lag.poly0", "Estimate"]
      lag0.test.se <- coef(summary(res1))[ "lag.poly0", "Cluster s.e."]
      lag0.test.pval <- coef(summary(res1))[ "lag.poly0", "Pr(>|t|)"]

      ## 1st lag
      lag1.test <- glht(res1, linfct = "lag.poly0 + lag.poly1 + lag.poly2 + lag.poly3 = 0")
      lag1.test.est <- coef(summary(lag1.test))[[1]]
      lag1.test.se <- sqrt(vcov(summary(lag1.test)))[[1]]
      lag1.test.pval <- 2*(1 - pnorm(abs(lag1.test.est/lag1.test.se)))

      ## 2nd lag
      lag2.test <- glht(res1, linfct = "lag.poly0 + 2*lag.poly1 + 4*lag.poly2 + 8*lag.poly3 = 0")
      lag2.test.est <- coef(summary(lag2.test))[[1]]
      lag2.test.se <- sqrt(vcov(summary(lag2.test)))[[1]]
      lag2.test.pval <- 2*(1 - pnorm(abs(lag2.test.est/lag2.test.se)))

      ## 3rd lag
      lag3.test <- glht(res1, linfct = "lag.poly0 + 3*lag.poly1 + 9*lag.poly2 + 27*lag.poly3 = 0")
      lag3.test.est <- coef(summary(lag3.test))[[1]]
      lag3.test.se <- sqrt(vcov(summary(lag3.test)))[[1]]
      lag3.test.pval <- 2*(1 - pnorm(abs(lag3.test.est/lag3.test.se)))

      ## 4th lag
      lag4.test <- glht(res1, linfct = "lag.poly0 + 4*lag.poly1 + 16*lag.poly2 + 64*lag.poly3 = 0")
      lag4.test.est <- coef(summary(lag4.test))[[1]]
      lag4.test.se <- sqrt(vcov(summary(lag4.test)))[[1]]
      lag4.test.pval <- 2*(1 - pnorm(abs(lag4.test.est/lag4.test.se)))


      ## sum lags
      flog.info("Summing lags...")
      lag.test <- glht(res1, linfct = "5*lag.poly0 + 10*lag.poly1 + 30*lag.poly2 + 100*lag.poly3 = 0")
      lag.test.est <- coef(summary(lag.test))[[1]]
      lag.test.se <- sqrt(vcov(summary(lag.test)))[[1]]
      lag.test.pval <- 2*(1 - pnorm(abs(lag.test.est/lag.test.se)))


      ## linear hypothesis results
      lp.dt <- data.table(
        rn = c("lead4.D.ln_sales_tax", "lead3.D.ln_sales_tax", "lead2.D.ln_sales_tax", "lead1.D.ln_sales_tax", "Pre.D.ln_sales_tax", "lag0.D.ln_sales_tax", "lag1.D.ln_sales_tax", "lag2.D.ln_sales_tax", "lag3.D.ln_sales_tax", "lag4.D.ln_sales_tax", "Post.D.ln_sales_tax"),
        Estimate = c(lead4.test.est, lead3.test.est, lead2.test.est, lead1.test.est, lead.test.est, lag0.test.est, lag1.test.est, lag2.test.est, lag3.test.est, lag4.test.est, lag.test.est),
        `Cluster s.e.` = c(lead4.test.se, lead3.test.se, lead2.test.se, lead1.test.se, lead.test.se, lag0.test.se, lag1.test.se, lag2.test.se, lag3.test.se, lag4.test.se, lag.test.se),
        `Pr(>|t|)` = c(lead4.test.pval, lead3.test.pval, lead2.test.pval, lead1.test.pval, lead.test.pval, lag0.test.pval, lag1.test.pval, lag2.test.pval, lag3.test.pval, lag4.test.pval, lag.test.pval),
        outcome = Y,
        controls = FE,
        econ = Econ_w_lags[1, i],
        lag.econ = Econ_w_lags[2, i],
        lead.econ = Econ_w_lags[3,i],
        poly = 3,
        Rsq = summary(res1)$r.squared,
        adj.Rsq = summary(res1)$adj.r.squared)
      LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T)
      fwrite(LRdiff_res, output.results.file)


    }
  }
}


## summary values --------------------------------------------------------------
LRdiff_res$N_obs <- nrow(yearly_data)
LRdiff_res$N_modules <- length(unique(yearly_data$product_module_code))
LRdiff_res$N_stores <- length(unique(yearly_data$store_code_uc))
LRdiff_res$N_counties <- uniqueN(yearly_data, by = c("fips_state", "fips_county"))
LRdiff_res$N_years <- uniqueN(yearly_data, by = c("year")) # should be 6 (we lose one because we difference)
LRdiff_res$N_county_modules <- uniqueN(yearly_data, by = c("fips_state", "fips_county",
                                                           "product_module_code"))

fwrite(LRdiff_res, output.results.file)


