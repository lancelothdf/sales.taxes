#' Author: Lancelot Henry de Frahan and John Bonney
#'
#'Same as LRdiff_diff_quarterly_preferred_cpricei but we plot the cumulative effect as opposed to changes

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
data.full.path <- "Data/all_nielsen_data_2006_2016_quarterly.csv"

zillow_path <- "Data/covariates/zillow_long_by_county_clean.csv"
zillow_state_path <- "Data/covariates/zillow_long_by_state_clean.csv"
unemp.path <- "Data/covariates/county_monthly_unemp_clean.csv"


## output filepaths ----------------------------------------------
output.results.file <- "Data/LRdiff_quarterly_results_preferred_cumulative_cpricei_2nddegree.csv"


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
all_pi <- fread(data.full.path)



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

##
all_pi <- merge(all_pi, zillow_dt, by = c("fips_state", "fips_county", "year", "quarter"), all.x = T)
rm(zillow_dt)


### Unemployment data
unemp.data <- fread(unemp.path)
unemp.data <- unemp.data[, c("fips_state", "fips_county", "year", "month", "rate")]
unemp.data <- unemp.data[, quarter := ceiling((month/12)*4)]
unemp.data <- unemp.data[, list(unemp = mean(rate)), by = .(year, quarter, fips_state, fips_county)]
unemp.data <- unemp.data[year >= 2006 & year <= 2016,]
unemp.data <- unemp.data[, ln_unemp := log(unemp)]

##
all_pi <- merge(all_pi, unemp.data, by = c("fips_state", "fips_county", "year", "quarter"), all.x = T)
rm(unemp.data)


## prep the 2006-2016 data --------------------------------------- ##NOTE: in this version we do not merge to "old price indices" because they are under construction
#old_pi <- fread(old_pi_path)

## merge on the old price indices
#all_pi <- merge(all_pi, old_pi,
#                     by = c("fips_state", "fips_county", "store_code_uc",
#                            "product_module_code", "year", "quarter"), all = T)
#rm(old_pi)

## merge on the census region/division info
all_pi <- merge(all_pi, geo_dt, by = "fips_state")

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
all_pi[, base.sales := sales[year == 2008 & quarter == 1],
            by = .(store_code_uc, product_module_code)]

all_pi <- all_pi[!is.na(base.sales) & !is.na(sales) & !is.na(ln_cpricei) &
                             !is.na(ln_sales_tax) & !is.na(ln_quantity)]
#                             & !is.na(ln_quantity2) & !is.na(ln_cpricei2)]

## balance on store-module level
keep_store_modules <- all_pi[, list(n = .N),
                                  by = .(store_code_uc, product_module_code)]
keep_store_modules <- keep_store_modules[n == (2016 - 2005) * 4]

setkey(all_pi, store_code_uc, product_module_code)
setkey(keep_store_modules, store_code_uc, product_module_code)

all_pi <- all_pi[keep_store_modules]
setkey(all_pi, store_code_uc, product_module_code, year, quarter)


#############################################################
## Delete some variables to save memory
all_pi <- all_pi[, c("fips_state", "fips_county", "year", "quarter", "store_code_uc", "product_module_code", "ln_cpricei", "ln_sales_tax", "ln_quantity", "base.sales", "store_by_module", "cal_time", "module_by_time", "module_by_state", "region_by_module_by_time", "division_by_module_by_time", "ln_home_price", "ln_unemp")]


#######################################################
## take first differences of outcomes and treatment
all_pi <- all_pi[order(store_code_uc, product_module_code, cal_time),] ##Sort on store by year-quarter (in ascending order)


all_pi[, D.ln_cpricei := ln_cpricei - shift(ln_cpricei, n=1, type="lag"),
       by = .(store_code_uc, product_module_code)]

all_pi[, D.ln_quantity := ln_quantity - shift(ln_quantity, n=1, type="lag"),
       by = .(store_code_uc, product_module_code)]

all_pi[, D.ln_sales_tax := ln_sales_tax - shift(ln_sales_tax, n=1, type="lag"),
       by = .(store_code_uc, product_module_code)]

all_pi[, D.ln_unemp := ln_unemp - shift(ln_unemp, n=1, type="lag"),
       by = .(store_code_uc, product_module_code)]

all_pi[, D.ln_home_price := ln_home_price - shift(ln_home_price, n=1, type="lag"),
       by = .(store_code_uc, product_module_code)]



## generate lags and leads of ln_sales_tax
for (lag.val in 1:8) {
  lag.X <- paste0("L", lag.val, ".D.ln_sales_tax")
  all_pi[, (lag.X) := shift(D.ln_sales_tax, n=lag.val, type="lag"),
         by = .(store_code_uc, product_module_code)]

  lead.X <- paste0("F", lag.val, ".D.ln_sales_tax")
  all_pi[, (lead.X) := shift(D.ln_sales_tax, n=lag.val, type="lead"),
         by = .(store_code_uc, product_module_code)]

  lag.X <- paste0("L", lag.val, ".D.ln_unemp")
  all_pi[, (lag.X) := shift(D.ln_unemp, n=lag.val, type="lag"),
         by = .(store_code_uc, product_module_code)]

  lag.X <- paste0("L", lag.val, ".D.ln_home_price")
  all_pi[, (lag.X) := shift(D.ln_home_price, n=lag.val, type="lag"),
         by = .(store_code_uc, product_module_code)]
}



### Estimation ---------------------------------------------------
all_pi <- all_pi[between(year, 2008, 2014)]
all_pi <- all_pi[ year >= 2009 | (year == 2008 & quarter >= 2)] ## First quarter of 2008, the difference was imputed not real data - so we drop it


formula_lags <- paste0("L", 1:8, ".D.ln_sales_tax", collapse = "+")
formula_leads <- paste0("F", 1:8, ".D.ln_sales_tax", collapse = "+")
formula_RHS <- paste0("D.ln_sales_tax + ", formula_lags, "+", formula_leads)



outcomes <- c("D.ln_cpricei")
econ.outcomes <- c("D.ln_unemp", "D.ln_home_price")
FE_opts <- c("module_by_time", "region_by_module_by_time", "division_by_module_by_time")


## for linear hypothesis tests
lead.vars <- paste(paste0("F", 8:1, ".D.ln_sales_tax"), collapse = " + ")
lag.vars <- paste(paste0("L", 8:1, ".D.ln_sales_tax"), collapse = " + ")
lead.lp.restr <- paste(lead.vars, "= 0")
lag.lp.restr <- paste(lag.vars, "+ D.ln_sales_tax = 0")
total.lp.restr <- paste(lag.vars, "+", lead.vars, "+ D.ln_sales_tax = 0")



### First: We impose smoothness on lagged and lead effects of tax rate on cpricei (but allow for a one-time impact at zero)
## Still No Econ controls (and we run the placebos)

## Create sum of lag/lead tax rates interacted with lag/lead
## Stupid way to code this
all_pi[, lead.poly0 := F8.D.ln_sales_tax + F7.D.ln_sales_tax + F6.D.ln_sales_tax + F5.D.ln_sales_tax + F4.D.ln_sales_tax + F3.D.ln_sales_tax + F2.D.ln_sales_tax + F1.D.ln_sales_tax ]
all_pi[, lead.poly1 := 7*F8.D.ln_sales_tax + 6*F7.D.ln_sales_tax + 5*F6.D.ln_sales_tax + 4*F5.D.ln_sales_tax + 3*F4.D.ln_sales_tax + 2*F3.D.ln_sales_tax + F2.D.ln_sales_tax]
all_pi[, lead.poly2 := 49*F8.D.ln_sales_tax + 36*F7.D.ln_sales_tax + 25*F6.D.ln_sales_tax + 16*F5.D.ln_sales_tax + 9*F4.D.ln_sales_tax + 4*F3.D.ln_sales_tax + F2.D.ln_sales_tax]
#all_pi[, lead.poly3 := 343*F8.D.ln_sales_tax + 216*F7.D.ln_sales_tax + 125*F6.D.ln_sales_tax + 64*F5.D.ln_sales_tax + 27*F4.D.ln_sales_tax + 8*F3.D.ln_sales_tax + F2.D.ln_sales_tax]

## For lags - we fit starting from lag 1
all_pi[, lag.poly0 := L8.D.ln_sales_tax + L7.D.ln_sales_tax + L6.D.ln_sales_tax + L5.D.ln_sales_tax + L4.D.ln_sales_tax + L3.D.ln_sales_tax + L2.D.ln_sales_tax + L1.D.ln_sales_tax ]
all_pi[, lag.poly1 := 7*L8.D.ln_sales_tax + 6*L7.D.ln_sales_tax + 5*L6.D.ln_sales_tax + 4*L5.D.ln_sales_tax + 3*L4.D.ln_sales_tax + 2*L3.D.ln_sales_tax + L2.D.ln_sales_tax]
all_pi[, lag.poly2 := 49*L8.D.ln_sales_tax + 36*L7.D.ln_sales_tax + 25*L6.D.ln_sales_tax + 16*L5.D.ln_sales_tax + 9*L4.D.ln_sales_tax + 4*L3.D.ln_sales_tax + L2.D.ln_sales_tax]
#all_pi[, lag.poly3 := 343*L8.D.ln_sales_tax + 216*L7.D.ln_sales_tax + 125*L6.D.ln_sales_tax + 64*L5.D.ln_sales_tax + 27*L4.D.ln_sales_tax + 8*L3.D.ln_sales_tax + L2.D.ln_sales_tax]
#all_pi[, lag.poly4 := 2401*L8.D.ln_sales_tax + 1296*L7.D.ln_sales_tax + 625*L6.D.ln_sales_tax + 256*L5.D.ln_sales_tax + 81*L4.D.ln_sales_tax + 16*L3.D.ln_sales_tax + L2.D.ln_sales_tax]


formula_RHS <- "D.ln_sales_tax + lead.poly0 + lead.poly1 + lead.poly2 + lag.poly0 + lag.poly1 + lag.poly2"
n.poly.lead <- 2
n.poly.lag <- 2


LRdiff_res <- data.table(NULL)
for (Y in outcomes) {
  for (FE in FE_opts) {

    formula1 <- as.formula(paste0(
      Y, "~", formula_RHS, "| ", FE, " | 0 | module_by_state"
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
    res1.dt[, parametric := "Yes"]
    res1.dt[, Rsq := summary(res1)$r.squared]
    res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)


    for(j in 2:8) { ## Number of leads and lags over which effect is assumed to matter

      ###### LEADS
      ## Create a name for estimate, se and pval of each lead
      lead.test.est.name <- paste("lead", j, ".test.est", sep = "")
      lead.test.se.name <- paste("lead", j, ".test.se", sep = "")
      lead.test.pval.name <- paste("lead", j, ".test.pval", sep = "")

      ## Create the formula to compute estimate at each lead
      lead.test.form <- "lead.poly0"
      for(k in 1:n.poly.lead) {

        lead.test.form <- paste(lead.test.form, " + lead.poly", k, "*", (j-1)^k, sep = "")

      }
      lead.test.form <- paste(lead.test.form, " = 0")


      ## Compute estimate and store in variables names
      lead.test <- glht(res1, linfct = lead.test.form)

      assign(lead.test.est.name, coef(summary(lead.test))[[1]])
      assign(lead.test.se.name, sqrt(vcov(summary(lead.test)))[[1]])
      assign(lead.test.pval.name, 2*(1 - pnorm(abs(coef(summary(lead.test))[[1]]/sqrt(vcov(summary(lead.test)))[[1]]))))


      ###### LAGS
      ## Create a name for estimate, se and pval of each lead
      lag.test.est.name <- paste("lag", j, ".test.est", sep = "")
      lag.test.se.name <- paste("lag", j, ".test.se", sep = "")
      lag.test.pval.name <- paste("lag", j, ".test.pval", sep = "")

      ## Create the formula to compute estimate at each lead
      lag.test.form <- "lag.poly0"
      for(k in 1:n.poly.lag) {

        lag.test.form <- paste(lag.test.form, " + lag.poly", k, "*", (j-1)^k, sep = "")

      }
      lag.test.form <- paste(lag.test.form, " = 0")


      ## Compute estimate and store in variables names
      lag.test <- glht(res1, linfct = lag.test.form)

      assign(lag.test.est.name, coef(summary(lag.test))[[1]])
      assign(lag.test.se.name, sqrt(vcov(summary(lag.test)))[[1]])
      assign(lag.test.pval.name, 2*(1 - pnorm(abs(coef(summary(lag.test))[[1]]/sqrt(vcov(summary(lag.test)))[[1]]))))

    }

    ## On Impact --> Effect = coefficient on D.ln_sales_tax
    lag0.test.est <- coef(summary(res1))[ "D.ln_sales_tax", "Estimate"]
    lag0.test.se <- coef(summary(res1))[ "D.ln_sales_tax", "Cluster s.e."]
    lag0.test.pval <- coef(summary(res1))[ "D.ln_sales_tax", "Pr(>|t|)"]

    ##
    lead1.test.est <- coef(summary(res1))[ "lead.poly0", "Estimate"]
    lead1.test.se <- coef(summary(res1))[ "lead.poly0", "Cluster s.e."]
    lead1.test.pval <- coef(summary(res1))[ "lead.poly0", "Pr(>|t|)"]

    ##
    lag1.test.est <- coef(summary(res1))[ "lag.poly0", "Estimate"]
    lag1.test.se <- coef(summary(res1))[ "lag.poly0", "Cluster s.e."]
    lag1.test.pval <- coef(summary(res1))[ "lag.poly0", "Pr(>|t|)"]


    ## sum leads
    flog.info("Summing leads...")
    lead.test.form <- "8*lead.poly0"
    for(k in 1:n.poly.lead) {

      tot.lead.n <- 1^k + 2^k + 3^k + 4^k + 5^k + 6^k + 7^k
      lead.test.form <- paste(lead.test.form, " + ", tot.lead.n, "*lead.poly", k, sep = "")
    }
    lead.test.form <- paste(lead.test.form, " = 0", sep = "")

    lead.test <- glht(res1, linfct = lead.test.form)
    lead.test.est <- coef(summary(lead.test))[[1]]
    lead.test.se <- sqrt(vcov(summary(lead.test)))[[1]]
    lead.test.pval <- 2*(1 - pnorm(abs(lead.test.est/lead.test.se)))


    ## sum lags
    flog.info("Summing lags...")
    lag.test.form <- "D.ln_sales_tax + 8*lag.poly0"
    for(k in 1:n.poly.lag) {

      tot.lag.n <- 1^k + 2^k + 3^k + 4^k + 5^k + 6^k + 7^k
      lag.test.form <- paste(lag.test.form, " + ", tot.lag.n, "*lag.poly", k, sep = "")
    }
    lag.test.form <- paste(lag.test.form, " = 0", sep = "")
    lag.test <- glht(res1, linfct = lag.test.form)
    lag.test.est <- coef(summary(lag.test))[[1]]
    lag.test.se <- sqrt(vcov(summary(lag.test)))[[1]]
    lag.test.pval <- 2*(1 - pnorm(abs(lag.test.est/lag.test.se)))


    ## linear hypothesis results
    lp.dt <- data.table(
      rn = c("lead8.D.ln_sales_tax", "lead7.D.ln_sales_tax", "lead6.D.ln_sales_tax", "lead5.D.ln_sales_tax", "lead4.D.ln_sales_tax", "lead3.D.ln_sales_tax", "lead2.D.ln_sales_tax", "lead1.D.ln_sales_tax", "Pre.D.ln_sales_tax", "lag0.D.ln_sales_tax", "lag1.D.ln_sales_tax", "lag2.D.ln_sales_tax", "lag3.D.ln_sales_tax", "lag4.D.ln_sales_tax", "lag5.D.ln_sales_tax", "lag6.D.ln_sales_tax", "lag7.D.ln_sales_tax", "lag8.D.ln_sales_tax", "Post.D.ln_sales_tax"),
      Estimate = c(lead8.test.est, lead7.test.est, lead6.test.est, lead5.test.est, lead4.test.est, lead3.test.est, lead2.test.est, lead1.test.est, lead.test.est, lag0.test.est, lag1.test.est, lag2.test.est, lag3.test.est, lag4.test.est, lag5.test.est, lag6.test.est, lag7.test.est, lag8.test.est, lag.test.est),
      `Cluster s.e.` = c(lead8.test.se, lead7.test.se, lead6.test.se, lead5.test.se, lead4.test.se, lead3.test.se, lead2.test.se, lead1.test.se, lead.test.se, lag0.test.se, lag1.test.se, lag2.test.se, lag3.test.se, lag4.test.se, lag5.test.se, lag6.test.se, lag7.test.se, lag8.test.se, lag.test.se),
      `Pr(>|t|)` = c(lead8.test.pval, lead7.test.pval, lead6.test.pval, lead5.test.pval, lead4.test.pval, lead3.test.pval, lead2.test.pval, lead1.test.pval, lead.test.pval, lag0.test.pval, lag1.test.pval, lag2.test.pval, lag3.test.pval, lag4.test.pval, lag5.test.pval, lag6.test.pval, lag7.test.pval, lag8.test.pval, lag.test.pval),
      outcome = Y,
      controls = FE,
      econ = "none",
      parametric = "Yes",
      Rsq = summary(res1)$r.squared,
      adj.Rsq = summary(res1)$adj.r.squared)
    LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)
    
    
    ##### Add the cumulative effect at each lead/lag (relative to -1)
    
    ### LEADS
    # Normalize to zero at -1
    cumul.lead1.est <- 0
    cumul.lead1.se <- NA
    cumul.lead1.pval <- NA
    
    # At -2, the cumulative effect is just -lead.poly0 (in level)
    cumul.lead2.est <- -coef(summary(res1))[ "lead.poly0", "Estimate"]
    cumul.lead2.se <- coef(summary(res1))[ "lead.poly0", "Cluster s.e."]
    cumul.lead2.pval <- coef(summary(res1))[ "lead.poly0", "Pr(>|t|)"]
    
    
    # LEADS from 3 to 9
    for(j in 3:9) {
      
      ## Create a name for estimate, se and pval of each lead
      cumul.test.est.name <- paste("cumul.lead", j, ".est", sep = "")
      cumul.test.se.name <- paste("cumul.lead", j, ".se", sep = "")
      cumul.test.pval.name <- paste("cumul.lead", j, ".pval", sep = "")
      
      cumul.test.form <- paste(-(j-1) , "*lead.poly0", sep = "")
      
      for(k in 1:n.poly.lead) {
        
        tot.lead.n <- sum((1:(j-2))^k)  
        cumul.test.form <- paste(cumul.test.form, " - ", tot.lead.n, "*lead.poly", k, sep = "")
        
      }
      
      cumul.test.form <- paste(cumul.test.form, " = 0", sep = "")
      cumul.test <- glht(res1, linfct = cumul.test.form)
      
      assign(cumul.test.est.name, coef(summary(cumul.test))[[1]])
      assign(cumul.test.se.name, sqrt(vcov(summary(cumul.test)))[[1]])
      assign(cumul.test.pval.name, 2*(1 - pnorm(abs(coef(summary(cumul.test))[[1]]/sqrt(vcov(summary(cumul.test)))[[1]]))))
      
    }
    
    
    ###LAGS
    ##### Add the cumulative effect at each lead/lag (relative to -1)
    
    # At 0, the cumulative effect is just D.ln_sales_tax (in level)
    cumul.lag0.est <- coef(summary(res1))[ "D.ln_sales_tax", "Estimate"]
    cumul.lag0.se <- coef(summary(res1))[ "D.ln_sales_tax", "Cluster s.e."]
    cumul.lag0.pval <- coef(summary(res1))[ "D.ln_sales_tax", "Pr(>|t|)"]
    
    # At 1, the cumulative effecit if just D.ln_sales_tax + lag.poly0 (cumulative relative to -1)
    cumul.test.form <- "D.ln_sales_tax + lag.poly0 = 0"
    cumul.test <- glht(res1, linfct = cumul.test.form)
    
    cumul.lag1.est <- coef(summary(cumul.test))[[1]]
    cumul.lag1.se <- sqrt(vcov(summary(cumul.test)))[[1]]
    cumul.lag1.pval <- 2*(1 - pnorm(abs(coef(summary(cumul.test))[[1]]/sqrt(vcov(summary(cumul.test)))[[1]])))
    
    
    # LAGS from 1 to 8
    for(j in 2:8) {
      
      ## Create a name for estimate, se and pval of each lag
      cumul.test.est.name <- paste("cumul.lag", j, ".est", sep = "")
      cumul.test.se.name <- paste("cumul.lag", j, ".se", sep = "")
      cumul.test.pval.name <- paste("cumul.lag", j, ".pval", sep = "")
      
      cumul.test.form <- paste("D.ln_sales_tax + ", j , "*lag.poly0", sep = "")
      
      for(k in 1:n.poly.lag) {
        
        tot.lag.n <- sum((1:(j-1))^k)  
        cumul.test.form <- paste(cumul.test.form, " + ", tot.lag.n, "*lag.poly", k, sep = "")
        
      }
      
      cumul.test.form <- paste(cumul.test.form, " = 0", sep = "")
      cumul.test <- glht(res1, linfct = cumul.test.form)
      
      assign(cumul.test.est.name, coef(summary(cumul.test))[[1]])
      assign(cumul.test.se.name, sqrt(vcov(summary(cumul.test)))[[1]])
      assign(cumul.test.pval.name, 2*(1 - pnorm(abs(coef(summary(cumul.test))[[1]]/sqrt(vcov(summary(cumul.test)))[[1]]))))
      
    }
    
    
    ## linear hypothesis results
    lp.dt <- data.table(
      rn = c("cumul.lead9.D.ln_sales_tax", "cumul.lead8.D.ln_sales_tax", "cumul.lead7.D.ln_sales_tax", "cumul.lead6.D.ln_sales_tax", "cumul.lead5.D.ln_sales_tax", "cumul.lead4.D.ln_sales_tax", "cumul.lead3.D.ln_sales_tax", "cumul.lead2.D.ln_sales_tax", "cumul.lead1.D.ln_sales_tax", "cumul.lag0.D.ln_sales_tax", "cumul.lag1.D.ln_sales_tax", "cumul.lag2.D.ln_sales_tax", "cumul.lag3.D.ln_sales_tax", "cumul.lag4.D.ln_sales_tax", "cumul.lag5.D.ln_sales_tax", "cumul.lag6.D.ln_sales_tax", "cumul.lag7.D.ln_sales_tax", "cumul.lag8.D.ln_sales_tax"),
      Estimate = c(cumul.lead9.est, cumul.lead8.est, cumul.lead7.est, cumul.lead6.est, cumul.lead5.est, cumul.lead4.est, cumul.lead3.est, cumul.lead2.est, cumul.lead1.est, cumul.lag0.est, cumul.lag1.est, cumul.lag2.est, cumul.lag3.est, cumul.lag4.est, cumul.lag5.est, cumul.lag6.est, cumul.lag7.est, cumul.lag8.est),
      `Cluster s.e.` = c(cumul.lead9.se, cumul.lead8.se, cumul.lead7.se, cumul.lead6.se, cumul.lead5.se, cumul.lead4.se, cumul.lead3.se, cumul.lead2.se, cumul.lead1.se, cumul.lag0.se, cumul.lag1.se, cumul.lag2.se, cumul.lag3.se, cumul.lag4.se, cumul.lag5.se, cumul.lag6.se, cumul.lag7.se, cumul.lag8.se),
      `Pr(>|t|)` = c(cumul.lead9.pval, cumul.lead8.pval, cumul.lead7.pval, cumul.lead6.pval, cumul.lead5.pval, cumul.lead4.pval, cumul.lead3.pval, cumul.lead2.pval, cumul.lead1.pval, cumul.lag0.pval, cumul.lag1.pval, cumul.lag2.pval, cumul.lag3.pval, cumul.lag4.pval, cumul.lag5.pval, cumul.lag6.pval, cumul.lag7.pval, cumul.lag8.pval),
      outcome = Y,
      controls = FE,
      econ = "none",
      parametric = "Yes",
      Rsq = summary(res1)$r.squared,
      adj.Rsq = summary(res1)$adj.r.squared)
    LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)


  }
}




### Second: We impose smoothness on lagged and lead effects of tax rate on outcomes
## Also include polynomials in lags of unemployment and house prices (This will probably be the preferred specification)
all_pi[, lag.unemp0 := D.ln_unemp + L8.D.ln_unemp + L7.D.ln_unemp + L6.D.ln_unemp + L5.D.ln_unemp + L4.D.ln_unemp + L3.D.ln_unemp + L2.D.ln_unemp + L1.D.ln_unemp ]
all_pi[, lag.unemp1 := 8*L8.D.ln_unemp + 7*L7.D.ln_unemp + 6*L6.D.ln_unemp + 5*L5.D.ln_unemp + 4*L4.D.ln_unemp + 3*L3.D.ln_unemp + 2*L2.D.ln_unemp + L1.D.ln_unemp ]
all_pi[, lag.unemp2 := 64*L8.D.ln_unemp + 49*L7.D.ln_unemp + 36*L6.D.ln_unemp + 25*L5.D.ln_unemp + 16*L4.D.ln_unemp + 9*L3.D.ln_unemp + 4*L2.D.ln_unemp + L1.D.ln_unemp ]
all_pi[, lag.unemp3 := 512*L8.D.ln_unemp + 343*L7.D.ln_unemp + 216*L6.D.ln_unemp + 125*L5.D.ln_unemp + 64*L4.D.ln_unemp + 27*L3.D.ln_unemp + 8*L2.D.ln_unemp + L1.D.ln_unemp ]
all_pi[, lag.unemp4 := 4096*L8.D.ln_unemp + 2401*L7.D.ln_unemp + 1296*L6.D.ln_unemp + 625*L5.D.ln_unemp + 256*L4.D.ln_unemp + 81*L3.D.ln_unemp + 16*L2.D.ln_unemp + L1.D.ln_unemp ]

all_pi[, lag.home_price0 := D.ln_home_price + L8.D.ln_home_price + L7.D.ln_home_price + L6.D.ln_home_price + L5.D.ln_home_price + L4.D.ln_home_price + L3.D.ln_home_price + L2.D.ln_home_price + L1.D.ln_home_price ]
all_pi[, lag.home_price1 := 8*L8.D.ln_home_price + 7*L7.D.ln_home_price + 6*L6.D.ln_home_price + 5*L5.D.ln_home_price + 4*L4.D.ln_home_price + 3*L3.D.ln_home_price + 2*L2.D.ln_home_price + L1.D.ln_home_price ]
all_pi[, lag.home_price2 := 64*L8.D.ln_home_price + 49*L7.D.ln_home_price + 36*L6.D.ln_home_price + 25*L5.D.ln_home_price + 16*L4.D.ln_home_price + 9*L3.D.ln_home_price + 4*L2.D.ln_home_price + L1.D.ln_home_price ]
all_pi[, lag.home_price3 := 512*L8.D.ln_home_price + 343*L7.D.ln_home_price + 216*L6.D.ln_home_price + 125*L5.D.ln_home_price + 64*L4.D.ln_home_price + 27*L3.D.ln_home_price + 8*L2.D.ln_home_price + L1.D.ln_home_price ]
all_pi[, lag.home_price4 := 4096*L8.D.ln_home_price + 2401*L7.D.ln_home_price + 1296*L6.D.ln_home_price + 625*L5.D.ln_home_price + 256*L4.D.ln_home_price + 81*L3.D.ln_home_price + 16*L2.D.ln_home_price + L1.D.ln_home_price ]


## Change formula_RHS
formula_RHS <- "D.ln_sales_tax + lead.poly0 + lead.poly1 + lead.poly2 + lead.poly3 + lag.poly0 + lag.poly1 + lag.poly2 + lag.poly3"
formula_RHS <- paste0(formula_RHS, " + lag.unemp0 + lag.unemp1 + lag.unemp2 + lag.unemp3 + lag.unemp4 + lag.home_price0 + lag.home_price1 + lag.home_price2 + lag.home_price3 + lag.home_price4")

##!!! NOTE: formula just above assume 3rd degree polynomial for lags
n.poly.lead <- 2
n.poly.lag <- 2


for (Y in outcomes) {
  for (FE in FE_opts) {

    formula1 <- as.formula(paste0(
      Y, "~", formula_RHS, "| ", FE, " | 0 | module_by_state"
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
    res1.dt[, econ := "ln_unemp + ln_home_price"]
    res1.dt[, parametric := "Yes"]
    res1.dt[, Rsq := summary(res1)$r.squared]
    res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)


    for(j in 2:8) { ## Number of leads and lags over which effect is assumed to matter

      ###### LEADS
      ## Create a name for estimate, se and pval of each lead
      lead.test.est.name <- paste("lead", j, ".test.est", sep = "")
      lead.test.se.name <- paste("lead", j, ".test.se", sep = "")
      lead.test.pval.name <- paste("lead", j, ".test.pval", sep = "")

      ## Create the formula to compute estimate at each lead
      lead.test.form <- "lead.poly0"
      for(k in 1:n.poly.lead) {

        lead.test.form <- paste(lead.test.form, " + lead.poly", k, "*", (j-1)^k, sep = "")

      }
      lead.test.form <- paste(lead.test.form, " = 0")


      ## Compute estimate and store in variables names
      lead.test <- glht(res1, linfct = lead.test.form)

      assign(lead.test.est.name, coef(summary(lead.test))[[1]])
      assign(lead.test.se.name, sqrt(vcov(summary(lead.test)))[[1]])
      assign(lead.test.pval.name, 2*(1 - pnorm(abs(coef(summary(lead.test))[[1]]/sqrt(vcov(summary(lead.test)))[[1]]))))


      ###### LAGS
      ## Create a name for estimate, se and pval of each lead
      lag.test.est.name <- paste("lag", j, ".test.est", sep = "")
      lag.test.se.name <- paste("lag", j, ".test.se", sep = "")
      lag.test.pval.name <- paste("lag", j, ".test.pval", sep = "")

      ## Create the formula to compute estimate at each lead
      lag.test.form <- "lag.poly0"
      for(k in 1:n.poly.lag) {

        lag.test.form <- paste(lag.test.form, " + lag.poly", k, "*", (j-1)^k, sep = "")

      }
      lag.test.form <- paste(lag.test.form, " = 0")


      ## Compute estimate and store in variables names
      lag.test <- glht(res1, linfct = lag.test.form)

      assign(lag.test.est.name, coef(summary(lag.test))[[1]])
      assign(lag.test.se.name, sqrt(vcov(summary(lag.test)))[[1]])
      assign(lag.test.pval.name, 2*(1 - pnorm(abs(coef(summary(lag.test))[[1]]/sqrt(vcov(summary(lag.test)))[[1]]))))

    }

    ## On Impact --> Effect = coefficient on D.ln_sales_tax
    lag0.test.est <- coef(summary(res1))[ "D.ln_sales_tax", "Estimate"]
    lag0.test.se <- coef(summary(res1))[ "D.ln_sales_tax", "Cluster s.e."]
    lag0.test.pval <- coef(summary(res1))[ "D.ln_sales_tax", "Pr(>|t|)"]

    ##
    lead1.test.est <- coef(summary(res1))[ "lead.poly0", "Estimate"]
    lead1.test.se <- coef(summary(res1))[ "lead.poly0", "Cluster s.e."]
    lead1.test.pval <- coef(summary(res1))[ "lead.poly0", "Pr(>|t|)"]

    ##
    lag1.test.est <- coef(summary(res1))[ "lag.poly0", "Estimate"]
    lag1.test.se <- coef(summary(res1))[ "lag.poly0", "Cluster s.e."]
    lag1.test.pval <- coef(summary(res1))[ "lag.poly0", "Pr(>|t|)"]


    ## sum leads
    flog.info("Summing leads...")
    lead.test.form <- "8*lead.poly0"
    for(k in 1:n.poly.lead) {

      tot.lead.n <- 1^k + 2^k + 3^k + 4^k + 5^k + 6^k + 7^k
      lead.test.form <- paste(lead.test.form, " + ", tot.lead.n, "*lead.poly", k, sep = "")
    }
    lead.test.form <- paste(lead.test.form, " = 0", sep = "")

    lead.test <- glht(res1, linfct = lead.test.form)
    lead.test.est <- coef(summary(lead.test))[[1]]
    lead.test.se <- sqrt(vcov(summary(lead.test)))[[1]]
    lead.test.pval <- 2*(1 - pnorm(abs(lead.test.est/lead.test.se)))


    ## sum lags
    flog.info("Summing lags...")
    lag.test.form <- "D.ln_sales_tax + 8*lag.poly0"
    for(k in 1:n.poly.lag) {

      tot.lag.n <- 1^k + 2^k + 3^k + 4^k + 5^k + 6^k + 7^k
      lag.test.form <- paste(lag.test.form, " + ", tot.lag.n, "*lag.poly", k, sep = "")
    }
    lag.test.form <- paste(lag.test.form, " = 0", sep = "")
    lag.test <- glht(res1, linfct = lag.test.form)
    lag.test.est <- coef(summary(lag.test))[[1]]
    lag.test.se <- sqrt(vcov(summary(lag.test)))[[1]]
    lag.test.pval <- 2*(1 - pnorm(abs(lag.test.est/lag.test.se)))


    ## linear hypothesis results
    lp.dt <- data.table(
      rn = c("lead8.D.ln_sales_tax", "lead7.D.ln_sales_tax", "lead6.D.ln_sales_tax", "lead5.D.ln_sales_tax", "lead4.D.ln_sales_tax", "lead3.D.ln_sales_tax", "lead2.D.ln_sales_tax", "lead1.D.ln_sales_tax", "Pre.D.ln_sales_tax", "lag0.D.ln_sales_tax", "lag1.D.ln_sales_tax", "lag2.D.ln_sales_tax", "lag3.D.ln_sales_tax", "lag4.D.ln_sales_tax", "lag5.D.ln_sales_tax", "lag6.D.ln_sales_tax", "lag7.D.ln_sales_tax", "lag8.D.ln_sales_tax", "Post.D.ln_sales_tax"),
      Estimate = c(lead8.test.est, lead7.test.est, lead6.test.est, lead5.test.est, lead4.test.est, lead3.test.est, lead2.test.est, lead1.test.est, lead.test.est, lag0.test.est, lag1.test.est, lag2.test.est, lag3.test.est, lag4.test.est, lag5.test.est, lag6.test.est, lag7.test.est, lag8.test.est, lag.test.est),
      `Cluster s.e.` = c(lead8.test.se, lead7.test.se, lead6.test.se, lead5.test.se, lead4.test.se, lead3.test.se, lead2.test.se, lead1.test.se, lead.test.se, lag0.test.se, lag1.test.se, lag2.test.se, lag3.test.se, lag4.test.se, lag5.test.se, lag6.test.se, lag7.test.se, lag8.test.se, lag.test.se),
      `Pr(>|t|)` = c(lead8.test.pval, lead7.test.pval, lead6.test.pval, lead5.test.pval, lead4.test.pval, lead3.test.pval, lead2.test.pval, lead1.test.pval, lead.test.pval, lag0.test.pval, lag1.test.pval, lag2.test.pval, lag3.test.pval, lag4.test.pval, lag5.test.pval, lag6.test.pval, lag7.test.pval, lag8.test.pval, lag.test.pval),
      outcome = Y,
      controls = FE,
      econ = "ln_unemp + ln_home_price",
      parametric = "Yes",
      Rsq = summary(res1)$r.squared,
      adj.Rsq = summary(res1)$adj.r.squared)
    LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)
    
    
    
    ##### Add the cumulative effect at each lead/lag (relative to -1)
    
    ### LEADS
    # Normalize to zero at -1
    cumul.lead1.est <- 0
    cumul.lead1.se <- NA
    cumul.lead1.pval <- NA
    
    # At -2, the cumulative effect is just -lead.poly0 (in level)
    cumul.lead2.est <- -coef(summary(res1))[ "lead.poly0", "Estimate"]
    cumul.lead2.se <- coef(summary(res1))[ "lead.poly0", "Cluster s.e."]
    cumul.lead2.pval <- coef(summary(res1))[ "lead.poly0", "Pr(>|t|)"]
    
    
    # LEADS from 3 to 9
    for(j in 3:9) {
      
      ## Create a name for estimate, se and pval of each lead
      cumul.test.est.name <- paste("cumul.lead", j, ".est", sep = "")
      cumul.test.se.name <- paste("cumul.lead", j, ".se", sep = "")
      cumul.test.pval.name <- paste("cumul.lead", j, ".pval", sep = "")
      
      cumul.test.form <- paste(-(j-1) , "*lead.poly0", sep = "")
      
      for(k in 1:n.poly.lead) {
        
        tot.lead.n <- sum((1:(j-2))^k)  
        cumul.test.form <- paste(cumul.test.form, " - ", tot.lead.n, "*lead.poly", k, sep = "")
        
      }
      
      cumul.test.form <- paste(cumul.test.form, " = 0", sep = "")
      cumul.test <- glht(res1, linfct = cumul.test.form)
      
      assign(cumul.test.est.name, coef(summary(cumul.test))[[1]])
      assign(cumul.test.se.name, sqrt(vcov(summary(cumul.test)))[[1]])
      assign(cumul.test.pval.name, 2*(1 - pnorm(abs(coef(summary(cumul.test))[[1]]/sqrt(vcov(summary(cumul.test)))[[1]]))))
      
    }
    
    
    ###LAGS
    ##### Add the cumulative effect at each lead/lag (relative to -1)
    
    # At 0, the cumulative effect is just D.ln_sales_tax (in level)
    cumul.lag0.est <- coef(summary(res1))[ "D.ln_sales_tax", "Estimate"]
    cumul.lag0.se <- coef(summary(res1))[ "D.ln_sales_tax", "Cluster s.e."]
    cumul.lag0.pval <- coef(summary(res1))[ "D.ln_sales_tax", "Pr(>|t|)"]
    
    # At 1, the cumulative effecit if just D.ln_sales_tax + lag.poly0 (cumulative relative to -1)
    cumul.test.form <- "D.ln_sales_tax + lag.poly0 = 0"
    cumul.test <- glht(res1, linfct = cumul.test.form)
    
    cumul.lag1.est <- coef(summary(cumul.test))[[1]]
    cumul.lag1.se <- sqrt(vcov(summary(cumul.test)))[[1]]
    cumul.lag1.pval <- 2*(1 - pnorm(abs(coef(summary(cumul.test))[[1]]/sqrt(vcov(summary(cumul.test)))[[1]])))
    
    
    # LAGS from 1 to 8
    for(j in 2:8) {
      
      ## Create a name for estimate, se and pval of each lag
      cumul.test.est.name <- paste("cumul.lag", j, ".est", sep = "")
      cumul.test.se.name <- paste("cumul.lag", j, ".se", sep = "")
      cumul.test.pval.name <- paste("cumul.lag", j, ".pval", sep = "")
      
      cumul.test.form <- paste("D.ln_sales_tax + ", j , "*lag.poly0", sep = "")
      
      for(k in 1:n.poly.lag) {
        
        tot.lag.n <- sum((1:(j-1))^k)  
        cumul.test.form <- paste(cumul.test.form, " + ", tot.lag.n, "*lag.poly", k, sep = "")
        
      }
      
      cumul.test.form <- paste(cumul.test.form, " = 0", sep = "")
      cumul.test <- glht(res1, linfct = cumul.test.form)
      
      assign(cumul.test.est.name, coef(summary(cumul.test))[[1]])
      assign(cumul.test.se.name, sqrt(vcov(summary(cumul.test)))[[1]])
      assign(cumul.test.pval.name, 2*(1 - pnorm(abs(coef(summary(cumul.test))[[1]]/sqrt(vcov(summary(cumul.test)))[[1]]))))
      
    }
    
    
    ## linear hypothesis results
    lp.dt <- data.table(
      rn = c("cumul.lead9.D.ln_sales_tax", "cumul.lead8.D.ln_sales_tax", "cumul.lead7.D.ln_sales_tax", "cumul.lead6.D.ln_sales_tax", "cumul.lead5.D.ln_sales_tax", "cumul.lead4.D.ln_sales_tax", "cumul.lead3.D.ln_sales_tax", "cumul.lead2.D.ln_sales_tax", "cumul.lead1.D.ln_sales_tax", "cumul.lag0.D.ln_sales_tax", "cumul.lag1.D.ln_sales_tax", "cumul.lag2.D.ln_sales_tax", "cumul.lag3.D.ln_sales_tax", "cumul.lag4.D.ln_sales_tax", "cumul.lag5.D.ln_sales_tax", "cumul.lag6.D.ln_sales_tax", "cumul.lag7.D.ln_sales_tax", "cumul.lag8.D.ln_sales_tax"),
      Estimate = c(cumul.lead9.est, cumul.lead8.est, cumul.lead7.est, cumul.lead6.est, cumul.lead5.est, cumul.lead4.est, cumul.lead3.est, cumul.lead2.est, cumul.lead1.est, cumul.lag0.est, cumul.lag1.est, cumul.lag2.est, cumul.lag3.est, cumul.lag4.est, cumul.lag5.est, cumul.lag6.est, cumul.lag7.est, cumul.lag8.est),
      `Cluster s.e.` = c(cumul.lead9.se, cumul.lead8.se, cumul.lead7.se, cumul.lead6.se, cumul.lead5.se, cumul.lead4.se, cumul.lead3.se, cumul.lead2.se, cumul.lead1.se, cumul.lag0.se, cumul.lag1.se, cumul.lag2.se, cumul.lag3.se, cumul.lag4.se, cumul.lag5.se, cumul.lag6.se, cumul.lag7.se, cumul.lag8.se),
      `Pr(>|t|)` = c(cumul.lead9.pval, cumul.lead8.pval, cumul.lead7.pval, cumul.lead6.pval, cumul.lead5.pval, cumul.lead4.pval, cumul.lead3.pval, cumul.lead2.pval, cumul.lead1.pval, cumul.lag0.pval, cumul.lag1.pval, cumul.lag2.pval, cumul.lag3.pval, cumul.lag4.pval, cumul.lag5.pval, cumul.lag6.pval, cumul.lag7.pval, cumul.lag8.pval),
      outcome = Y,
      controls = FE,
      econ = "ln_unemp + ln_home_price",
      parametric = "Yes",
      Rsq = summary(res1)$r.squared,
      adj.Rsq = summary(res1)$adj.r.squared)
    LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)


  }
}



## summary values --------------------------------------------------------------
LRdiff_res$N_obs <- nrow(all_pi)
LRdiff_res$N_modules <- length(unique(all_pi$product_module_code))
LRdiff_res$N_stores <- length(unique(all_pi$store_code_uc))
LRdiff_res$N_counties <- uniqueN(all_pi, by = c("fips_state", "fips_county"))
LRdiff_res$N_years <- uniqueN(all_pi, by = c("year")) # should be 6 (we lose one because we difference)
LRdiff_res$N_county_modules <- uniqueN(all_pi, by = c("fips_state", "fips_county",
                                                           "product_module_code"))

fwrite(LRdiff_res, output.results.file)


