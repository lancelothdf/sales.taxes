#' Author: Lancelot Henry de Frahan and John Bonney
#'
#'Same as LRdiff_diff_semesterly_preferred_part1 but we plot the cumulative effect as opposed to changes

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

zillow_path <- "Data/covariates/zillow_long_by_county_clean.csv"
zillow_state_path <- "Data/covariates/zillow_long_by_state_clean.csv"
unemp.path <- "Data/covariates/county_monthly_unemp_clean.csv"


## output filepaths ----------------------------------------------
output.results.file <- "Data/LRdiff_semesterly_results_preferred_cumulative_IV.csv"
output.covv.mat <- "Data/LRdiff_semesterly_results_IV_vcov"


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


##### Load the semesterly price index and sales data (prep/clean them later)
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


## collapse to semesters
zillow_dt <- zillow_dt[, semester := ceiling((month/12)*2)]
zillow_dt <- zillow_dt[, list(ln_home_price = log(mean(median_home_price))),
                       by = .(year, semester, fips_state, fips_county)]

##
all_pi <- merge(all_pi, zillow_dt, by = c("fips_state", "fips_county", "year", "semester"), all.x = T)
rm(zillow_dt)

### Unemployment data
unemp.data <- fread(unemp.path)
unemp.data <- unemp.data[, c("fips_state", "fips_county", "year", "month", "rate")]
unemp.data <- unemp.data[, semester := ceiling((month/12)*2)]
unemp.data <- unemp.data[, list(unemp = mean(rate)), by = .(year, semester, fips_state, fips_county)]
unemp.data <- unemp.data[year >= 2006 & year <= 2016,]
unemp.data <- unemp.data[, ln_unemp := log(unemp)]

##
all_pi <- merge(all_pi, unemp.data, by = c("fips_state", "fips_county", "year", "semester"), all.x = T)
rm(unemp.data)


## prep the 2006-2016 data --------------------------------------- ##NOTE: in this version we do not merge to "old price indices" because they are under construction


## merge on the census region/division info
all_pi <- merge(all_pi, geo_dt, by = "fips_state")

# impute tax rates prior to 2008 and after 2014
all_pi[, sales_tax := ifelse(year < 2008, sales_tax[year == 2008 & semester == 1], sales_tax),
            by = .(store_code_uc, product_module_code)]
all_pi[, sales_tax := ifelse(year > 2014, sales_tax[year == 2014 & semester == 2], sales_tax),
            by = .(store_code_uc, product_module_code)]

# create necessary variables
all_pi[, ln_cpricei := log(cpricei)]
all_pi[, ln_sales_tax := log(sales_tax)]
all_pi[, ln_quantity := log(sales) - log(pricei)]
all_pi[, store_by_module := .GRP, by = .(store_code_uc, product_module_code)]
all_pi[, cal_time := 2* year + semester]
all_pi[, module_by_time := .GRP, by = .(product_module_code, cal_time)]
all_pi[, module_by_state := .GRP, by = .(product_module_code, fips_state)]
all_pi[, region_by_module_by_time := .GRP, by = .(region, product_module_code, cal_time)]
all_pi[, division_by_module_by_time := .GRP, by = .(division, product_module_code, cal_time)]

## get sales weights
all_pi[, base.sales := sales[year == 2008 & semester == 1],
            by = .(store_code_uc, product_module_code)]

all_pi <- all_pi[!is.na(base.sales) & !is.na(sales) & !is.na(ln_cpricei) &
                             !is.na(ln_sales_tax) & !is.na(ln_quantity)]
#                             & !is.na(ln_quantity2) & !is.na(ln_cpricei2)]

## balance on store-module level
keep_store_modules <- all_pi[, list(n = .N),
                                  by = .(store_code_uc, product_module_code)]
keep_store_modules <- keep_store_modules[n == (2016 - 2005) * 2]

setkey(all_pi, store_code_uc, product_module_code)
setkey(keep_store_modules, store_code_uc, product_module_code)

all_pi <- all_pi[keep_store_modules]
setkey(all_pi, store_code_uc, product_module_code, year, semester)


#############################################################
## Delete some variables to save memory
all_pi <- all_pi[, c("fips_state", "fips_county", "year", "semester", "store_code_uc", "product_module_code", "ln_cpricei", "ln_sales_tax", "ln_quantity", "base.sales", "store_by_module", "cal_time", "module_by_time", "module_by_state", "region_by_module_by_time", "division_by_module_by_time", "ln_home_price", "ln_unemp")]


#######################################################
## take first differences of outcomes and treatment
all_pi <- all_pi[order(store_code_uc, product_module_code, cal_time),] ##Sort on store by year-semester (in ascending order)


all_pi[, D.ln_cpricei := ln_cpricei - shift(ln_cpricei, n=1, type="lag"),
       by = .(store_code_uc, product_module_code)]

all_pi[, D.ln_quantity := ln_quantity - shift(ln_quantity, n=1, type="lag"),
       by = .(store_code_uc, product_module_code)]

all_pi[, D.ln_sales_tax := ln_sales_tax - shift(ln_sales_tax, n=1, type="lag"),
       by = .(store_code_uc, product_module_code)]

#all_pi[, D.ln_unemp := ln_unemp - shift(ln_unemp, n=1, type="lag"),
#       by = .(store_code_uc, product_module_code)]

#all_pi[, D.ln_home_price := ln_home_price - shift(ln_home_price, n=1, type="lag"),
#       by = .(store_code_uc, product_module_code)]



## generate lags and leads of ln_sales_tax
for (lag.val in 1:4) {
  lag.X <- paste0("L", lag.val, ".D.ln_sales_tax")
  all_pi[, (lag.X) := shift(D.ln_sales_tax, n=lag.val, type="lag"),
         by = .(store_code_uc, product_module_code)]

  lead.X <- paste0("F", lag.val, ".D.ln_sales_tax")
  all_pi[, (lead.X) := shift(D.ln_sales_tax, n=lag.val, type="lead"),
         by = .(store_code_uc, product_module_code)]

  #lag.X <- paste0("L", lag.val, ".D.ln_unemp")
  #all_pi[, (lag.X) := shift(D.ln_unemp, n=lag.val, type="lag"),
  #       by = .(store_code_uc, product_module_code)]

  #lag.X <- paste0("L", lag.val, ".D.ln_home_price")
  #all_pi[, (lag.X) := shift(D.ln_home_price, n=lag.val, type="lag"),
  #       by = .(store_code_uc, product_module_code)]
}



### Estimation ---------------------------------------------------
all_pi <- all_pi[between(year, 2008, 2014)]
all_pi <- all_pi[ year >= 2009 | (year == 2008 & semester == 2)] ## First semester of 2008, the difference was imputed not real data - so we drop it


formula_lags <- paste0("L", 1:4, ".D.ln_sales_tax", collapse = "+")
formula_leads <- paste0("F", 1:4, ".D.ln_sales_tax", collapse = "+")
formula_RHS <- paste0("D.ln_sales_tax + ", formula_lags, "+", formula_leads)



outcomes <- c("D.ln_cpricei", "D.ln_quantity")
#econ.outcomes <- c("D.ln_unemp", "D.ln_home_price")
FE_opts <- c("module_by_time", "region_by_module_by_time", "division_by_module_by_time")


## for linear hypothesis tests
lead.vars <- paste(paste0("F", 4:1, ".D.ln_sales_tax"), collapse = " + ")
lag.vars <- paste(paste0("L", 4:1, ".D.ln_sales_tax"), collapse = " + ")
lead.lp.restr <- paste(lead.vars, "= 0")
lag.lp.restr <- paste(lag.vars, "+ D.ln_sales_tax = 0")
total.lp.restr <- paste(lag.vars, "+", lead.vars, "+ D.ln_sales_tax = 0")



### First: run regression and estimate leads and lags directly (without imposing smoothness)
## No Econ controls (but we run the placebos)
LRdiff_res <- data.table(NULL)
  for (FE in FE_opts) {

    ### First: Quantity regression
    formula1 <- as.formula(paste0(
      "D.ln_quantity ~", formula_RHS, "| ", FE, " | 0 | module_by_state"
    ))
    flog.info("Estimating with %s as outcome with %s FE.", Y, FE)
    res1 <- felm(formula = formula1, data = all_pi,
                 weights = all_pi$base.sales, keepCX = TRUE)
    flog.info("Finished estimating with %s as outcome with %s FE.", Y, FE)


    ## attach results
    flog.info("Writing results...")
    res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
    res1.dt[, outcome := "ln_quantity"]
    res1.dt[, controls := FE]
    res1.dt[, econ := "none"]
    res1.dt[, parametric := "No"]
    res1.dt[, Rsq := summary(res1)$r.squared]
    res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)


    ## Compute covariance matrix

    #Start with getting the matrix of residualized regressors
    resid.quantity <- as.data.frame(res1$cX)  ##Get the expanded matrix (centered = residualized after FEs)
    xx.names <- names(resid.quantity)
    xx.mat <- as.matrix(resid.quantity) ## Create the X'X matrix that will be used in the standard errors

    #Create a dataframe that is going to contain the sums of residuals*X (*weights = base.sales) for each cluster (stateXmodule)
    resid.quantity$module_by_state <- all_pi$module_by_state
    resid.quantity$weights <- all_pi$base.sales
    resid.quantity$residuals <- res1$residuals

    setDT(resid.quantity)
    resid.quantity <- resid.quantity[, list(D.ln_sales_tax = sum(weights*D.ln_sales_tax*residuals), L1.D.ln_sales_tax = sum(weights*L1.D.ln_sales_tax*residuals), L2.D.ln_sales_tax = sum(weights*L2.D.ln_sales_tax*residuals), L3.D.ln_sales_tax = sum(weights*L3.D.ln_sales_tax*residuals), L4.D.ln_sales_tax = sum(weights*L4.D.ln_sales_tax*residuals), F1.D.ln_sales_tax = sum(weights*F1.D.ln_sales_tax*residuals), F2.D.ln_sales_tax = sum(weights*F2.D.ln_sales_tax*residuals), F3.D.ln_sales_tax = sum(weights*F3.D.ln_sales_tax*residuals), F4.D.ln_sales_tax = sum(weights*F4.D.ln_sales_tax*residuals), weights = sum(weights)), by = "module_by_state"]


    # Create "Sandwich" (X'WX) - where W is the diagonal matrix with the weights
    xx.mat <- t(xx.mat*as.vector(all_pi$base.sales))%*%xx.mat ## At some point, we need to make sure that variables in xx.mat and resid are ordered the same way -
    xx.mat <- solve(xx.mat)
    names(xx.mat) <- xx.names



    ### Second: Price regression
    formula1 <- as.formula(paste0(
      "D.ln_cpricei ~", formula_RHS, "| ", FE, " | 0 | module_by_state"
    ))
    flog.info("Estimating with %s as outcome with %s FE.", Y, FE)
    res1 <- felm(formula = formula1, data = all_pi,
                 weights = all_pi$base.sales, keepCX = TRUE)
    flog.info("Finished estimating with %s as outcome with %s FE.", Y, FE)


    ## attach results
    flog.info("Writing results...")
    res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
    res1.dt[, outcome := "ln_cpricei"]
    res1.dt[, controls := FE]
    res1.dt[, econ := "none"]
    res1.dt[, parametric := "No"]
    res1.dt[, Rsq := summary(res1)$r.squared]
    res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)


    #Create matrix of residuals for cpricei
    resid.cpricei <- as.data.frame(res1$cX)
    resid.cpricei$module_by_state <- all_pi$module_by_state
    resid.cpricei$weights <- all_pi$base.sales
    resid.cpricei$residuals <- res1$residuals

    setDT(resid.cpricei)
    resid.cpricei <- resid.cpricei[, list(c.D.ln_sales_tax = sum(weights*D.ln_sales_tax*residuals), c.L1.D.ln_sales_tax = sum(weights*L1.D.ln_sales_tax*residuals), c.L2.D.ln_sales_tax = sum(weights*L2.D.ln_sales_tax*residuals), c.L3.D.ln_sales_tax = sum(weights*L3.D.ln_sales_tax*residuals), c.L4.D.ln_sales_tax = sum(weights*L4.D.ln_sales_tax*residuals), c.F1.D.ln_sales_tax = sum(weights*F1.D.ln_sales_tax*residuals), c.F2.D.ln_sales_tax = sum(weights*F2.D.ln_sales_tax*residuals), c.F3.D.ln_sales_tax = sum(weights*F3.D.ln_sales_tax*residuals), c.F4.D.ln_sales_tax = sum(weights*F4.D.ln_sales_tax*residuals), weights = sum(weights)), by = "module_by_state"]


    ### Combine these matrices into a covariance matrix for all parameters
    resid <- merge(resid.quantity, resid.cpricei, all.x = TRUE, by = "module_by_state")
    B.clu <- t(as.matrix(resid[, c("D.ln_sales_tax", "L1.D.ln_sales_tax", "L2.D.ln_sales_tax", "L3.D.ln_sales_tax", "L4.D.ln_sales_tax", "F1.D.ln_sales_tax", "F2.D.ln_sales_tax", "F3.D.ln_sales_tax", "F4.D.ln_sales_tax", "c.D.ln_sales_tax", "c.L1.D.ln_sales_tax", "c.L2.D.ln_sales_tax", "c.L3.D.ln_sales_tax", "c.L4.D.ln_sales_tax", "c.F1.D.ln_sales_tax", "c.F2.D.ln_sales_tax", "c.F3.D.ln_sales_tax", "c.F4.D.ln_sales_tax")]))%*%as.matrix(resid[, c("D.ln_sales_tax", "L1.D.ln_sales_tax", "L2.D.ln_sales_tax", "L3.D.ln_sales_tax", "L4.D.ln_sales_tax", "F1.D.ln_sales_tax", "F2.D.ln_sales_tax", "F3.D.ln_sales_tax", "F4.D.ln_sales_tax", "c.D.ln_sales_tax", "c.L1.D.ln_sales_tax", "c.L2.D.ln_sales_tax", "c.L3.D.ln_sales_tax", "c.L4.D.ln_sales_tax", "c.F1.D.ln_sales_tax", "c.F2.D.ln_sales_tax", "c.F3.D.ln_sales_tax", "c.F4.D.ln_sales_tax")])

    nvar <- dim(B.clu)[1]/2
    sandwich <- matrix(0, nrow = 2*nvar, ncol = 2*nvar)
    sandwich[1:nvar, 1:nvar] <- xx.mat
    sandwich[(nvar + 1):(2*nvar), (nvar + 1):(2*nvar)] <- xx.mat

    covv.mat <- sandwich%*%B.clu
    covv.mat <- covv.mat%*%sandwich

    output.temp <- paste(output.covv.mat, "_", FE_opts, ".csv", sep = "")
    fwrite(covv.mat, output.temp)
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


