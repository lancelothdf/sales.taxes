#' Author: John Bonney & Lancelot Henry de Frahan
#' NOTE:  In this code, we reweigh the control group so that distribution of
#' products matches exactly distribution of products in the treatment group
#'
#'    - Only one regression per cohort (not product-specific)
#'
#'    - We bootstrap (draw at the county-level to preserve within cluster/county
#'      correlation) so that we can compute std errors and confidence intervals
#'
#' This adapts the quarterly version to estimate on a **monthly** level.
#' This version is to examine **unemployment**

library(data.table)
library(lfe)
library(futile.logger)
library(AER)
library(readstata13)

#' Switching to monthly: we need to
#'   -make sure the monthly price and quantity data is correctly loaded
#'   -switch reforms to be on monthly level (done)
#'   -adjust cohorts to loop over monthly level (done)
#'   -change saved filepaths (done)
#'   -double check sales-weighted monthly tax rate (done)
#'   -search through all the catt_lead4..catt_4 (done)


setwd("/project2/igaarder")
change_of_interest <- "Ever increase"
prep_enviro <- T

output.results.filepath <- "Data/unemp_ei_monthly_regression_res_prodmatch_combined.csv"
output.residuals.cpricei.filepath <- "Data/unemp_ei_monthly_regression_prodmatch_residuals_cpricei_combined.csv"
output.residuals.tax.filepath <-"Data/unemp_ei_monthly_regression_prodmatch_residuals_tax_combined.csv"
output.xx.filepath <- "Data/unemp_ei_monthly_regression_prodmatch_xx_combined.csv"


## useful filepaths ------------------------------------------------------------
# all_goods_pi_path <- "Data/Nielsen/price_quantity_indices_allitems_2006-2016_notaxinfo.csv"
taxable_pi_path <- "Data/Nielsen/price_quantity_indices_taxableitems_2006-2016.csv"
eventstudy_tr_path <- "Data/event_study_tr_groups_comprehensive_firstonly_no2012q4_2013q1q2.csv"
tr_groups_path <- "Data/tr_groups_comprehensive_firstonly_no2012q4_2013q1q2.csv"
sales_data_path <- "Data/sales_monthly_2006-2016.csv"
monthly_tax_path <- "Data/Nielsen/sales_tax_rate_monthly_product_level.dta"
unemp_path <- "Data/covariates/county_monthly_unemp_clean.csv"


## Want to run cohort-product specific regressions.
## Data is on store-product-quarter level

######
##Create a function to residualize/demean data
#var is a list of variables to residualize, group are the factors over which to residualize, w are the weights, mtx is the name of the data.frame
get.res <- function(var, group, w, mtx) {

  form <- as.formula(paste0(var, "~ 1 | ", group, " | 0 | 0", sep = ""))
  return(felm(data = mtx, formula = form , weights = mtx[,get(w)])$fitted.values)

}

## Prepare the monthly data ----------------------------------------------------

if (prep_enviro){
  ## create .csv's of taxable and all goods ------------------------------------
  nonfood_pi <- read.dta13("Data/Nielsen/Monthly_price_quantity_indices_nonfood.dta")
  nonfood_pi <- as.data.table(nonfood_pi)
  nonfood_pi[, c("cspricei", "squantityi", "spricei", "geocpricei",
                 "geoquantityi", "geopricei") := NULL] # to save space
  # fwrite(nonfood_pi, "Data/Nielsen/Monthly_price_quantity_indices_nonfood.dta")

  food_pi <- fread("Data/Nielsen/monthly_price_quantity_indices_food.csv")
  food_pi[, c("cspricei", "squantityi", "spricei", "geocpricei",
              "geoquantityi", "geopricei") := NULL]

  all_pi <- rbind(food_pi, nonfood_pi)
  all_pi <- all_pi[year %in% 2006:2014]
  rm(nonfood_pi, food_pi)
  gc()

  ### attach county and state FIPS codes, sales, and tax rates -----------------
  sales_data <- fread(sales_data_path)
  sales_data <- sales_data[, .(store_code_uc, product_module_code, fips_county,
                               fips_state, month, year, sales)]
  sales_data <- sales_data[year %in% 2006:2014]

  all_pi <- merge(all_pi, sales_data, by = c("store_code_uc", "month", "year",
                                             "product_module_code" ))
  rm(sales_data)
  gc()

  all.tax <- fread(monthly_tax_path)

  all_pi <- merge(all_pi, all.tax,
                  by = c("fips_county", "fips_state", "product_module_code",
                         "year", "month", "product_group_code"),
                  all.x = T)

  # fwrite(all_pi, all_goods_pi_path)

  rm(all.tax)
  gc()

} else {
  all_pi <- fread(all_goods_pi_path)
}

# Start with event-no-event case ===============================================

## prep the data ---------------------------------------------------------------

all_pi <- all_pi[year %in% 2006:2014 & !is.na(cpricei)]
# limit it to taxable goods
all_pi <- all_pi[sales_tax > 1 | (year < 2008 & is.na(sales_tax))]

# do `arbitrary` correction for the 2013 Q1 jump in the data
## calculate price index in 2013 Q1 / cpricei in 2012 Q4
all_pi[, correction := pricei[year == 2013 & month == 1] / pricei[year == 2012 & month == 12],
       by = .(store_code_uc, product_module_code)]
## divide price index after 2013 Q1 (inclusive) by above value
all_pi[year >= 2013, cpricei := cpricei / correction]

## take logs
all_pi[, cpricei := log(cpricei)]
all_pi[, sales_tax := log(sales_tax)]

## get sales weights
all_pi[, base.sales := sales[year == 2008 & month == 1],
       by = .(store_code_uc, product_module_code)]

all_pi[, sales := NULL]
all_pi <- all_pi[!is.na(base.sales)]

## balance on store-module level
keep_store_modules <- all_pi[, list(n = .N),
                             by = .(store_code_uc, product_module_code)]
keep_store_modules <- keep_store_modules[n == (2014 - 2005) * 12]

setkey(all_pi, store_code_uc, product_module_code)
setkey(keep_store_modules, store_code_uc, product_module_code)

all_pi <- all_pi[keep_store_modules]
setkey(all_pi, fips_county, fips_state)

## attach info to covariate of choice ------------------------------------------

all_pi <- all_pi[, list(base.sales = sum(base.sales)),
                 by = .(fips_state, fips_county)]

## at this point, we have the original dataset, we can attach it to the covariate of interest

unemp_dt <- fread(unemp_path)
unemp_dt <- unemp_dt[between(year, 2006, 2014)]

all_pi <- merge(unemp_dt, all_pi, by = c("fips_county", "fips_state"))
rm(unemp_dt)
gc()

## normalize
all_pi[, normalized.rate := rate] # just for consistency in notation -- we don't actually normalized unemp
all_pi <- all_pi[!is.na(normalized.rate) & !is.na(base.sales)]

## balance on county level
keep_counties <- all_pi[, list(n = .N), by = .(fips_state, fips_county)]
keep_counties <- keep_counties[n == (2014 - 2005) * 12]

setkey(all_pi, fips_county, fips_state)
setkey(keep_counties, fips_county, fips_state)

all_pi <- all_pi[keep_counties]
setnames(all_pi, "normalized.rate", "cpricei") # just to keep the code simple

### create unique dataset of never treated counties ----------------------------
control_counties <- fread(tr_groups_path)
control_counties <- control_counties[tr_group == "No change"]
control_counties <- unique(control_counties[, .(fips_county, fips_state)])
control_dt <- merge(all_pi, control_counties, by = c("fips_state", "fips_county"))
control_dt[, ref_year := Inf]
control_dt[, ref_month := Inf]
control_dt[, init_treat := Inf]

## merge treatment, attach event times -----------------------------------------
treated_counties <- fread(eventstudy_tr_path)
# identify first treatment quarter (whether decrease or increase)
treated_counties[, ref_ct := ref_year * 12 + ref_month]
treated_counties[, init_treat := min(ref_ct), by = .(fips_state, fips_county)]
treated_counties[, ref_ct := NULL]

treated_counties <- treated_counties[tr_group == change_of_interest]

all_pi <- merge(all_pi, treated_counties, by = c("fips_state", "fips_county"))

val1 <- uniqueN(treated_counties, by = c("fips_state", "fips_county"))
val2 <- uniqueN(all_pi, by = c("fips_state", "fips_county"))
if (val1 != val2) {
  warning(sprintf("val1 (%s) != val2 (%s)", val1, val2))
}

## now we have the treated and untreated groups
print("TREATED")
print(head(all_pi))
print("CONTROL")
print(head(control_dt))

## combine the two groups ------------------------------------------------------
all_pi <- rbind(all_pi, control_dt, fill = T)
all_pi[, county_ID := .GRP, by = .(fips_state, fips_county)]

print("TREATED + CONTROL")
print(head(all_pi))

#Matrix that will store the sume of regressors X residuals (X weight = base.sales) within each cluster (county)
#Each new regression at the cohort-level produces new regressors/parameters
clustered.res.cpricei <- data.table(NULL)
# clustered.res.tax <- data.table(NULL)

#Matrix to store the "sandwiches" for OLS on cpricei
xx.cpricei <- data.table(NULL)

## loop through all possible treatment yr and mon ("cohorts") ------------------
cp.all.res <- data.table(NULL)

for (yr in 2009:2013) {
  for (mon in 1:12) {
    if (nrow(all_pi[ref_year == yr & ref_month == mon]) == 0) {
      next
    }
    flog.info("Estimating for %s m%s", yr, mon)

    # prepare a subset of data -----------------------------------------------

    # limit to the cohort or the untreated/future treated (over 1 year)
    ss_pi <- all_pi[((ref_year == yr & ref_month == mon) |
                     init_treat > (yr * 12 + mon + 12))]

    # limit estimation to 4 pre-periods and four post-periods
    ss_pi[, tt_event := (year * 12 + month) - (yr * 12 + mon)]
    ss_pi <- ss_pi[between(tt_event, -12, 12)]

    ss_pi[, treated := as.integer(ref_year == yr & ref_month == mon)]

    # count how many treated counties in the cohort
    N_counties <- length(unique(ss_pi[treated == 1]$county_ID))
    sum_sales.weights <- sum(ss_pi[treated == 1 & tt_event == 0]$base.sales)

    ss_pi$weights <- ss_pi$base.sales

    flog.info("Created subset of data for the selected groups.")
    ## create dummies for event times (except -6)
    start_cols <- copy(colnames(ss_pi))
    for (r in setdiff(-12:12, -6)) {
      var <- sprintf("catt%s", r)
      ss_pi[, (var) := as.integer(treated == 1 & tt_event == r)]
    }
    flog.info("Created mutually exclusive treatment columns.")
    # print(head(ss_pi))

    ## rename columns to prevent confusion for felm
    new_cols <- setdiff(colnames(ss_pi), start_cols)
    new_cols_used <- gsub("\\-", "lead", new_cols)
    setnames(ss_pi, new_cols, new_cols_used)

    ## estimate for cpricei =================================================
    felm_formula_input <- paste(new_cols_used, collapse = "+")
    cXp_formula <- as.formula(paste0("cpricei ~ ", felm_formula_input,
                                       " | county_ID + tt_event | 0 | county_ID"))

    res.cp <- felm(data = ss_pi, formula = cXp_formula,
                     weights = ss_pi$weights)
    flog.info("Estimated with price index as outcome.")
    # print(coef(summary(res.cp)))

    ## Get residuals for standard errors ##
    get.res2 <- function(var) { return(get.res(var, "county_ID + tt_event", "weights", ss_pi)) } #Make get.res a function of 1 variable only to iterate over

    resid <- as.data.frame(do.call(cbind, lapply(c(new_cols_used), FUN = get.res2))) #apply get.res2 to list of variables included in regression to residualize
    xx.mat <- as.matrix(resid) ## Create the X'X matrix that will be used in in the standard errors

    #Create a dataframe that is going to contain the sums of residuals*X (*weights = base.sales) for each cluster (county)
    resid$county_ID <- ss_pi$county_ID
    resid$residuals <- res.cp$residuals
    resid$weights <- ss_pi$weights

    setDT(resid)
    resid.cpricei <- resid[, list(cattlead12 = sum(weights*cattlead12*residuals),
                                  cattlead11 = sum(weights*cattlead11*residuals),
                                  cattlead10 = sum(weights*cattlead10*residuals),
                                  cattlead9 = sum(weights*cattlead9*residuals),
                                  cattlead8 = sum(weights*cattlead8*residuals),
                                  cattlead7 = sum(weights*cattlead7*residuals),
                                  cattlead5 = sum(weights*cattlead5*residuals),
                                  cattlead4 = sum(weights*cattlead4*residuals),
                                  cattlead3 = sum(weights*cattlead3*residuals),
                                  cattlead2 = sum(weights*cattlead2*residuals),
                                  cattlead1 = sum(weights*cattlead1*residuals),
                                  catt0 = sum(weights*catt0*residuals),
                                  catt1 = sum(weights*catt1*residuals),
                                  catt2 = sum(weights*catt2*residuals),
                                  catt3 = sum(weights*catt3*residuals),
                                  catt4 = sum(weights*catt4*residuals),
                                  catt5 = sum(weights*catt5*residuals),
                                  catt6 = sum(weights*catt6*residuals),
                                  catt7 = sum(weights*catt7*residuals),
                                  catt8 = sum(weights*catt8*residuals),
                                  catt9 = sum(weights*catt9*residuals),
                                  catt10 = sum(weights*catt10*residuals),
                                  catt11 = sum(weights*catt11*residuals),
                                  catt12 = sum(weights*catt12*residuals),
                                  weights = sum(weights)), by = "county_ID"]
    resid.cpricei$ref_year <- yr
    resid.cpricei$ref_mon <- mon
    resid.cpricei$n <- dim(ss_pi)[1]

    #Save these sums of "interacted" residuals for each county
    clustered.res.cpricei <- rbind(clustered.res.cpricei, resid.cpricei)
    rm(resid.cpricei)

    #Create "Sandwich" (X'WX) - where W is the diagonal matrix with the weights
    xx.mat <- t(xx.mat*as.vector(ss_pi$weights))%*%xx.mat
    xx.mat <- solve(xx.mat) ##The final matrix (including all products and cohorts) is a block-diagonal matrix - so inverse is the block diagnoal matrix with inverse of each block on the diagonal
    xx.mat <- as.data.frame(xx.mat)
    names(xx.mat) <- c("cattlead12", "cattlead11", "cattlead10",
                       "cattlead9", "cattlead8","cattlead7", "cattlead5",
                       "cattlead4","cattlead3", "cattlead2", "cattlead1",
                       "catt0", "catt1", "catt2", "catt3", "catt4",
                       "catt5", "catt6", "catt7", "catt8", "catt9",
                       "catt10", "catt11", "catt12")
    xx.mat$ref_year <- yr
    xx.mat$ref_mon <- mon

    #Save thes "sandwhich" matrices
    xx.cpricei <- rbind(xx.cpricei, xx.mat)

    ## clean and save output
    #WARNING: very inefficient but somehow the command after running regression have changed the nature of res.cp - so re-run
    res.cp <- felm(data = ss_pi, formula = cXp_formula,
                   weights = ss_pi$weights)

    res.cp <- as.data.table(summary(res.cp, robust = T)$coefficients, keep.rownames = T)
    res.cp[, rn := gsub("lead", "-", rn)]

    res.cp[, tt_event := as.integer(NA)]

    for (c in setdiff(-12:12, -6)) {
      res.cp[grepl(sprintf("catt%s", c), rn) & is.na(tt_event), tt_event := as.integer(c)]
    }
    res.cp <- res.cp[!is.na(tt_event)]

    res.cp[, ref_year := yr]
    res.cp[, ref_month := mon]
    res.cp[, outcome := "cpricei"]
    setnames(res.cp,
             old = c("Estimate", "Cluster s.e.", "Pr(>|t|)"),
             new = c("estimate", "cluster_se", "pval"))
    res.cp[, n_counties := N_counties]
    res.cp[, total_sales := sum_sales.weights]

    flog.info("Attaching output to master data.table.")
    cp.all.res <- rbind(cp.all.res, res.cp)


    rm(ss_pi)
    gc()

    #}
    # re-write once a cohort in case it crashes
    fwrite(cp.all.res, output.results.filepath)
    fwrite(clustered.res.cpricei, output.residuals.cpricei.filepath)
    # fwrite(clustered.res.tax, output.residuals.tax.filepath)
    fwrite(xx.cpricei, output.xx.filepath)

  }
}



####### Step2:  Combine information to produce asymptotic standard errors

library(tidyverse)
library(data.table)
library(readstata13)
#library(sales.taxes)
library(zoo)
library(reshape)
#library(Rcpp)
#library(RcppZiggurat)
#library(Rfast)
library(Matrix)
library(MASS)


setwd("/project2/igaarder")

###OUTPUT
output.cov.cpricei <- "Data/unemp_ei_monthly_regression_prodmatch_cpricei_varcov_matrix_combined.csv"
output.skeleton <- "Data/unemp_ei_monthly_regression_prodmatch_varcov_matrix_skeleton_combined.csv"

### INPUTS
output.results.filepath <- "Data/unemp_ei_monthly_regression_res_prodmatch_combined.csv"
output.residuals.cpricei.filepath <- "Data/unemp_ei_monthly_regression_prodmatch_residuals_cpricei_combined.csv"
#output.residuals.tax.filepath <-"Data/unemp_ei_monthly_regression_prodmatch_residuals_tax_combined.csv" ##We do not need this one here because we are interested in unemployment
output.xx.filepath <- "Data/unemp_ei_monthly_regression_prodmatch_xx_combined.csv"


cpricei.res <- fread(output.residuals.cpricei.filepath)
#tax.res <- fread(output.residuals.tax.filepath)
xx <- fread(output.xx.filepath)

#Create lists of unique values for counties, products, ref-years, ref-quarters and "parameters names"
list.counties <- unique(cpricei.res$county_ID)
list.years <- unique(cpricei.res$ref_year)
list.mon <- unique(cpricei.res$ref_mon)
list.leads <- c("cattlead12", "cattlead11", "cattlead10",
                "cattlead9", "cattlead8","cattlead7", "cattlead5",
                "cattlead4","cattlead3", "cattlead2", "cattlead1",
                "catt0", "catt1", "catt2", "catt3", "catt4",
                "catt5", "catt6", "catt7", "catt8", "catt9",
                "catt10", "catt11", "catt12")

#Create a "skeleton" containing all combination of these unique lists (except for counties)
#Create a list of all ref_yearXref_quarter for which there are some estimates
non.empty.blocks <- cpricei.res[, .(.N), .(ref_year, ref_mon)] ##Note tax.res[, .(.N), .(product_module_code, ref_year, ref_qtr)] gives exactly the same data.frame
skeleton <- expand.grid.df(as.data.frame(list.leads), non.empty.blocks)
colnames(skeleton) <- c("lead", "ref_year", "ref_mon", "N")
K.param <- dim(skeleton)[1]


### Loop over counties to create the "residuals matrix" for clustered std errors
residual.mat <- matrix(0, nrow = K.param, ncol = K.param)

start_time <- Sys.time()
k <- 1
#for(cty in list.counties[1:3])
  for(cty in list.counties) {

  ##Price Indices
  c.cpricei <- cpricei.res[county_ID == cty]
  c.cpricei <- c.cpricei %>% gather(key = "lead", value = "parameter",
                                    "cattlead12", "cattlead11", "cattlead10",
                                    "cattlead9", "cattlead8","cattlead7", "cattlead5",
                                    "cattlead4","cattlead3", "cattlead2", "cattlead1",
                                    "catt0", "catt1", "catt2", "catt3", "catt4",
                                    "catt5", "catt6", "catt7", "catt8", "catt9",
                                    "catt10", "catt11", "catt12")

  c.cpricei <- merge(skeleton, c.cpricei[,c("ref_year", "ref_mon", "lead", "parameter")], by = c("ref_year", "ref_mon", "lead"), all.x = TRUE)
  c.cpricei[is.na(c.cpricei)] <- 0



## Tried different ways because this step takes a long time mat.mult is slower here
residual.mat <- residual.mat + as.vector(c.cpricei$parameter)%*%t(as.vector(c.cpricei$parameter))

print(paste0("County number ", k, sep = ""))
end_time <- Sys.time()
print(end_time - start_time)

#Save the matrix every ten counties in case the code breaks ##I commented it out because writing the file takes forever - we definitely do not want to do this in a loop
#if(floor(k/10) == k) {

#  fwrite(residual.mat, "Data/Mat_residuals_temp.csv")

#}

k <- k + 1
}

write.table(residual.mat, "Data/large_vcov_matrices/Mat_residuals_prodmatch_ei_monthly_pi_combined.csv")

##

## Here we could construct a block diagonal matrix of (X'X)^-1 and do matrix multiplication
#But it seems easier and potentially more efficient to loop over the block by block multiplication of each section of the matrix
cov.matrix <- matrix(0, nrow = K.param, ncol = K.param)

for(i in 1:(K.param/24)) {

  print(paste0("Currently looking at row ", i, sep = ""))

  for(j in 1:(K.param/24)) {

    yr.i <- skeleton$ref_year[(i-1)*24 + 1]
    mon.i <- skeleton$ref_mon[(i-1)*24 + 1]
    yr.j <- skeleton$ref_year[(j-1)*24 + 1]
    mon.j <- skeleton$ref_mon[(j-1)*24 + 1]

    cov.matrix[((i-1)*24 + 1):((i-1)*24 + 24), ((j-1)*24 + 1):((j-1)*24 + 24)] <- as.matrix(xx[ref_year == yr.i & ref_mon == mon.i, c("cattlead12", "cattlead11", "cattlead10",
                                                                                                                                      "cattlead9", "cattlead8","cattlead7", "cattlead5",
                                                                                                                                      "cattlead4","cattlead3", "cattlead2", "cattlead1",
                                                                                                                                      "catt0", "catt1", "catt2", "catt3", "catt4",
                                                                                                                                      "catt5", "catt6", "catt7", "catt8", "catt9",
                                                                                                                                      "catt10", "catt11", "catt12")])%*%residual.mat[((i-1)*24 + 1):((i-1)*24 + 24), ((j-1)*24 + 1):((j-1)*24 + 24)]%*%as.matrix(xx[ref_year == yr.j & ref_mon == mon.j, c("cattlead12", "cattlead11", "cattlead10",
                                                                                                                                                                                                                                                                                                           "cattlead9", "cattlead8","cattlead7", "cattlead5",
                                                                                                                                                                                                                                                                                                           "cattlead4","cattlead3", "cattlead2", "cattlead1",
                                                                                                                                                                                                                                                                                                           "catt0", "catt1", "catt2", "catt3", "catt4",
                                                                                                                                                                                                                                                                                                           "catt5", "catt6", "catt7", "catt8", "catt9",
                                                                                                                                                                                                                                                                                                           "catt10", "catt11", "catt12")])
  }
}


fwrite(cov.matrix, output.cov.cpricei)
fwrite(skeleton, output.skeleton)



##### Step 3: Produce standard errors of specific parameters/averages of parameters using the Delta method
output.estimates.stderr.filepath <- "Data/Passthrough_estimates_stderr_ei_monthly_combined.csv"


#cov.matrix <- fread(output.cov.cpricei)
#skeleton <- fread(output.skeleton)
#cp.all.res <- fread(output.results.filepath)
#K.param <- dim(skeleton)[1]

##Produce standard errors for cpricei pooled across cohorts
#cohort.weights <- cp.all.res[,.(weights = mean(total_sales)), by = .(ref_year, ref_quarter)]  ##The mean operator does not matter here - total_sales is constant within cohort but we just want to collapse to get a vector
cohort.weights <- cp.all.res[outcome == 'cpricei', "total_sales"]
colnames(cohort.weights) <- "weights"
pooled.var <- matrix(0, nrow = 24, ncol = 1)
for(i in 1:24) {

  delta <- c(rep(c(rep(0, (i-1)), 1, rep(0, (24-i))), K.param/24), rep(0, K.param))

  gradient <- delta*cohort.weights$weights
  norm.gradient <- gradient/sum(gradient)


  pooled.var[i] <- t(as.vector(norm.gradient))%*%as.matrix(cov.matrix)%*%as.vector(norm.gradient)
}

estimates <- cp.all.res[,.(estimates = weighted.mean(estimate, w = total_sales)), by = .(rn, outcome)]
estimates$std.errors <- sqrt(pooled.var)


##Standard errors for pooled post-reform estimates (pooled across cohorts and across catt0-catt4)
##Cpricei
weights <- setDT(cp.all.res[outcome == 'cpricei',])
delta <- c(rep(c(rep(0, 11), rep(1, 13)), K.param/24), rep(0, K.param)) #Selects all post-period estimates

gradient <- delta*weights$total_sales
norm.gradient <- gradient/sum(gradient)

var <- t(as.vector(norm.gradient))%*%as.matrix(cov.matrix)%*%as.vector(norm.gradient)
est <- mean(estimates[outcome == 'cpricei' & (rn %in% c('catt0', 'catt1', 'catt2', 'catt3', 'catt4',
                                                        'catt5', 'catt6', 'catt7', 'catt8', 'catt9',
                                                        'catt10', 'catt11', 'catt12')), estimates])

## R was weird and would convert the estimates value to factors - so I get around this by creating the estimates and std.error columns separately
#This is really stupid, we should change this
temp <- estimates[,c("rn", "outcome")]
temp2 <- as.data.frame(t(as.vector(c("post-treatment", "cpricei"))))
colnames(temp2) <- c("rn", "outcome")
temp <- rbind(temp, temp2)

temp3 <- estimates[,c("estimates", "std.errors")]
temp2 <- t(as.vector(c(est, sqrt(var))))
temp2 <- as.data.frame(temp2)
colnames(temp2) <- c("estimates", "std.errors")
estimates <- rbind(temp3, temp2)
estimates$rn <- temp$rn
estimates$outcome <- temp$outcome


###Write to a file
fwrite(estimates, output.estimates.stderr.filepath)
