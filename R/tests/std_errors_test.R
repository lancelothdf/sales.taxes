#' Author: John Bonney & Lancelot Henry de Frahan
#' NOTE:  In this code, we reweigh the control group so that distribution of
#' products matches exactly distribution of products in the treatment group
#'
#'    - Only one regression per cohort (not product-specific)
#'
#'    - We bootstrap (draw at the county-level to preserve within cluster/county
#'      correlation) so that we can compute std errors and confidence intervals
#'
#' Idea is to think about a regression specification that relates to the
#'     figures we have been making. We will want to run two regressions,
#'     one corresponding to the event-no-event case and one corresponding
#'     to the time-of-event case.

library(data.table)
library(lfe)
library(futile.logger)
library(AER)


setwd("/project2/igaarder")
change_of_interest <- "Ever increase"


output.results.filepath <- "Data/pi_ei_regression_res_prodmatch_combined.csv"
output.residuals.cpricei.filepath <- "Data/pi_ei_regression_prodmatch_residuals_cpricei_combined.csv"
output.residuals.tax.filepath <-"Data/pi_ei_regression_prodmatch_residuals_tax_combined.csv"
output.xx.filepath <- "Data/pi_ei_regression_prodmatch_xx_combined.csv"


## useful filepaths ------------------------------------------------------------
all_goods_pi_path <- "Data/Nielsen/price_quantity_indices_allitems_2006-2016_notaxinfo.csv"
taxable_pi_path <- "Data/Nielsen/price_quantity_indices_taxableitems_2006-2016.csv"
eventstudy_tr_path <- "Data/event_study_tr_groups_comprehensive_firstonly_no2012q4_2013q1q2.csv"
tr_groups_path <- "Data/tr_groups_comprehensive_firstonly_no2012q4_2013q1q2.csv"


## Want to run cohort-product specific regressions.
## Data is on store-product-quarter level

######
##Create a function to residualize/demean data
#var is a list of variables to residualize, group are the factors over which to residualize, w are the weights, mtx is the name of the data.frame

# Start with event-no-event case ===============================================

## prep the data ---------------------------------------------------------------

all_pi <- fread(all_goods_pi_path)
all_pi <- all_pi[year %in% 2006:2014 & !is.na(cpricei)]
# limit it to taxable goods
all_pi <- all_pi[sales_tax > 1 | (year < 2008 & is.na(sales_tax))]

# do `arbitrary` correction for the 2013 Q1 jump in the data
## calculate price index in 2013 Q1 / cpricei in 2012 Q4
all_pi[, correction := pricei[year == 2013 & quarter == 1] / pricei[year == 2012 & quarter == 4],
       by = .(store_code_uc, product_module_code)]
## divide price index after 2013 Q1 (inclusive) by above value
all_pi[year >= 2013, cpricei := cpricei / correction]

## take logs
all_pi[, cpricei := log(cpricei)]
all_pi[, sales_tax := log(sales_tax)]

## get sales weights
all_pi[, base.sales := sales[year == 2008 & quarter == 1],
       by = .(store_code_uc, product_module_code)]

all_pi[, sales := NULL]
all_pi <- all_pi[!is.na(base.sales)]

## balance on store-module level
keep_store_modules <- all_pi[, list(n = .N),
                             by = .(store_code_uc, product_module_code)]
keep_store_modules <- keep_store_modules[n == (2014 - 2005) * 4]

setkey(all_pi, store_code_uc, product_module_code)
setkey(keep_store_modules, store_code_uc, product_module_code)

all_pi <- all_pi[keep_store_modules]
setkey(all_pi, fips_county, fips_state)

### create unique dataset of never treated counties ----------------------------
control_counties <- fread(tr_groups_path)
control_counties <- control_counties[tr_group == "No change"]
control_counties <- unique(control_counties[, .(fips_county, fips_state)])
control_dt <- merge(all_pi, control_counties, by = c("fips_state", "fips_county"))
control_dt[, ref_year := Inf]
control_dt[, ref_quarter := Inf]
control_dt[, init_treat := Inf]

## merge treatment, attach event times -----------------------------------------
treated_counties <- fread(eventstudy_tr_path)
# identify first treatment quarter (whether decrease or increase)
treated_counties[, ref_ct := ref_year * 4 + ceiling(ref_month / 3)]
treated_counties[, init_treat := min(ref_ct), by = .(fips_state, fips_county)]
treated_counties[, ref_ct := NULL]

treated_counties <- treated_counties[tr_group == change_of_interest]
treated_counties[, ref_quarter := ceiling(ref_month / 3)]
treated_counties[, ref_month := NULL]

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
clustered.res.tax <- data.table(NULL)


##############
##############
get.res <- function(var, group, w, mtx) {

  form <- as.formula(paste0(var, "~ 1 | ", group, " | 0 | 0", sep = ""))
  return(felm(data = mtx, formula = form , weights = mtx[,get(w)])$residuals)

}




#Matrix to store the "sandwiches" for OLS on cpricei
xx.cpricei <- data.table(NULL)


## loop through all possible treatment yr and qtr ("cohorts") ------------------
cp.all.res <- data.table(NULL)

#for (yr in 2009:2013) {
#  for (qtr in 1:4) {
yr <- 2009
qtr <- 1
    if (nrow(all_pi[ref_year == yr & ref_quarter == qtr]) == 0) {
      next
    }
    flog.info("Estimating for %s Q%s", yr, qtr)


    #Make a list of unique product codes in the treatment group
    list.prod <- unique(all_pi[ref_year == yr & ref_quarter == qtr]$product_module_code)
    # prepare a subset of data -----------------------------------------------

    # limit to the cohort or the untreated/future treated (over 1 year)
    ss_pi <- all_pi[((ref_year == yr & ref_quarter == qtr) |
                     init_treat > (yr * 4 + qtr + 4))]

    # limit estimation to 4 pre-periods and four post-periods
    ss_pi[, tt_event := (year * 4 + quarter) - (yr * 4 + qtr)]
    ss_pi <- ss_pi[between(tt_event, -4, 4)]

    #Keep only products that are in the treatment group
    ss_pi <- ss_pi[product_module_code %in% list.prod]

    ss_pi[, treated := as.integer(ref_year == yr & ref_quarter == qtr)]

    # count how many treated counties in the cohort
    N_counties <- length(unique(ss_pi[treated == 1]$county_ID))
    sum_sales.weights <- sum(ss_pi[treated == 1 & tt_event == 0]$base.sales)

    ##Create weights for control group to matche exactly distribution of products (weighted by sales) in treatment group
    ss_pi[, base.sales.tr := sum(base.sales[year == yr & quarter == qtr & treated == 1]),
          by = .(product_module_code)]

    ss_pi[, base.sales.ctl := sum(base.sales[year == yr & quarter == qtr & treated == 0]),
          by = .(product_module_code)]

    ss_pi$weights <- ss_pi$base.sales
    ss_pi[treated == 0]$weights <- ss_pi[treated == 0]$base.sales*ss_pi[treated == 0]$base.sales.tr/ss_pi[treated == 0]$base.sales.ctl


    flog.info("Created subset of data for the selected groups.")
    ## create dummies for event times (except -2)
    start_cols <- copy(colnames(ss_pi))
    for (r in setdiff(-4:4, -2)) {
      var <- sprintf("catt%s", r)
      ss_pi[, (var) := as.integer(treated == 1 & tt_event == r)]
    }
    flog.info("Created mutually exclusive treatment columns.")
    print(head(ss_pi))

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
    print(coef(summary(res.cp)))

    ## Get residuals for standard errors ##
    get.res2 <- function(var) { return(get.res(var, "county_ID + tt_event", "weights", ss_pi)) } #Make get.res a function of 1 variable only to iterate over

    resid <- as.data.frame(do.call(cbind, lapply(c(new_cols_used), FUN = get.res2))) #apply get.res2 to list of variables included in regression to residualize
    xx.mat <- as.matrix(resid) ## Create the X'X matrix that will be used in in the standard errors

    #Create a dataframe that is going to contain the sums of residuals*X (*weights = base.sales) for each cluster (county)
    resid$county_ID <- ss_pi$county_ID
    resid$residuals <- res.cp$residuals
    resid$weights <- ss_pi$weights

    setDT(resid)
    resid.cpricei <- resid[, list(cattlead4 = sum(weights*cattlead4*residuals), cattlead3 = sum(weights*cattlead3*residuals), cattlead1 = sum(weights*cattlead1*residuals), catt0 = sum(weights*catt0*residuals), catt1 = sum(weights*catt1*residuals), catt2 = sum(weights*catt2*residuals), catt3 = sum(weights*catt3*residuals), catt4 = sum(weights*catt4*residuals), weights = sum(weights)), by = "county_ID"]
    resid.cpricei$ref_year = yr
    resid.cpricei$ref_qtr = qtr
    resid.cpricei$n <- dim(ss_pi)[1]

    #Save these sums of "interacted" residuals for each county
    #clustered.res.cpricei <- rbind(clustered.res.cpricei, resid.cpricei)
    #rm(resid.cpricei)
    clustered.res.cpricei <- resid.cpricei

    #Create "Sandwich" (X'WX) - where W is the diagonal matrix with the weights
    xx.mat <- t(xx.mat*as.vector(ss_pi$weights))%*%xx.mat
    xx.mat <- solve(xx.mat) ##The final matrix (including all products and cohorts) is a block-diagonal matrix - so inverse is the block diagnoal matrix with inverse of each block on the diagonal
    xx.mat <- as.data.frame(xx.mat)
    names(xx.mat) <- c("cattlead4", "cattlead3", "cattlead1", "catt0", "catt1", "catt2", "catt3", "catt4")
    xx.mat$ref_year <- yr
    xx.mat$ref_qtr <- qtr

    #Save thes "sandwhich" matrices
    #xx.cpricei <- rbind(xx.cpricei, xx.mat)
    xx.cpricei <- xx.mat

    ## clean and save output
    #WARNING: very inefficient but somehow the command after running regression have changed the nature of res.cp - so re-run
    res.cp <- felm(data = ss_pi, formula = cXp_formula,
                   weights = ss_pi$weights)

    res.cp <- as.data.table(summary(res.cp, robust = T)$coefficients, keep.rownames = T)
    res.cp[, rn := gsub("lead", "-", rn)]

    res.cp[, tt_event := as.integer(NA)]

    for (c in setdiff(-4:4, -2)) {
      res.cp[grepl(sprintf("catt%s", c), rn) & is.na(tt_event), tt_event := as.integer(c)]
    }
    res.cp <- res.cp[!is.na(tt_event)]

    res.cp[, ref_year := yr]
    res.cp[, ref_quarter := qtr]
    res.cp[, outcome := "cpricei"]
    setnames(res.cp,
             old = c("Estimate", "Cluster s.e.", "Pr(>|t|)"),
             new = c("estimate", "cluster_se", "pval"))
    res.cp[, n_counties := N_counties]
    res.cp[, total_sales := sum_sales.weights]

    flog.info("Attaching output to master data.table.")
    cp.all.res <- rbind(cp.all.res, res.cp)

#  }
#}



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


#setwd("/project2/igaarder")

###OUTPUT
#output.cov.cpricei <- "Data/pi_ei_regression_prodmatch_cpricei_varcov_matrix_combined.csv"
#output.skeleton <- "Data/pi_ei_regression_prodmatch_varcov_matrix_skeleton_combined.csv"

### INPUTS
#output.results.filepath <- "Data/pi_ei_regression_res_prodmatch_combined.csv"
#output.residuals.cpricei.filepath <- "Data/pi_ei_regression_prodmatch_residuals_cpricei_combined.csv"
#output.residuals.tax.filepath <-"Data/pi_ei_regression_prodmatch_residuals_tax_combined.csv"
#output.xx.filepath <- "Data/pi_ei_regression_prodmatch_xx_combined.csv"


#cpricei.res <- fread(output.residuals.cpricei.filepath)
#tax.res <- fread(output.residuals.tax.filepath)
#xx <- fread(output.xx.filepath)

#Create lists of unique values for counties, products, ref-years, ref-quarters and "parameters names"
cpricei.res <- resid.cpricei

list.counties <- unique(cpricei.res$county_ID)
list.years <- unique(cpricei.res$ref_year)
list.qtr <- unique(cpricei.res$ref_qtr)
list.leads <- c("cattlead4", "cattlead3", "cattlead1", "catt0", "catt1", "catt2", "catt3", "catt4")

#Create a "skeleton" containing all combination of these unique lists (except for counties)
#Create a list of all ref_yearXref_quarter for which there are some estimates
non.empty.blocks <- cpricei.res[, .(.N), .(ref_year, ref_qtr)] ##Note tax.res[, .(.N), .(product_module_code, ref_year, ref_qtr)] gives exactly the same data.frame
skeleton <- expand.grid.df(as.data.frame(list.leads), non.empty.blocks)
colnames(skeleton) <- c("lead", "ref_year", "ref_qtr", "N")
K.param <- dim(skeleton)[1]


### Loop over counties to create the "residuals matrix" for clustered std errors
residual.mat <- matrix(0, nrow = K.param, ncol = K.param)

start_time <- Sys.time()
k <- 1
#for(cty in list.counties[1:3])
  for(cty in list.counties) {

  ##Price Indices
  c.cpricei <- cpricei.res[county_ID == cty]
  c.cpricei <- c.cpricei %>% gather(key = "lead", value = "parameter", "cattlead4", "cattlead3", "cattlead1", "catt0", "catt1", "catt2", "catt3", "catt4")

  c.cpricei <- merge(skeleton, c.cpricei[,c("ref_year", "ref_qtr", "lead", "parameter")], by = c("ref_year", "ref_qtr", "lead"), all.x = TRUE)
  c.cpricei <- c.cpricei[match(skeleton$lead, c.cpricei$lead),] ##Make sure that the sequence follows cattlead4 to cattlead1, catt0 to catt4
  c.cpricei[is.na(c.cpricei)] <- 0


c.all <- c.cpricei

## Tried different ways because this step takes a long time mat.mult is slower here
#residual.mat <- residual.mat + mat.mult(as.vector(c.cpricei$parameter), t(as.vector(c.cpricei$parameter)))
#residual.mat <- residual.mat + crossprod(t(as.vector(c.cpricei$parameter)), y = NULL)
#residual.mat <- residual.mat + as.vector(c.cpricei$parameter)%o%as.vector(c.cpricei$parameter)
residual.mat <- residual.mat + as.vector(c.all$parameter)%*%t(as.vector(c.all$parameter)) ##This way is the fastest but still 40 seconds on average for each iteration (given large number of counties - the loop is slow)

print(paste0("County number ", k, sep = ""))
end_time <- Sys.time()
print(end_time - start_time)

#Save the matrix every ten counties in case the code breaks ##I commented it out because writing the file takes forever - we definitely do not want to do this in a loop
#if(floor(k/10) == k) {

#  fwrite(residual.mat, "Data/Mat_residuals_temp.csv")

#}

k <- k + 1
}

#write.table(residual.mat, "Data/large_vcov_matrices/Mat_residuals_prodmatch_ei_pi_combined.csv")

##

## Here we could construct a block diagonal matrix of (X'X)^-1 and do matrix multiplication
#But it seems easier and potentially more efficient to loop over the block by block multiplication of each section of the matrix
cov.matrix <- matrix(0, nrow = K.param, ncol = K.param)

for(i in 1:(K.param/8)) {

  print(paste0("Currently looking at row ", i, sep = ""))

  for(j in 1:(K.param/8)) {

    yr.i <- skeleton$ref_year[(i-1)*8 + 1]
    qtr.i <- skeleton$ref_qtr[(i-1)*8 + 1]
    yr.j <- skeleton$ref_year[(j-1)*8 + 1]
    qtr.j <- skeleton$ref_qtr[(j-1)*8 + 1]

    cov.matrix[((i-1)*8 + 1):((i-1)*8 + 8), ((j-1)*8 + 1):((j-1)*8 + 8)] <- as.matrix(xx[ref_year == yr.i & ref_qtr == qtr.i, c("cattlead4", "cattlead3", "cattlead1", "catt0", "catt1", "catt2", "catt3", "catt4")])%*%residual.mat[((i-1)*8 + 1):((i-1)*8 + 8), ((j-1)*8 + 1):((j-1)*8 + 8)]%*%as.matrix(xx[ref_year == yr.j & ref_qtr == qtr.j, c("cattlead4", "cattlead3", "cattlead1", "catt0", "catt1", "catt2", "catt3", "catt4")])
  }
}

std.error <- sqrt(diag(cov.matrix))


