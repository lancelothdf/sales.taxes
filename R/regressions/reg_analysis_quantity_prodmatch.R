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
#'
#' Command line argument one: the change of interest.
#' Command line argument two: the output filepath.
#'
#' E.g., $ Rscript reg_analysis_quantity_prodmatch.R "Ever increase" "pi_ei"

library(data.table)
library(lfe)
library(futile.logger)
library(AER)


setwd("/project2/igaarder")
args <- commandArgs(TRUE)
if (length(args) != 2) {
  stop("Two arguments are needed")
}

change_of_interest <- args[1]
if (!change_of_interest %in% c("Ever increase", "Ever decrease", "Increase only")) {
  stop(sprintf("`change_of_interest` = %s is invalid", change_of_interest))
}

output.filestub <- args[2]

output.results.filepath <- paste0("Data/", output.filestub, "_regression_res_prodmatch.csv")
output.residuals.ln_quantity.filepath <- paste0("Data/", output.filestub, "_regression_prodmatch_residuals.csv")
output.residuals.tax.filepath <- paste0("Data/", output.filestub, "_regression_prodmatch_residuals_tax.csv")
output.xx.filepath <- paste0("Data/", output.filestub, "_regression_prodmatch_xx.csv")


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
get.res <- function(var, group, w, mtx) {

  form <- as.formula(paste0(var, "~ 1 | ", group, " | 0 | 0", sep = ""))
  return(felm(data = mtx, formula = form , weights = mtx[,get(w)])$residuals)

}


# Start with event-no-event case ===============================================

## prep the data ---------------------------------------------------------------

all_pi <- fread(all_goods_pi_path)
all_pi <- all_pi[year %in% 2006:2014 & !is.na(cpricei)]
# limit it to taxable goods
all_pi <- all_pi[sales_tax > 1 | (year < 2008 & is.na(sales_tax))]

## take logs
all_pi[, ln_pricei := log(pricei)]
all_pi[, sales_tax := log(sales_tax)]

## get sales weights
all_pi[, base.sales := sales[year == 2008 & quarter == 1],
       by = .(store_code_uc, product_module_code)]

all_pi <- all_pi[!is.na(base.sales)]
all_pi[, ln_quantity := log(sales) - ln_pricei]

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
clustered.res.ln_quantity <- data.table(NULL)
clustered.res.tax <- data.table(NULL)

#Matrix to store the "sandwiches" for OLS on ln_quantity
xx.ln_quantity <- data.table(NULL)

## loop through all possible treatment yr and qtr ("cohorts") ------------------
cp.all.res <- data.table(NULL)

for (yr in 2009:2013) {
  for (qtr in 1:4) {
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

    ## estimate for ln_quantity =================================================
    felm_formula_input <- paste(new_cols_used, collapse = "+")
    cXp_formula <- as.formula(paste0("ln_quantity ~ ", felm_formula_input,
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
    resid.ln_quantity <- resid[, list(cattlead4 = sum(weights*cattlead4*residuals), cattlead3 = sum(weights*cattlead3*residuals), cattlead1 = sum(weights*cattlead1*residuals), catt0 = sum(weights*catt0*residuals), catt1 = sum(weights*catt1*residuals), catt2 = sum(weights*catt2*residuals), catt3 = sum(weights*catt3*residuals), catt4 = sum(weights*catt4*residuals), weights = sum(weights)), by = "county_ID"]
    resid.ln_quantity$ref_year = yr
    resid.ln_quantity$ref_qtr = qtr
    resid.ln_quantity$n <- dim(ss_pi)[1]

    #Save these sums of "interacted" residuals for each county
    clustered.res.ln_quantity <- rbind(clustered.res.ln_quantity, resid.ln_quantity)
    rm(resid.ln_quantity)

    #Create "Sandwich" (X'WX) - where W is the diagonal matrix with the weights
    xx.mat <- t(xx.mat*as.vector(ss_pi$weights))%*%xx.mat  ### In general, we potentially want to make sure that variables are ordered right in this matrix (but because all regressors in this case are mutually exclusive dummies - this matrix actually has a constant on the diagnoal and same number off diagonal)
    xx.mat <- solve(xx.mat) ##The final matrix (including all products and cohorts) is a block-diagonal matrix - so inverse is the block diagnoal matrix with inverse of each block on the diagonal
    xx.mat <- as.data.frame(xx.mat)
    names(xx.mat) <- c("cattlead4", "cattlead3", "cattlead1", "catt0", "catt1", "catt2", "catt3", "catt4")
    xx.mat$ref_year <- yr
    xx.mat$ref_qtr <- qtr

    #Save thes "sandwhich" matrices
    xx.ln_quantity <- rbind(xx.ln_quantity, xx.mat)

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
    res.cp[, outcome := "ln_quantity"]
    setnames(res.cp,
             old = c("Estimate", "Cluster s.e.", "Pr(>|t|)"),
             new = c("estimate", "cluster_se", "pval"))
    res.cp[, n_counties := N_counties]
    res.cp[, total_sales := sum_sales.weights]

    flog.info("Attaching output to master data.table.")
    cp.all.res <- rbind(cp.all.res, res.cp)

    ## run for log(1 + tax) as well ==========================================
    #drop_cols <- paste0("cattlead", 8:5)
    #tax_cols <- setdiff(new_cols_used, drop_cols)
    tax_cols <- new_cols_used

    #ss_pi[, c(drop_cols) := NULL]
    #ss_pi <- ss_pi[between(tt_event, -4, 4)] # to be consistent across cohorts

    ## create formula
    tax_formula_input <- paste(tax_cols, collapse = "+")
    tax_formula <- as.formula(paste0("sales_tax ~ ", tax_formula_input,
                                       " | county_ID + tt_event | 0 | county_ID"))

    res.tax <- felm(data = ss_pi, formula = tax_formula,
                      weights = ss_pi$weights)
    flog.info("Estimated with tax rate as outcome.")
    print(coef(summary(res.tax)))

    #resid is same as for previous regression - so we re-use it (NEED TO REPLACE RESIDUALS THOUGH)
    resid$residuals <- res.tax$residuals

    setDT(resid)
    resid.tax <- resid[, list(cattlead4 = sum(weights*cattlead4*residuals), cattlead3 = sum(weights*cattlead3*residuals), cattlead1 = sum(weights*cattlead1*residuals), catt0 = sum(weights*catt0*residuals), catt1 = sum(weights*catt1*residuals), catt2 = sum(weights*catt2*residuals), catt3 = sum(weights*catt3*residuals), catt4 = sum(weights*catt4*residuals), weights = sum(weights)), by = "county_ID"]
    resid.tax$ref_year = yr
    resid.tax$ref_qtr = qtr
    resid.tax$n <- dim(ss_pi)[1]

    #Save these sums of "interacted" residuals for each county
    clustered.res.tax <- rbind(clustered.res.tax, resid.tax)
    rm(resid.tax, resid)

    res.tax <- as.data.table(summary(res.tax, robust = T)$coefficients, keep.rownames = T)
    res.tax[, rn := gsub("lead", "-", rn)]

    res.tax[, tt_event := as.integer(NA)]

    for (c in setdiff(-4:4, -2)) {
      res.tax[grepl(sprintf("catt%s", c), rn) & is.na(tt_event), tt_event := as.integer(c)]
    }
    res.tax <- res.tax[!is.na(tt_event)]

    res.tax[, ref_year := yr]
    res.tax[, ref_quarter := qtr]
    res.tax[, outcome := "sales_tax"]
    setnames(res.tax,
             old = c("Estimate", "Cluster s.e.", "Pr(>|t|)"),
             new = c("estimate", "cluster_se", "pval"))
    res.tax[, n_counties := N_counties]
    res.tax[, total_sales := sum_sales.weights]

    flog.info("Attaching output to master data.table.")
    cp.all.res <- rbind(cp.all.res, res.tax)

    rm(ss_pi)
    gc()

    #}
    # re-write once a cohort in case it crashes
    fwrite(cp.all.res, output.results.filepath)
    fwrite(clustered.res.ln_quantity, output.residuals.ln_quantity.filepath)
    fwrite(clustered.res.tax, output.residuals.tax.filepath)
    fwrite(xx.ln_quantity, output.xx.filepath)

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
output.cov.ln_quantity <- paste0("Data/", output.filestub, "_regression_prodmatch_varcov_matrix.csv")
output.skeleton <- paste0("Data/", output.filestub, "_regression_prodmatch_varcov_matrix_skeleton.csv")

### INPUTS
output.results.filepath <- paste0("Data/", output.filestub, "_regression_res_prodmatch.csv")
output.residuals.ln_quantity.filepath <- paste0("Data/", output.filestub, "_regression_prodmatch_residuals.csv")
output.residuals.tax.filepath <- paste0("Data/", output.filestub, "_regression_prodmatch_residuals_tax.csv")
output.xx.filepath <- paste0("Data/", output.filestub, "_regression_prodmatch_xx.csv")

ln_quantity.res <- fread(output.residuals.ln_quantity.filepath)
tax.res <- fread(output.residuals.tax.filepath)
xx <- fread(output.xx.filepath)

#Create lists of unique values for counties, products, ref-years, ref-quarters and "parameters names"
list.counties <- unique(ln_quantity.res$county_ID)
list.years <- unique(ln_quantity.res$ref_year)
list.qtr <- unique(ln_quantity.res$ref_qtr)
list.leads <- c("cattlead4", "cattlead3", "cattlead1", "catt0", "catt1", "catt2", "catt3", "catt4")

#Create a "skeleton" containing all combination of these unique lists (except for counties)
#Create a list of all ref_yearXref_quarter for which there are some estimates
non.empty.blocks <- ln_quantity.res[, .(.N), .(ref_year, ref_qtr)] ##Note tax.res[, .(.N), .(product_module_code, ref_year, ref_qtr)] gives exactly the same data.frame
skeleton <- expand.grid.df(as.data.frame(list.leads), non.empty.blocks)
colnames(skeleton) <- c("lead", "ref_year", "ref_qtr", "N")
K.param <- dim(skeleton)[1]
skeleton$ID <- c(1:K.param) ##Create an ID to re-order observations in the same way after every merge


### Loop over counties to create the "residuals matrix" for clustered std errors
residual.mat <- matrix(0, nrow = 2*K.param, ncol = 2*K.param)

start_time <- Sys.time()
k <- 1
#for(cty in list.counties[1:3])
  for(cty in list.counties) {

  ##Price Indices
  c.ln_quantity <- ln_quantity.res[county_ID == cty]
  c.ln_quantity <- c.ln_quantity %>% gather(key = "lead", value = "parameter", "cattlead4", "cattlead3", "cattlead1", "catt0", "catt1", "catt2", "catt3", "catt4")

  c.ln_quantity <- merge(skeleton, c.ln_quantity[,c("ref_year", "ref_qtr", "lead", "parameter")], by = c("ref_year", "ref_qtr", "lead"), all.x = TRUE)

  setDT(c.ln_quantity)
  c.ln_quantity <- c.ln_quantity[order(ID),] ##Make sure that the sequence follows cattlead4 to cattlead1, catt0 to catt4
  c.ln_quantity[is.na(c.ln_quantity)] <- 0

## Taxes
c.tax <- tax.res[county_ID == cty] #More interesting example because this county shows up multiple times (being in the control group)
c.tax <- c.tax %>% gather(key = "lead", value = "parameter", "cattlead4", "cattlead3", "cattlead1", "catt0", "catt1", "catt2", "catt3", "catt4")

c.tax <- merge(skeleton, c.tax[,c("ref_year", "ref_qtr", "lead", "parameter")], by = c("ref_year", "ref_qtr", "lead"), all.x = TRUE)

setDT(c.tax)
c.tax <- c.tax[order(ID),] ##Make sure that the sequence follows cattlead4 to cattlead1, catt0 to catt4
c.tax[is.na(c.tax)] <- 0

c.all <- rbind(c.ln_quantity, c.tax)
rm(c.ln_quantity, c.tax)

## Tried different ways because this step takes a long time mat.mult is slower here
#residual.mat <- residual.mat + mat.mult(as.vector(c.ln_quantity$parameter), t(as.vector(c.ln_quantity$parameter)))
#residual.mat <- residual.mat + crossprod(t(as.vector(c.ln_quantity$parameter)), y = NULL)
#residual.mat <- residual.mat + as.vector(c.ln_quantity$parameter)%o%as.vector(c.ln_quantity$parameter)
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

write.table(residual.mat, paste0("Data/large_vcov_matrices/Mat_residuals_prodmatch_", output.filestub, ".csv"))

##

## Here we could construct a block diagonal matrix of (X'X)^-1 and do matrix multiplication
#But it seems easier and potentially more efficient to loop over the block by block multiplication of each section of the matrix
cov.matrix <- matrix(0, nrow = 2*K.param, ncol = 2*K.param)

for(i in 1:(K.param/8)) {

  print(paste0("Currently looking at row ", i, sep = ""))

  for(j in 1:(K.param/8)) {

    yr.i <- skeleton$ref_year[(i-1)*8 + 1]
    qtr.i <- skeleton$ref_qtr[(i-1)*8 + 1]
    yr.j <- skeleton$ref_year[(j-1)*8 + 1]
    qtr.j <- skeleton$ref_qtr[(j-1)*8 + 1]

    cov.matrix[((i-1)*8 + 1):((i-1)*8 + 8), ((j-1)*8 + 1):((j-1)*8 + 8)] <- as.matrix(xx[ref_year == yr.i & ref_qtr == qtr.i, c("cattlead4", "cattlead3", "cattlead1", "catt0", "catt1", "catt2", "catt3", "catt4")])%*%residual.mat[((i-1)*8 + 1):((i-1)*8 + 8), ((j-1)*8 + 1):((j-1)*8 + 8)]%*%as.matrix(xx[ref_year == yr.j & ref_qtr == qtr.j, c("cattlead4", "cattlead3", "cattlead1", "catt0", "catt1", "catt2", "catt3", "catt4")])
    cov.matrix[(K.param + (i-1)*8 + 1):(K.param + (i-1)*8 + 8), ((j-1)*8 + 1):((j-1)*8 + 8)] <- as.matrix(xx[ref_year == yr.i & ref_qtr == qtr.i, c("cattlead4", "cattlead3", "cattlead1", "catt0", "catt1", "catt2", "catt3", "catt4")])%*%residual.mat[(K.param + (i-1)*8 + 1):(K.param + (i-1)*8 + 8), ((j-1)*8 + 1):((j-1)*8 + 8)]%*%as.matrix(xx[ref_year == yr.j & ref_qtr == qtr.j, c("cattlead4", "cattlead3", "cattlead1", "catt0", "catt1", "catt2", "catt3", "catt4")])
    cov.matrix[((i-1)*8 + 1):((i-1)*8 + 8), (K.param + (j-1)*8 + 1):(K.param + (j-1)*8 + 8)] <- as.matrix(xx[ref_year == yr.i & ref_qtr == qtr.i, c("cattlead4", "cattlead3", "cattlead1", "catt0", "catt1", "catt2", "catt3", "catt4")])%*%residual.mat[((i-1)*8 + 1):((i-1)*8 + 8), (K.param + (j-1)*8 + 1):(K.param + (j-1)*8 + 8)]%*%as.matrix(xx[ref_year == yr.j & ref_qtr == qtr.j, c("cattlead4", "cattlead3", "cattlead1", "catt0", "catt1", "catt2", "catt3", "catt4")])
    cov.matrix[(K.param + (i-1)*8 + 1):(K.param + (i-1)*8 + 8), (K.param + (j-1)*8 + 1):(K.param + (j-1)*8 + 8)] <- as.matrix(xx[ref_year == yr.i & ref_qtr == qtr.i, c("cattlead4", "cattlead3", "cattlead1", "catt0", "catt1", "catt2", "catt3", "catt4")])%*%residual.mat[(K.param + (i-1)*8 + 1):(K.param + (i-1)*8 + 8), (K.param + (j-1)*8 + 1):(K.param + (j-1)*8 + 8)]%*%as.matrix(xx[ref_year == yr.j & ref_qtr == qtr.j, c("cattlead4", "cattlead3", "cattlead1", "catt0", "catt1", "catt2", "catt3", "catt4")])
  }
}


fwrite(cov.matrix, output.cov.ln_quantity)
fwrite(skeleton, output.skeleton)



##### Step 3: Produce standard errors of specific parameters/averages of parameters using the Delta method
output.estimates.stderr.filepath <- paste0("Data/Passthrough_estimates_stderr_", output.filestub, ".csv")


#cov.matrix <- fread(output.cov.ln_quantity)
#skeleton <- fread(output.skeleton)
#cp.all.res <- fread(output.results.filepath)
#K.param <- dim(skeleton)[1]

##Produce standard errors for ln_quantity and tax estimates pooled across cohorts
#cohort.weights <- cp.all.res[,.(weights = mean(total_sales)), by = .(ref_year, ref_quarter)]  ##The mean operator does not matter here - total_sales is constant within cohort but we just want to collapse to get a vector
cohort.weights <- cp.all.res[outcome == 'ln_quantity', "total_sales"]
colnames(cohort.weights) <- "weights"
pooled.var <- matrix(0, nrow = 16, ncol = 1)
for(i in 1:8) {

  delta <- c(rep(c(rep(0, (i-1)), 1, rep(0, (8-i))), K.param/8), rep(0, K.param))
  delta.tax <- c(rep(0, K.param), rep(c(rep(0, (i-1)), 1, rep(0, (8-i))), K.param/8))

  gradient <- delta*cohort.weights$weights
  norm.gradient <- gradient/sum(gradient)

  gradient.tax <- delta.tax*cohort.weights$weights
  norm.grad.tax <- gradient.tax/sum(gradient.tax)

  pooled.var[i] <- t(as.vector(norm.gradient))%*%as.matrix(cov.matrix)%*%as.vector(norm.gradient)
  pooled.var[8+i] <- t(as.vector(norm.grad.tax))%*%as.matrix(cov.matrix)%*%as.vector(norm.grad.tax)
}

estimates <- cp.all.res[,.(estimates = weighted.mean(estimate, w = total_sales)), by = .(rn, outcome)]
estimates$std.errors <- sqrt(pooled.var)


##Standard errors for pooled post-reform estimates (pooled across cohorts and across catt0-catt4)
##ln_quantity
weights <- setDT(cp.all.res[outcome == 'ln_quantity',])
delta <- c(rep(c(0,0,0,1,1,1,1,1), K.param/8), rep(0, K.param)) #Selects all post-period estimates

gradient <- delta*weights$total_sales
norm.gradient <- gradient/sum(gradient)

var <- t(as.vector(norm.gradient))%*%as.matrix(cov.matrix)%*%as.vector(norm.gradient)
est <- mean(estimates[outcome == 'ln_quantity' & (rn == 'catt0' | rn == 'catt1' | rn == 'catt2' | rn == 'catt3' | rn == 'catt4'), estimates])

## R was weird and would convert the estimates value to factors - so I get around this by creating the estimates and std.error columns separately
#This is really stupid, we should change this
temp <- estimates[,c("rn", "outcome")]
temp2 <- as.data.frame(t(as.vector(c("post-treatment", "ln_quantity"))))
colnames(temp2) <- c("rn", "outcome")
temp <- rbind(temp, temp2)

temp3 <- estimates[,c("estimates", "std.errors")]
temp2 <- t(as.vector(c(est, sqrt(var))))
temp2 <- as.data.frame(temp2)
colnames(temp2) <- c("estimates", "std.errors")
estimates <- rbind(temp3, temp2)
estimates$rn <- temp$rn
estimates$outcome <- temp$outcome


##Tax
weights <- setDT(cp.all.res[outcome == 'ln_quantity',]) #Could replace with sales_tax but does not matter weights are the same
delta <- c(rep(0, K.param), rep(c(0,0,0,1,1,1,1,1), K.param/8)) #Selects all post-period estimates

gradient <- delta*weights$total_sales
norm.gradient <- gradient/sum(gradient)

var <- t(as.vector(norm.gradient))%*%as.matrix(cov.matrix)%*%as.vector(norm.gradient)
setDT(estimates)
est <- mean(estimates[outcome == 'sales_tax' & (rn == 'catt0' | rn == 'catt1' | rn == 'catt2' | rn == 'catt3' | rn == 'catt4'),]$estimates)

## R was weird and would convert the estimates value to factors - so I get around this by creating the estimates and std.error columns separately
##THis is really stupid, we should change this
temp <- estimates[,c("rn", "outcome")]
temp2 <- as.data.frame(t(as.vector(c("post-treatment", "sales_tax"))))
colnames(temp2) <- c("rn", "outcome")
temp <- rbind(temp, temp2)

temp3 <- estimates[,c("estimates", "std.errors")]
temp2 <- t(as.vector(c(est, sqrt(var))))
temp2 <- as.data.frame(temp2)
colnames(temp2) <- c("estimates", "std.errors")
estimates <- rbind(temp3, temp2)
estimates$rn <- temp$rn
estimates$outcome <- temp$outcome


#### Finally standard errors for Pass-through
#2 ways: 1) divide (for each cohort) then average and 2) average then divide

##1) Divide (for each cohort) then average pass-through across cohorts
#First estimates
#Here it would be more elegant to use gather/spread but this is easy
pass <- cp.all.res[outcome == 'ln_quantity', -c("outcome")]
temp <- cp.all.res[outcome == 'sales_tax', -c("outcome")]

pass$ln_quantity <- pass$estimate
pass$sales_tax <- temp$estimate
pass$passthrough <- pass$ln_quantity/pass$sales_tax

#Now need to create a vector with 1/gamma then -beta/gamma^2 (because Delta Method)
deriv <- as.vector(c(1/pass$sales_tax, -pass$ln_quantity/(pass$sales_tax^2)))
pooled.pass.var <- matrix(0, nrow = 5, ncol = 1)
for(i in 1:5) {

  delta <- c(rep(c(rep(0, (3 + i-1)), 1, rep(0, (5-i))), (K.param/8)))

  gradient <- delta*cohort.weights$weights
  norm.gradient <- gradient/sum(gradient)
  norm.gradient <- as.vector(rep(norm.gradient, 2))
  norm.gradient <- norm.gradient*deriv


  pooled.pass.var[i] <- t(as.vector(norm.gradient))%*%as.matrix(cov.matrix)%*%as.vector(norm.gradient)
}

## Pooled estimate of passthrough
est <- pass[tt_event >= 0, .(estimates = weighted.mean(passthrough, w = total_sales)), by = .(rn)]
est$std.errors <- sqrt(pooled.pass.var)
est$outcome <- rep("passthrough_1", 5)

estimates <- rbind(estimates, est)


###Pooling across leads
delta <- c(rep(c(rep(0, 3), rep(1, (5))), (K.param/8)))

gradient <- delta*cohort.weights$weights
norm.gradient <- gradient/sum(gradient)
norm.gradient <- as.vector(rep(norm.gradient, 2))
norm.gradient <- norm.gradient*deriv


pooled.all.pass <- t(as.vector(norm.gradient))%*%as.matrix(cov.matrix)%*%as.vector(norm.gradient)

## Pooled all estimate of passthrough
est <- pass[tt_event >= 0, .(estimates = weighted.mean(passthrough, w = total_sales))]
est$std.errors <- sqrt(pooled.all.pass)
est$rn <- "post-treatment"
est$outcome <- "passthrough_1"

estimates <- rbind(estimates, est)



##2) Average price and tax estimate across cohorts then divide
#First get the estimates estimates
#test <- estimates[outcome == 'ln_quantity' & rn %in% c("catt0", "catt1", "catt2", "catt3", "catt4"), "estimates"]
estprice.pass <- estimates[outcome == 'ln_quantity' & rn %in% c("catt-4", "catt-3", "catt-1", "catt0", "catt1", "catt2", "catt3", "catt4"), "estimates"]
esttax.pass <- estimates[outcome == 'sales_tax' & rn %in% c("catt-4", "catt-3", "catt-1", "catt0", "catt1", "catt2", "catt3", "catt4"), "estimates"]
est.passthrough <- estprice.pass/esttax.pass

deriv1 <- rep(1/as.vector(esttax.pass$estimates), K.param/8)
deriv2 <-  - as.vector(cp.all.res[outcome == 'ln_quantity', "estimate"]$estimate)
deriv2 <- deriv2*deriv1^2

deriv <- as.vector(c(deriv1, deriv2))
pooled.pass.var2 <- matrix(0, nrow = 5, ncol = 1)

for(i in 1:5) {

  delta <- c(rep(c(rep(0, (3 + i-1)), 1, rep(0, (5-i))), (K.param/8)))

  gradient <- delta*cohort.weights$weights
  norm.gradient <- gradient/sum(gradient)
  norm.gradient <- as.vector(rep(norm.gradient, 2))
  norm.gradient <- norm.gradient*deriv


  pooled.pass.var2[i] <- t(as.vector(norm.gradient))%*%as.matrix(cov.matrix)%*%as.vector(norm.gradient)
}


## Pooled estimate of passthrough
est <- pass[tt_event >= 0, .(ln_quantity = weighted.mean(ln_quantity, w = total_sales), sales_tax = weighted.mean(sales_tax, w = total_sales)), by = .(rn)]
est$estimates <- est$ln_quantity/est$sales_tax
est <- est[,-c("ln_quantity", "sales_tax")]
est$std.errors <- sqrt(pooled.pass.var2)
est$outcome <- rep("passthrough_2", 5)


estimates <- rbind(estimates, est)


###Pooling across leads (for passthrough 2)
delta <- c(rep(c(rep(0, 3), rep(1, (5))), (K.param/8)))

gradient <- delta*cohort.weights$weights
norm.gradient <- gradient/sum(gradient)
norm.gradient <- as.vector(rep(norm.gradient, 2))
norm.gradient <- norm.gradient*deriv

pooled.all.pass2 <- t(as.vector(norm.gradient))%*%as.matrix(cov.matrix)%*%as.vector(norm.gradient)


## Pooled all estimate of passthrough
est <- pass[tt_event >= 0, .(ln_quantity = weighted.mean(ln_quantity, w = total_sales), sales_tax = weighted.mean(sales_tax, w = total_sales))]
est$estimates <- est$ln_quantity/est$sales_tax
est <- est[,-c("ln_quantity", "sales_tax")]
est$std.errors <- sqrt(pooled.all.pass2)
est$rn <- "post-treatment"
est$outcome <- "passthrough_2"

estimates <- rbind(estimates, est)


###Write to a file
fwrite(estimates, output.estimates.stderr.filepath)
