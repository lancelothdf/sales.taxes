#' Author: Lancelot Henry de Frahan
#'
# Run similar regressions as "main_semesterly_regressions_commonsupport_c1.R", but with county X module data 
# + Importantly run the regression cohort-by-cohort then average the effect across cohorts
# Here we define a cohort based on time of ``treatment"

library(data.table)
library(futile.logger)
library(lfe)
library(multcomp)
#library(estimatr)


########## !!!!!!! ###########
#setwd("/project2/igaarder")
setwd("/Users/lancelot/Documents/Sales Taxes/")


## input filepaths -----------------------------------------------
#' This data set contains quarterly Laspeyres indices and sales from 2006 to
#' 2016. It also contains sales tax rates from the same time period.
all_goods_pi_path <- "Data/Nielsen/price_quantity_indices_allitems_2006-2016_notaxinfo.csv"
#' This data set contains an old price index that Lance constructed, from
old_pi_path <- "Data/Nielsen/Quarterly_old_pi.csv"
#' This data is the same as all_goods_pi_path, except it has 2015-2016 data as well.

######## !!!!!! ############
#data.full.path <- "Data/Nielsen/semester_nielsen_data_county.csv"
data.full.path <- "LRdiff_semesterly_COUNTY_random.csv"


zillow_path <- "Data/covariates/zillow_long_by_county_clean.csv"
zillow_state_path <- "Data/covariates/zillow_long_by_state_clean.csv"
unemp.path <- "Data/covariates/county_monthly_unemp_clean.csv"
wage.path <- "Data/covariates/qcew_quarterly_clean.csv"



## output filepaths ----------------------------------------------
output.results.file <- "Data/LRdiff_cohortbycohort_time.csv"


##### 
all_pi <- fread(data.full.path)


## No Econ controls for now



# Create lagged value (initial)
all_pi[, L.ln_sales_tax := ln_sales_tax - D.ln_sales_tax]

# need to demean lag price to compare appropiately
all_pi[, module_by_time := .GRP, by = .(product_module_code, year)]
all_pi[, L.ln_cpricei2 := ln_cpricei2 - D.ln_cpricei2]
all_pi[, dm.L.ln_cpricei2 := L.ln_cpricei2 - mean(L.ln_cpricei2, na.rm = T), by = module_by_time]
all_pi[, dm.ln_cpricei2 := ln_cpricei2 - mean(ln_cpricei2, na.rm = T), by = module_by_time]
all_pi[, dm.ln_quantity3 := ln_quantity3 - mean(ln_quantity3, na.rm = T), by = module_by_time]

# Defining common support
control <- all_pi[D.ln_sales_tax == 0,]
treated <- all_pi[D.ln_sales_tax != 0,]

# Price 
pct1.control <- quantile(control$dm.L.ln_cpricei2, probs = 0.01, na.rm = T, weight=control$base.sales)
pct1.treated <- quantile(treated$dm.L.ln_cpricei2, probs = 0.01, na.rm = T, weight=treated$base.sales)

pct99.control <- quantile(control$dm.L.ln_cpricei2, probs = 0.99, na.rm = T, weight=control$base.sales)
pct99treated <- quantile(treated$dm.L.ln_cpricei2, probs = 0.99, na.rm = T, weight=treated$base.sales)

all_pi[, cs_price := ifelse(dm.L.ln_cpricei2 > max(pct1.treated, pct1.control) & 
                              dm.L.ln_cpricei2 < min(pct99treated, pct99.control), 1, 0)]
# Make sure missings are 0s
all_pi[, cs_price := ifelse(is.na(dm.L.ln_cpricei2), 0, cs_price)]

## Keep within the common support
all_pi <- all_pi[cs_price == 1,]

## cut the tails (keep between 1st and 99th percentile)
pct1 <- quantile(all_pi$dm.ln_cpricei2, probs = 0.01, na.rm = T, weight=base.sales)
pct99 <- quantile(all_pi$dm.ln_cpricei2, probs = 0.99, na.rm = T, weight=base.sales)
all_pi <- all_pi[(dm.ln_cpricei2 > pct1 & dm.ln_cpricei2 < pct99),]


## Setting up loop to estimate
formula_lags <- paste0("L", 1:4, ".D.ln_sales_tax", collapse = "+")
formula_leads <- paste0("F", 1:4, ".D.ln_sales_tax", collapse = "+")
formula_RHS <- paste0("D.ln_sales_tax + ", formula_lags, "+", formula_leads)


outcomes <- c("D.ln_cpricei", "D.ln_cpricei2", "D.ln_quantity", "D.ln_quantity2", "D.ln_quantity3")
FE_opts <- c("module_by_time", "region_by_module_by_time", "division_by_module_by_time")


### Keep only data between 2008-2014 (Note: previous and subsequent data is used to calculate lead and lagged variables)
all_pi <- all_pi[year >= 2008 & year <= 2014,]
all_pi <- all_pi[year > 2008 | (year == 2008 & semester == 2),] ## Get rid of 2008S1 cohort
#ids <- unique(sort(all_pi$module_by_time))
ids <- unique(sort(all_pi$cal_time)) ## Define cohorts based on YearXsemester (calendar time)


LRdiff_res <- data.table(NULL)

for(co in ids) {
  for (Y in c(outcomes)) {
      for (FE in FE_opts) {
        
        formula1 <- as.formula(paste0(
          Y, "~", formula_RHS, "| ", FE, " | 0 | 0"
        ))
        flog.info("Estimating with %s as outcome with %s FE.", Y, FE)
        res1 <- felm(formula = formula1, data = all_pi[cal_time == co,],
                     weights = all_pi[cal_time == co,]$base.sales)
        flog.info("Finished estimating with %s as outcome with %s FE.", Y, FE)
        
        
        
        ## attach results
        flog.info("Writing results...")

        
        ##### Add the cumulative effect at each lead/lag (relative to -1)
        ##LEADS
        cumul.lead1.est <- 0
        cumul.lead2.est <- - coef(summary(res1))[ "F1.D.ln_sales_tax", "Estimate"]
        cumul.lead3.est <- cumul.lead2.est - coef(summary(res1))[ "F2.D.ln_sales_tax", "Estimate"]
        cumul.lead4.est <- cumul.lead3.est - coef(summary(res1))[ "F3.D.ln_sales_tax", "Estimate"]
        cumul.lead5.est <- cumul.lead4.est - coef(summary(res1))[ "F4.D.ln_sales_tax", "Estimate"]
        
        ##LAGS
        cumul.lag0.est <- coef(summary(res1))[ "D.ln_sales_tax", "Estimate"]
        cumul.lag1.est <- cumul.lag0.est + coef(summary(res1))[ "L1.D.ln_sales_tax", "Estimate"]
        cumul.lag2.est <- cumul.lag1.est + coef(summary(res1))[ "L2.D.ln_sales_tax", "Estimate"]
        cumul.lag3.est <- cumul.lag2.est + coef(summary(res1))[ "L3.D.ln_sales_tax", "Estimate"]
        cumul.lag4.est <- cumul.lag3.est + coef(summary(res1))[ "L4.D.ln_sales_tax", "Estimate"]
        
        
        ## Store results
        res1.dt <- data.table(
          rn = c("cumul.lead5.D.ln_sales_tax", "cumul.lead4.D.ln_sales_tax", "cumul.lead3.D.ln_sales_tax", "cumul.lead2.D.ln_sales_tax", "cumul.lead1.D.ln_sales_tax", "cumul.lag0.D.ln_sales_tax", "cumul.lag1.D.ln_sales_tax", "cumul.lag2.D.ln_sales_tax", "cumul.lag3.D.ln_sales_tax", "cumul.lag4.D.ln_sales_tax"),
          Estimate = c(cumul.lead5.est, cumul.lead4.est, cumul.lead3.est, cumul.lead2.est, cumul.lead1.est, cumul.lag0.est, cumul.lag1.est, cumul.lag2.est, cumul.lag3.est, cumul.lag4.est),
          outcome = Y,
          controls = FE,
          cohort = co)
        
        # Add summary values
        res1.dt[, N_obs := nrow(all_pi)]
        res1.dt[, N_modules := length(unique(all_pi$product_module_code))]
        res1.dt[, N_stores :=  length(unique(all_pi$store_code_uc))]
        res1.dt[, N_counties := uniqueN(all_pi, by = c("fips_state", "fips_county"))]
        res1.dt[, N_years := uniqueN(all_pi, by = c("year"))]
        res1.dt[, N_county_modules := uniqueN(all_pi, by = c("fips_state", "fips_county",
                                                             "product_module_code"))]
        res1.dt[, base.sales := sum(all_pi[cal_time == co,]$base.sales)]
        
        
        LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
        fwrite(LRdiff_res, output.results.file)
        
      }
  }
}


#### Now bootstrap the results
