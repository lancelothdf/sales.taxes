#' Author: Lancelot Henry de Frahan
#'
# Run similar regressions as "main_semesterly_regressions_commonsupport_c1.R", but with county X module data 
# + Importantly run the regression cohort-by-cohort then average the effect across cohorts
# Here we define a cohort based on time of ``treatment"

library(data.table)
library(futile.logger)
library(lfe)
library(multcomp)


########## !!!!!!! ###########
setwd("/project2/igaarder")
#setwd("/Users/lancelot/Documents/Sales Taxes/")


## input filepaths -----------------------------------------------
#' This data set contains quarterly Laspeyres indices and sales from 2006 to
#' 2016. It also contains sales tax rates from the same time period.
all_goods_pi_path <- "Data/Nielsen/price_quantity_indices_allitems_2006-2016_notaxinfo.csv"
#' This data set contains an old price index that Lance constructed, from
old_pi_path <- "Data/Nielsen/Quarterly_old_pi.csv"
#' This data is the same as all_goods_pi_path, except it has 2015-2016 data as well.

######## !!!!!! ############
data.full.path <- "Data/Nielsen/semester_nielsen_data.csv"
#data.full.path <- "LRdiff_semesterly_COUNTY_random.csv"


## output filepaths ----------------------------------------------
output.results.file <- "Data/Cohort_TWFE.csv" ### Main results
boot.results.file <- "Data/Boot_cohort_TWFE.csv" ### Bootstrap


##### 
all_pi <- fread(data.full.path)
all_pi <- all_pi[, c("fips_state", "fips_county", "store_code_uc", "product_module_code", "year", "semester", "ln_cpricei2", "ln_quantity3", "ln_sales_tax", "base.sales", "sales", "store_by_module", "cal_time", "module_by_time", "module_by_state", "region_by_module_by_time", "division_by_module_by_time", "D.ln_sales_tax", "D.ln_cpricei2")]


## Create lagged value (initial)
all_pi[, L.ln_sales_tax := ln_sales_tax - D.ln_sales_tax]

## need to demean lag price to compare appropiately
all_pi[, module_by_yr := .GRP, by = .(product_module_code, year)]
all_pi[, L.ln_cpricei2 := ln_cpricei2 - D.ln_cpricei2]
all_pi[, dm.L.ln_cpricei2 := L.ln_cpricei2 - mean(L.ln_cpricei2, na.rm = T), by = module_by_yr]
all_pi[, dm.ln_cpricei2 := ln_cpricei2 - mean(ln_cpricei2, na.rm = T), by = module_by_yr]
all_pi[, dm.ln_quantity3 := ln_quantity3 - mean(ln_quantity3, na.rm = T), by = module_by_yr]

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



### De-mean variable used in regressions
all_pi[, ln_cpricei2 := ln_cpricei2 - mean(ln_cpricei2, na.rm = T), by = store_by_module]
all_pi[, ln_quantity3 := ln_quantity3 - mean(ln_quantity3, na.rm = T), by = store_by_module]
all_pi[, ln_sales_tax := ln_sales_tax - mean(ln_sales_tax, na.rm = T), by = store_by_module]

outcomes <- c("ln_cpricei2", "ln_quantity3")


### Keep only data between 2008-2014 (Note: previous and subsequent data is used to calculate lead and lagged variables)
all_pi <- all_pi[year >= 2008 & year <= 2014,]

c_ids <- unique(sort(all_pi$region_by_module_by_time)) ## Define cohorts based on YearXsemesterXmoduleXCensus Region


flog.info("Iteration 0")
LRdiff_res <- data.table(NULL)

for(co in c_ids) {
  for (Y in c(outcomes)) {
        
          
          formula1 <- as.formula(paste0(
            Y, "~ ln_sales_tax"))
          flog.info("Estimating with %s as outcome with no FEs.", Y)
          res1 <- lm(formula = formula1, data = all_pi[region_by_module_by_time == co,],
                       weights = all_pi[region_by_module_by_time == co,]$base.sales)
          flog.info("Finished estimating with %s as outcome with no FEs.", Y)
          
          
          ## attach results
          flog.info("Writing results...")
        
          
            temp.est <- - coef(summary(res1))[ "ln_sales_tax", "Estimate"]
            cumul.lead2.se <- coef(summary(res1))[ "ln_sales_tax", "Std. Error"]
            cumul.lead2.pval <- coef(summary(res1))[ "ln_sales_tax", "Pr(>|t|)"]
          
          
          ## Store results
          res1.dt <- data.table(
            Estimate = coef(summary(res1))[ "ln_sales_tax", "Estimate"],
            `Std. Error` = coef(summary(res1))[ "ln_sales_tax", "Std. Error"],
            `Pr(>|t|)` = coef(summary(res1))[ "ln_sales_tax", "Pr(>|t|)"],
            outcome = Y,
            cohort = co)
          
        res1.dt[, base.sales := sum(all_pi[region_by_module_by_time == co,]$base.sales)]
        res1.dt[, sales := sum(all_pi[region_by_module_by_time == co,]$sales)]
        
        LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
        fwrite(LRdiff_res, output.results.file)
        
    }
}



#### Now bootstrap the results
### Start manual bootstrap
set.seed(1941)
ids <- unique(all_pi$module_by_state)
LRdiff_boot <- data.table(NULL)


for (rep in 1:200) {
  
  flog.info("Iteration %s", rep)
  
  # Sample by block
  sampled.ids <- data.table(sample(ids, replace = T))
  setnames(sampled.ids, old= "V1", new = "module_by_state")
  
  # Merge data to actual data
  sampled.data <- merge(sampled.ids, all_pi, by = c("module_by_state") , allow.cartesian = T, all.x = T)

  
  LRdiff_res <- data.table(NULL)
  for(co in c_ids) {
    for (Y in c(outcomes)) {
      
      formula1 <- as.formula(paste0(
        Y, "~ ln_sales_tax"))

      
      res1 <- lm(formula = formula1, data = all_pi[region_by_module_by_time == co,],
                 weights = all_pi[region_by_module_by_time == co,]$base.sales)
      
      
      ## Store results
      res1.dt <- data.table(
        Estimate = coef(summary(res1))[ "ln_sales_tax", "Estimate"],
        `Std. Error` = coef(summary(res1))[ "ln_sales_tax", "Std. Error"],
        `Pr(>|t|)` = coef(summary(res1))[ "ln_sales_tax", "Pr(>|t|)"],
        outcome = Y,
        cohort = co,
        iteration = rep)
      
      res1.dt[, base.sales := sum(all_pi[region_by_module_by_time == co,]$base.sales)]
      res1.dt[, sales := sum(all_pi[region_by_module_by_time == co,]$sales)]
      
      LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
      #fwrite(LRdiff_res, output.results.file)
      
    }
  }
  

  ## Append each iteration
  LRdiff_boot <- rbind(LRdiff_boot, LRdiff_res, fill = T)
  fwrite(LRdiff_boot, boot.results.file)
  
}

