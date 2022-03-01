#' Author: Lancelot Henry de Frahan
#'
# Run similar regressions as "main_semesterly_regressions_commonsupport_c1.R", but with county X module data 
# + Importantly run the regression cohort-by-cohort then average the effect across cohorts
# We explore various definitions of cohorts in this file nad study robustness of results to these different definitions

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
output.results.file <- "Data/Cohort_TWFE_multiple.csv" ### Main results


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



### Keep only data between 2008-2014 (Note: previous and subsequent data is used to calculate lead and lagged variables)
all_pi <- all_pi[year >= 2008 & year <= 2014,]

## Collapse at county-level to save some memory
all_pi <- all_pi[, list(ln_cpricei2 = weighted.mean(ln_cpricei2, w = base.sales), ln_quantity3 = weighted.mean(ln_quantity3, w = base.sales), ln_sales_tax = weighted.mean(ln_sales_tax, w = base.sales), base.sales = sum(base.sales), sales = sum(sales)), by = .(fips_state, fips_county, product_module_code, year, semester)]


## Re-create the FEs
## Prep Census region/division data ------------------------------
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


####
## Merge on the census region/division info
all_pi <- merge(all_pi, geo_dt, by = "fips_state")

all_pi[, cal_time := 2 * year + semester]
all_pi[, module_by_state := .GRP, by = .(product_module_code, fips_state)]
all_pi[, module_by_time := .GRP, by = .(product_module_code, fips_state)]
all_pi[, region_by_module_by_time := .GRP, by = .(region, product_module_code, cal_time)]
all_pi[, division_by_module_by_time := .GRP, by = .(division, product_module_code, cal_time)]
all_pi[, region_by_time := .GRP, by = .(region, cal_time)]
all_pi[, division_by_time := .GRP, by = .(division, cal_time)]
all_pi[, region_by_module := .GRP, by = .(region, product_module_code)]
all_pi[, division_by_module := .GRP, by = .(division, product_module_code)]


### First run regression in the full sample
outcomes <- c("ln_cpricei2", "ln_quantity3")
FE_opts <- c("region_by_module_by_time", "division_by_module_by_time")


LRdiff_res <- data.table(NULL)

for(Y in outcomes) {
  for (FE in FE_opts) {
    
    formula1 <- as.formula(paste0(
      Y, "~ ln_sales_tax | ", FE, " | 0 | module_by_state"
    ))

    res1 <- felm(formula = formula1, data = all_pi,
             weights = all_pi$base.sales)
    flog.info("Finished estimating full sample regression with %s as outcome with %s FE.", Y, FE)

    
    res1.dt <- data.table(
      Estimate = coef(summary(res1))[ "ln_sales_tax", "Estimate"],
      `Std. Error` = coef(summary(res1))[ "ln_sales_tax", "Std. Error"],
      `Pr(>|t|)` = coef(summary(res1))[ "ln_sales_tax", "Pr(>|t|)"],
      outcome = Y,
      specification = "No cohorts",
      controls = FE,
      cohort = NA)
    
    res1.dt[, base.sales := sum(all_pi$base.sales)]
    res1.dt[, sales := sum(all_pi$sales)]
    
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)
  }
}


### Cohorts = Regions
flog.info("Estimating the specification where regions are cohorts")

c_ids <- unique(sort(all_pi$region))
FE_opts = c("module_by_time", "division_by_module_by_time")

for(Y in outcomes) {
  for (FE in FE_opts) {
    for(co in c_ids) {
      
      formula1 <- as.formula(paste0(
        Y, "~ ln_sales_tax | ", FE, " | 0 | module_by_state"
      ))
      
      res1 <- felm(formula = formula1, data = all_pi[region == co,],
                   weights = all_pi[region == co,]$base.sales)
      #flog.info("Finished estimating full sample regression with %s as outcome with %s FE.", Y, FE)
      
      
      res1.dt <- data.table(
        Estimate = coef(summary(res1))[ "ln_sales_tax", "Estimate"],
        `Std. Error` = coef(summary(res1))[ "ln_sales_tax", "Std. Error"],
        `Pr(>|t|)` = coef(summary(res1))[ "ln_sales_tax", "Pr(>|t|)"],
        outcome = Y,
        specification = "region cohorts",
        controls = FE,
        cohort = co)
      
      res1.dt[, base.sales := sum(all_pi[region == co,]$base.sales)]
      res1.dt[, sales := sum(all_pi[region == co,]$sales)]
      
      LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
      fwrite(LRdiff_res, output.results.file)
      
    }
  }
}


### Cohorts = Divisions
flog.info("Estimating the specification where divisions are cohorts")

c_ids <- unique(sort(all_pi$division))
FE_opts = c("module_by_time")

for(Y in outcomes) {
  for (FE in FE_opts) {
    for(co in c_ids) {
      
      formula1 <- as.formula(paste0(
        Y, "~ ln_sales_tax | ", FE, " | 0 | module_by_state"
      ))
      
      res1 <- felm(formula = formula1, data = all_pi[division == co,],
                   weights = all_pi[division == co,]$base.sales)
      #flog.info("Finished estimating full sample regression with %s as outcome with %s FE.", Y, FE)
      
      
      res1.dt <- data.table(
        Estimate = coef(summary(res1))[ "ln_sales_tax", "Estimate"],
        `Std. Error` = coef(summary(res1))[ "ln_sales_tax", "Std. Error"],
        `Pr(>|t|)` = coef(summary(res1))[ "ln_sales_tax", "Pr(>|t|)"],
        outcome = Y,
        specification = "division cohorts",
        controls = FE,
        cohort = co)
      
      res1.dt[, base.sales := sum(all_pi[division == co,]$base.sales)]
      res1.dt[, sales := sum(all_pi[division == co,]$sales)]
      
      LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
      fwrite(LRdiff_res, output.results.file)
      
    }
  }
}


### Cohorts = time/semesters
flog.info("Estimating the specification where semesters are cohorts")

c_ids <- unique(sort(all_pi$cal_time))
FE_opts = c("region_by_module_by_time", "division_by_module_by_time")

for(Y in outcomes) {
  for (FE in FE_opts) {
    for(co in c_ids) {
      
      formula1 <- as.formula(paste0(
        Y, "~ ln_sales_tax | ", FE, " | 0 | module_by_state"
      ))
      
      res1 <- felm(formula = formula1, data = all_pi[cal_time == co,],
                   weights = all_pi[cal_time == co,]$base.sales)
      #flog.info("Finished estimating full sample regression with %s as outcome with %s FE.", Y, FE)
      
      
      res1.dt <- data.table(
        Estimate = coef(summary(res1))[ "ln_sales_tax", "Estimate"],
        `Std. Error` = coef(summary(res1))[ "ln_sales_tax", "Std. Error"],
        `Pr(>|t|)` = coef(summary(res1))[ "ln_sales_tax", "Pr(>|t|)"],
        outcome = Y,
        specification = "time cohorts",
        controls = FE,
        cohort = co)
      
      res1.dt[, base.sales := sum(all_pi[cal_time == co,]$base.sales)]
      res1.dt[, sales := sum(all_pi[cal_time == co,]$sales)]
      
      LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
      fwrite(LRdiff_res, output.results.file)
      
    }
  }
}


### Cohorts = modules
flog.info("Estimating the specification where modules are cohorts")

c_ids <- unique(sort(all_pi$product_module_code))
FE_opts = c("region_by_module_by_time", "division_by_module_by_time")

for(Y in outcomes) {
  for (FE in FE_opts) {
    for(co in c_ids) {
      
      formula1 <- as.formula(paste0(
        Y, "~ ln_sales_tax | ", FE, " | 0 | module_by_state"
      ))
      
      res1 <- felm(formula = formula1, data = all_pi[product_module_code == co,],
                   weights = all_pi[product_module_code == co,]$base.sales)
      #flog.info("Finished estimating full sample regression with %s as outcome with %s FE.", Y, FE)
      
      
      res1.dt <- data.table(
        Estimate = coef(summary(res1))[ "ln_sales_tax", "Estimate"],
        `Std. Error` = coef(summary(res1))[ "ln_sales_tax", "Std. Error"],
        `Pr(>|t|)` = coef(summary(res1))[ "ln_sales_tax", "Pr(>|t|)"],
        outcome = Y,
        specification = "module cohorts",
        controls = FE,
        cohort = co)
      
      res1.dt[, base.sales := sum(all_pi[product_module_code == co,]$base.sales)]
      res1.dt[, sales := sum(all_pi[product_module_code == co,]$sales)]
      
      LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
      fwrite(LRdiff_res, output.results.file)
      
    }
  }
}


### Cohorts = region by time
flog.info("Estimating the specification where region by time are cohorts")

c_ids <- unique(sort(all_pi$region_by_time))
FE_opts = c("region_by_module_by_time", "division_by_module_by_time")

for(Y in outcomes) {
  for (FE in FE_opts) {
    for(co in c_ids) {
      
      formula1 <- as.formula(paste0(
        Y, "~ ln_sales_tax | ", FE, " | 0 | module_by_state"
      ))
      
      res1 <- felm(formula = formula1, data = all_pi[region_by_time == co,],
                   weights = all_pi[region_by_time == co,]$base.sales)
      #flog.info("Finished estimating full sample regression with %s as outcome with %s FE.", Y, FE)
      
      
      res1.dt <- data.table(
        Estimate = coef(summary(res1))[ "ln_sales_tax", "Estimate"],
        `Std. Error` = coef(summary(res1))[ "ln_sales_tax", "Std. Error"],
        `Pr(>|t|)` = coef(summary(res1))[ "ln_sales_tax", "Pr(>|t|)"],
        outcome = Y,
        specification = "region by time cohorts",
        controls = FE,
        cohort = co)
      
      res1.dt[, base.sales := sum(all_pi[region_by_time == co,]$base.sales)]
      res1.dt[, sales := sum(all_pi[region_by_time == co,]$sales)]
      
      LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
      fwrite(LRdiff_res, output.results.file)
      
    }
  }
}


### Cohorts = division by time
flog.info("Estimating the specification where division by time are cohorts")

c_ids <- unique(sort(all_pi$division_by_time))
FE_opts = c("division_by_module_by_time")

for(Y in outcomes) {
  for (FE in FE_opts) {
    for(co in c_ids) {
      
      formula1 <- as.formula(paste0(
        Y, "~ ln_sales_tax | ", FE, " | 0 | module_by_state"
      ))
      
      res1 <- felm(formula = formula1, data = all_pi[division_by_time == co,],
                   weights = all_pi[division_by_time == co,]$base.sales)
      #flog.info("Finished estimating full sample regression with %s as outcome with %s FE.", Y, FE)
      
      
      res1.dt <- data.table(
        Estimate = coef(summary(res1))[ "ln_sales_tax", "Estimate"],
        `Std. Error` = coef(summary(res1))[ "ln_sales_tax", "Std. Error"],
        `Pr(>|t|)` = coef(summary(res1))[ "ln_sales_tax", "Pr(>|t|)"],
        outcome = Y,
        specification = "division by time cohorts",
        controls = FE,
        cohort = co)
      
      res1.dt[, base.sales := sum(all_pi[division_by_time == co,]$base.sales)]
      res1.dt[, sales := sum(all_pi[division_by_time == co,]$sales)]
      
      LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
      fwrite(LRdiff_res, output.results.file)
      
    }
  }
}



### Cohorts = region by module
flog.info("Estimating the specification where region by module are cohorts")

c_ids <- unique(sort(all_pi$region_by_module))
FE_opts = c("region_by_module_by_time", "division_by_module_by_time")

for(Y in outcomes) {
  for (FE in FE_opts) {
    for(co in c_ids) {
      
      formula1 <- as.formula(paste0(
        Y, "~ ln_sales_tax | ", FE, " | 0 | module_by_state"
      ))
      
      res1 <- felm(formula = formula1, data = all_pi[region_by_module == co,],
                   weights = all_pi[region_by_module == co,]$base.sales)
      #flog.info("Finished estimating full sample regression with %s as outcome with %s FE.", Y, FE)
      
      
      res1.dt <- data.table(
        Estimate = coef(summary(res1))[ "ln_sales_tax", "Estimate"],
        `Std. Error` = coef(summary(res1))[ "ln_sales_tax", "Std. Error"],
        `Pr(>|t|)` = coef(summary(res1))[ "ln_sales_tax", "Pr(>|t|)"],
        outcome = Y,
        specification = "region by module cohorts",
        controls = FE,
        cohort = co)
      
      res1.dt[, base.sales := sum(all_pi[region_by_module == co,]$base.sales)]
      res1.dt[, sales := sum(all_pi[region_by_module == co,]$sales)]
      
      LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
      fwrite(LRdiff_res, output.results.file)
      
    }
  }
}


### Cohorts = division by module
flog.info("Estimating the specification where division by module are cohorts")

c_ids <- unique(sort(all_pi$division_by_module))
FE_opts = c("division_by_module_by_time")

for(Y in outcomes) {
  for (FE in FE_opts) {
    for(co in c_ids) {
      
      formula1 <- as.formula(paste0(
        Y, "~ ln_sales_tax | ", FE, " | 0 | module_by_state"
      ))
      
      res1 <- felm(formula = formula1, data = all_pi[division_by_module == co,],
                   weights = all_pi[division_by_module == co,]$base.sales)
      #flog.info("Finished estimating full sample regression with %s as outcome with %s FE.", Y, FE)
      
      
      res1.dt <- data.table(
        Estimate = coef(summary(res1))[ "ln_sales_tax", "Estimate"],
        `Std. Error` = coef(summary(res1))[ "ln_sales_tax", "Std. Error"],
        `Pr(>|t|)` = coef(summary(res1))[ "ln_sales_tax", "Pr(>|t|)"],
        outcome = Y,
        specification = "division by module cohorts",
        controls = FE,
        cohort = co)
      
      res1.dt[, base.sales := sum(all_pi[division_by_module == co,]$base.sales)]
      res1.dt[, sales := sum(all_pi[division_by_module == co,]$sales)]
      
      LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
      fwrite(LRdiff_res, output.results.file)
      
    }
  }
}



### Cohorts = module by time
flog.info("Estimating the specification where module by time are cohorts")

c_ids <- unique(sort(all_pi$module_by_time))
FE_opts = c("region_by_module_by_time", "division_by_module_by_time")

for(Y in outcomes) {
  for (FE in FE_opts) {
    for(co in c_ids) {
      
      formula1 <- as.formula(paste0(
        Y, "~ ln_sales_tax | ", FE, " | 0 | module_by_state"
      ))
      
      res1 <- felm(formula = formula1, data = all_pi[module_by_time == co,],
                   weights = all_pi[module_by_time == co,]$base.sales)
      #flog.info("Finished estimating full sample regression with %s as outcome with %s FE.", Y, FE)
      
      
      res1.dt <- data.table(
        Estimate = coef(summary(res1))[ "ln_sales_tax", "Estimate"],
        `Std. Error` = coef(summary(res1))[ "ln_sales_tax", "Std. Error"],
        `Pr(>|t|)` = coef(summary(res1))[ "ln_sales_tax", "Pr(>|t|)"],
        outcome = Y,
        specification = "module by time cohorts",
        controls = FE,
        cohort = co)
      
      res1.dt[, base.sales := sum(all_pi[module_by_time == co,]$base.sales)]
      res1.dt[, sales := sum(all_pi[module_by_time == co,]$sales)]
      
      LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
      fwrite(LRdiff_res, output.results.file)
      
    }
  }
}

