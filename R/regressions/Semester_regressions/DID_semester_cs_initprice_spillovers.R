#' Sales Taxes Project
#' Robustness Check Spillovers:
#' Check estimates of statutory sales tax at the county level for tax-exempt items. Also estimate main spec on taxable items only
#' We estimate the LR DiD estimates but also the LDM.


library(data.table)
library(futile.logger)
library(lfe)
library(multcomp)


setwd("/project2/igaarder")


## input filepaths -----------------------------------------------
#' This data is the same as all_goods_pi_path, except it has 2015-2016 data as well.
data.semester <- "Data/Nielsen/semester_nielsen_data.csv"
data.taxes <- "Data/county_monthly_tax_rates_2008_2014.csv"


## output filepaths ----------------------------------------------
results.file <- "Data/robust_spillover_estimates_initprice_semester.csv"

## Open all data and merge statutory tax rate -----
all_pi <- fread(data.semester)

taxes <- fread(data.taxes)
# Compute mean to the quarter. Take log and collapse to the semester mean.
taxes[, quarter:= floor(month/4) + 1]
taxes <- taxes[, .(sales_tax = mean(1+sales_tax)), by = .(fips_state, fips_county, year, quarter)]
taxes[, semester:= floor(quarter/3) + 1]
taxes <- taxes[, .(ln_statutory_tax = mean(log(sales_tax))), by = .(fips_state, fips_county, year, semester)]
# merge to goods data
all_pi <- merge(all_pi, taxes, by = c("fips_state", "fips_county", "year", "semester"), all.x = T)


### Set up Semester Data ---------------------------------
all_pi[, w.ln_sales_tax := ln_sales_tax - mean(ln_sales_tax), by = .(store_by_module)]
all_pi[, w.ln_statutory_tax := ln_statutory_tax - mean(ln_statutory_tax), by = .(store_by_module)]
all_pi[, w.ln_cpricei2 := ln_cpricei2 - mean(ln_cpricei2), by = .(store_by_module)]
all_pi[, w.ln_quantity3 := ln_quantity3 - mean(ln_quantity3), by = .(store_by_module)]

# Need to demean
all_pi[, module_by_time := .GRP, by = .(product_module_code, semester, year)]
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


## Divide samples: always tax-exempt, always taxable, change taxability ------
all_pi[, tax_exempt := ln_sales_tax == 0]
all_pi[, T_tax_exempt := sum(tax_exempt), by = .(store_by_module)]
all_pi[, T_taxable := sum(1-tax_exempt), by = .(store_by_module)]
all_pi[, T_total := .N, by = .(store_by_module)]

all_pi[, all_taxable:= ifelse(T_taxable/T_total == 1,1,0)]
all_pi[, all_taxexempt:= ifelse(T_tax_exempt/T_total == 1,1,0)]
all_pi[, change_taxab:= ifelse(T_tax_exempt/T_total != 1 & T_taxable/T_total != 1,1,0)]

## Run estimations -----------------

FE_opts <- c("region_by_module_by_time", "division_by_module_by_time")
outcomes <- c("w.ln_cpricei2", "w.ln_quantity3")
samples <- c("all_taxable", "all_taxexempt", "change_taxab")


LRdiff_res <- data.table(NULL)
## Loop over the sample we look at
## Estimate RF and FS
for (sam in samples) {
  sample <- all_pi[sam == 1]
  for (FE in FE_opts) {
    for (Y in outcomes) {
      formula1 <- as.formula(paste0(
        Y, " ~ w.ln_statutory_tax | ", FE, "| 0 | module_by_state"
      ))
      res1 <- felm(formula = formula1, data = sample,
                   weights = sample$base.sales)
      
      
      ## attach results
      res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
      res1.dt[, outcome := Y]
      res1.dt[, controls := FE]
      res1.dt[, sample := sam]
      
      ## Descriptives
      res1.dt$N_obs <- nrow(sample)
      res1.dt$N_stores <- uniqueN(sample, by = c("store_code_uc") )
      res1.dt$N_modules <- length(unique(sample$product_module_code))
      res1.dt$N_counties <- uniqueN(sample, by = c("fips_state", "fips_county"))
      res1.dt$N_years <- uniqueN(sample, by = c("year"))
      res1.dt$N_county_modules <- uniqueN(sample, by = c("fips_state", "fips_county",
                                                         "product_module_code"))
      LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
      fwrite(LRdiff_res, results.file)
    }
  }
}


  


