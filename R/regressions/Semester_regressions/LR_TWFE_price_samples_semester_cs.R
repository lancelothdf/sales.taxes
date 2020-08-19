#' Sales Taxes Project
#' Run standard Two-way FE model on price over different subsamples
#' Check estimates of long run design
#' 


library(data.table)
library(futile.logger)
library(lfe)
library(multcomp)

setwd("/project2/igaarder")


## input filepaths -----------------------------------------------
#' This data is the same as all_goods_pi_path, except it has 2015-2016 data as well.
data.semester <- "Data/Nielsen/semester_nielsen_data.csv"

## output filepaths ----------------------------------------------
output.results.file <- "Data/LR_TWFE_pasthrough_samples.csv"

## Open data  -----
all_pi <- fread(data.semester)


# Create demeaned variables
all_pi[, w.ln_sales_tax := ln_sales_tax - mean(ln_sales_tax), by = .(store_by_module)]
all_pi[, w.ln_cpricei2 := ln_cpricei2 - mean(ln_cpricei2), by = .(store_by_module)]
all_pi[, w.ln_quantity3 := ln_quantity3 - mean(ln_quantity3), by = .(store_by_module)]
all_pi[, w.ln_pricei2 := ln_pricei2 - mean(ln_pricei2), by = .(store_by_module)]
all_pi[, w.ln_sales := ln_sales - mean(ln_sales), by = .(store_by_module)]


# Need to demean
all_pi[, module_by_time := .GRP, by = .(product_module_code, semester, year)]
all_pi[, L.ln_cpricei2 := ln_cpricei2 - D.ln_cpricei2]
all_pi[, dm.L.ln_cpricei2 := L.ln_cpricei2 - mean(L.ln_cpricei2, na.rm = T), by = module_by_time]
all_pi[, dm.ln_cpricei2 := ln_cpricei2 - mean(ln_cpricei2, na.rm = T), by = module_by_time]
all_pi[, dm.ln_pricei2 := ln_pricei2 - mean(ln_pricei2, na.rm = T), by = module_by_time]
all_pi[, dm.L.ln_pricei2 := L.ln_pricei2 - mean(L.ln_pricei2, na.rm = T), by = module_by_time]
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

## Divide the sample: above/below lagged (pre-tax) price
all_pi[, median(dm.L.ln_pricei2, weight=base.sales)]

all_pi[, pt_g := as.integer(dm.L.ln_pricei2 >= median(dm.L.ln_pricei2, weight=base.sales))]
## Divide the sample: above/below (pre-tax) price
all_pi[, ln_statutory_tax := max(ln_sales_tax, na.rm = T), by = .(fips_state, fips_county, year, semester)]
all_pi[, median(ln_statutory_tax, weight=base.sales)]

all_pi[, pt_t := as.integer(ln_statutory_tax >= median(ln_statutory_tax, na.rm = T, weight=base.sales))]



#### Prepare Estimations

#outcomes <- c("w.ln_cpricei2", "w.ln_quantity3", "w.ln_pricei2", "w.ln_sales")
FE_opts <- c("region_by_module_by_time", "division_by_module_by_time")

LRdiff_res <- data.table(NULL)
## Run in levels
for (sm1 in 0:1) {
  for (sm2 in 0:1) {
    est.data <- all_pi[pt_g == sm1 & pt_t == sm2]
    print(est.data[, .N])
    for (FE in FE_opts) {
      
      formula1 <- as.formula(paste0(
        "w.ln_cpricei2 ~ w.ln_sales_tax | ", FE, " | 0 | module_by_state "
      ))
      res1 <- felm(formula = formula1, data = est.data,
                   weights = est.data$base.sales)

      
      ## attach results
      flog.info("Writing results...")
      res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
      res1.dt[, controls := FE]
      res1.dt[, sub.p := sm1]
      res1.dt[, sub.t := sm2]
      # Add summary values
      res1.dt[, Rsq := summary(res1)$r.squared]
      res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
      res1.dt[, N_obs := nrow(est.data)]
      res1.dt[, N_modules := length(unique(est.data$product_module_code))]
      res1.dt[, N_stores :=  length(unique(est.data$store_code_uc))]
      res1.dt[, N_counties := uniqueN(est.data, by = c("fips_state", "fips_county"))]
      res1.dt[, N_years := uniqueN(est.data, by = c("year"))]
      res1.dt[, N_county_modules := uniqueN(est.data, by = c("fips_state", "fips_county",
                                                           "product_module_code"))]
      LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
      fwrite(LRdiff_res, output.results.file)
    } 
  }
}
