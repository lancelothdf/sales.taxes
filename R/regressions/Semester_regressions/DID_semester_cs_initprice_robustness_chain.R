#' Sales Taxes Project
#' Robustness Check: How does the pass-through estimate changes when we exclude to stores that are not 
#' part of a large national chain to address people's objections related to the findings of Dellavigna 
#' and Gentzkow that large national chains do not adjust prices to local economic conditions
#' To do this we take 2 approaches:
#' Approach 1: Restrict to Groceries. This because is DellaVigna and Gentzkow find that drug and 
#' merchandise stores in the data mostly implement national pricing within retail chains.
#' Approach 2: Exclude Chains as identified by DellaVigna and Gentzkow.
#' We run the long run twoway FE version in the "common support" sample in the semesterly data.
#' 

library(data.table)
library(futile.logger)
library(lfe)
library(multcomp)


setwd("/project2/igaarder")


## input filepaths -----------------------------------------------
#' This data is the same as all_goods_pi_path, except it has 2015-2016 data as well.
data.semester <- "Data/Nielsen/semester_nielsen_data.csv"
data.stores <- "Data/Nielsen/stores_all.csv"


## output filepaths ----------------------------------------------
results.file <- "Data/robust_chains_estimates_initial_price_semester.csv"

### Set up Semester Data ---------------------------------
all_pi <- fread(data.semester)
all_pi[, w.ln_sales_tax := ln_sales_tax - mean(ln_sales_tax), by = .(store_by_module)]
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

## Merge Stores characteristics
stores <- fread(data.stores)
all_pi <- merge(all_pi, stores, by = c("year", "store_code_uc"), all.x = T)

## Run estimations -----------------

FE_opts <- c("region_by_module_by_time", "division_by_module_by_time")
outcomes <- c("w.ln_cpricei2", "w.ln_quantity3")

LRdiff_res <- data.table(NULL)

## Full sample
for (FE in FE_opts) {
  for (Y in outcomes) {
    formula1 <- as.formula(paste0(
      Y, " ~ w.ln_sales_tax | ", FE , " | 0 | module_by_state"
    ))
    res1 <- felm(formula = formula1, data = all_pi,
                 weights = all_pi$base.sales)
    
    
    ## attach results
    res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
    res1.dt[, outcome := Y]
    res1.dt[, controls := FE]
    res1.dt[, sample := "Full"]
    
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, results.file)
  }
}
    
## Only Groceries
groceries <- all_pi[channel_code == "F"]
for (FE in FE_opts) {
  for (Y in outcomes) {
    formula1 <- as.formula(paste0(
      Y, " ~ w.ln_sales_tax | ", FE , " | 0 | module_by_state"
    ))
    res1 <- felm(formula = formula1, data = groceries,
                 weights = groceries$base.sales)
    
    
    ## attach results
    res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
    res1.dt[, outcome := Y]
    res1.dt[, controls := FE]
    res1.dt[, sample := "Food"]
    
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, results.file)
  }
}   

## Only not chains
DGsample <- all_pi[is.na(DGsample)]
for (FE in FE_opts) {
  for (Y in outcomes) {
    formula1 <- as.formula(paste0(
      Y, " ~ w.ln_sales_tax | ", FE , " | 0 | module_by_state"
    ))
    res1 <- felm(formula = formula1, data = DGsample,
                 weights = DGsample$base.sales)
    
    
    ## attach results
    res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
    res1.dt[, outcome := Y]
    res1.dt[, controls := FE]
    res1.dt[, sample := "DG"]
    
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, results.file)
  }
}   