#' Sales Taxes Project
#' Robustness Check: How does the pass-through estimate changes when we exclude to stores that are not 
#' part of a large national chain to address people's objections related to the findings of Dellavigna 
#' and Gentzkow that large national chains do not adjust prices to local economic conditions
#' In this code we take 2 approaches:
#' Approach 1: Run regression at a chain level.
#' Approach 2: Exclude the chains that are present in more than 4 states and run the usual regression.
#' For A2: We run the long run twoway FE version in the "common support" sample in the semesterly data.
#' We also run by subsamples of inital price
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
results.file.a1 <- "Data/robust_chainlev_estimates_initprice_semester.csv"
results.file.a2 <- "Data/robust_chainsize_estimates_initprice_semester.csv"

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

## Doubly residualize
all_pi[, mdmt.ln_sales_tax := weighted.mean(ln_sales_tax, w = base.sales, na.rm = T), by = .(division_by_module_by_time)]
all_pi[, dd.ln_sales_tax := ln_sales_tax - mean(ln_sales_tax, na.rm = T) - mdmt.ln_sales_tax, by = .(store_by_module)]

all_pi[, mdmt.ln_cpricei2 := weighted.mean(ln_cpricei2, w = base.sales, na.rm = T), by = .(division_by_module_by_time)]
all_pi[, dd.ln_cpricei2 := ln_cpricei2 - mean(ln_cpricei2, na.rm = T) - mdmt.ln_cpricei2, by = .(store_by_module)]

all_pi[, mdmt.ln_quantity3 := weighted.mean(ln_quantity3, w = base.sales, na.rm = T), by = .(division_by_module_by_time)]
all_pi[, dd.ln_quantity3 := ln_quantity3 - mean(ln_quantity3, na.rm = T) - mdmt.ln_quantity3, by = .(store_by_module)]


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

#  Exclude stores unidentified chains
all_pi <- all_pi[!is.na(chain)]

##### A1. Run chain level regressions -----



# Collapse at the module x chain x semester
chain.level <- all_pi[, .(dd.price = weighted.mean(dd.ln_cpricei2, w = base.sales),
                               dd.quantity = weighted.mean(dd.ln_quantity3, w = base.sales),
                               dd.sales.tax = weighted.mean(dd.ln_sales_tax, w = base.sales),
                               base.sales = sum(base.sales)), by = .(module_by_time, product_module_code, semester, year, chain)]

## Run regressions
outcomes <- c("dd.price", "dd.quantity")
LRdiff_res <- data.table(NULL)
for (Y in outcomes) {
  formula1 <- as.formula(paste0(Y, " ~ dd.sales.tax"))
  res1 <- lm(formula = formula1, data = chain.level,
             weights = chain.level$base.sales)
  
  ## attach results
  res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
  res1.dt[, outcome := Y]

  ## Descriptives
  res1.dt$N_obs <- nrow(chain.level)
  res1.dt$N_chains <- uniqueN(chain.level, by = c("chain") )
  res1.dt$N_modules <- length(unique(chain.level$product_module_code))
  LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
  fwrite(LRdiff_res, results.file.a1)
  
}

##### A2. Estimate pass-thorugh by chain size ------

## Identify number of states for each chain
chains <- stores[, .(n_stores = 1), by = .(chain, store_code_uc, fips_state_code)]
chains <- chains[, .(n_stores = sum(n_stores)), by = .(chain, fips_state_code)]
chains <- chains[, .(n_states = .N, n_stores = sum(n_stores)), by = .(chain)]
all_pi <- merge(all_pi, chains, by = "chain")

## Identify "big" and "small" chains
all_pi[, big := n_states >= 5]

## Run estimates for each subsample
FE_opts <- c("group_region_by_module_by_time", "group_division_by_module_by_time")
outcomes <- c("w.ln_cpricei2", "w.ln_quantity3")

LRdiff_res <- data.table(NULL)
## Loop over number of quantiles
for (n.g in 1:7) {
  
  ##### Full sample
  
  # Create groups of initial values of tax rate
  # We use the full weighted distribution
  all_pi <- all_pi[, quantile := cut(dm.L.ln_cpricei2,
                                     breaks = quantile(dm.L.ln_cpricei2, probs = seq(0, 1, by = 1/n.g), na.rm = T, weight = base.sales),
                                     labels = 1:n.g, right = FALSE)]
  quantlab <- round(quantile(all_pi$dm.L.ln_cpricei2, 
                             probs = seq(0, 1, by = 1/n.g), na.rm = T, 
                             weight = all_pi$base.sales), digits = 4)
  # Saturate fixed effects
  all_pi[, group_region_by_module_by_time := .GRP, by = .(region_by_module_by_time, quantile)]
  all_pi[, group_division_by_module_by_time := .GRP, by = .(division_by_module_by_time, quantile)]
  
  
  for (d in c(0,1)) {
    
    sample <- all_pi[big == d]
    
    ## Estimate RF and FS
    for (FE in FE_opts) {
      for (Y in outcomes) {
        formula1 <- as.formula(paste0(
          Y, " ~ w.ln_sales_tax:quantile | ", FE, "+ quantile"
        ))
        if (n.g == 1) { formula1 <- as.formula(paste0(Y, " ~ w.ln_sales_tax  | ", FE)) }
        res1 <- felm(formula = formula1, data = sample,
                     weights = sample$base.sales)
        
        
        ## attach results
        res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
        res1.dt[, outcome := Y]
        res1.dt[, controls := FE]
        res1.dt[, n.groups := n.g]
        res1.dt[, lev := quantlab[-1]]
        res1.dt[, big_chains := d]
        
        ## Descriptives
        res1.dt$N_obs <- nrow(sample)
        res1.dt$N_stores <- uniqueN(sample, by = c("store_code_uc") )
        res1.dt$N_modules <- length(unique(sample$product_module_code))
        res1.dt$N_counties <- uniqueN(sample, by = c("fips_state", "fips_county"))
        res1.dt$N_years <- uniqueN(sample, by = c("year")) # should be 6 (we lose one because we difference)
        res1.dt$N_county_modules <- uniqueN(sample, by = c("fips_state", "fips_county",
                                                           "product_module_code"))
        LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
        fwrite(LRdiff_res, results.file.a2)
      }
    }
  }
}