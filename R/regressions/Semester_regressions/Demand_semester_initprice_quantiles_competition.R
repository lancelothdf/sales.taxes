#' Sales Taxes Project
#' Robustness Check Competition. We want to see how our estimates look under different subsamples: 
#' those in which we observe more competition and thuse where we observe more. Do we get different conduct parameters?
#' To do this, in this code we extract 3 info:
#' 1. Price distribution in each sub sample
#' 2. Pass-through estimate in each subsample
#' 3. Demand function in each subsample
#' We use the 2 measures we have so-far: HHI index at the county level and "competitors" for each store

library(data.table)
library(futile.logger)
library(lfe)
library(multcomp)


setwd("/project2/igaarder")


## input filepaths -----------------------------------------------
#' This data is the same as all_goods_pi_path, except it has 2015-2016 data as well.
data.semester <- "Data/Nielsen/semester_nielsen_data.csv"
data.stores <- "Data/Nielsen/stores_all.csv"
data.hhi <- "Data/county_HHI.csv"


## output filepaths ----------------------------------------------
price.dist <- "Data/price_competition_sample.csv"
results.file <- "Data/competition_estimates_initprice_semester.csv"
theta.results.file <- "Data/Demand_theta_competition_initprice_semester.csv"

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

## Merge characteristics
stores <- fread(data.stores)
hhi <- fread(data.hhi)

## To the full data merge county HHIs
stores[, fips_county := fips_state_code*1000 +fips_county_code]
stores <- merge(hhi, stores, by = c("fips_county"), all.y = T)

# Collapse at the store level and calculate the median of the competition measure
stores <- stores[, .(distances_10_trips = mean(distances_10_trips, na.rm = T),
                     hh.clev.index = mean(hh.clev.index, na.rm = T)), by = .(store_code_uc)]
stores[, comp_high := median(distances_10_trips, na.rm = T)]
stores[, comp_high := distances_10_trips >= comp_high]
stores[, hhi_high := median(hh.clev.index, na.rm = T)]
stores[, hhi_high := hh.clev.index >= hhi_high]
all_pi <- merge(all_pi, stores, by = c("store_code_uc"), all.x = T)



##### Estiamtion Set up

FE_opts <- c("group_region_by_module_by_time", "group_division_by_module_by_time")
outcomes <- c("w.ln_cpricei2", "w.ln_quantity3")


LRdiff_res <- data.table(NULL)
target_res <- data.table(NULL)


##### HHI measure of competition ----------


# 1. Price distribution
prices.hhi <- all_pi[, quantile := cut(dm.L.ln_cpricei2, breaks = quantile(dm.L.ln_cpricei2, probs = seq(0, 1, by = 1/10), na.rm = T, weight = base.sales),
                                       labels = 1:10, right = T), by = .(hhi_high)]
prices.hhi <- prices.hhi[, .(price = max(dm.L.ln_cpricei2, na.rm = T)), by = .(hhi_high, quantile)]
setnames(prices.hhi, "hhi_high", "high")
prices.hhi[, type := "hhi"]

# 2. and 3. Pass-through estimates and demand recovery 

for (n.g in 1:5) {
  
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
  
  ##### Run fully saturated: split sample
  
  for (d in 0:1) {
    
    # Keep sample we use for estimation
    sample <- all_pi[hhi_high == d]
    
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
        res1.dt[, het.g := d]
        res1.dt[, het.sam := "hhi"]
        
        ## Descriptives
        res1.dt$N_obs <- nrow(sample)
        res1.dt$N_stores <- uniqueN(sample, by = c("store_code_uc") )
        res1.dt$N_modules <- length(unique(sample$product_module_code))
        res1.dt$N_counties <- uniqueN(sample, by = c("fips_state", "fips_county"))
        res1.dt$N_years <- uniqueN(sample, by = c("year")) # should be 6 (we lose one because we difference)
        res1.dt$N_county_modules <- uniqueN(sample, by = c("fips_state", "fips_county",
                                                           "product_module_code"))
        LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
        fwrite(LRdiff_res, results.file)
      }
      
      ## Estimate IVs and retrieve in vector
      IV <- LRdiff_res[outcome == "w.ln_quantity3" & n.groups == n.g & controls == FE &
                         het.sam == "hhi" & het.g == d,][["Estimate"]]/LRdiff_res[outcome == "w.ln_cpricei2" &
                                                                                    n.groups == n.g & controls == FE & het.sam == "hhi" & het.g == d,][["Estimate"]]
      
      ## Estimate the matrix of the implied system of equations
      if (n.g > 1) {
        # Get the empirical distribution of prices by quantile
        sample[, base.sales.q := base.sales/sum(base.sales), by = .(quantile)]
        sample[, p_group := floor((dm.ln_cpricei2 - min(dm.ln_cpricei2, na.rm = T))/((max(dm.ln_cpricei2, na.rm = T)-min(dm.ln_cpricei2, na.rm = T))/500)), by = .(quantile)]
        sample[, p_ll := p_group*((max(dm.ln_cpricei2, na.rm = T)-min(dm.ln_cpricei2, na.rm = T))/500), by = .(quantile)]
        sample[, p_ll := p_ll + min(dm.ln_cpricei2, na.rm = T), by = .(quantile)]
        sample[, p_ul := p_ll + ((max(dm.ln_cpricei2, na.rm = T)-min(dm.ln_cpricei2, na.rm = T))/500), by = .(quantile)]
        
        ed.price.quantile <- sample[, .(w1 = (sum(base.sales.q))), by = .(p_ul, p_ll, quantile)]
        ed.price.quantile[, p_m := (p_ul+p_ll)/2]
        
        
        # Create the derivative of the polynomial of prices and multiplicate by weights
        for (n in 1:n.g){
          ed.price.quantile[, paste0("b",n) := (n)*w1*(p_m^(n-1))]
        }
        # Calculate integral
        gamma <- ed.price.quantile[ , lapply(.SD, sum), by = .(quantile), .SDcols = paste0("b",1:n.g)]
        gamma <- gamma[!is.na(quantile),][order(quantile)][, -c("quantile")]
        
        ## Retrieve target parameters
        beta_hat <- as.vector(solve(as.matrix(gamma))%*%(as.matrix(IV)))
        # Estimate intercept
        mean.q <- all_pi[, mean(ln_quantity3, weights = base.sales)]
        mean.p <- all_pi[, mean(dm.ln_cpricei2, weights = base.sales)]
        beta_0_hat <- mean.q - sum((beta_hat)*(mean.p^(1:n.g)))
        beta_hat <- c(beta_0_hat, beta_hat)
        
        ## Export estimated target parameters
        estimated.target <- data.table(beta_hat)
        estimated.target[, beta_n := .I-1]
        estimated.target[, n.groups := n.g]
        estimated.target[, controls := FE]
        estimated.target[, het.g := d]
        estimated.target[, het.sam := "hhi"]
        
        
        target_res <- rbind(target_res, estimated.target)
        fwrite(target_res, theta.results.file)
      }
    }  
  }
}

##### G&D measure of competition. We use 10 km trips for now ----------


# 1. Price distribution
prices.comp <- all_pi[, quantile := cut(dm.L.ln_cpricei2, breaks = quantile(dm.L.ln_cpricei2, probs = seq(0, 1, by = 1/10), na.rm = T, weight = base.sales),
                                                          labels = 1:10, right = T), by = .(comp_high)]
prices.comp <- prices.comp[, .(price = max(dm.L.ln_cpricei2, na.rm = T)), by = .(comp_high, quantile)]
setnames(prices.comp, "comp_high", "high")
prices.comp[, type := "comp"]

# 2. and 3. Pass-through estimates and demand recovery 

for (n.g in 1:5) {
  
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
  
  ##### Run fully saturated: split sample
  
  for (d in 0:1) {
    
    # Keep sample we use for estimation
    sample <- all_pi[comp_high == d]
    
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
        res1.dt[, het.g := d]
        res1.dt[, het.sam := "comp"]
        
        ## Descriptives
        res1.dt$N_obs <- nrow(sample)
        res1.dt$N_stores <- uniqueN(sample, by = c("store_code_uc") )
        res1.dt$N_modules <- length(unique(sample$product_module_code))
        res1.dt$N_counties <- uniqueN(sample, by = c("fips_state", "fips_county"))
        res1.dt$N_years <- uniqueN(sample, by = c("year")) # should be 6 (we lose one because we difference)
        res1.dt$N_county_modules <- uniqueN(sample, by = c("fips_state", "fips_county",
                                                           "product_module_code"))
        LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
        fwrite(LRdiff_res, results.file)
      }
      
      ## Estimate IVs and retrieve in vector
      IV <- LRdiff_res[outcome == "w.ln_quantity3" & n.groups == n.g & controls == FE &
                         het.sam == "comp" & het.g == d,][["Estimate"]]/LRdiff_res[outcome == "w.ln_cpricei2" &
                                                                                                      n.groups == n.g & controls == FE & het.sam == "comp" & het.g == d,][["Estimate"]]
      
      ## Estimate the matrix of the implied system of equations
      if (n.g > 1) {
        # Get the empirical distribution of prices by quantile
        sample[, base.sales.q := base.sales/sum(base.sales), by = .(quantile)]
        sample[, p_group := floor((dm.ln_cpricei2 - min(dm.ln_cpricei2, na.rm = T))/((max(dm.ln_cpricei2, na.rm = T)-min(dm.ln_cpricei2, na.rm = T))/500)), by = .(quantile)]
        sample[, p_ll := p_group*((max(dm.ln_cpricei2, na.rm = T)-min(dm.ln_cpricei2, na.rm = T))/500), by = .(quantile)]
        sample[, p_ll := p_ll + min(dm.ln_cpricei2, na.rm = T), by = .(quantile)]
        sample[, p_ul := p_ll + ((max(dm.ln_cpricei2, na.rm = T)-min(dm.ln_cpricei2, na.rm = T))/500), by = .(quantile)]
        
        ed.price.quantile <- sample[, .(w1 = (sum(base.sales.q))), by = .(p_ul, p_ll, quantile)]
        ed.price.quantile[, p_m := (p_ul+p_ll)/2]
        
        
        # Create the derivative of the polynomial of prices and multiplicate by weights
        for (n in 1:n.g){
          ed.price.quantile[, paste0("b",n) := (n)*w1*(p_m^(n-1))]
        }
        # Calculate integral
        gamma <- ed.price.quantile[ , lapply(.SD, sum), by = .(quantile), .SDcols = paste0("b",1:n.g)]
        gamma <- gamma[!is.na(quantile),][order(quantile)][, -c("quantile")]
        
        ## Retrieve target parameters
        beta_hat <- as.vector(solve(as.matrix(gamma))%*%(as.matrix(IV)))
        # Estimate intercept
        mean.q <- all_pi[, mean(ln_quantity3, weights = base.sales)]
        mean.p <- all_pi[, mean(dm.ln_cpricei2, weights = base.sales)]
        beta_0_hat <- mean.q - sum((beta_hat)*(mean.p^(1:n.g)))
        beta_hat <- c(beta_0_hat, beta_hat)
        
        ## Export estimated target parameters
        estimated.target <- data.table(beta_hat)
        estimated.target[, beta_n := .I-1]
        estimated.target[, n.groups := n.g]
        estimated.target[, controls := FE]
        estimated.target[, het.g := d]
        estimated.target[, het.sam := "comp"]
        
        
        target_res <- rbind(target_res, estimated.target)
        fwrite(target_res, theta.results.file)
      }
    }  
  }
}



###### Export prices distribution -----

prices <- rbind(prices.comp, prices.hhi)
fwrite(prices, price.dist)
