##### Santiago Lacouture
#' Sales Taxes
#' Replication File. Updated on 8/8/2022
#' Step 6(a): Point identification/main estimates -- robustness to control for households' characteristics


library(data.table)
library(futile.logger)
library(lfe)
library(Matrix)
library(zoo)
library(tidyverse)
library(stringr)

setwd("/project2/igaarder")
rm(list = ls())

## input filepath ----------------------------------------------
all_pi <- fread("Data/Replication/all_pi.csv")
stores <- fread("Data/Replication/stores_all.csv")

## output filepaths ----------------------------------------------
results.file <- "Data/Replication/robust_demog_estimates_initial_price_semester.csv"
theta.results.file <- "Data/Replication/Demand_theta_robust_demog_initial_price_semester.csv"

# restrict to relevant sample
all_pi <- all_pi[non_imp_tax == 1,]

## Merge Stores characteristics to retail data
all_pi <- merge(all_pi, stores, by = c("year", "store_code_uc"), all.x = T)

## Keep only the stores with observed characteristics 
all_pi <- all_pi[!is.na(av_hh_income_sales)]


FE_opts <- c("group_region_by_module_by_time", "group_division_by_module_by_time")
outcomes <- c("w.ln_cpricei2", "w.ln_quantity3")
demographics <- c("av_hh_income_trips", 'per_bachelor_25_trips', 'median_age_trips',
                  'per_hisp_trips', 'per_black_trips', 'per65_trips')


LRdiff_res <- data.table(NULL)
target_res <- data.table(NULL)

## Run estimations in the whole sample where characteristics are observed -----------------

## Loop over number of quantiles
for (n.g in 1:3) {
  

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
  
  ## Estimate RF and FS
  for (FE in FE_opts) {
    for (Y in outcomes) {
      formula1 <- as.formula(paste0(
        Y, " ~ w.ln_sales_tax:quantile | ", FE, "+ quantile"
      ))
      if (n.g == 1) { formula1 <- as.formula(paste0(Y, " ~ w.ln_sales_tax | ", FE)) }
      res1 <- felm(formula = formula1, data = all_pi,
                   weights = all_pi$base.sales)
      
      
      ## attach results
      res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
      res1.dt[, outcome := Y]
      res1.dt[, controls := FE]
      res1.dt[, n.groups := n.g]
      res1.dt[, lev := quantlab[-1]]
      res1.dt[, het := "Het.Sample"]
      res1.dt[, het.g := NA]
      res1.dt[, n.het.g := NA]
      
      ## Descriptives
      res1.dt$N_obs <- nrow(all_pi)
      res1.dt$N_stores <- uniqueN(all_pi, by = c("store_code_uc") )
      res1.dt$N_modules <- length(unique(all_pi$product_module_code))
      res1.dt$N_counties <- uniqueN(all_pi, by = c("fips_state", "fips_county"))
      res1.dt$N_years <- uniqueN(all_pi, by = c("year")) # should be 6 (we lose one because we difference)
      res1.dt$N_county_modules <- uniqueN(all_pi, by = c("fips_state", "fips_county",
                                                             "product_module_code"))
      LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
      fwrite(LRdiff_res, results.file)
    }
    
    ## Estimate IVs and retrieve in vector
    IV <- LRdiff_res[outcome == "w.ln_quantity3" & n.groups == n.g & controls == FE &
                       het == "Het.Sample",][["Estimate"]]/LRdiff_res[outcome == "w.ln_cpricei2" &
                                                                           n.groups == n.g & controls == FE & het == "Het.Sample",][["Estimate"]]
    
    ## Estimate the matrix of the implied system of equations
    if (n.g > 1) {
      ## Get the empirical distribution of prices by quantile, weighted properly by base.sales \times 
      # start by creating the weights and normalizing them 
      # Part 1 of weight: (base.sales) weighted variance of de-meaned sales tax within cohort (FE)
      all_pi[, wVAR := weighted.mean((w.ln_sales_tax - 
                                        weighted.mean(w.ln_sales_tax, 
                                                      w = base.sales, na.rm = T))^2,
                                     w = base.sales, na.rm = T), by = FE]
      all_pi[, wVAR := ifelse(is.na(wVAR), 0, wVAR)]
      # Weight normalized within quantile
      all_pi[, base.sales.q := (wVAR*base.sales)/sum(wVAR*base.sales), by = .(quantile)]
      all_pi[, p_group := floor((dm.ln_cpricei2 - min(dm.ln_cpricei2, na.rm = T))/((max(dm.ln_cpricei2, na.rm = T)-min(dm.ln_cpricei2, na.rm = T))/500)), by = .(quantile)]
      all_pi[, p_ll := p_group*((max(dm.ln_cpricei2, na.rm = T)-min(dm.ln_cpricei2, na.rm = T))/500), by = .(quantile)]
      all_pi[, p_ll := p_ll + min(dm.ln_cpricei2, na.rm = T), by = .(quantile)]
      all_pi[, p_ul := p_ll + ((max(dm.ln_cpricei2, na.rm = T)-min(dm.ln_cpricei2, na.rm = T))/500), by = .(quantile)]
      
      ed.price.quantile <- all_pi[, .(w1 = (sum(base.sales.q))), by = .(p_ul, p_ll, quantile)]
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
      estimated.target[, het := "Het.Sample"]
      estimated.target[, het.g := NA]
      estimated.target[, n.het.g := NA]
      estimated.target[, P.het.d := NA]
      
      target_res <- rbind(target_res, estimated.target)
      fwrite(target_res, theta.results.file)
    }
  }
}



## Run estimations saturating by demographics -----------------

## Loop over the demographic we look at
for (dem in demographics) {
  for (sat.groups in 2:5) {
    
    # Divide the sample by the demographic's quantiles (at the level store)
    median <- all_pi[, .(D = mean(get(dem), na.rm = T)), by = .(store_code_uc)]
    median <- median[, het := cut(D, breaks = quantile(D, probs = seq(0, 1, by = 1/sat.groups), na.rm = T),
                                  labels = 1:sat.groups, right = FALSE)]
    all_pi_het <- merge(all_pi, median, by = "store_code_uc")
    
    # Saturate fixed effects
    all_pi_het[, dem_group_region_by_module_by_time := .GRP, by = .(region_by_module_by_time, het)]
    all_pi_het[, dem_group_division_by_module_by_time := .GRP, by = .(division_by_module_by_time, het)]
    
    ## Loop over number of quantiles of initial prices
    for (n.g in 1:5) {
      
      # Create groups of initial values of tax rate
      # We use the full weighted distribution
      all_pi_het <- all_pi_het[, quantile := cut(dm.L.ln_cpricei2,
                                                 breaks = quantile(dm.L.ln_cpricei2, probs = seq(0, 1, by = 1/n.g), na.rm = T, weight = base.sales),
                                                 labels = 1:n.g, right = FALSE)]
      quantlab <- round(quantile(all_pi_het$dm.L.ln_cpricei2, 
                                 probs = seq(0, 1, by = 1/n.g), na.rm = T, 
                                 weight = all_pi_het$base.sales), digits = 4)
      # Saturate fixed effects
      all_pi_het[, group_region_by_module_by_time := .GRP, by = .(region_by_module_by_time, quantile)]
      all_pi_het[, group_division_by_module_by_time := .GRP, by = .(division_by_module_by_time, quantile)]
      
      ##### Run fully saturated: split sample
      for (d in 1:sat.groups) {
        
        # Keep sample we use for estimation
        sample <- all_pi_het[het == d]
        
        # Capture sample proportion
        prop <- nrow(sample)/nrow(all_pi[!is.na(dem)])
        
        
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
            res1.dt[, het := dem]
            res1.dt[, het.g := d]
            res1.dt[, n.het.g := sat.groups]
            
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
                             n.het.g == sat.groups & het == dem & het.g == d,][["Estimate"]]/LRdiff_res[outcome == "w.ln_cpricei2" &
                                                                                                          n.groups == n.g & controls == FE & n.het.g == sat.groups & het == dem & het.g == d,][["Estimate"]]
          
          ## Estimate the matrix of the implied system of equations
          if (n.g > 1) {
            ## Get the empirical distribution of prices by quantile, weighted properly by base.sales \times 
            # start by creating the weights and normalizing them 
            # Part 1 of weight: (base.sales) weighted variance of de-meaned sales tax within cohort (FE)
            sample[, wVAR := weighted.mean((w.ln_sales_tax - 
                                              weighted.mean(w.ln_sales_tax, 
                                                            w = base.sales, na.rm = T))^2,
                                           w = base.sales, na.rm = T), by = FE]
            sample[, wVAR := ifelse(is.na(wVAR), 0, wVAR)]
            # Weight normalized within quantile
            sample[, base.sales.q := (wVAR*base.sales)/sum(wVAR*base.sales), by = .(quantile)]
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
            estimated.target[, het := dem]
            estimated.target[, het.g := d]
            estimated.target[, n.het.g := sat.groups]
            estimated.target[, P.het.d := prop]
            
            
            target_res <- rbind(target_res, estimated.target)
            fwrite(target_res, theta.results.file)
          }
        }  
      }
    }
  }
}
