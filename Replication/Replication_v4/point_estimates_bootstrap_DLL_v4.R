##### Wesley Janson and Santiago Lacouture
#' Sales Taxes
#' Replication File. Updated on 04/18/2023
#' Step 6: Point identification/main estimates and bootstrap portion of replication

library(data.table)
library(futile.logger)
library(lfe)
library(Matrix)
library(zoo)
library(tidyverse)
library(stringr)

setwd("/project/igaarder")
rm(list = ls())

## input filepath ----------------------------------------------
all_pi <- fread("Data/Replication_v4/all_pi.csv")


## output filepath ----------------------------------------------
theta.output.results.file <- "Data/Replication_v4/Demand_theta_sat_initial_price_semester_boot_r.csv"
iv.output.results.file <- "Data/Replication_v4/Demand_iv_sat_initial_price_semester_boot_r.csv"


### 6. Point identification (K = L). L= 1, ..., 5 Main estimates and Bootstrap -----------
outcomes <- c("DL.ln_cpricei2", "DL.ln_quantity3")
FE_opts <- c("group_region_by_module_by_time", "group_division_by_module_by_time")

## We only want to use the "true" tax variation
all_pi <- all_pi[non_imp_tax_strong == 1] ## Should already be limited to this sample

# Check: Drop observations with missings so that bootstrap should work
# print(nrow(all_pi))
# all_pi <- all_pi[!is.na(ln_sales_tax) & !is.na(ln_quantity3) & !is.na(ln_cpricei2) & is.finite(ln_cpricei2) & is.finite(ln_quantity3)]
# print(nrow(all_pi))

LRdiff_res <- data.table(NULL)
target_res <- data.table(NULL)
## Run within
flog.info("Iteration 0")
for (n.g in c(1:3,5)) {
  
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

  # # Demean properly by quantile
  # if (n.g > 1) {
  #   all_pi[, w.ln_quantity3 := ln_quantity3 - mean(ln_quantity3, na.rm = T), by = .(store_by_module, quantile)]
  #   all_pi[, w.ln_cpricei2 := ln_cpricei2 - mean(ln_cpricei2, na.rm = T), by = .(store_by_module, quantile)]
  #   all_pi[, w.ln_sales_tax := ln_sales_tax - mean(ln_sales_tax, na.rm = T), by = .(store_by_module, quantile)]
  # }  
    
  ## Estimate RF and FS
  for (FE in FE_opts) {
    for (Y in outcomes) {
      formula1 <- as.formula(paste0(
        Y, " ~ DL.ln_sales_tax:quantile | ", FE, "+ quantile"
      ))
      if (n.g == 1) { formula1 <- as.formula(paste0(Y, " ~ DL.ln_sales_tax | ", FE)) }
      res1 <- felm(formula = formula1, data = all_pi,
                   weights = all_pi$base.sales)
      
      
      ## attach results
      res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
      res1.dt[, outcome := Y]
      res1.dt[, controls := FE]
      res1.dt[, n.groups := n.g]
      res1.dt[, lev := quantlab[-1]]
      res1.dt[, iter := 0]
      
      LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
      fwrite(LRdiff_res, iv.output.results.file)
    }
    
    ## Estimate IVs and retrieve in vector
    IV <- LRdiff_res[outcome == "DL.ln_quantity3" & n.groups == n.g & controls == FE,][["Estimate"]]/LRdiff_res[outcome == "DL.ln_cpricei2" & n.groups == n.g & controls == FE,][["Estimate"]]
    
    ## Estimate the matrix of the implied system of equations
    if (n.g > 1) {
      ## Get the empirical distribution of prices by quantile, weighted properly by base.sales \times 
      # start by creating the weights and normalizing them 
      # Part 1 of weight: (base.sales) weighted variance of de-meaned sales tax within cohort (FE)
      all_pi[, wVAR := weighted.mean((DL.ln_sales_tax - 
                                           weighted.mean(DL.ln_sales_tax, 
                                                         w = base.sales, na.rm = T))^2,
                                        w = base.sales, na.rm = T), by = FE]
      all_pi[, wVAR := ifelse(is.na(wVAR), 0, wVAR)]
      # Weight normalized within quantile
      all_pi[, base.sales.q := (wVAR*base.sales)/sum(wVAR*base.sales), by = .(quantile)]
      
      # To compute the empirical distribution of prices, we need to bin the support.
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
      estimated.target[, iter := 0]
      target_res <- rbind(target_res, estimated.target)
      fwrite(target_res, theta.output.results.file)
    }
  } 
}

### Start manual bootstrap
set.seed(2019)

# Sample within quantile
ids <- unique(all_pi$module_by_state)
ids <- ids[order(ids)]

for (rep in 1:100) {
  
  flog.info("Iteration %s", rep)
  
  # Sample by block
  sampled.ids <- data.table(module_by_state = sample(ids, replace = T))
  # Merge data to actual data
  sampled.data <- merge(sampled.ids, all_pi, by = c("module_by_state") , allow.cartesian = T, all.x = T)
  
  # First split by quantile
  for (n.g in c(1:3,5)) {
    
    # Create groups of initial values of tax rate
    # We use the full weighted distribution
    sampled.data <- sampled.data[, quantile := cut(dm.L.ln_cpricei2,
                                                   breaks = quantile(dm.L.ln_cpricei2, probs = seq(0, 1, by = 1/n.g), na.rm = T, weight = base.sales),
                                                   labels = 1:n.g, right = FALSE)]
    quantlab <- round(quantile(sampled.data$dm.L.ln_cpricei2, 
                               probs = seq(0, 1, by = 1/n.g), na.rm = T, 
                               weight = sampled.data$base.sales), digits = 4)
    # Saturate fixed effects
    sampled.data[, group_region_by_module_by_time := .GRP, by = .(region_by_module_by_time, quantile)]
    sampled.data[, group_division_by_module_by_time := .GRP, by = .(division_by_module_by_time, quantile)]
    

    
    # # Demean properly by quantile
    # if (n.g > 1) {
    #   sampled.data[, w.ln_quantity3 := ln_quantity3 - mean(ln_quantity3, na.rm = T), by = .(store_by_module, quantile)]
    #   sampled.data[, w.ln_cpricei2 := ln_cpricei2 - mean(ln_cpricei2, na.rm = T), by = .(store_by_module, quantile)]
    #   sampled.data[, w.ln_sales_tax := ln_sales_tax - mean(ln_sales_tax, na.rm = T), by = .(store_by_module, quantile)]
    # }
    ## Estimate RF and FS
    for (FE in FE_opts) {
      for (Y in outcomes) {
        formula1 <- as.formula(paste0(
          Y, " ~ DL.ln_sales_tax:quantile | ", FE, "+ quantile"
        ))
        if (n.g == 1) { formula1 <- as.formula(paste0(Y, " ~ DL.ln_sales_tax | ", FE)) }
        res1 <- felm(formula = formula1, data = sampled.data,
                     weights = sampled.data$base.sales)
        
        
        ## attach results
        res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
        res1.dt[, outcome := Y]
        res1.dt[, controls := FE]
        res1.dt[, n.groups := n.g]
        res1.dt[, lev := quantlab[-1]]
        res1.dt[, iter := rep]
        
        LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
        fwrite(LRdiff_res, iv.output.results.file)
      }
      
      ## Estimate IVs and retrieve in vector
      IV <- LRdiff_res[outcome == "DL.ln_quantity3" & n.groups == n.g & controls == FE & iter == rep,][["Estimate"]]/LRdiff_res[outcome == "DL.ln_cpricei2" & n.groups == n.g & controls == FE & iter == rep,][["Estimate"]]
      
      ## Estimate the matrix of the implied system of equations
      if (n.g > 1 & n.g <5) { # do not build matrices for L = 5
        
        ## Get the empirical distribution of prices by quantile, weighted properly by base.sales \times 
        # start by creating the weights and normalizing them 
        # Part 1 of weight: (base.sales) weighted variance of de-meaned sales tax within cohort (FE)
        sampled.data[, wVAR := weighted.mean((DL.ln_sales_tax - 
                                                  weighted.mean(DL.ln_sales_tax, 
                                                                w = base.sales, na.rm = T))^2,
                                               w = base.sales, na.rm = T), by = FE]
        sampled.data[, wVAR := ifelse(is.na(wVAR), 0, wVAR)]
        # Weight normalized within quantile
        sampled.data[, base.sales.q := (wVAR*base.sales)/sum(wVAR*base.sales), by = .(quantile)]
        # To compute the empirical distribution of prices, we need to bin the support.
        sampled.data[, p_group := floor((dm.ln_cpricei2 - min(dm.ln_cpricei2, na.rm = T))/((max(dm.ln_cpricei2, na.rm = T)-min(dm.ln_cpricei2, na.rm = T))/500)), by = .(quantile)]
        sampled.data[, p_ll := p_group*((max(dm.ln_cpricei2, na.rm = T)-min(dm.ln_cpricei2, na.rm = T))/500), by = .(quantile)]
        sampled.data[, p_ll := p_ll + min(dm.ln_cpricei2, na.rm = T), by = .(quantile)]
        sampled.data[, p_ul := p_ll + ((max(dm.ln_cpricei2, na.rm = T)-min(dm.ln_cpricei2, na.rm = T))/500), by = .(quantile)]
        
        ed.price.quantile <- sampled.data[, .(w1 = (sum(base.sales.q))), by = .(p_ul, p_ll, quantile)]
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
        mean.q <- sampled.data[, mean(ln_quantity3, weights = base.sales)]
        mean.p <- sampled.data[, mean(dm.ln_cpricei2, weights = base.sales)]
        beta_0_hat <- mean.q - sum((beta_hat)*(mean.p^(1:n.g)))
        beta_hat <- c(beta_0_hat, beta_hat)
        
        ## Export estimated target parameters
        estimated.target <- data.table(beta_hat)
        estimated.target[, beta_n := .I-1]
        estimated.target[, n.groups := n.g]
        estimated.target[, controls := FE]
        estimated.target[, iter := rep]
        target_res <- rbind(target_res, estimated.target)
        fwrite(target_res, theta.output.results.file)
      }
    } 
  }
}
