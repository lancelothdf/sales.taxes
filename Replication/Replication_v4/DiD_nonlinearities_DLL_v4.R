##### Santiago Lacouture & Lancelot Henry de Frahan
#' Sales Taxes
#' Replication File. Updated on 3/07/2023
#' Step 5b: Reduced form evidence of non-linearities.
#' Export IV estimates by quantiles of lagged price distribution,
#' and distribution of current prices, using relevant estimation weights.
#' 

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
all_pi <- fread("Data/Replication_v2/all_pi_DLL.csv")
pricedist <- T

## output filepath ----------------------------------------------
iv.output.results.file <- "Data/Replication_v2/IV_subsamples_initprice.csv"
output.emp.price.dist <- "Data/Replication_v2/Emp_price_subsamples_initprice.csv"
#iv.output.results.file.boot <- "Data/Replication_v2/IV_subsamples_initprice_boot.csv"


## We only want to use the "true" tax variation
all_pi <- all_pi[non_imp_tax_strong == 1]

# Create demeaned current prices
all_pi[, n.ln_cpricei2 := ln_cpricei2 - mean(ln_cpricei2, na.rm = T), by = .(module_by_time)]

# Create treatment groups
all_pi[, treated := D.ln_sales_tax != 0]

FE_opts <- c("region_by_module_by_time", "division_by_module_by_time")

### Estimation ----

LRdiff_res <- data.table(NULL)
empirical_price_dist <- data.table(NULL)
## Run within
flog.info("Iteration 0")
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
  
  
  
  ## Estimate RF and FS
  for (FE in FE_opts) {
    
    
    ## Produce appropiate weights implied by regression
    grouped_FE <- paste0("group_", FE)
    all_pi[, wVAR := weighted.mean((DL.ln_sales_tax - 
                                      weighted.mean(DL.ln_sales_tax, 
                                                    w = base.sales, na.rm = T))^2,
                                   w = base.sales, na.rm = T), by = grouped_FE]
    all_pi[, wVAR := ifelse(is.na(wVAR), 0, wVAR)]
    # Weight normalized within quantile
    all_pi[, base.sales.q := (wVAR*base.sales)/sum(wVAR*base.sales), by = .(quantile)]
    all_pi[, base.sales.qor := base.sales/sum(base.sales), by = .(quantile)]
    
    if (pricedist) {
      
      # capture prices by bins
      step.log.p <- (max(all_pi$ln_cpricei2, na.rm = T) - min(all_pi$ln_cpricei2, na.rm = T) )/1500
      step.n.log.p <- (max(all_pi$n.ln_cpricei2, na.rm = T) - min(all_pi$n.ln_cpricei2, na.rm = T)) /1500
      min.log.p <- min(all_pi$ln_cpricei2, na.rm = T)
      min.n.log.p <- min(all_pi$n.ln_cpricei2, na.rm = T)
      all_pi[, d.lp := floor((ln_cpricei2 - min.log.p)/step.log.p)]
      all_pi[, d.n.lp := floor((n.ln_cpricei2 - min.n.log.p)/step.n.log.p)]
      
      ### Version 1: using bases.sales
      
      # Produce empirical weighted distribution of (de-meaned) current prices
      d1 <- all_pi[, .(dens.log.p = sum(base.sales.qor)), by = .(quantile, d.lp)]
      d1[, dens.log.p := dens.log.p/sum(dens.log.p), by =.(quantile)]
      d1[, log.p := d.lp*step.log.p + min.log.p + step.log.p/2]
      # Produce empirical weighted distribution of log (de-meaned) current prices
      d2 <- all_pi[, .(dens.n.log.p = sum(base.sales.qor)), by = .(quantile, d.n.lp)]
      d2[, dens.n.log.p := dens.n.log.p/sum(dens.n.log.p), by =.(quantile)]
      d2[, log.n.p := d.n.lp*step.n.log.p + min.n.log.p + step.n.log.p/2]
      
      prices_densities <- merge(d1, d2, by.x = c("d.lp", "quantile"), by.y = c("d.n.lp", "quantile"))
      prices_densities[, n.groups := n.g]
      prices_densities[, controls := FE]
      prices_densities[, treated := NA]
      prices_densities[, w := "base.sales"]
      empirical_price_dist<- rbind(empirical_price_dist, prices_densities)
      fwrite(empirical_price_dist, output.emp.price.dist)   
      
      ## Repeat by treatment group
      
      # Produce empirical weighted distribution of log (de-meaned) current prices
      d1 <- all_pi[, .(dens.log.p = sum(base.sales.qor)), by = .(quantile, d.lp, treated)]
      d1[, dens.log.p := dens.log.p/sum(dens.log.p), by =.(quantile, treated)]
      d1[, log.p := d.lp*step.log.p + min.log.p + step.log.p/2]
      d2 <- all_pi[, .(dens.n.log.p = sum(base.sales.qor)), by = .(quantile, d.n.lp, treated)]
      d2[, dens.n.log.p := dens.n.log.p/sum(dens.n.log.p), by =.(quantile, treated)]
      d2[, log.n.p := d.n.lp*step.n.log.p + min.n.log.p + step.n.log.p/2]    
      prices_densities <- merge(d1, d2, by.x = c("d.lp", "quantile", "treated"), by.y = c("d.n.lp", "quantile", "treated"))
      prices_densities[, n.groups := n.g]
      prices_densities[, controls := FE]
      prices_densities[, w := "base.sales"]
      empirical_price_dist<- rbind(empirical_price_dist, prices_densities)
      fwrite(empirical_price_dist, output.emp.price.dist)     
      
      
      
      
      ### Version 2: using ``cohort-corrected'' weights
      
      
      # Produce empirical weighted distribution of (de-meaned) current prices
      d1 <- all_pi[, .(dens.log.p = sum(base.sales.q)), by = .(quantile, d.lp)]
      d1[, dens.log.p := dens.log.p/sum(dens.log.p), by =.(quantile)]
      d1[, log.p := d.lp*step.log.p + min.log.p + step.log.p/2]
      # Produce empirical weighted distribution of log (de-meaned) current prices
      d2 <- all_pi[, .(dens.n.log.p = sum(base.sales.q)), by = .(quantile, d.n.lp)]
      d2[, dens.n.log.p := dens.n.log.p/sum(dens.n.log.p), by =.(quantile)]
      d2[, log.n.p := d.n.lp*step.n.log.p + min.n.log.p + step.n.log.p/2]
      
      prices_densities <- merge(d1, d2, by.x = c("d.lp", "quantile"), by.y = c("d.n.lp", "quantile"))
      prices_densities[, n.groups := n.g]
      prices_densities[, controls := FE]
      prices_densities[, treated := NA]
      prices_densities[, w := "base.sales.q"]
      empirical_price_dist<- rbind(empirical_price_dist, prices_densities)
      fwrite(empirical_price_dist, output.emp.price.dist)   
      
      ## Repeat by treatment group
      
      # Produce empirical weighted distribution of log (de-meaned) current prices
      d1 <- all_pi[, .(dens.log.p = sum(base.sales.q)), by = .(quantile, d.lp, treated)]
      d1[, dens.log.p := dens.log.p/sum(dens.log.p), by =.(quantile, treated)]
      d1[, log.p := d.lp*step.log.p + min.log.p + step.log.p/2]
      d2 <- all_pi[, .(dens.n.log.p = sum(base.sales.q)), by = .(quantile, d.n.lp, treated)]
      d2[, dens.n.log.p := dens.n.log.p/sum(dens.n.log.p), by =.(quantile, treated)]
      d2[, log.n.p := d.n.lp*step.n.log.p + min.n.log.p + step.n.log.p/2]    
      prices_densities <- merge(d1, d2, by.x = c("d.lp", "quantile", "treated"), by.y = c("d.n.lp", "quantile", "treated"))
      prices_densities[, n.groups := n.g]
      prices_densities[, controls := FE]
      prices_densities[, w := "base.sales.q"]
      empirical_price_dist<- rbind(empirical_price_dist, prices_densities)
      fwrite(empirical_price_dist, output.emp.price.dist)     
      
    }

    ## Produce IVs
    for (q in unique(all_pi$quantile)) {
      if (nrow(all_pi[quantile == q]) > 0) {
        
        formula1 <- as.formula(paste0("DL.ln_quantity3 ~ 0 | ", 
                                      FE, 
                                      " | (DL.ln_cpricei2 ~ DL.ln_sales_tax) | module_by_state"))
        res1 <- felm(formula = formula1, data = all_pi[quantile == q],
                     weights = all_pi[quantile == q]$base.sales)
        
        ## attach results
        res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
        res1.dt[, outcome := "IV"]
        res1.dt[, controls := FE]
        res1.dt[, group := q]
        res1.dt[, n.groups := n.g]
        
        LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
        fwrite(LRdiff_res, iv.output.results.file)
        
        
        ## First-stage 
        formula1 <- as.formula(paste0("DL.ln_cpricei2 ~ DL.ln_sales_tax | ", FE, " | 0 | module_by_state"))
        res1 <- felm(formula = formula1, data = all_pi[quantile == q],
                     weights = all_pi[quantile == q]$base.sales)
        
        ## attach results
        res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
        res1.dt[, outcome := "DL.ln_cpricei2"]
        res1.dt[, controls := FE]
        res1.dt[, group := q]
        res1.dt[, n.groups := n.g]
        
        LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
        fwrite(LRdiff_res, iv.output.results.file)
        
        
        ## Reduced-Form
        formula1 <- as.formula(paste0("DL.ln_quantity3 ~ DL.ln_sales_tax | ", FE, " | 0 | module_by_state"))
        res1 <- felm(formula = formula1, data = all_pi[quantile == q],
                     weights = all_pi[quantile == q]$base.sales)
        
        ## attach results
        res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
        res1.dt[, outcome := "DL.ln_quantity3"]
        res1.dt[, controls := FE]
        res1.dt[, group := q]
        res1.dt[, n.groups := n.g]
        
        LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
        fwrite(LRdiff_res, iv.output.results.file)
        
      }

    }
  }
}

