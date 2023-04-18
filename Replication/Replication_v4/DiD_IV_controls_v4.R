##### Santiago Lacouture & Lancelot Henry de Frahan
#' Sales Taxes
#' Replication File. Updated on 04/18/2023
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
all_pi <- fread("Data/Replication_v4/all_pi_econ.csv")
pricedist <- T

## output filepath ----------------------------------------------
iv.output.results.file <- "Data/Replication_v4/IV_Dem_Sup_controls.csv"


## We only want to use the "true" tax variation
all_pi <- all_pi[non_imp_tax_strong == 1] ## all_pi should already only include this sample

# Create demeaned current prices
all_pi[, n.ln_cpricei2 := ln_cpricei2 - mean(ln_cpricei2, na.rm = T), by = .(module_by_time)]

# Create treatment groups
all_pi[, treated := DL.ln_sales_tax != 0]

FE_opts <- c("region_by_module_by_time", "division_by_module_by_time")

### Estimation ----

LRdiff_res <- data.table(NULL)
empirical_price_dist <- data.table(NULL)
## Run within
flog.info("Iteration 0")
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

    ## Produce IVs
    for (q in unique(all_pi$quantile)) {
      if (nrow(all_pi[quantile == q]) > 0) {
        
        ## IV for elasticity of Demand
        formula1 <- as.formula(paste0("DL.ln_quantity3 ~ DL.ln_home_price + DL.ln_unemp | ", 
                                      FE, 
                                      " | (DL.ln_cpricei2 ~ DL.ln_sales_tax) | module_by_state"))
        res1 <- felm(formula = formula1, data = all_pi[quantile == q],
                     weights = all_pi[quantile == q]$base.sales)
        
        ## attach results
        res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
        res1.dt[, outcome := "Demand"]
        res1.dt[, controls := FE]
        res1.dt[, group := q]
        res1.dt[, n.groups := n.g]
        
        LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
        fwrite(LRdiff_res, iv.output.results.file)
        
        ## First-stage 
        formula1 <- as.formula(paste0("DL.ln_cpricei2 ~ DL.ln_sales_tax + DL.ln_home_price + DL.ln_unemp | ", FE, " | 0 | module_by_state"))
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
        formula1 <- as.formula(paste0("DL.ln_quantity3 ~ DL.ln_sales_tax + DL.ln_home_price + DL.ln_unemp | ", FE, " | 0 | module_by_state"))
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
        
        
        ## IV for inverse supply
        formula1 <- as.formula(paste0("DL.ln_pricei2 ~ DL.ln_home_price + DL.ln_unemp | ", 
                                      FE, 
                                      " | (DL.ln_quantity3 ~ DL.ln_sales_tax) | module_by_state"))
        res1 <- felm(formula = formula1, data = all_pi[quantile == q],
                     weights = all_pi[quantile == q]$base.sales)
        
        ## attach results
        res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
        res1.dt[, outcome := "Inverse Supply"]
        res1.dt[, controls := FE]
        res1.dt[, group := q]
        res1.dt[, n.groups := n.g]
        
        LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
        fwrite(LRdiff_res, iv.output.results.file)
        
        
        ## Reduced-Form 2
        formula1 <- as.formula(paste0("DL.ln_pricei2 ~ DL.ln_sales_tax + DL.ln_home_price + DL.ln_unemp | ", FE, " | 0 | module_by_state"))
        res1 <- felm(formula = formula1, data = all_pi[quantile == q],
                     weights = all_pi[quantile == q]$base.sales)
        
        ## attach results
        res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
        res1.dt[, outcome := "DL.ln_pricei2"]
        res1.dt[, controls := FE]
        res1.dt[, group := q]
        res1.dt[, n.groups := n.g]
        
        LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
        fwrite(LRdiff_res, iv.output.results.file)
        
      }

    }
  }
}

