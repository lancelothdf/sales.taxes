##### Wesley Janson and Santiago Lacouture
#' Sales Taxes
#' Replication File. Updated on 8/24/2022
#' Step 5a: Reduced form evidence on nonlinearities. Here we want to work with the passthrough only and hopefully show it is stable

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

## Output
pt.output.results.file <- "Data/Replication/Passthrough_nonlinear_sat_initial_price_semester_boot.csv"


FE_opts <- c("group_region_by_module_by_time", "group_division_by_module_by_time")

## We only want to use the "true" tax variation
all_pi <- all_pi[non_imp_tax == 1]

LRdiff_res <- data.table(NULL)
target_res <- data.table(NULL)
## Run within
flog.info("Iteration 0")
for (n.g in c(1:3,5)) {
  
  # Create groups of initial values of tax rate
  for (inp in c(dm.L.ln_cpricei2, dm.ln_pricei2)) {
    # We use the full weighted distribution
    all_pi <- all_pi[, quantile := cut(get(inp),
                                       breaks = quantile(get(inp), probs = seq(0, 1, by = 1/n.g), na.rm = T, weight = base.sales),
                                       labels = 1:n.g, right = FALSE)]
    quantlab <- round(quantile(all_pi[[inp]], 
                               probs = seq(0, 1, by = 1/n.g), na.rm = T, 
                               weight = all_pi$base.sales), digits = 4)
    
    # Saturate fixed effects
    all_pi[, group_region_by_module_by_time := .GRP, by = .(region_by_module_by_time, quantile)]
    all_pi[, group_division_by_module_by_time := .GRP, by = .(division_by_module_by_time, quantile)]
    
    ## Estimate RF and FS
    for (FE in FE_opts) {
      
        formula1 <- as.formula(paste0("w.ln_cpricei2 ~ w.ln_sales_tax:quantile | ", FE, "+ quantile"))
        
        if (n.g == 1) { formula1 <- as.formula(paste0("w.ln_cpricei2 ~ w.ln_sales_tax | ", FE)) }
        res1 <- felm(formula = formula1, data = all_pi,
                     weights = all_pi$base.sales)
        
        
        ## attach results
        res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
        res1.dt[, initprice := inp]
        res1.dt[, controls := FE]
        res1.dt[, n.groups := n.g]
        res1.dt[, group := seq_len(.N)]
        res1.dt[, iter := 0]
        
        LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
        fwrite(LRdiff_res, pt.output.results.file)
      
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
    for (inp in c(dm.L.ln_cpricei2, dm.ln_pricei2)) {
      # We use the full weighted distribution
      sampled.data <- sampled.data[, quantile := cut(get(inp),
                                         breaks = quantile(get(inp), probs = seq(0, 1, by = 1/n.g), na.rm = T, weight = base.sales),
                                         labels = 1:n.g, right = FALSE)]
      quantlab <- round(quantile(sampled.data[[inp]], 
                                 probs = seq(0, 1, by = 1/n.g), na.rm = T, 
                                 weight = sampled.data$base.sales), digits = 4)
      
      # Saturate fixed effects
      sampled.data[, group_region_by_module_by_time := .GRP, by = .(region_by_module_by_time, quantile)]
      sampled.data[, group_division_by_module_by_time := .GRP, by = .(division_by_module_by_time, quantile)]
      
      ## Estimate RF and FS
      for (FE in FE_opts) {
        
        formula1 <- as.formula(paste0("w.ln_cpricei2 ~ w.ln_sales_tax:quantile | ", FE, "+ quantile"))
        
        if (n.g == 1) { formula1 <- as.formula(paste0("w.ln_cpricei2 ~ w.ln_sales_tax | ", FE)) }
        res1 <- felm(formula = formula1, data = sampled.data,
                     weights = sampled.data$base.sales)
        
        
        ## attach results
        res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
        res1.dt[, initprice := inp]
        res1.dt[, controls := FE]
        res1.dt[, n.groups := n.g]
        res1.dt[, group := seq_len(.N)]
        res1.dt[, iter := rep]
        
        LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
        fwrite(LRdiff_res, pt.output.results.file)
        
      }
    } 
  }
}
