##### Santiago Lacouture
#' Sales Taxes
#' Replication File. Updated on 8/9/2022
#' Step 7: Partial identification. This code produces the matrices for the Bernestein polynomials, bootstrapping.
#' We take that input and solve the linear programming in a separate code (step 8)
#' and produce the average estimates across states (step 9)
#' Here we only run the division \times module \times time specification

library(data.table)
library(futile.logger)
library(Matrix)
library(zoo)
library(tidyverse)
library(stringr)

setwd("/project2/igaarder")
rm(list = ls())

## input filepath ----------------------------------------------
all_pi <- fread("Data/Replication/all_pi.csv")

theta.bernstein <- "Data/Replication/Demand_gamma_sat_initial_price_semester_boot_r_K"
pq.output.results.file <- "Data/Replication/Demand_pq_sat_initial_price_semester_boot_r_partial.csv"

## We only want to use the "true" tax variation
all_pi <- all_pi[non_imp_tax == 1]



## Bernstein basis Function
bernstein <- function(x, k, K){
  choose(K, k) * x^k * (1 - x)^(K - k)
}

## Bernstein basis Function Derivative
d.bernstein <-function(x, k, K) {
  K*(bernstein(x, k-1, K-1) - bernstein(x, k, K-1))
}

FE <- "group_division_by_module_by_time"

# Re-Define de-meaned prices
min.p <- all_pi[, min(dm.ln_cpricei2)]
max.p <- all_pi[, max(dm.ln_cpricei2)]
all_pi[, r.dm.ln_cpricei2 := (dm.ln_cpricei2 - min.p)/(max.p - min.p) ]

LRdiff_res <- data.table(NULL)
pq_res <- data.table(NULL)
# To estimate the intercept
mean.q <- all_pi[, mean(ln_quantity3, weights = base.sales, na.rm = T)]
mean.p <- all_pi[, mean(r.dm.ln_cpricei2, weights = base.sales, na.rm = T)]

estimated.pq <- data.table(mean.q, mean.p, min.p, max.p)
estimated.pq[,iter := 0]
pq_res <- rbind(pq_res, estimated.pq)
fwrite(pq_res, pq.output.results.file)
# Run other
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
  
  # Saturate with FE
  all_pi[, paste0(FE) := .GRP, by = .(division_by_module_by_time, quantile)]
  
  ## Do partial identification
  ## Estimate the matrix of the implied system of equations. For each possible polynomial degree and compute 
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
  all_pi[, p_group := floor((r.dm.ln_cpricei2 - min(r.dm.ln_cpricei2, na.rm = T))/((max(r.dm.ln_cpricei2, na.rm = T)-min(r.dm.ln_cpricei2, na.rm = T))/100)), by = .(quantile)]
  all_pi[, p_ll := p_group*((max(r.dm.ln_cpricei2, na.rm = T)-min(r.dm.ln_cpricei2, na.rm = T))/100), by = .(quantile)]
  all_pi[, p_ll := p_ll + min(r.dm.ln_cpricei2, na.rm = T), by = .(quantile)]
  all_pi[, p_ul := p_ll + ((max(r.dm.ln_cpricei2, na.rm = T)-min(r.dm.ln_cpricei2, na.rm = T))/100), by = .(quantile)]
  
  ed.price.quantile <- all_pi[, .(w1 = (sum(base.sales.q))), by = .(p_ul, p_ll, quantile)]
  ed.price.quantile[, p_m := (p_ul+p_ll)/2]
  
  for (K in (n.g+1):10) {
    
    # Create the derivative of the polynomial of prices and multiplicate by weights
    for (n in 0:(K-1)){
      ed.price.quantile[, paste0("b",n) := w1*(bernstein(p_m,n,K-1))]
    }
    
    # Calculate integral
    gamma <- ed.price.quantile[ , lapply(.SD, sum), by = .(quantile), .SDcols = paste0("b",0:(K-1))]
    gamma <- gamma[!is.na(quantile),][order(quantile)][, -c("quantile")]
    
    # Export Calculation
    gamma[, n.groups := n.g]
    gamma[, iter := 0]
    
    ## Read Previous and write
    theta.output.results.file <- paste0(theta.bernstein, K,"_bern.csv")
    
    if (n.g == 1) {
      fwrite(gamma, theta.output.results.file)
    } else {
      previous.data <- fread(theta.output.results.file)
      previous.data <- rbind(previous.data, gamma)
      fwrite(previous.data, theta.output.results.file)
    }
    
  }
  
}


### Start manual bootstrap
set.seed(2019)
ids <- unique(all_pi$module_by_state)

for (rep in 1:100) {
  
  flog.info("Iteration %s", rep)
  
  # Sample by block
  sampled.ids <- data.table(sample(ids, replace = T))
  setnames(sampled.ids, old= "V1", new = "module_by_state")
  
  # Merge data to actual data
  sampled.data <- merge(sampled.ids, all_pi, by = c("module_by_state") , allow.cartesian = T, all.x = T)
  
  # Capture means
  mean.q <- sampled.data[, mean(ln_quantity3, weights = base.sales, na.rm = T)]
  mean.p <- sampled.data[, mean(r.dm.ln_cpricei2, weights = base.sales, na.rm = T)]
  
  estimated.pq <- data.table(mean.q, mean.p, min.p, max.p)
  estimated.pq[, iter := rep]
  pq_res <- rbind(pq_res, estimated.pq)
  fwrite(pq_res, pq.output.results.file)  
  
  
  for (n.g in 1:3) {
    # Create groups of initial values of tax rate
    # We use the full weighted distribution
    sampled.data <- sampled.data[, quantile := cut(dm.L.ln_cpricei2,
                                       breaks = quantile(dm.L.ln_cpricei2, probs = seq(0, 1, by = 1/n.g), na.rm = T, weight = base.sales),
                                       labels = 1:n.g, right = FALSE)]
    quantlab <- round(quantile(sampled.data$dm.L.ln_cpricei2, 
                               probs = seq(0, 1, by = 1/n.g), na.rm = T, 
                               weight = sampled.data$base.sales), digits = 4)
    
    # Saturate with FE
    sampled.data[, paste0(FE) := .GRP, by = .(division_by_module_by_time, quantile)]
    
    ## Do partial identification
    ## Estimate the matrix of the implied system of equations. For each possible polynomial degree and compute 
    ## Get the empirical distribution of prices by quantile, weighted properly by base.sales \times 
    # start by creating the weights and normalizing them 
    # Part 1 of weight: (base.sales) weighted variance of de-meaned sales tax within cohort (FE)
    sampled.data[, wVAR := weighted.mean((w.ln_sales_tax - 
                                      weighted.mean(w.ln_sales_tax, 
                                                    w = base.sales, na.rm = T))^2,
                                   w = base.sales, na.rm = T), by = FE]
    sampled.data[, wVAR := ifelse(is.na(wVAR), 0, wVAR)]
    # Weight normalized within quantile
    sampled.data[, base.sales.q := (wVAR*base.sales)/sum(wVAR*base.sales), by = .(quantile)]
    sampled.data[, p_group := floor((r.dm.ln_cpricei2 - min(r.dm.ln_cpricei2, na.rm = T))/((max(r.dm.ln_cpricei2, na.rm = T)-min(r.dm.ln_cpricei2, na.rm = T))/100)), by = .(quantile)]
    sampled.data[, p_ll := p_group*((max(r.dm.ln_cpricei2, na.rm = T)-min(r.dm.ln_cpricei2, na.rm = T))/100), by = .(quantile)]
    sampled.data[, p_ll := p_ll + min(r.dm.ln_cpricei2, na.rm = T), by = .(quantile)]
    sampled.data[, p_ul := p_ll + ((max(r.dm.ln_cpricei2, na.rm = T)-min(r.dm.ln_cpricei2, na.rm = T))/100), by = .(quantile)]
    
    ed.price.quantile <- sampled.data[, .(w1 = (sum(base.sales.q))), by = .(p_ul, p_ll, quantile)]
    ed.price.quantile[, p_m := (p_ul+p_ll)/2]
    
    for (K in (n.g+1):10) {
      
      # Create the derivative of the polynomial of prices and multiplicate by weights
      for (n in 0:(K-1)){
        ed.price.quantile[, paste0("b",n) := w1*(bernstein(p_m,n,K-1))]
      }
      
      # Calculate integral
      gamma <- ed.price.quantile[ , lapply(.SD, sum), by = .(quantile), .SDcols = paste0("b",0:(K-1))]
      gamma <- gamma[!is.na(quantile),][order(quantile)][, -c("quantile")]
      
      # Export Calculation
      gamma[, n.groups := n.g]
      gamma[, iter := rep]
      
      ## Read Previous and write
      theta.output.results.file <- paste0(theta.bernstein, K,"_bern.csv")
      previous.data <- fread(theta.output.results.file)
      previous.data <- rbind(previous.data, gamma)
      fwrite(previous.data, theta.output.results.file)
      
      
    }
  }
}