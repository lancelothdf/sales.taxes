##### Wesley Janson
#' Sales Taxes
#' Replication File. Updated on 5/21/2022
#' Step 7: Partial identification portion of replication

library(data.table)
library(futile.logger)
library(lfe)
library(multcomp)
library(Matrix)
library(zoo)
library(tidyverse)
library(stringr)
library(nloptr)
library(doParallel)
library(MASS)
library(pracma)

setwd("/project2/igaarder")
rm(list = ls())

## input filepath ----------------------------------------------
all_pi <- fread("Data/Replication/all_pi.csv")

## output filepath ----------------------------------------------
theta.output.results.file <- "Data/Replication/Demand_theta_sat_initial_price_semester_boot_r.csv"
theta.bernstein <- "Data/Replication/Demand_gamma_sat_initial_price_semester_boot_r_K"



### 7. Partial identification. Extract Matrices of Weigths based on Bernestein Polynomials ------------

## Bernstein basis Function
bernstein <- function(x, k, K){
  choose(K, k) * x^k * (1 - x)^(K - k)
}

## Bernstein basis Function Derivative
d.bernstein <-function(x, k, K) {
  K*(bernstein(x, k-1, K-1) - bernstein(x, k, K-1))
}

# Re-Define de-meaned prices
min.p <- all_pi[, min(dm.ln_cpricei2)]
max.p <- all_pi[, max(dm.ln_cpricei2)]
all_pi[, r.dm.ln_cpricei2 := (dm.ln_cpricei2 - min.p)/(max.p - min.p) ]

LRdiff_res <- data.table(NULL)
pq_res <- data.table(NULL)
## Not bootstrapping!!
# To estimate the intercept
mean.q <- all_pi[, mean(ln_quantity3, weights = base.sales, na.rm = T)]
mean.p <- all_pi[, mean(r.dm.ln_cpricei2, weights = base.sales, na.rm = T)]

estimated.pq <- data.table(mean.q, mean.p, min.p, max.p)
pq_res <- rbind(pq_res, estimated.pq)
fwrite(pq_res, pq.output.results.file)
# Run others
for (n.g in 1:5) {
  
  # Create groups of initial values of tax rate
  # We use the full weighted distribution
  all_pi <- all_pi[, quantile := cut(dm.L.ln_cpricei2,
                                     breaks = quantile(dm.L.ln_cpricei2, probs = seq(0, 1, by = 1/n.g), na.rm = T, weight = base.sales),
                                     labels = 1:n.g, right = FALSE)]
  quantlab <- round(quantile(all_pi$dm.L.ln_cpricei2, 
                             probs = seq(0, 1, by = 1/n.g), na.rm = T, 
                             weight = all_pi$base.sales), digits = 4)
  
  ## Do partial identification
  ## Estimate the matrix of the implied system of equations. For each possible polynomial degree and compute 
  # Get the empirical distribution of prices by quantile
  all_pi[, base.sales.q := base.sales/sum(base.sales), by = .(quantile)]
  all_pi[, p_group := floor((r.dm.ln_cpricei2 - min(r.dm.ln_cpricei2, na.rm = T))/((max(r.dm.ln_cpricei2, na.rm = T)-min(r.dm.ln_cpricei2, na.rm = T))/100)), by = .(quantile)]
  all_pi[, p_ll := p_group*((max(r.dm.ln_cpricei2, na.rm = T)-min(r.dm.ln_cpricei2, na.rm = T))/100), by = .(quantile)]
  all_pi[, p_ll := p_ll + min(r.dm.ln_cpricei2, na.rm = T), by = .(quantile)]
  all_pi[, p_ul := p_ll + ((max(r.dm.ln_cpricei2, na.rm = T)-min(r.dm.ln_cpricei2, na.rm = T))/100), by = .(quantile)]
  
  ed.price.quantile <- all_pi[, .(w1 = (sum(base.sales.q))), by = .(p_ul, p_ll, quantile)]
  ed.price.quantile[, p_m := (p_ul+p_ll)/2]
  
  for (K in (n.g):10) {
    
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
