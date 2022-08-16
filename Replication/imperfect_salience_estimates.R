##### Santiago Lacouture
#' Sales Taxes
#' Replication File. Updated on 8/13/2022
#' Step 10: Repeat estimation for partial identification but with demand under imperfect salience

library(data.table)
library(futile.logger)
library(lfe)
library(Matrix)
library(zoo)
library(stringr)

setwd("/project2/igaarder")
rm(list = ls())

## input filepath ----------------------------------------------
all_pi <- fread("Data/Replication/all_pi.csv")


## output filepath ----------------------------------------------
iv.output.salience.results.file <- "Data/Replication/Demand_iv_sat_initial_price_semester_salience.csv"
theta.output.salience.results.file <- "Data/Replication/Demand_theta_sat_initial_price_semester_salience.csv"
pq.output.salience.results.file <- "Data/Replication/Demand_pq_sat_initial_price_semester_salience.csv"
theta.berstein.sal <- "Data/Replication/Demand_gamma_sat_initial_price_semester_salience_K"


#### Part 1. IVs and point identified cases ------

## Options
FE_opts <- c("region_by_module_by_time", "division_by_module_by_time")


LRdiff_res <- data.table(NULL)
target_res <- data.table(NULL)
## Loop across sigma 
for (sig in c(0.25, 0.5, 0.75, 1)) {
  
  outcomes <- c(paste0("w.ln_cpricei2_sig",sig), "w.ln_quantity3")
  ## cut the tails (keep between 1st and 99th percentile, not for sigma =1 since thatw as already done)
  if (sig != 1) {
    pct1 <- quantile(all_pi[[paste0("dm.ln_cpricei2_sig", sig)]], probs = 0.01, na.rm = T, weight=base.sales)
    pct99 <- quantile(all_pi[[paste0("dm.ln_cpricei2_sig", sig)]], probs = 0.99, na.rm = T, weight=base.sales)
    all_pi_est <- all_pi[(get(paste0("dm.ln_cpricei2_sig", sig)) > pct1 & get(paste0("dm.ln_cpricei2_sig", sig)) < pct99),]
  }
  ## Full sample estimates (L=1)
  for (FE in FE_opts) {
    ## Full sample IV
    formula1 <- as.formula(paste0(
      "w.ln_quantity3 ~ 0 | ", FE, " | (w.ln_cpricei2_sig", sig," ~ w.ln_sales_tax) | module_by_state"
    ))
    res1 <- felm(formula = formula1, data = all_pi_est,
                 weights = all_pi_est$base.sales)
    
    
    ## attach results
    res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
    res1.dt[, outcome := "IV"]
    res1.dt[, controls := FE]
    res1.dt[, lev := 1]
    res1.dt[, n.groups := 1]
    res1.dt[, sigma := sig]
    
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, iv.output.salience.results.file)
    ## Full sample passthrough
    formula1 <- as.formula(paste0(
      "w.ln_cpricei2_sig", sig," ~ w.ln_sales_tax | ", FE, " | 0 | module_by_state"
    ))
    res1 <- felm(formula = formula1, data = all_pi_est,
                 weights = all_pi_est$base.sales)
    
    
    ## attach results
    res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
    res1.dt[, outcome := "rho"]
    res1.dt[, controls := FE]
    res1.dt[, lev := 100]
    res1.dt[, n.groups := 1]
    res1.dt[, sigma := sig]
    
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, iv.output.salience.results.file)      
  }
  ## Demand for K=L
  for (n.g in 2:3) {
    # Create groups of initial values of tax rate
    # We use the full weighted distribution
    all_pi_est <- all_pi_est[, quantile := cut(get(paste0("dm.L.ln_cpricei2_sig", sig)),
                                               breaks = quantile(get(paste0("dm.L.ln_cpricei2_sig", sig)), probs = seq(0, 1, by = 1/n.g), na.rm = T, weight = base.sales),
                                               labels = 1:n.g, right = FALSE)]
    quantlab <- round(quantile(all_pi_est[[paste0("dm.L.ln_cpricei2_sig", sig)]], 
                               probs = seq(0, 1, by = 1/n.g), na.rm = T, 
                               weight = all_pi_est$base.sales), digits = 4)
    # Saturate fixed effects
    all_pi_est[, group_region_by_module_by_time := .GRP, by = .(region_by_module_by_time, quantile)]
    all_pi_est[, group_division_by_module_by_time := .GRP, by = .(division_by_module_by_time, quantile)]
    ## Estimate RF and FS
    for (FE in FE_opts) {
      for (Y in outcomes) {
        formula1 <- as.formula(paste0(
          Y, " ~ w.ln_sales_tax:quantile | group_", FE, "+ quantile"
        ))
        res1 <- felm(formula = formula1, data = all_pi_est,
                     weights = all_pi_est$base.sales)
        
        
        ## attach results
        res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
        res1.dt[, outcome := Y]
        res1.dt[, controls := FE]
        res1.dt[, n.groups := n.g]
        res1.dt[, lev := quantlab[-1]]
        res1.dt[, sigma := sig]
        
        LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
        fwrite(LRdiff_res, iv.output.salience.results.file)
      }
      
      ## Estimate IVs and retrieve in vector
      
      IV <- LRdiff_res[outcome == "w.ln_quantity3" & n.groups == n.g & controls == FE & sigma == sig,][["Estimate"]]/LRdiff_res[outcome == paste0("w.ln_cpricei2_sig",sig) & n.groups == n.g & controls == FE & sigma == sig,][["Estimate"]]
      
      ### Estimate the matrix of the implied system of equations
      ## Get the empirical distribution of prices by quantile, weighted properly by base.sales \times 
      # start by creating the weights and normalizing them 
      # Part 1 of weight: (base.sales) weighted variance of de-meaned sales tax within cohort (FE)
      all_pi_est[, wVAR := weighted.mean((w.ln_sales_tax - 
                                            weighted.mean(w.ln_sales_tax, 
                                                          w = base.sales, na.rm = T))^2,
                                         w = base.sales, na.rm = T), by = FE]
      all_pi_est[, wVAR := ifelse(is.na(wVAR), 0, wVAR)]
      # Weight normalized within quantile
      all_pi_est[, base.sales.q := (wVAR*base.sales)/sum(wVAR*base.sales), by = .(quantile)]
      all_pi_est[, p_group := floor((get(paste0("dm.ln_cpricei2_sig", sig)) - min(get(paste0("dm.ln_cpricei2_sig", sig)), na.rm = T))/((max(get(paste0("dm.ln_cpricei2_sig", sig)), na.rm = T)-min(get(paste0("dm.ln_cpricei2_sig", sig)), na.rm = T))/500)), by = .(quantile)]
      all_pi_est[, p_ll := p_group*((max(get(paste0("dm.ln_cpricei2_sig", sig)), na.rm = T)-min(get(paste0("dm.ln_cpricei2_sig", sig)), na.rm = T))/500), by = .(quantile)]
      all_pi_est[, p_ll := p_ll + min(get(paste0("dm.ln_cpricei2_sig", sig)), na.rm = T), by = .(quantile)]
      all_pi_est[, p_ul := p_ll + ((max(get(paste0("dm.ln_cpricei2_sig", sig)), na.rm = T)-min(get(paste0("dm.ln_cpricei2_sig", sig)), na.rm = T))/500), by = .(quantile)]
      
      ed.price.quantile <- all_pi_est[, .(w1 = (sum(base.sales.q))), by = .(p_ul, p_ll, quantile)]
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
      mean.q <- all_pi_est[, mean(ln_quantity3, weights = base.sales)]
      mean.p <- all_pi_est[, mean(get(paste0("dm.ln_cpricei2_sig", sig)), weights = base.sales)]
      beta_0_hat <- mean.q - sum((beta_hat)*(mean.p^(1:n.g)))
      beta_hat <- c(beta_0_hat, beta_hat)
      
      ## Export estimated target parameters
      estimated.target <- data.table(beta_hat)
      estimated.target[, beta_n := .I-1]
      estimated.target[, n.groups := n.g]
      estimated.target[, controls := FE]
      estimated.target[, sigma := sig]
      target_res <- rbind(target_res, estimated.target)
      fwrite(target_res, theta.output.salience.results.file)
      
    } 

  }
  
}

#### Part 2. IVs and partially identified cases: matrices ------



# Functions for bernstein polynomial computation
bernstein <- function(x, k, K){
  choose(K, k) * x^k * (1 - x)^(K - k)
}
int.bernstein <- function(x,k,K) {
  
  result <- 0
  for (j in (k+1):(K+1)) {
    result <- result + bernstein(x,j,K+1)
  }
  return(result/(K+1))
  
}

### Matrices for different types of extrapolation supports
# Only for interest FE
FE <- "group_division_by_module_by_time"

## Loop across sigmas
LRdiff_res <- data.table(NULL)
pq_res <- data.table(NULL)
for (sig in c(0.25, 0.5, 0.75, 1)) {
  
  ## cut the tails (keep between 1st and 99th percentile)
  if (sig != 1) {
    pct1 <- quantile(all_pi[[paste0("dm.ln_cpricei2_sig", sig)]], probs = 0.01, na.rm = T, weight=base.sales)
    pct99 <- quantile(all_pi[[paste0("dm.ln_cpricei2_sig", sig)]], probs = 0.99, na.rm = T, weight=base.sales)
    all_pi_est <- all_pi[(get(paste0("dm.ln_cpricei2_sig", sig)) > pct1 & get(paste0("dm.ln_cpricei2_sig", sig)) < pct99),]
  }
  
  ###### Original Range
  extrap <- "Original"
  ## Normalize
  min.p.or <- min.p <- all_pi_est[, min(get(paste0("dm.ln_cpricei2_sig", sig)))]
  max.p.or <- max.p <- all_pi_est[, max(get(paste0("dm.ln_cpricei2_sig", sig)))]
  all_pi_est[, r.dm.ln_cpricei2 := (get(paste0("dm.ln_cpricei2_sig", sig)) - min.p)/(max.p - min.p)]
  
  ## Export values to re-estimate the intercept
  mean.q <- all_pi_est[, mean(ln_quantity3, weights = base.sales, na.rm = T)]
  mean.p <- all_pi_est[, mean(r.dm.ln_cpricei2, weights = base.sales, na.rm = T)]
  
  estimated.pq <- data.table(mean.q, mean.p, min.p, max.p, sigma = sig, extrap)
  pq_res <- rbind(pq_res, estimated.pq)
  fwrite(pq_res, pq.output.salience.results.file)
  
  for (n.g in 1:3) {
    
    
    # Create groups of initial values of tax rate
    # We use the full weighted distribution
    all_pi_est <- all_pi_est[, quantile := cut(get(paste0("dm.L.ln_cpricei2_sig", sig)),
                                               breaks = quantile(get(paste0("dm.L.ln_cpricei2_sig", sig)), probs = seq(0, 1, by = 1/n.g), na.rm = T, weight = base.sales),
                                               labels = 1:n.g, right = FALSE)]
    quantlab <- round(quantile(all_pi_est[[paste0("dm.L.ln_cpricei2_sig", sig)]], 
                               probs = seq(0, 1, by = 1/n.g), na.rm = T, 
                               weight = all_pi_est$base.sales), digits = 4)
    
    # Saturate fixed effects
    all_pi_est[, group_region_by_module_by_time := .GRP, by = .(region_by_module_by_time, quantile)]
    all_pi_est[, group_division_by_module_by_time := .GRP, by = .(division_by_module_by_time, quantile)]

    ## Do partial identification
    ### Estimate the matrix of the implied system of equations. For each possible polynomial degree and compute 
    ## Get the empirical distribution of prices by quantile, weighted properly by base.sales \times 
    # start by creating the weights and normalizing them 
    # Part 1 of weight: (base.sales) weighted variance of de-meaned sales tax within cohort (FE)
    all_pi_est[, wVAR := weighted.mean((w.ln_sales_tax - 
                                          weighted.mean(w.ln_sales_tax, 
                                                        w = base.sales, na.rm = T))^2,
                                       w = base.sales, na.rm = T), by = FE]
    all_pi_est[, wVAR := ifelse(is.na(wVAR), 0, wVAR)]
    # Weight normalized within quantile
    all_pi_est[, base.sales.q := (wVAR*base.sales)/sum(wVAR*base.sales), by = .(quantile)]
    all_pi_est[, p_group := floor((r.dm.ln_cpricei2 - min(r.dm.ln_cpricei2, na.rm = T))/((max(r.dm.ln_cpricei2, na.rm = T)-min(r.dm.ln_cpricei2, na.rm = T))/100)), by = .(quantile)]
    all_pi_est[, p_ll := p_group*((max(r.dm.ln_cpricei2, na.rm = T)-min(r.dm.ln_cpricei2, na.rm = T))/100), by = .(quantile)]
    all_pi_est[, p_ll := p_ll + min(r.dm.ln_cpricei2, na.rm = T), by = .(quantile)]
    all_pi_est[, p_ul := p_ll + ((max(r.dm.ln_cpricei2, na.rm = T)-min(r.dm.ln_cpricei2, na.rm = T))/100), by = .(quantile)]
    
    ed.price.quantile <- all_pi_est[, .(w1 = (sum(base.sales.q))), by = .(p_ul, p_ll, quantile)]
    ed.price.quantile[, p_m := (p_ul+p_ll)/2]
    
    #### Matrices of Polynomials for Elasticity: elasticity is itself a bernstein Polynomial
    for (K in (n.g):10) {
      
      if (K>1){
        # Create the derivative of the polynomial of prices and multiplicate by weights
        for (n in 0:(K-1)){
          ed.price.quantile[, paste0("b",n) := w1*(bernstein(p_m,n,K-1))]
        }
        
        # Calculate integral
        gamma <- ed.price.quantile[ , lapply(.SD, sum), by = .(quantile), .SDcols = paste0("b",0:(K-1))]
        gamma <- gamma[!is.na(quantile),][order(quantile)][, -c("quantile")]
        
        # Export Calculation
        gamma[, n.groups := n.g]
        gamma[, sigma := sig]
        gamma[, extrap := "Original"]
        
        ## Read Previous and write
        theta.output.results.file <- paste0(theta.berstein.sal, K,"_bern.csv")
        
        if (n.g == 1 & sig == 0.25) {
          fwrite(gamma, theta.output.results.file)
        } else {
          previous.data <- fread(theta.output.results.file)
          previous.data <- rbind(previous.data, gamma)
          fwrite(previous.data, theta.output.results.file)
        }
      }
    }
  }
  
  
  ##### No tax Case
  extrap <- "No Tax"
  all_pi_est[, ex_p := get(paste0("dm.ln_cpricei2_sig", sig)) - ln_sales_tax]
  
  ## Define re-scaled prices to use Bernstein polynomials in that range
  min.p <- min(all_pi_est[, min(ex_p)], min.p.or)
  max.p <- max(all_pi_est[, max(ex_p)], max.p.or)
  ## Normalize
  all_pi_est[, r.dm.ln_cpricei2 := (get(paste0("dm.ln_cpricei2_sig", sig)) - min.p)/(max.p - min.p)]
  
  ## Export values to re-estimate the intercept
  mean.q <- all_pi_est[, mean(ln_quantity3, weights = base.sales, na.rm = T)]
  mean.p <- all_pi_est[, mean(r.dm.ln_cpricei2, weights = base.sales, na.rm = T)]
  
  estimated.pq <- data.table(mean.q, mean.p, min.p, max.p, sigma = sig, extrap)
  pq_res <- rbind(pq_res, estimated.pq)
  fwrite(pq_res, pq.output.salience.results.file)
  
  for (n.g in 1:3) {
    
    # Create groups of initial values of tax rate
    # We use the full weighted distribution
    all_pi_est <- all_pi_est[, quantile := cut(get(paste0("dm.L.ln_cpricei2_sig", sig)),
                                               breaks = quantile(get(paste0("dm.L.ln_cpricei2_sig", sig)), probs = seq(0, 1, by = 1/n.g), na.rm = T, weight = base.sales),
                                               labels = 1:n.g, right = FALSE)]
    quantlab <- round(quantile(all_pi_est[[paste0("dm.L.ln_cpricei2_sig", sig)]], 
                               probs = seq(0, 1, by = 1/n.g), na.rm = T, 
                               weight = all_pi_est$base.sales), digits = 4)
    
    # Saturate fixed effects
    all_pi_est[, group_region_by_module_by_time := .GRP, by = .(region_by_module_by_time, quantile)]
    all_pi_est[, group_division_by_module_by_time := .GRP, by = .(division_by_module_by_time, quantile)]
    
    ## Do partial identification
    ## Estimate the matrix of the implied system of equations. For each possible polynomial degree and compute 
    ## Get the empirical distribution of prices by quantile, weighted properly by base.sales \times 
    # start by creating the weights and normalizing them 
    # Part 1 of weight: (base.sales) weighted variance of de-meaned sales tax within cohort (FE)
    all_pi_est[, wVAR := weighted.mean((w.ln_sales_tax - 
                                          weighted.mean(w.ln_sales_tax, 
                                                        w = base.sales, na.rm = T))^2,
                                       w = base.sales, na.rm = T), by = FE]
    all_pi_est[, wVAR := ifelse(is.na(wVAR), 0, wVAR)]
    # Weight normalized within quantile
    all_pi_est[, base.sales.q := (wVAR*base.sales)/sum(wVAR*base.sales), by = .(quantile)]
    all_pi_est[, p_group := floor((r.dm.ln_cpricei2 - min(r.dm.ln_cpricei2, na.rm = T))/((max(r.dm.ln_cpricei2, na.rm = T)-min(r.dm.ln_cpricei2, na.rm = T))/100)), by = .(quantile)]
    all_pi_est[, p_ll := p_group*((max(r.dm.ln_cpricei2, na.rm = T)-min(r.dm.ln_cpricei2, na.rm = T))/100), by = .(quantile)]
    all_pi_est[, p_ll := p_ll + min(r.dm.ln_cpricei2, na.rm = T), by = .(quantile)]
    all_pi_est[, p_ul := p_ll + ((max(r.dm.ln_cpricei2, na.rm = T)-min(r.dm.ln_cpricei2, na.rm = T))/100), by = .(quantile)]
    
    ed.price.quantile <- all_pi_est[, .(w1 = (sum(base.sales.q))), by = .(p_ul, p_ll, quantile)]
    ed.price.quantile[, p_m := (p_ul+p_ll)/2]
    
    #### Matrices of Polynomials for Elasticity: elasticity is itself a bernstein Polynomial
    for (K in (n.g):10) {
      
      if (K>1){
        # Create the derivative of the polynomial of prices and multiplicate by weights
        for (n in 0:(K-1)){
          ed.price.quantile[, paste0("b",n) := w1*(bernstein(p_m,n,K-1))]
        }
        
        # Calculate integral
        gamma <- ed.price.quantile[ , lapply(.SD, sum), by = .(quantile), .SDcols = paste0("b",0:(K-1))]
        gamma <- gamma[!is.na(quantile),][order(quantile)][, -c("quantile")]
        
        # Export Calculation
        gamma[, n.groups := n.g]
        gamma[, sigma := sig]
        gamma[, extrap := "No Tax"]
        
        ## Read Previous and write
        theta.output.results.file <- paste0(theta.berstein.sal, K,"_bern.csv")
        previous.data <- fread(theta.output.results.file)
        previous.data <- rbind(previous.data, gamma)
        fwrite(previous.data, theta.output.results.file)
      }
    }
  }
  
  
  ##### Plus 5 range case
  extrap <- "plus 5 Tax"
  all_pi_est[, ex_p := get(paste0("dm.ln_cpricei2_sig", sig)) + log(1+0.05)]
  
  ## Define re-scaled prices to use Bernstein polynomials in that range
  min.p <- min(all_pi_est[, min(ex_p)], min.p.or)
  max.p <- max(all_pi_est[, max(ex_p)], max.p.or)
  ## Normalize
  all_pi_est[, r.dm.ln_cpricei2 := (get(paste0("dm.ln_cpricei2_sig", sig)) - min.p)/(max.p - min.p)]
  
  ## Export values to re-estimate the intercept
  mean.q <- all_pi_est[, mean(ln_quantity3, weights = base.sales, na.rm = T)]
  mean.p <- all_pi_est[, mean(r.dm.ln_cpricei2, weights = base.sales, na.rm = T)]
  
  estimated.pq <- data.table(mean.q, mean.p, min.p, max.p, sigma = sig, extrap)
  pq_res <- rbind(pq_res, estimated.pq)
  fwrite(pq_res, pq.output.salience.results.file)
  
  for (n.g in 1:3) {
    
    
    # Create groups of initial values of tax rate
    # We use the full weighted distribution
    all_pi_est <- all_pi_est[, quantile := cut(get(paste0("dm.L.ln_cpricei2_sig", sig)),
                                               breaks = quantile(get(paste0("dm.L.ln_cpricei2_sig", sig)), probs = seq(0, 1, by = 1/n.g), na.rm = T, weight = base.sales),
                                               labels = 1:n.g, right = FALSE)]
    quantlab <- round(quantile(all_pi_est[[paste0("dm.L.ln_cpricei2_sig", sig)]], 
                               probs = seq(0, 1, by = 1/n.g), na.rm = T, 
                               weight = all_pi_est$base.sales), digits = 4)
    
    # Saturate fixed effects
    all_pi_est[, group_region_by_module_by_time := .GRP, by = .(region_by_module_by_time, quantile)]
    all_pi_est[, group_division_by_module_by_time := .GRP, by = .(division_by_module_by_time, quantile)]
    
    
    ## Do partial identification
    ## Estimate the matrix of the implied system of equations. For each possible polynomial degree and compute 
    ## Get the empirical distribution of prices by quantile, weighted properly by base.sales \times 
    # start by creating the weights and normalizing them 
    # Part 1 of weight: (base.sales) weighted variance of de-meaned sales tax within cohort (FE)
    all_pi_est[, wVAR := weighted.mean((w.ln_sales_tax - 
                                          weighted.mean(w.ln_sales_tax, 
                                                        w = base.sales, na.rm = T))^2,
                                       w = base.sales, na.rm = T), by = FE]
    all_pi_est[, wVAR := ifelse(is.na(wVAR), 0, wVAR)]
    # Weight normalized within quantile
    all_pi_est[, base.sales.q := (wVAR*base.sales)/sum(wVAR*base.sales), by = .(quantile)]
    all_pi_est[, p_group := floor((r.dm.ln_cpricei2 - min(r.dm.ln_cpricei2, na.rm = T))/((max(r.dm.ln_cpricei2, na.rm = T)-min(r.dm.ln_cpricei2, na.rm = T))/100)), by = .(quantile)]
    all_pi_est[, p_ll := p_group*((max(r.dm.ln_cpricei2, na.rm = T)-min(r.dm.ln_cpricei2, na.rm = T))/100), by = .(quantile)]
    all_pi_est[, p_ll := p_ll + min(r.dm.ln_cpricei2, na.rm = T), by = .(quantile)]
    all_pi_est[, p_ul := p_ll + ((max(r.dm.ln_cpricei2, na.rm = T)-min(r.dm.ln_cpricei2, na.rm = T))/100), by = .(quantile)]
    
    ed.price.quantile <- all_pi_est[, .(w1 = (sum(base.sales.q))), by = .(p_ul, p_ll, quantile)]
    ed.price.quantile[, p_m := (p_ul+p_ll)/2]
    
    #### Matrices of Polynomials for Elasticity: elasticity is itself a bernstein Polynomial
    for (K in (n.g):10) {
      
      if (K>1){
        # Create the derivative of the polynomial of prices and multiplicate by weights
        for (n in 0:(K-1)){
          ed.price.quantile[, paste0("b",n) := w1*(bernstein(p_m,n,K-1))]
        }
        
        # Calculate integral
        gamma <- ed.price.quantile[ , lapply(.SD, sum), by = .(quantile), .SDcols = paste0("b",0:(K-1))]
        gamma <- gamma[!is.na(quantile),][order(quantile)][, -c("quantile")]
        
        # Export Calculation
        gamma[, n.groups := n.g]
        gamma[, sigma := sig]
        gamma[, extrap := "plus 5 Tax"]
        
        ## Read Previous and write
        theta.output.results.file <- paste0(theta.berstein.sal, K,"_bern.csv")
        previous.data <- fread(theta.output.results.file)
        previous.data <- rbind(previous.data, gamma)
        fwrite(previous.data, theta.output.results.file)
        
      }
    }
  }
  
}


#### Part 3.1 Partial identification estimation setup ------

### Curently Stops here. We never ran this
quit(save="no")


## 0. Used functions 

## Function to obtain partially identified estimates for a given iteration. 
# The goal is to use this function in the multi core lapply 
obtain.bounds <- function(ests, prices, params, noise = F) {
  #' @param ests must be a list with $gamma, $beta, $desclist and  as components
  #' each $gamma and $beta are lists too: 
  #' Each list contains data.frames, for a given value of L (beta) or K (gamma)
  #' the data frame of the beta must contain 2 columns: L (with the value), beta (ordered)
  #' $desclist is also a list, but that only contains vectors with needed values
  #' 
  #' @param prices is a vector with the values of prices to compute extrapolated values
  #' @param params is a list containing options to pass to gurobi
  
  # unpack list of descriptives
  p.bar <- ests$desclist$p.bar
  q.bar <- ests$desclist$q.bar
  p.min <- ests$desclist$p.min
  p.max <- ests$desclist$p.max
  sigma <- ests$desclist$sig
  extrap <- ests$desclist$ex

  # Capture list of betas
  beta.list <- ests$beta
  
  futile.logger::flog.info("Solving for exercise %s with sigma = %s...", extrap, sigma)
  
  # create results files
  elasticity <- data.table(NULL)
  # Loop over K
  for (dat.k in ests$gamma) {
    
    # Capture value of K
    K <- unique(dat.k$K)
    
    
    # Loop over L
    for (L in unique(dat.k$n.groups)) {
      
      beta.data <- beta.list[[L]]
      # Capture beta 
      beta <- beta.data$beta
      
      ## A1. Build the constraints matrix 
      constr <- as.matrix(dat.k[n.groups == L][, -c("n.groups", "K")])   ## For elasticity
      constr.dd <- cbind(constr, 
                         matrix(0, nrow = nrow(constr), ncol = 1)
      )                                  ## For demand
      
      ## A2. Build RHS
      RHS <- beta   
      
      ## A3. Set monotonicity of bernstein polynomials. Elasticity < 0 
      constr.mono <- Diagonal(ncol(constr))              ## For elasticity
      constr.mono.dd <- cbind(constr.mono,
                              matrix(0, nrow = nrow(constr.mono), ncol = 1)
      )             ## For demand
      RHS.mono <- rep(0, K)
      
      ## A4. Get intercept constraint. Demand
      constr.inter <- rep(0, K)
      for (i in 0:(K-1)) {
        constr.inter[i+1] <- -int.bernstein(p.bar,i,K-1)
      }
      constr.inter <- t(as.matrix(c(constr.inter,1))) ## Add the intercept and transform to matrix
      RHS.inter <- q.bar
      
      
      ## A4. If L > 1 we have to estimate the minimum criterion: min sum_s abs(gamma_s(theta) - beta_s)  
      # To do this I define a set of auxiliar variables a_s such that: 
      # a_s + gamma_s >= beta_s and a_s - gamma_s(theta) >= - beta_s
      # And I minimize over them - thetas are now 0s in the objective function:
      # obj is sum_s (1*a_s) + 0 *theta
      # Shape constraints still hold
      if (L > 1) {
        
        if (noise) print(paste0("Solving min.criteria for K=", K, ", L=", L))
        ## Define the problem
        min.crit <- list() 
        min.crit$A <- rbind(cbind(Diagonal(nrow(constr)), constr), 
                            cbind(Diagonal(nrow(constr)), -constr),
                            cbind(matrix(0, nrow = nrow(constr.mono), ncol = nrow(constr)), 
                                  constr.mono)
        )
        min.crit$rhs <- c(RHS, -RHS, RHS.mono)
        min.crit$sense <- c( rep('>=', 2*length(RHS)), rep('<=',K))
        min.crit$obj <- c(rep(1, nrow(constr)), rep(0, ncol(constr)))
        min.crit$lb <- c(rep(0, nrow(constr)), rep(-Inf, ncol(constr)))  
        min.crit$modelsense <- 'min'
        
        paramsminc <- list()
        if (!is.null(params$OutputFlag)) paramsminc$OutputFlag <- params$OutputFlag
        
        ## Solve for the minimum criteria
        min.crit.sol <- gurobi(min.crit, paramsminc)
        
        ## Get the minimum criterion estimated and modify the setting of the problem
        min.criteria <- min.crit.sol$objval
        tuning <- min.criteria*(1 + tolerance)
        
        if (noise) print(paste0("Min crit. succesful for K=",K, ", L=",L))
        
      }
      else min.criteria <- 0
      
      ## A5. Start loop at a given price
      if (noise) print(paste0("Starting loop over p for K=", K, " L=", L))
      for (p in prices) {
        
        ## B0. Normalize price
        n.p <- (p - round(p.min, 3))/(round(p.max, 3) - round(p.min, 3))
        
        ## B1. Specify objective function. Elasticity at p
        objec <- rep(0, K)
        for (i in 0:(K-1)) {
          objec[i+1] <- bernstein(n.p,i,K-1)
          if (is.nan(objec[i+1])) {objec[i+1] <- 0}
        }
        
        ## B2. Set-Up LP with all the inputs created. Elasticity
        model <- list()                                        ## Create
        model$A <- rbind(constr, constr.mono)                  ## Constraints
        model$rhs <- c(RHS, RHS.mono)                          ## RHS
        model$sense <- c(rep('=', length(RHS)), rep('<=',K))   ## Equalities
        model$obj <- objec                                     ## Objective function
        model$lb <- rep(-Inf, length(objec))                   ## Let theta be negative
        
        
        ## B2.A. If L > 1 we have to modify the problem to allow for the inequalities up to the estimated tuning parameter
        if (L > 1) {
          
          model$A <- rbind(constr, constr, constr.mono)                                  ## Constraints
          model$rhs <- c(c(RHS + tuning), c(RHS - tuning), RHS.mono)                     ## RHS
          model$sense <- c(rep('<=', length(RHS)), rep('>=', length(RHS)), rep('<=',K))  ## Equalities
        }
        
        ## B3. Upper bound. Elasticity
        model$modelsense <- 'max'
        result <- gurobi(model, params)
        elas.up <- result$objval
        theta.up <- result$x
        if(is.null(elas.up) | is_empty(elas.up)) {elas.up <- NA}
        
        
        ## B4. Lower bound. Elasticity
        model$modelsense <- 'min'
        result <- gurobi(model, params)
        elas.down <- result$objval
        theta.down <- result$x 
        if(is.null(elas.down) | is_empty(elas.down)) {elas.down <- NA}
        
        
        ## B5. Specify objective function. Demand at p
        objec <- rep(0, K)
        for (i in 0:(K-1)) {
          objec[i+1] <- int.bernstein(n.p,i,K-1)
          if (is.nan(objec[i+1])) {objec[i+1] <- 0}
        }
        objec <- c(objec, 1) ## Add the intercept
        
        
        ## B6. Set-Up LP with all the inputs created. Demand
        model <- list()                                            ## Create
        model$A <- rbind(constr.dd, constr.mono.dd, constr.inter)  ## Constraints
        model$rhs <- c(RHS, RHS.mono, RHS.inter)                   ## RHS
        model$sense <- c(rep('=', length(RHS)), rep('<=',K), '=')  ## Equalities
        model$obj <- objec                                         ## Objective function
        model$lb <- rep(-Inf, length(objec))                       ## Let theta be negative
        
        
        ## B6.A. If L > 1 we have to modify the problem to allow for the inequalities up to the estimated tuning parameter
        if (L > 1) {
          
          model$A <- rbind(constr.dd, constr.dd, constr.mono.dd, constr.inter)                   ## Constraints
          model$rhs <- c(c(RHS + tuning), c(RHS - tuning), RHS.mono, RHS.inter)               ## RHS
          model$sense <- c(rep('<=', length(RHS)), rep('>=', length(RHS)), rep('<=',K), '=')  ## Equalities
          
        }
        
        ## B3. Upper bound. Demand
        model$modelsense <- 'max'
        result <- gurobi(model, params)
        dd.up <- result$objval
        theta.up <- result$x
        if(is.null(dd.up) | is_empty(dd.up)) {dd.up <- NA}
        
        
        ## B4. Lower bound. Demand
        model$modelsense <- 'min'
        result <- gurobi(model, params)
        dd.down <- result$objval
        theta.down <- result$x 
        if(is.null(dd.down) | is_empty(dd.down)) {dd.down <- NA}
        
        ## B5. Save. Elasticity bounds
        elasticity.p <- data.table(elas.down, elas.up, 
                                   dd.down, dd.up, 
                                   p, L, K, 
                                   min.criteria, iter, sigma, extrap)
        elasticity <- rbind(elasticity, elasticity.p)
        
      }
      
      if (noise) print(paste0("Bounds succesful for K=",K, ", L=",L, ", at all p"))
      
    }
  }
  futile.logger::flog.info("Exercise %s with sigma = %s, results sucessfull", extrap, sigma)

  return(elasticity)
}


## 1. Input and output files
# inputs (saved above)
# output
partial.results.file.salience <- "Data/Replication/partial_point_results_salience.csv"
## 2. Set up Optimization Parameters
# These options will make Gurobi think more about numerical issues
params <- list()
params$NumericFocus <- 3
params$ScaleFlag <- 2
params$Method <- 1
params$Presolve <- 0
params$OutputFlag <- 0

## 3. Set up Tolerance 
tolerance <- 1e-6
params$FeasibilityTol <- tolerance

## 4. range of p to bound elasticity
prices <- seq(-.25, .25, 0.001)

## 5. Load inputs used across iterations
# Load betas
res.ivs.all <- fread(iv.output.salience.results.file)
# Load p and qs
res.pq.all <- fread(pq.output.salience.results.file)


## 2. Capture elements across values of sigma and organize them 
all.iters <- list()
run <- 1
for (sig in c(0.25, 0.5, 0.75, 1)) {
  for (exer in c("plus 5 Tax", "No Tax", "plus 5")) {
    
    gamma <- list()
    beta <- list()
    desclist <- list()
    
    ## 1. Load average p and q's 
    res.pq <- res.pq.all[sigma == rep & extrap == exer]
    desclist$p.bar <- res.pq[["mean.p"]]
    desclist$q.bar <- res.pq[["mean.q"]]
    desclist$p.min <- res.pq[["min.p"]]
    desclist$p.max <- res.pq[["max.p"]]
    desclist$sig <- sig
    desclist$ex <- exer
    
    ## 2. Set up betas
    # 2.1 Keep iterest results
    res.ivs <- res.ivs.all[iter == rep & extrap == exer]
    # 2.2  dcast outcomes
    res.ivs <- dcast(res.ivs, n.groups + lev ~ outcome,  fun=sum, value.var = c("Estimate"))
    # 2.3 Calculate IV
    res.ivs[, estimate := w.ln_quantity3/w.ln_cpricei2]
    # 2.4 Order appropiately
    res.ivs <- res.ivs[order(n.groups, lev)]
    # 2.5 Save in list of betas
    for (L in unique(res.ivs$n.groups)) {
      
      # Keep relevant data
      data.L <- res.ivs[n.groups == L, c("estimate", "n.groups")]
      # Format
      setnames(data.L, c("estimate", "n.groups"), c("beta", "L"))
      # save in list
      beta[[L]] <- data.L
    }
    
    ## 3. Set up gammas
    for (K in 2:10) {
      
      ## 3.1. Load Matrix of gamma (this extrictly depends on K since the basis change)
      in.file <- paste0(theta.berstein.sal, K,"_bern.csv")
      gamma.full.data <- fread(in.file)
      
      ## 3.2 Restrict gamma file. Constant across p
      gamma.K <- gamma.full.data[iter == rep & extrap == exer][, c(paste0("b", 0:(K-1)), "n.groups"), with = F]             ## For elasticity
      gamma.K[, "K" := K]
      
      ## save in list
      gamma[[K-1]] <- gamma.K
      
    }
    
    ## Part 4. Save together in iteration and then into all iterations results
    iter <- list(gamma = gamma,
                 beta = beta,
                 desclist = desclist)
    all.iters[[run]] <- iter
    run <- run + 1
  }
}


## 3. Run estimation
res.l <- sapply(all.iters, FUN = obtain.bounds, 
                prices = prices, params = params, 
                simplify = F, noise = F)

print(res.l)
# rbind results and save them
results = data.table::rbindlist(res.l, fill = T)
fwrite(results, partial.results.file.salience)

