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
  ## cut the tails (keep between 1st and 99th percentile)
  pct1 <- quantile(all_pi[[paste0("dm.ln_cpricei2_sig", sig)]], probs = 0.01, na.rm = T, weight=base.sales)
  pct99 <- quantile(all_pi[[paste0("dm.ln_cpricei2_sig", sig)]], probs = 0.99, na.rm = T, weight=base.sales)
  all_pi_est <- all_pi[(get(paste0("dm.ln_cpricei2_sig", sig)) > pct1 & get(paste0("dm.ln_cpricei2_sig", sig)) < pct99),]
  
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
                                               labels = 1:2, right = FALSE)]
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
      
      IV <- LRdiff_res[outcome == "w.ln_quantity3" & n.groups == 2 & controls == FE & sigma == sig,][["Estimate"]]/LRdiff_res[outcome == paste0("w.ln_cpricei2_sig",sig) & n.groups == 2 & controls == FE & sigma == sig,][["Estimate"]]
      
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

#### Part 2. IVs and partially identified cases ------

### Matrices for different types of extrapolation supports
# Only for interest FE
FE <- "group_by_division_by_module_by_time"

## Loop across sigmas
LRdiff_res <- data.table(NULL)
pq_res <- data.table(NULL)
for (sig in c(0.25, 0.5, 0.75, 1)) {
  
  ## cut the tails (keep between 1st and 99th percentile)
  pct1 <- quantile(all_pi[[paste0("dm.ln_cpricei2_sig", sig)]], probs = 0.01, na.rm = T, weight=base.sales)
  pct99 <- quantile(all_pi[[paste0("dm.ln_cpricei2_sig", sig)]], probs = 0.99, na.rm = T, weight=base.sales)
  all_pi_est <- all_pi[(get(paste0("dm.ln_cpricei2_sig", sig)) > pct1 & get(paste0("dm.ln_cpricei2_sig", sig)) < pct99),]
  
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

