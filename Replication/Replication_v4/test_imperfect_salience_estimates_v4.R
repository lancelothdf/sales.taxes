##### Santiago Lacouture
#' Sales Taxes
#' Replication File. Updated on 04/20/2023
#' Step 10a: Repeat estimation for point identification but with demand under imperfect salience
#' This version includes bootstrapping

library(data.table)
library(futile.logger)
library(lfe)
library(Matrix)
library(zoo)
library(stringr)

setwd("/project/igaarder")
rm(list = ls())

## input filepath ----------------------------------------------
all_pi <- fread("Data/Replication_v4/all_pi_salience.csv")
# restrict to relevant sample
all_pi <- all_pi[non_imp_tax_strong == 1,]

## output filepath ----------------------------------------------
iv.output.salience.results.file <- "Data/Replication_v4/Demand_iv_sat_initial_price_semester_salience.csv"
theta.output.salience.results.file <- "Data/Replication_v4/Demand_theta_sat_initial_price_semester_salience.csv"


### cut tails
for (sig in seq(0.25, 1, 0.05)) {
  
  ## cut the tails (keep between 1st and 99th percentile, not for sigma =1 since that was already done)
  if (sig != 1) {
    pct1 <- quantile(all_pi[[paste0("dm.ln_cpricei2_sig", sig)]], probs = 0.01, na.rm = T, weight=base.sales)
    pct99 <- quantile(all_pi[[paste0("dm.ln_cpricei2_sig", sig)]], probs = 0.99, na.rm = T, weight=base.sales)
    all_pi[, paste0("sample_", sig*100) := (get(paste0("dm.ln_cpricei2_sig", sig)) > pct1 & get(paste0("dm.ln_cpricei2_sig", sig)) < pct99)]
  }
  else all_pi[, sample_100 := 1]
}

#### Part 1. IVs and point identified cases ------


set.seed(2019)

LRdiff_res <- data.table(NULL)
target_res <- data.table(NULL)
## Loop across sigma 
for (rep in  0:100) {
  
  flog.info("Iteration %s", rep)
  if (rep == 0) sigmas.test <- seq(0.25,1,0.05)
  else sigmas.test <- c(0.25, 0.5, 0.75, 1)
  for (sig in sigmas.test) {
    
    flog.info("Estimating for Sigma = %s", sig)
    ## cut the tails (keep between 1st and 99th percentile, not for sigma =1 since that was already done)
    print(paste0("Number of obs in sample is ", nrow(all_pi)))
    all_pi_est <- all_pi[ get(paste0("sample_", sig*100)) == 1, ]
    print(paste0("Number of obs in sample is ", nrow(all_pi_est), "after restriction"))
    
    if (rep > 0) {
      # Sample by block
      ids <- unique(all_pi_est$module_by_state)
      
      sampled.ids <- data.table(sample(ids, replace = T))
      setnames(sampled.ids, old= "V1", new = "module_by_state")
      
      # Merge data to actual data
      all_pi_est <- merge(sampled.ids, all_pi_est, by = c("module_by_state") , allow.cartesian = T, all.x = T)
      ## FE Options (fewer for boot)
      FE_opts <- c("region_by_module_by_time", "division_by_module_by_time") ## May have to modify this
      n.groups.max <- 2
    }
    else {
      ## FE Options
      FE_opts <- c("region_by_module_by_time", "division_by_module_by_time")
      n.groups.max <- 3
      
    }
    outcomes <- c(paste0("DL.ln_cpricei2_sig",sig), "DL.ln_quantity3")
    

    ## Full sample estimates (L=1)
    for (FE in FE_opts) {
      for (Y in outcomes) {
        ## Full sample passthrough
        formula1 <- as.formula(paste0(
          Y," ~ DL.ln_sales_tax | ", FE
        ))
        res1 <- felm(formula = formula1, data = all_pi_est,
                     weights = all_pi_est$base.sales)
        
        
        ## attach results
        res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
        res1.dt[, outcome := Y]
        res1.dt[, controls := FE]
        res1.dt[, lev := 100]
        res1.dt[, n.groups := 1]
        res1.dt[, sigma := sig]
        res1.dt[, iter := rep]
        
        LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
        fwrite(LRdiff_res, iv.output.salience.results.file)      
        
      }
    }
    ## Demand for K=L>1
    for (n.g in 2:n.groups.max) {
      flog.info("...Estimating K=L=%s cases", n.g)
      # Create groups of initial values of tax rate
      # We use the full weighted distribution
      all_pi_est <- all_pi_est[, quantile := cut(get(paste0("dm.L.ln_cpricei2_sig", sig)),
                                                 breaks = quantile(get(paste0("dm.L.ln_cpricei2_sig", sig)), 
                                                                   probs = seq(0, 1, by = 1/n.g), na.rm = T, weight = base.sales),
                                                 labels = 1:n.g, right = FALSE)]
      quantlab <- round(quantile(all_pi_est[[paste0("dm.L.ln_cpricei2_sig", sig)]], 
                                 probs = seq(0, 1, by = 1/n.g), na.rm = T, 
                                 weight = all_pi_est$base.sales), digits = 4)
      # Saturate fixed effects
      all_pi_est[, group_region_by_module_by_time := .GRP, by = .(region_by_module_by_time, quantile)]
      all_pi_est[, group_division_by_module_by_time := .GRP, by = .(division_by_module_by_time, quantile)]
      ## Estimate RF and FS
      for (FE in FE_opts) {
        FEg <- paste0("group_", FE)
        for (Y in outcomes) {
          formula1 <- as.formula(paste0(
            Y, " ~ DL.ln_sales_tax:quantile | group_", FE, "+ quantile"
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
          res1.dt[, iter := rep]
          
          LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
          fwrite(LRdiff_res, iv.output.salience.results.file)
        }
        
        ## Estimate IVs and retrieve in vector
        
        IV <- LRdiff_res[outcome == "DL.ln_quantity3" & n.groups == n.g & controls == FE & sigma == sig & iter == rep,][["Estimate"]]/LRdiff_res[outcome == paste0("DL.ln_cpricei2_sig",sig) & n.groups == n.g & controls == FE & sigma == sig & iter == rep,][["Estimate"]]
        
        ### Estimate the matrix of the implied system of equations
        ## Get the empirical distribution of prices by quantile, weighted properly by base.sales \times 
        # start by creating the weights and normalizing them 
        # Part 1 of weight: (base.sales) weighted variance of de-meaned sales tax within cohort (FE)
        all_pi_est[, wVAR := weighted.mean((DL.ln_sales_tax - 
                                              weighted.mean(DL.ln_sales_tax, 
                                                            w = base.sales, na.rm = T))^2,
                                           w = base.sales, na.rm = T), by = FEg]
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
        estimated.target[, iter := rep]
        target_res <- rbind(target_res, estimated.target)
        fwrite(target_res, theta.output.salience.results.file)
        
      } 
      
    }
    
  }
}
