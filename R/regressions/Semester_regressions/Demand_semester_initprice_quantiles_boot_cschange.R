#' Sales Taxes Project
#' This code estimates the demand using the proposed method. First, we 
#' run a Basic DiD model by initial price level and estimate the "long run" models 
#' splitting the sample by quantiles increasing the number of groups.
#' Here initial level means previous period and we divide by groups within the "common" support
#' In this case, we run a fully saturated model (instead of splitting the sample)
#' We get the Implied IV and recover the implied demand function varying the degree estimated (no. of quantiles)
#' Bootstrap to get CIs. We get the non demeaned q estimated of the intercept


library(data.table)
library(futile.logger)
library(lfe)
library(multcomp)


setwd("/project2/igaarder")


## input filepaths -----------------------------------------------
#' This data is the same as all_goods_pi_path, except it has 2015-2016 data as well.
data.semester <- "Data/Nielsen/semester_nielsen_data.csv"
data.year <- "Data/Nielsen/yearly_nielsen_data.csv"

## output filepaths ----------------------------------------------
iv.output.results.file <- "Data/Demand_iv_sat_initial_price_semester_boot_r_cschange.csv"
theta.output.results.file <- "Data/Demand_theta_sat_initial_price_semester_boot_r_cschange.csv"
cs.output.results.file <- "Data/Demand_conssur_sat_initial_price_semester_boot_r_cschange.csv"

## Function needed for the consumer surplus calculations ---------
integrand <- function(p, theta) {
  f <- exp(p)
  for (i in 1:length(theta)) {
    f <- f*exp(theta[i]*(p)^(i-1))
  }
  return(f)
}

### Set up Semester Data ---------------------------------
all_pi <- fread(data.semester)
all_pi[, w.ln_sales_tax := ln_sales_tax - mean(ln_sales_tax), by = .(store_by_module)]
all_pi[, w.ln_cpricei2 := ln_cpricei2 - mean(ln_cpricei2), by = .(store_by_module)]
all_pi[, w.ln_quantity3 := ln_quantity3 - mean(ln_quantity3), by = .(store_by_module)]

# Need to demean
all_pi[, module_by_time := .GRP, by = .(product_module_code, semester, year)]
all_pi[, L.ln_cpricei2 := ln_cpricei2 - D.ln_cpricei2]
all_pi[, dm.L.ln_cpricei2 := L.ln_cpricei2 - mean(L.ln_cpricei2, na.rm = T), by = module_by_time]
all_pi[, dm.ln_cpricei2 := ln_cpricei2 - mean(ln_cpricei2, na.rm = T), by = module_by_time]
all_pi[, dm.ln_quantity3 := ln_quantity3 - mean(ln_quantity3, na.rm = T), by = module_by_time]


# Defining common support
control <- all_pi[D.ln_sales_tax == 0,]
treated <- all_pi[D.ln_sales_tax != 0,]

# Price 
pct1.control <- quantile(control$dm.L.ln_cpricei2, probs = 0.01, na.rm = T, weight=control$base.sales)
pct1.treated <- quantile(treated$dm.L.ln_cpricei2, probs = 0.01, na.rm = T, weight=treated$base.sales)

pct99.control <- quantile(control$dm.L.ln_cpricei2, probs = 0.99, na.rm = T, weight=control$base.sales)
pct99treated <- quantile(treated$dm.L.ln_cpricei2, probs = 0.99, na.rm = T, weight=treated$base.sales)

all_pi[, cs_price := ifelse(dm.L.ln_cpricei2 > max(pct1.treated, pct1.control) & 
                              dm.L.ln_cpricei2 < min(pct99treated, pct99.control), 1, 0)]
# Make sure missings are 0s
all_pi[, cs_price := ifelse(is.na(dm.L.ln_cpricei2), 0, cs_price)]

## Keep within the common support
all_pi <- all_pi[cs_price == 1,]



outcomes <- c("w.ln_cpricei2", "w.ln_quantity3")
FE_opts <- c("group_region_by_module_by_time", "group_division_by_module_by_time")


LRdiff_res <- data.table(NULL)
target_res <- data.table(NULL)
cs_res <- data.table(NULL)
## Run within
flog.info("Iteration 0")
for (n.g in 2:5) {
    
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
    IV <- LRdiff_res[outcome == "w.ln_quantity3" & n.groups == n.g & controls == FE,][["Estimate"]]/LRdiff_res[outcome == "w.ln_cpricei2" & n.groups == n.g & controls == FE,][["Estimate"]]
    
    ## Estimate the matrix of the implied system of equations
    
    # Get the empirical distribution of prices by quantile
    all_pi[, base.sales.q := base.sales/sum(base.sales), by = .(quantile)]
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
    
    ## Calculate interesting consumer surplus changes
    
    # 1. Expected value of the scale factor
    all_pi[, qhat := beta_hat[1]]
    for (i in 2:length(beta_hat)) {
      all_pi[, qhat := qhat+(beta_hat[i])*dm.ln_cpricei2^(i-1)]
    }
    all_pi[, alpha_m := mean(ln_quantity3 - qhat, na.rm = T), by = store_by_module]
    all_pi[, p_bar := mean(ln_cpricei2, na.rm = T), by = module_by_time]
    
    scale.factor <- all_pi[, mean(exp(alpha_m+p_bar), na.rm = T)]   
    
    # 2. Integral and export
    for (change in c(0.01, 0.05, 0.1)) {
      
      cschangepos <- integrate(integrand, 0, change, theta = beta_hat)
      cschangeneg <- integrate(integrand, -change, 0, theta = beta_hat)
      
      from0 <- (cschangepos$value)
      to0 <- (cschangeneg$value)
      
      # Export estimates
      estimated.cschange <- data.table(from0, to0, scale.factor)
      estimated.cschange[, n.groups := n.g]
      estimated.cschange[, controls := FE]
      estimated.cschange[, iter := 0]    
      estimated.cschange[, change := change]    
      cs_res <- rbind(cs_res, estimated.cschange)
    }
    fwrite(cs_res, cs.output.results.file)
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
  
  for (n.g in 2:5) {
    
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
    
    ## Estimate RF and FS
    for (FE in FE_opts) {
      for (Y in outcomes) {
        formula1 <- as.formula(paste0(
          Y, " ~ w.ln_sales_tax:quantile | ", FE, "+ quantile"
        ))
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
      IV <- LRdiff_res[outcome == "w.ln_quantity3" & n.groups == n.g & controls == FE & iter == rep,][["Estimate"]]/LRdiff_res[outcome == "w.ln_cpricei2" & n.groups == n.g & controls == FE & iter == rep,][["Estimate"]]
      
      ## Estimate the matrix of the implied system of equations
      
      # Get the empirical distribution of prices by quantile
      sampled.data[, base.sales.q := base.sales/sum(base.sales), by = .(quantile)]
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
      
      
      ## Calculate interesting consumer surplus changes
      
      # 1. Expected value of the scale factor
      sampled.data[, qhat := beta_hat[1]]
      for (i in 2:length(beta_hat)) {
        sampled.data[, qhat := qhat+(beta_hat[i])*dm.ln_cpricei2^(i-1)]
      }
      sampled.data[, alpha_m := mean(ln_quantity3 - qhat, na.rm = T), by = store_by_module]
      sampled.data[, p_bar := mean(ln_cpricei2, na.rm = T), by = module_by_time]
      
      scale.factor <- all_pi[, mean(exp(alpha_m+p_bar), na.rm = T)]   
      
      # 2. Integral and export
      for (change in c(0.01, 0.05, 0.1)) {
        
        cschangepos <- integrate(integrand, 0, change, theta = beta_hat)
        cschangeneg <- integrate(integrand, -change, 0, theta = beta_hat)
        
        from0 <- (cschangepos$value)
        to0 <- (cschangeneg$value)
        
        # Export estimates
        estimated.cschange <- data.table(from0, to0, scale.factor)
        estimated.cschange[, n.groups := n.g]
        estimated.cschange[, controls := FE]
        estimated.cschange[, iter := rep]    
        estimated.cschange[, change := change]    
        cs_res <- rbind(cs_res, estimated.cschange)
      }
      fwrite(cs_res, cs.output.results.file)
      
    } 
  }
}

