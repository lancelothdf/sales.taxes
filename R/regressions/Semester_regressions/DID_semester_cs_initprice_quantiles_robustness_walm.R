#' Sales Taxes Project
#' Robustness Check Concentration. We identify which counties have the haighest WalMArt concentration of sales. 
#' We now its only 4% of the sample but we will try to see wheter we observe different passthroughs across samples
#' We run 2 different specifications:
#' 1) We only allow the intercept to vary in each stage
#' 2) both the intercept and the slope vary in each stage 


library(data.table)
library(futile.logger)
library(lfe)
library(multcomp)


setwd("/project2/igaarder")


## input filepaths -----------------------------------------------
#' This data is the same as all_goods_pi_path, except it has 2015-2016 data as well.
data.semester <- "Data/Nielsen/semester_nielsen_data.csv"
data.walmart <- "Data/walmart_share_county.csv"


## output filepaths ----------------------------------------------
results.file <- "Data/robust_walmart_estimates_initial_price_semester.csv"
theta.results.file <- "Data/Demand_theta_robust_walmart_initial_price_semester.csv"

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

## cut the tails (keep between 1st and 99th percentile)
pct1 <- quantile(all_pi$dm.ln_cpricei2, probs = 0.01, na.rm = T, weight=base.sales)
pct99 <- quantile(all_pi$dm.ln_cpricei2, probs = 0.99, na.rm = T, weight=base.sales)
all_pi <- all_pi[(dm.ln_cpricei2 > pct1 & dm.ln_cpricei2 < pct99),]

## Merge with identified counties
w.counties <- fread(data.walmart)
all_pi <- merge(all_pi, w.counties, by = c("fips_state","fips_county"), all.x = T)

## Modify variable
all_pi[, D := ifelse(is.na(walmart_sample), 0,walmart_sample)]
all_pi[, sum(D)]


# Saturate fixed effects
all_pi[, dem_group_region_by_module_by_time := .GRP, by = .(region_by_module_by_time, D)]
all_pi[, dem_group_division_by_module_by_time := .GRP, by = .(division_by_module_by_time, D)]


## Run estimations -----------------

FE_opts <- c("group_region_by_module_by_time", "group_division_by_module_by_time")
outcomes <- c("w.ln_cpricei2", "w.ln_quantity3")

LRdiff_res <- data.table(NULL)
target_res <- data.table(NULL)


  
## Loop over number of quantiles
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
  
  
  ##### Estimation changing only the intercept
  
  ## Estimate RF and FS
  for (FE in FE_opts) {
    for (Y in outcomes) {
      formula1 <- as.formula(paste0(
        Y, " ~ w.ln_sales_tax:quantile + w.ln_sales_tax:D | ", FE, "+ quantile + D + ", paste0("dem_", FE)
      ))
      if (n.g == 1) { formula1 <- as.formula(paste0(Y, " ~ w.ln_sales_tax + w.ln_sales_tax:D | ", FE, " + D + ", paste0("dem_", FE))) }
      res1 <- felm(formula = formula1, data = all_pi,
                   weights = all_pi$base.sales)
      
      
      ## attach results
      res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
      res1.dt[, outcome := Y]
      res1.dt[, controls := FE]
      res1.dt[, n.groups := n.g]
      res1.dt[, lev := c(quantlab[-1], "D")]
      res1.dt[, spec := "Intercept"]
      res1.dt[, walm := NA]
      
      ## Descriptives
      res1.dt$N_obs <- nrow(all_pi)
      res1.dt$N_stores <- uniqueN(all_pi, by = c("store_code_uc") )
      res1.dt$N_modules <- length(unique(all_pi$product_module_code))
      res1.dt$N_counties <- uniqueN(all_pi, by = c("fips_state", "fips_county"))
      res1.dt$N_years <- uniqueN(all_pi, by = c("year")) # should be 6 (we lose one because we difference)
      res1.dt$N_county_modules <- uniqueN(all_pi, by = c("fips_state", "fips_county",
                                                         "product_module_code"))
      LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
      fwrite(LRdiff_res, results.file)
    }
  
    ## Estimate IVs and retrieve in vector
    b_1_q <- LRdiff_res[outcome == "w.ln_quantity3" & n.groups == n.g & controls == FE &
                        spec == "Intercept" & rn == "w.ln_sales_tax:DTRUE",][["Estimate"]]
    
    b_1_p <- LRdiff_res[outcome == "w.ln_cpricei2" & n.groups == n.g & controls == FE &
                          spec == "Intercept" & rn == "w.ln_sales_tax:DTRUE",][["Estimate"]]
  
    
    IV0 <- LRdiff_res[outcome == "w.ln_quantity3" & n.groups == n.g & controls == FE &
                        spec == "Intercept" & rn != "w.ln_sales_tax:DTRUE" ,][["Estimate"]]/LRdiff_res[outcome == "w.ln_cpricei2" & n.groups == n.g & controls == FE & 
                             spec == "Intercept" & rn != "w.ln_sales_tax:DTRUE" ,][["Estimate"]]
    IV1 <- (LRdiff_res[outcome == "w.ln_quantity3" & n.groups == n.g & controls == FE &
                         spec == "Intercept" & rn != "w.ln_sales_tax:DTRUE" ,][["Estimate"]] + b_1_q
    )/(LRdiff_res[outcome == "w.ln_cpricei2" & n.groups == n.g & controls == FE & 
                    spec == "Intercept" & rn != "w.ln_sales_tax:DTRUE",][["Estimate"]] + b_1_p)
    
    ## Estimate the matrix of the implied system of equations
    if (n.g > 1) {
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
      beta_hat_0 <- as.vector(solve(as.matrix(gamma))%*%(as.matrix(IV0)))
      beta_hat_1 <- as.vector(solve(as.matrix(gamma))%*%(as.matrix(IV1)))
      # Estimate intercept
      mean.q <- all_pi[, mean(ln_quantity3, weights = base.sales)]
      mean.p <- all_pi[, mean(dm.ln_cpricei2, weights = base.sales)]
      beta_0_hat_0 <- mean.q - sum((beta_hat_0)*(mean.p^(1:n.g)))
      beta_0_hat_1 <- mean.q - sum((beta_hat_1)*(mean.p^(1:n.g)))
      beta_hat_0 <- c(beta_0_hat_0, beta_hat_0)
      beta_hat_1 <- c(beta_0_hat_1, beta_hat_1)
      
      ## Export estimated target parameters
      beta_hat <- beta_hat_0
      estimated.target_0 <- data.table(beta_hat)
      estimated.target_0[, beta_n := .I-1]
      estimated.target_0[, n.groups := n.g]
      estimated.target_0[, controls := FE]
      estimated.target_0[, spec := "Intercept"]
      estimated.target_0[, walm := NA]
      
      beta_hat <- beta_hat_1
      estimated.target_1 <- data.table(beta_hat)
      estimated.target_1[, beta_n := .I-1]
      estimated.target_1[, n.groups := n.g]
      estimated.target_1[, controls := FE]
      estimated.target_1[, spec := "Intercept"]
      estimated.target_1[, walm := NA]
      
      target_res <- rbind(target_res, estimated.target_0, estimated.target_1)
      fwrite(target_res, theta.results.file)
    }
  }
  ##### Run fully saturated: split sample
  
  for (d in c(0,1)) {
    
    sample <- all_pi[D == d]
    
    ## Estimate RF and FS
    for (FE in FE_opts) {
      for (Y in outcomes) {
        formula1 <- as.formula(paste0(
          Y, " ~ w.ln_sales_tax:quantile | ", FE, "+ quantile"
        ))
        if (n.g == 1) { formula1 <- as.formula(paste0(Y, " ~ w.ln_sales_tax  | ", FE)) }
        res1 <- felm(formula = formula1, data = sample,
                     weights = sample$base.sales)
        
        
        ## attach results
        res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
        res1.dt[, outcome := Y]
        res1.dt[, controls := FE]
        res1.dt[, n.groups := n.g]
        res1.dt[, lev := quantlab[-1]]
        res1.dt[, spec := "Slope"]
        res1.dt[, walm := d]
        
        ## Descriptives
        res1.dt$N_obs <- nrow(sample)
        res1.dt$N_stores <- uniqueN(sample, by = c("store_code_uc") )
        res1.dt$N_modules <- length(unique(sample$product_module_code))
        res1.dt$N_counties <- uniqueN(sample, by = c("fips_state", "fips_county"))
        res1.dt$N_years <- uniqueN(sample, by = c("year")) # should be 6 (we lose one because we difference)
        res1.dt$N_county_modules <- uniqueN(sample, by = c("fips_state", "fips_county",
                                                           "product_module_code"))
        LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
        fwrite(LRdiff_res, results.file)
      }
    
      ## Estimate IVs and retrieve in vector
      IV <- LRdiff_res[outcome == "w.ln_quantity3" & n.groups == n.g & controls == FE &
                          spec == "Slope" & walm == d,][["Estimate"]]/LRdiff_res[outcome == "w.ln_cpricei2" &
                                                                                           n.groups == n.g & controls == FE & spec == "Slope" & walm == d,][["Estimate"]]

      ## Estimate the matrix of the implied system of equations
      if (n.g > 1) {
        # Get the empirical distribution of prices by quantile
        sample[, base.sales.q := base.sales/sum(base.sales), by = .(quantile)]
        sample[, p_group := floor((dm.ln_cpricei2 - min(dm.ln_cpricei2, na.rm = T))/((max(dm.ln_cpricei2, na.rm = T)-min(dm.ln_cpricei2, na.rm = T))/500)), by = .(quantile)]
        sample[, p_ll := p_group*((max(dm.ln_cpricei2, na.rm = T)-min(dm.ln_cpricei2, na.rm = T))/500), by = .(quantile)]
        sample[, p_ll := p_ll + min(dm.ln_cpricei2, na.rm = T), by = .(quantile)]
        sample[, p_ul := p_ll + ((max(dm.ln_cpricei2, na.rm = T)-min(dm.ln_cpricei2, na.rm = T))/500), by = .(quantile)]
        
        ed.price.quantile <- sample[, .(w1 = (sum(base.sales.q))), by = .(p_ul, p_ll, quantile)]
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
        estimated.target[, spec := "Slope"]
        estimated.target[, walm := d]
        

        target_res <- rbind(target_res, estimated.target)
        fwrite(target_res, theta.results.file)
      }
    }  
  }
}


  


