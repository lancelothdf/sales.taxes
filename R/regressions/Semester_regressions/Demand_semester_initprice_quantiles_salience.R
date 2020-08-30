#' Sales Taxes Project
#' Demand under imperfect salience
#' This code estimates the demand using the proposed method. First, we 
#' run a Basic DiD model by initial price level and estimate the "long run" models 
#' splitting the sample by quantiles increasing the number of groups.
#' Here initial level means previous period and we divide by groups within the "common" support
#' In this case, we run a fully saturated model (instead of splitting the sample)
#' We get the Implied IV and recover the implied demand function varying the degree estimated (no. of quantiles)
#' We alsoe stimate full sample IV by 2SLS
#' 


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
iv.output.results.file <- "Data/Demand_iv_sat_initial_price_semester_salience.csv"
theta.output.results.file <- "Data/Demand_theta_sat_initial_price_semester_salience.csv"

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


## Create transformed price under nonsalience for all estimations
all_pi[, L.ln_sales_tax := ln_sales_tax - D.ln_sales_tax]

for (sig in c(0.25, 0.5, 0.75, 1)) {
  # build p^sigma
  all_pi[, paste0("ln_cpricei2_sig", sig) := ln_cpricei2 +sig*ln_sales_tax]
  # Create within
  all_pi[, paste0("w.ln_cpricei2_sig", sig) := get(paste0("ln_cpricei2_sig", sig)) - mean(get(paste0("ln_cpricei2_sig", sig))), by = .(store_by_module)]
  # Create de-meaned for cutting tails
  all_pi[, paste0("dm.ln_cpricei2_sig", sig)  := get(paste0("ln_cpricei2_sig", sig)) - mean(get(paste0("ln_cpricei2_sig", sig)), na.rm = T), by = module_by_time]
  # Created lagged and de-meaned lagegd for splitting sample
  all_pi[, paste0("D.ln_cpricei2_sig", sig) := D.ln_cpricei2 +sig*D.ln_sales_tax]
  all_pi[, paste0("L.ln_cpricei2_sig", sig) := get(paste0("ln_cpricei2_sig", sig)) - get(paste0("D.ln_cpricei2_sig", sig))]
  all_pi[, paste0("dm.L.ln_cpricei2_sig", sig) := get(paste0("L.ln_cpricei2_sig", sig)) - mean(get(paste0("L.ln_cpricei2_sig", sig)), na.rm = T), by = module_by_time]
  
}

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


## Options
outcomes.sep <- c("w.ln_cpricei2", "w.ln_pricei2", "w.ln_quantity3")
FE_opts <- c("region_by_module_by_time", "division_by_module_by_time")


LRdiff_res <- data.table(NULL)
target_res <- data.table(NULL)
## Loop across sigma 
for (sig in c(0.25, 0.5, 0.75, 1)) {
  
  ## cut the tails (keep between 1st and 99th percentile)
  pct1 <- quantile(all_pi[[paste0("dm.ln_cpricei2_sig", sig)]], probs = 0.01, na.rm = T, weight=base.sales)
  pct99 <- quantile(all_pi[[paste0("dm.ln_cpricei2_sig", sig)]], probs = 0.99, na.rm = T, weight=base.sales)
  all_pi_est <- all_pi[(get(paste0("dm.ln_cpricei2_sig", sig)) > pct1 & get(paste0("dm.ln_cpricei2_sig", sig)) < pct99),]
  
  ## Full sample IV
  for (FE in FE_opts) {
      formula1 <- as.formula(paste0(
        "w.ln_quantity3 ~ 0 | ", FE, " | (w.ln_cpricei2_sig", sig," ~ w.ln_sales_tax) | module_by_state"
      ))
      res1 <- felm(formula = formula1, data = all_pi_est,
                   weights = all_pi_est$base.sales)
      
      
      ## attach results
      res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
      res1.dt[, outcome := "IV"]
      res1.dt[, controls := FE]
      res1.dt[, lev := 100]
      res1.dt[, n.groups := 1]
      res1.dt[, sigma := sig]
      
      LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
      fwrite(LRdiff_res, iv.output.results.file)
  }
  ## Demand for K=L=2
  # Create groups of initial values of tax rate
  # We use the full weighted distribution
  all_pi_est <- all_pi_est[, quantile := cut(get(paste0("dm.L.ln_cpricei2_sig", sig)),
                                     breaks = quantile(get(paste0("dm.L.ln_cpricei2_sig", sig)), probs = seq(0, 1, by = 1/2), na.rm = T, weight = base.sales),
                                     labels = 1:2, right = FALSE)]
  quantlab <- round(quantile(all_pi_est[[paste0("dm.L.ln_cpricei2_sig", sig)]], 
                             probs = seq(0, 1, by = 1/2), na.rm = T, 
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
      res1 <- felm(formula = formula1, data = all_pi,
                   weights = all_pi$base.sales)
      
      
      ## attach results
      res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
      res1.dt[, outcome := Y]
      res1.dt[, controls := FE]
      res1.dt[, n.groups := 2]
      res1.dt[, lev := quantlab[-1]]
      res1.dt[, sigma := sig]
      
      LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
      fwrite(LRdiff_res, iv.output.results.file)
    }
    
    ## Estimate IVs and retrieve in vector
    IV <- LRdiff_res[outcome == "w.ln_quantity3" & n.groups == 2 & controls == FE & sigma == sig,][["Estimate"]]/LRdiff_res[outcome == "w.ln_cpricei2" & n.groups == 2 & controls == FE & sigma == sig,][["Estimate"]]
    
    ## Estimate the matrix of the implied system of equations
    # Get the empirical distribution of prices by quantile
    all_pi[, base.sales.q := base.sales/sum(base.sales), by = .(quantile)]
    all_pi[, p_group := floor((get(paste0("dm.L.ln_cpricei2_sig", sig)) - min(get(paste0("dm.L.ln_cpricei2_sig", sig)), na.rm = T))/((max(get(paste0("dm.L.ln_cpricei2_sig", sig)), na.rm = T)-min(get(paste0("dm.L.ln_cpricei2_sig", sig)), na.rm = T))/500)), by = .(quantile)]
    all_pi[, p_ll := p_group*((max(get(paste0("dm.L.ln_cpricei2_sig", sig)), na.rm = T)-min(get(paste0("dm.L.ln_cpricei2_sig", sig)), na.rm = T))/500), by = .(quantile)]
    all_pi[, p_ll := p_ll + min(get(paste0("dm.L.ln_cpricei2_sig", sig)), na.rm = T), by = .(quantile)]
    all_pi[, p_ul := p_ll + ((max(get(paste0("dm.L.ln_cpricei2_sig", sig)), na.rm = T)-min(get(paste0("dm.L.ln_cpricei2_sig", sig)), na.rm = T))/500), by = .(quantile)]
    
    ed.price.quantile <- all_pi[, .(w1 = (sum(base.sales.q))), by = .(p_ul, p_ll, quantile)]
    ed.price.quantile[, p_m := (p_ul+p_ll)/2]
    
    
    # Create the derivative of the polynomial of prices and multiplicate by weights
    for (n in 1:2){
      ed.price.quantile[, paste0("b",n) := (n)*w1*(p_m^(n-1))]
    }
    # Calculate integral
    gamma <- ed.price.quantile[ , lapply(.SD, sum), by = .(quantile), .SDcols = paste0("b",1:2)]
    gamma <- gamma[!is.na(quantile),][order(quantile)][, -c("quantile")]
    
    ## Retrieve target parameters
    beta_hat <- as.vector(solve(as.matrix(gamma))%*%(as.matrix(IV)))
    # Estimate intercept
    mean.q <- all_pi[, mean(ln_quantity3, weights = base.sales)]
    mean.p <- all_pi[, mean(get(paste0("dm.ln_cpricei2_sig", sig)), weights = base.sales)]
    beta_0_hat <- mean.q - sum((beta_hat)*(mean.p^(1:2)))
    beta_hat <- c(beta_0_hat, beta_hat)
    
    ## Export estimated target parameters
    estimated.target <- data.table(beta_hat)
    estimated.target[, beta_n := .I-1]
    estimated.target[, n.groups := 2]
    estimated.target[, controls := FE]
    estimated.target[, sigma := sig]
    target_res <- rbind(target_res, estimated.target)
    fwrite(target_res, theta.output.results.file)
    
  } 
  
}
