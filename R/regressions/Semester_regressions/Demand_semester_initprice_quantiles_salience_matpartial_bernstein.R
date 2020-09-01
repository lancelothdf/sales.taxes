#' Sales Taxes Project
#' This code estimates the demand using the proposed method. First, we 
#' run a Basic DiD model by initial price level and estimate the "long run" models 
#' splitting the sample by quantiles increasing the number of groups.
#' Here initial level means previous period and we divide by groups within the "common" support
#' In this case, we run a fully saturated model (instead of splitting the sample)
#' We do partial identification in this case, so we extract the gamma matrix plus the mean q and demean log p. 
#' For now, we don't Bootstrap to get CIs. In this case we use bernstein polynomials so we re-scale prices to lay in [0,1]
#' new 12/12/19: Trim tails after defining common support, tails are so long they make partial id. infeasible (shape constraint)
#' Extension 6/12/20: Extend support for extrapolations
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
pq.output.results.file <- "Data/Demand_pq_sat_initial_price_semester_salience.csv"
output.path <- "Data/Demand_gamma_sat_initial_price_semester_salience_K"


## Bernstein basis Function -------------------------------------------

bernstein <- function(x, k, K){
  choose(K, k) * x^k * (1 - x)^(K - k)
}

## Bernstein basis Function Derivative ---------------------------------
d.bernstein <-function(x, k, K) {
  K*(bernstein(x, k-1, K-1) - bernstein(x, k, K-1))
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


## Create transformed price under nonsalience for all estimations
all_pi[, L.ln_sales_tax := ln_sales_tax - D.ln_sales_tax]

for (sig in c(0.25, 0.5, 0.75, 1)) {
  # build p^sigma
  all_pi[, paste0("ln_cpricei2_sig", sig) := ln_pricei2 +sig*ln_sales_tax]
  # Create within
  all_pi[, paste0("w.ln_cpricei2_sig", sig) := get(paste0("ln_cpricei2_sig", sig)) - mean(get(paste0("ln_cpricei2_sig", sig))), by = .(store_by_module)]
  # Create de-meaned for cutting tails
  all_pi[, paste0("dm.ln_cpricei2_sig", sig)  := get(paste0("ln_cpricei2_sig", sig)) - mean(get(paste0("ln_cpricei2_sig", sig)), na.rm = T), by = module_by_time]
  # Created lagged and de-meaned lagegd for splitting sample
  all_pi[, paste0("D.ln_cpricei2_sig", sig) := D.ln_pricei2 +sig*D.ln_sales_tax]
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



## Loop across sigmas
LRdiff_res <- data.table(NULL)
pq_res <- data.table(NULL)
for (sig in c(0.25, 0.5, 0.75, 1)) {
  
  ## cut the tails (keep between 1st and 99th percentile)
  pct1 <- quantile(all_pi[[paste0("dm.ln_cpricei2_sig", sig)]], probs = 0.01, na.rm = T, weight=base.sales)
  pct99 <- quantile(all_pi[[paste0("dm.ln_cpricei2_sig", sig)]], probs = 0.99, na.rm = T, weight=base.sales)
  all_pi_est <- all_pi[(get(paste0("dm.ln_cpricei2_sig", sig)) > pct1 & get(paste0("dm.ln_cpricei2_sig", sig)) < pct99),]


  ## Define re-scaled prices to use Bernstein polynomials in that range
  min.p <- all_pi_est[, min(get(paste0("dm.ln_cpricei2_sig", sig)))]
  max.p <- all_pi_est[, max(get(paste0("dm.ln_cpricei2_sig", sig)))]
  all_pi_est[, r.dm.ln_cpricei2 := (get(paste0("dm.ln_cpricei2_sig", sig)) - min.p)/(max.p - min.p)]
  
  ## Export values to re-estimate the intercept
  mean.q <- all_pi[, mean(ln_quantity3, weights = base.sales, na.rm = T)]
  mean.p <- all_pi[, mean(r.dm.ln_cpricei2, weights = base.sales, na.rm = T)]
  
  estimated.pq <- data.table(mean.q, mean.p, min.p, max.p, sigma = sig)
  pq_res <- rbind(pq_res, estimated.pq)
  fwrite(pq_res, pq.output.results.file)
  
  for (n.g in 1:2) {
    
    
    # Create groups of initial values of tax rate
    # We use the full weighted distribution
    all_pi_est <- all_pi_est[, quantile := cut(get(paste0("dm.L.ln_cpricei2_sig", sig)),
                                               breaks = quantile(get(paste0("dm.L.ln_cpricei2_sig", sig)), probs = seq(0, 1, by = 1/2), na.rm = T, weight = base.sales),
                                               labels = 1:2, right = FALSE)]
    quantlab <- round(quantile(all_pi_est[[paste0("dm.L.ln_cpricei2_sig", sig)]], 
                               probs = seq(0, 1, by = 1/2), na.rm = T, 
                               weight = all_pi_est$base.sales), digits = 4)
    
    ## Do partial identification
    ## Estimate the matrix of the implied system of equations. For each possible polynomial degree and compute 
    # Get the empirical distribution of prices by quantile
    all_pi_est[, base.sales.q := base.sales/sum(base.sales), by = .(quantile)]
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
        
        ## Read Previous and write
        theta.output.results.file <- paste0(output.path, K,"_bern.csv")
        
        if (n.g == 1 & sigma == 0.25) {
          fwrite(gamma, theta.output.results.file)
        } else {
          previous.data <- fread(theta.output.results.file)
          previous.data <- rbind(previous.data, gamma)
          fwrite(previous.data, theta.output.results.file)
        }
      }
    }
  }
}

  
  