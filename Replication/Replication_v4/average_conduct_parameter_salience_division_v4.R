##### Santiago Lacouture
#' Sales Taxes
#' Replication File. Updated on 4/24/2023
#' Step 11: Estimate Average conduct parameter under imperfect salience

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
# input
iv.output.salience.results.file <- "Data/Replication_v4/Demand_iv_sat_initial_price_semester_salience.csv"
theta.output.salience.results.file <- "Data/Replication_v4/Demand_theta_sat_initial_price_semester_salience.csv"
# output
conduct.parameter.file <- "Data/Replication_v4/salience_conduct_parameter_at_p_division.csv"



## Function that directly solves for theta
theta.direct <- function(q1, q2, es, rho, sigma) {
  ems.inv <- (q1-q2)/(q1^2)
  
  if (is.infinite(es)) {
    return((1-rho)/(rho*(ems.inv)-sigma/q1-(1-sigma)*ems.inv))
  } else {
    return((1-(1-sigma)*q1/es-rho*(1-q1/es))/(rho*(ems.inv-1/es)-sigma/q1+(1-sigma)*(1/es-ems.inv)))
  }
}


### Set-up previous estimates

#### passthourghs
rho.old <- fread(iv.output.salience.results.file)
rho.old <- rho.old[ outcome != "DL.ln_quantity3" & controls == "division_by_module_by_time" & iter == 0,]

## Betas estimation
betas.old <- fread(theta.output.salience.results.file)
betas.old <- betas.old[controls == "division_by_module_by_time" & iter == 0,]


### Manually incorporate values needed
epsilon <- 0.0000001
ed <- 0.502 # Added manually from full sample estimates with division by module fixed effects
#ed <- 0.56704857131384 # Added manually from full sample estimates
implied1 <- 1/(1+ed)
sol <- c(0.02, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, implied1, 0.7, 0.8, 0.9, 1)
val.es <- sol*ed / (1- sol)

results <- data.table(NULL)
###  Loop across values of sigma
for (sig in seq(0.25, 1, 0.05)) {
  
  ## cut the tails (keep between 1st and 99th percentile)
  if (sig != 1) {
    pct1 <- quantile(all_pi[[paste0("dm.ln_cpricei2_sig", sig)]], probs = 0.01, na.rm = T, weight=base.sales)
    pct99 <- quantile(all_pi[[paste0("dm.ln_cpricei2_sig", sig)]], probs = 0.99, na.rm = T, weight=base.sales)
    all_pi_est <- all_pi[(get(paste0("dm.ln_cpricei2_sig", sig)) > pct1 & get(paste0("dm.ln_cpricei2_sig", sig)) < pct99),]
  }
  
  # Prices to evaluate the function
  pctiles <- seq(0,1,0.01)
  values <- round(quantile(all_pi_est[[paste0("dm.ln_cpricei2_sig", sig)]], 
                           probs = pctiles, na.rm = T, 
                           weight = all_pi_est$base.sales), digits = 6)
  prices <- (values[-1] + values[-length(values)])/2 # find the mid point
  
  ## Capture passthorugh
  rho <- rho.old[sigma == 1 & n.groups == 1][["Estimate"]] # We use the perfect salience because is equivalent to the others up to sigma scale
  
  # Loop across K
  for (K in 2:3) {
    ## Capture estimated demand
    demand <- betas.old[sigma == sig & n.groups == K][["beta_hat"]]
    
    for (es.val in val.es) {
      for (p in prices) {
        
        ## 1. build estimated elasticity at p
        q1 <- 0
        for (k in 2:length(demand)) {
          q1 <- q1 + (k-1)*demand[k]*(p)^(k-2)
        }
        
        ## 2. build estimated second derivative at p
        q2 <- 0
        for (k in 3:length(demand)) {
          q2 <- q2 + (k-1)*(k-2)*demand[k]*(p)^(k-3)
        } 
        
        # 3. Find the value of theta solving directly
        theta <- theta.direct(q1 = q1, q2 = q2, es = es.val, rho = rho, sigma = sig)
        
        
        ## 6. Export
        results <- rbind(results, data.table(sigma = sig, K, es.val, p, q1, q2, rho, theta))
      }
    }
    fwrite(results, conduct.parameter.file)  
    
  }
}


