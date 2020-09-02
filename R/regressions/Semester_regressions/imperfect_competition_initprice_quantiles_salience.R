#' Conduct parameter under salience
#' USe same formula as before but modify estimates scaling by sigma

library(data.table)
library(futile.logger)
library(lfe)
library(multcomp)


setwd("/project2/igaarder")


#### functions ----------

## function to identify the asymptote
asymptote.fun <- function(theta, q1, q2, es) {
  if (is.infinite(es)) {
    return(q1*(q1 + theta) - theta*q2)
  } else { 
    return((q1 + theta)*(es*q1 - 1) - theta*q2*es)
  }
}

## function to solve for the root
pass.through.eq <- function(theta, q1, q2, es, rho){
  if (is.infinite(es)) {
    return(rho - (q1*(q1 + theta))/(q1*(q1 + theta) - theta*q2))
  } else {
    return(rho - (es*q1*(q1 + theta))/((q1 + theta)*(es*q1 - 1) - theta*q2*es))
  }
}

## input filepaths -----------------------------------------------
#' This data is the same as all_goods_pi_path, except it has 2015-2016 data as well.
data.semester <- "Data/Nielsen/semester_nielsen_data.csv"
data.year <- "Data/Nielsen/yearly_nielsen_data.csv"

iv.output.results.file <- "Data/Demand_iv_sat_initial_price_semester_salience.csv"
theta.output.results.file <- "Data/Demand_theta_sat_initial_price_semester_salience.csv"
## output filepaths ----------------------------------------------
estimates.theta.out <- "Data/salience_conduct_parameter_at_p.csv"



### Set-up previous estimates -----

#### passthourghs
rho.old <- fread(iv.output.results.file)
rho.old <- rho.old[ outcome == "rho" & controls == "division_by_module_by_time",]

## Betas estimation
betas.old <- fread(theta.output.results.file)
betas.old <- betas.old[controls == "division_by_module_by_time",]

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

### Manually incorporate values needed
epsilon <- 0.0000001
ed <- 0.54811
sol <- c(0.02, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.6459489, 0.7, 0.8, 0.9, 1)
val.es <- sol*ed / (1- sol)


results <- data.table(NULL)
###  Loop across values of sigma
for (sig in c(0.25, 0.5, 0.75, 1)) {
  
  
  ## cut the tails (keep between 1st and 99th percentile)
  pct1 <- quantile(all_pi[[paste0("dm.ln_cpricei2_sig", sig)]], probs = 0.01, na.rm = T, weight=base.sales)
  pct99 <- quantile(all_pi[[paste0("dm.ln_cpricei2_sig", sig)]], probs = 0.99, na.rm = T, weight=base.sales)
  all_pi_est <- all_pi[(get(paste0("dm.ln_cpricei2_sig", sig)) > pct1 & get(paste0("dm.ln_cpricei2_sig", sig)) < pct99),]
  
  # Prices to evaluate the function
  pctiles <- seq(0,1,0.1)
  values <- round(quantile(all_pi_est[[paste0("dm.ln_cpricei2_sig", sig)]], 
                           probs = pctiles, na.rm = T, 
                           weight = all_pi_est$base.sales), digits = 4)
  prices <- (values[-1] + values[-length(values)])/2 # find the mid point
  
  ## Capture passthorugh
  rho <- 0.0508/sig #estimated effect on producer price
  ## Capture estimated demand
  demand <- betas.old[sigma == sig][["beta_hat"]]

  
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
      
      
      ## 3. find asymptote
      solve <- uniroot(asymptote.fun, c(0,1), extendInt="yes", q1 = q1, q2 = q2, es = es.val, tol = .Machine$double.eps^2)
      asymptote <- solve$root
      
      ## 4. Find value of theta smartly
      # Start calculating values at asymptote (each side), 0 and 1
      # ub <- asymptote+epsilon
      # lb <- asymptote-epsilon
      # l <- -0.25
      # u <- 1.25
      # f.u <- pass.through.eq(ub, q1 = q1, q2 = q2, es = es.val, rho = rho)
      # f.l <- pass.through.eq(lb, q1 = q1, q2 = q2, es = es.val, rho = rho)
      # f.0 <- pass.through.eq(l, q1 = q1, q2 = q2, es = es.val, rho = rho)
      # f.1 <- pass.through.eq(u, q1 = q1, q2 = q2, es = es.val, rho = rho)
      # 
      # if (asymptote > l & asymptote < u) {
      #   ## Case 1
      #   if (f.l>0 & f.l > f.0) {
      #     if (f.0 < 0) {
      #       solve2 <- uniroot(pass.through.eq, c(l,lb), q1 = q1, q2 = q2, es = es.val, rho = rho)
      #       theta <- solve2$root
      #       is.0 <- solve2$f.root
      #     }
      #     else {
      #       # try the oposite area if possible
      #       if (f.1 > 0 & f.u < 0) {
      #         solve2 <- uniroot(pass.through.eq, c(ub,u), q1 = q1, q2 = q2, es = es.val, rho = rho)
      #         theta <- solve2$root
      #         is.0 <- solve2$f.root
      #       }
      #       else {
      #         theta <- NA
      #         is.0 <- NA              
      #       }
      #     }
      #   }
      #   else {
      #     ## Case 2 (very unlikely given the values?)
      #     if (f.u>0 & f.u > f.1) {
      #       if (f.1 < 0) {
      #         solve2 <- uniroot(pass.through.eq, c(ub,u), q1 = q1, q2 = q2, es = es.val, rho = rho)
      #         theta <- solve2$root
      #         is.0 <- solve2$f.root
      #       }
      #       else {
      #         # try the oposite area if possible
      #         if (f.0 > 0 & f.l < 0) {
      #           solve2 <- uniroot(pass.through.eq, c(l,lb), q1 = q1, q2 = q2, es = es.val, rho = rho)
      #           theta <- solve2$root
      #           is.0 <- solve2$f.root             
      #         }
      #         else {
      #           theta <- NA
      #           is.0 <- NA              
      #         }
      #       }          
      #     }          
      #   }
      # }
      # ## Case 3: asymptote falls outside interest area
      # else {
      #   ## just see if limits have different signs
      #   if ((f.1 >0 & f.0 < 0) | (f.1 <0 & f.0 > 0)) {
      #     solve2 <- uniroot(pass.through.eq, c(l,u), q1 = q1, q2 = q2, es = es.val, rho = rho)
      #     theta <- solve2$root
      #     is.0 <- solve2$f.root          
      #   }
      #   else {
      #     theta <- NA
      #     is.0 <- NA              
      #   }
      # }
      
      # old method works better
      ub <- asymptote-epsilon
      lb <- -.1
      if (ub < 0.15) { lb <- ub-0.25}
      # Force opposite sign starting point
      ## make f(lb) opposite value of f(ub) up to an extreme point -.2 and 1.2
      # Only looking for solutions to the left of the asymptote
      f.u <- pass.through.eq.s(ub, sigma = sig.val, q1 = q1, q2 = q2, es = es.val, rho = rho)
      f.l <- pass.through.eq.s(lb, sigma = sig.val, q1 = q1, q2 = q2, es = es.val, rho = rho)
      if (f.u*f.l > 0 ) {
        while (f.l > 0 & lb > - 0.2) {
          lb <- lb - 0.01
          f.l <- pass.through.eq.s(lb, sigma = sig.val, q1 = q1, q2 = q2, es = es.val, rho = rho)
        } 
        while (f.u < 0 & ub < 1.2) {
          ub <- ub + 0.01
          f.u <- pass.through.eq.s(ub, sigma = sig.val, q1 = q1, q2 = q2, es = es.val, rho = rho)
        }
      }
      if (f.u*f.l < 0 ) {
        solve2 <- uniroot(pass.through.eq.s, c(lb,ub), extendInt="downX", sigma = sig.val, q1 = q1, q2 = q2, es = es.val, rho = rho)
        theta <- solve2$root
        is.0 <- solve2$f.root
      } else {
        theta <- is.0 <- NA
      }
      
      
      ## 6. Export
      #results <- rbind(results, data.table(sigma = sig, es.val, p, q1, q2, asymptote, f.0, f.1, rho, theta, is.0))
      results <- rbind(results, data.table(sigma = sig, es.val, p, q1, q2, asymptote, f.0, f.1, rho, theta, is.0))
    }
  }
  fwrite(results, estimates.theta.out)  
}


