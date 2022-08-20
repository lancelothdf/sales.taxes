##### Santiago Lacouture
#' Sales Taxes
#' Replication File. Updated on 8/18/2022
#' Step 13: Welfare extrapolation. Nationwide average
#' This code contains the base functions used to perform 
#' welfare extrapolation. It is to be called by any script
#' running this type of exercises
#' 


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


### input and output paths ----
# inputs
binned.data.tax
iv.output.salience.results.file <- "Data/Replication/Demand_iv_sat_initial_price_semester_salience.csv"
theta.berstein.sal <- "Data/Replication/Demand_gamma_sat_initial_price_semester_salience_K"
pq.output.salience.results.file <- "Data/Replication/Demand_pq_sat_initial_price_semester_salience.csv"
out.file.mc.welf <- "Data/Replication/mincriteria_welfare_boot.csv"



### General setup ----


# 0. Parallelize options
# use the environment variable SLURM_NTASKS_PER_NODE to set the number of cores
n.cores <- Sys.getenv("SLURM_NTASKS_PER_NODE")
registerDoParallel(cores=n.cores)

# 1. Open data
data <- fread(binned.data.tax)
data[, p_cml := p_m - tau]
data[, tauno := 0]
data[, tau5 := tau + log(1+0.05)]

# 2. Open IV estimates
IVs <- fread(iv.output.salience.results.file)
IVs <- IVs[controls == "division_by_module_by_time"]

# 3. Values to Tests. In practice, this is done in batches to avoid running all cases and exceed the running time
sigmas.test <- c(0.25, 0.5, 0.75, 1)

## Conduct parameters
thetas.list <- list()
# All
thetas.list$s25 <- c(0, 0.058897778, 0.564015475)
thetas.list$s50 <- c(0, 0.018501935, 0.202776786)
thetas.list$s75 <- c(0, 0.008007016, 0.102212766)
thetas.list$s100 <- c(0, 0.004861793, 0.066616166)
# # Priority
# thetas.list$s25 <- c(0)
# thetas.list$s50 <- c(0)
# thetas.list$s75 <- c(0)
# thetas.list$s100 <- c(0, 0.066616166)
# # Priority 2
# thetas.list$s25 <- c(0.058897778, 0.564015475)
# thetas.list$s50 <- c(0.018501935, 0.202776786)
# thetas.list$s75 <- c(0.008007016, 0.102212766)
# thetas.list$s100 <- c(0.004861793)

## 4. Set up IV estimates for each sigma
# For L = 1
IVs1 <- IVs[outcome == "IV", -c("Std. Error", "controls", "rn", "Cluster s.e.", "t value", "Pr(>|t|)", "outcome")]
# For L > 1
IVs2 <- dcast(IVs[n.groups > 1], n.groups + lev + sigma ~ outcome,  fun=sum, value.var = c("Estimate"))
IVs2[, w.ln_cpricei2 := w.ln_cpricei2_sig0.25 + w.ln_cpricei2_sig0.5 + w.ln_cpricei2_sig0.75 + w.ln_cpricei2_sig1]
IVs2[, Estimate := w.ln_quantity3/w.ln_cpricei2]
IVs2 <- IVs2[, -c(paste0("w.ln_cpricei2_sig", c(0.25, 0.5, 0.75, 1)), "w.ln_quantity3", "w.ln_cpricei2")]
# Merge and Order appropiately
res.ivs <- rbind(IVs1, IVs2)
res.ivs <- res.ivs[order(sigma, n.groups, lev)]
rm(IVs, IVs1, IVs2)

## 5. Open Min - Max files and min criteria
res.pq <- fread(pq.output.salience.results.file)
min.criteria <- fread(out.file.mc.welf)


## 6. Set up Ks (priority)
K.test <- c(2, 8)
scenarios <- c("Original", "No Tax", "plus 5 Tax")

## 7. Set up Optimization Parameters (algorithm for now)
nlo.opts.local.df <- list(
  "algorithm"="NLOPT_LN_COBYLA",
  "maxeval" = 600,
  "xtol_rel"=1.0e-8
)

## 8. Source extrapolation functions
source("Code/sales.taxes/Replication/welfare_functions.R")




results <- data.table(NULL)

for (sc in scenarios) {
  
  if (sc == "No Tax") {
    t0 <- "tauno"
    t1 <- "tau"
  } 
  if (sc == "plus 5 Tax")  {
    t0 <- "tau"
    t1 <- "tau5"
  }
  
  for (K in K.test) {
    
    ## B.1. Load Matrix of gamma (this extrictly depends on K since the basis change)
    in.file <- paste0(theta.berstein.sal, K,"_bern.csv")
    gamma.full.data <- fread(in.file)
    
    ## C. Loop across sigmas
    i <- 0
    for (sig in sigmas.test) {
      i <- i + 1
      thetas.test <- thetas.list[[i]]
      ## C.1 Extract support to use
      p.min <- res.pq[extrap == sc & sigma == sig][["min.p"]]
      p.max <- res.pq[extrap == sc & sigma == sig][["max.p"]]
      
      ## C.2 Restrict gamma file. Constant across p
      gamma <- gamma.full.data[extrap == sc & n.groups < 3 & sigma == sig][, c(paste0("b", 0:(K-1)), "n.groups"), with = F]             ## For elasticity
      
      ## D Start Loop at number of groups
      for (D in unique(gamma$n.groups)) { #unique(gamma$n.groups)
        
        ## D1. Build the constraints matrix 
        constr <- as.matrix(gamma[n.groups == D][, -c("n.groups")])   ## For elasticity
        
        ## D2. Retrieve IVs
        IVs <- res.ivs[n.groups == D  & sigma == sig][["Estimate"]] 
        
        ## D3. Load min.criterion for case
        mc <- min.criteria[Degree == K & L == D & sigma == sig & extrap == "Original",][["min.criteria"]]
        
        ## D4. Generate an initial value somewhere in the middle to test algorithms
        # init.val0max <- init.val0min <- get.init.val(constr, IVs, mc)
        
        ## E. Loop across thetas
        for (theta in thetas.test) {
          
          case <- data.table(sc, D , K, sigma = sig, theta)
          target <- merge(prev.sol, case, by = c("sc", "sigma", "theta", "K", "D"))
          
          init.val0min <- target[["sol1"]]
          init.val0max <- target[["sol2"]]
          target <- target[, .(it1 = mean(it1), it2 = mean(it2),
                               down = mean(down), up = mean(up),
                               s1 = mean(s1), s2 = mean(s2)), by = .(sc,sigma,theta,K,D)]
          
          
          if (sc == "Original") {
            # F2. Marginal change
            # F2a1. Min calculation
            res0 <- nloptr( x0=init.val0min,
                            eval_f= av.marginal.change,
                            eval_g_ineq = eval_restrictions_marg_av,
                            opts = nlo.opts.local.df,
                            data = data,
                            pp = "p_cml",
                            tau = "tau",
                            theta = theta,
                            sigma = sig,
                            w = "eta_m", 
                            st.code = "fips_state", 
                            min = p.min, 
                            max = p.max,
                            constr_mat = constr, 
                            IV_mat = IVs, 
                            min.crit = mc,
                            elas = T,
                            ub = rep(0, K),
                            lb = rep(min(IVs)/min(constr), K)
            )
            # F2a2 Results extraction
            down <- res0$objective
            s1 <- res0$status
            it1 <- res0$iterations
            sol1 <- res0$solution
            # F2b1. Max calculation
            res0 <- nloptr( x0=init.val0max,
                            eval_f= max.av.marginal.change,
                            eval_g_ineq = eval_restrictions_marg_av,
                            opts = nlo.opts.local.df,
                            data = data,
                            pp = "p_cml",
                            tau = "tau",
                            theta = theta,
                            sigma = sig,
                            w = "eta_m", 
                            st.code = "fips_state", 
                            min = p.min, 
                            max = p.max,
                            constr_mat = constr, 
                            IV_mat = IVs, 
                            min.crit = mc,
                            elas = T,
                            ub = rep(0, K),
                            lb = rep(min(IVs)/min(constr), K)
            )
            # F2b2 Results extraction
            up <- -res0$objective
            s2 <- res0$status
            it2 <- res0$iterations
            sol2 <- res0$solution
            
          }
          else {
            # F2. Non Marginal change
            # B3 Run minimization: derivative free 
            if (target[["it1"]] == 400) {
              res0 <- nloptr( x0=init.val0min,
                              eval_f= av.non.marginal.change.parallel,
                              eval_g_ineq = eval_restrictions_nmarg_av,
                              opts = nlo.opts.local.df,
                              data = data,
                              pp = "p_cml", 
                              t0 = t0, 
                              t1 = t1,
                              theta = theta,
                              sigma = sig,
                              w = "eta_m", 
                              st.code = "fips_state", 
                              min = p.min, 
                              max = p.max,
                              constr_mat = constr, 
                              IV_mat = IVs, 
                              min.crit = mc,
                              elas = T,
                              ub = rep(0, K),
                              lb = rep(min(IVs)/min(constr), K)
              )       
              # B4. Extract minimization results
              down <- res0$objective
              s1 <- res0$status
              it1 <- res0$iterations
              sol1 <- res0$solution
            }
            else {
              down <- target[["down"]]
              s1 <- target[["s1"]]
              it1 <- target[["it1"]]
              sol1 <- init.val0min
            }
            
            
            if (target[["it2"]] == 400) {
              # B5 Run maximization: derivative free 
              res0 <- nloptr( x0=init.val0max,
                              eval_f= max.av.non.marginal.change.parallel,
                              eval_g_ineq = eval_restrictions_nmarg_av,
                              opts = nlo.opts.local.df,
                              data = data,
                              pp = "p_cml", 
                              t0 = t0, 
                              t1 = t1,
                              theta = theta,
                              sigma = sig,
                              w = "eta_m", 
                              st.code = "fips_state", 
                              min = p.min, 
                              max = p.max,
                              constr_mat = constr, 
                              IV_mat = IVs, 
                              min.crit = mc,
                              elas = T,
                              ub = rep(0, K),
                              lb = rep(min(IVs)/min(constr), K)
              )       
              # B6. Extract minimization results
              up <- sol <- -res0$objective
              s2 <- res0$status
              it2 <- res0$iterations
              sol2 <- res0$solution
            }
            else {
              up <- target[["up"]]
              s2 <- target[["s2"]]
              it2 <- target[["it2"]]
              sol2 <- init.val0max
            }
            
          }
          ## F2c Export
          welfare.theta <- data.table(down, up, sc, D , K, sigma = sig, theta, s1, s2, it1, it2, sol1, sol2, R)
          results <- rbind(results, welfare.theta)
          fwrite(results, out.file.average)
          
        }
        
      }
    }    
  }  
}


