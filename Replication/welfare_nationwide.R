##### Santiago Lacouture
#' Sales Taxes
#' Replication File. Updated on 8/18/2022
#' Step 13: Welfare extrapolation. Nationwide average


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
binned.data.tax <- "Data/Replication/extraction_state_binned_tax.csv"
iv.output.salience.results.file <- "Data/Replication/Demand_iv_sat_initial_price_semester_salience.csv"
theta.berstein.sal <- "Data/Replication/Demand_gamma_sat_initial_price_semester_salience_K"
pq.output.salience.results.file <- "Data/Replication/Demand_pq_sat_initial_price_semester_salience.csv"
out.file.mc.welf <- "Data/Replication/mincriteria_welfare_boot.csv"
conduct.parameter.at.p <- "Data/Replication/salience_conduct_parameter_at_p.csv"


### General setup ----

# 0. Parallelize options
# use the environment variable SLURM_NTASKS_PER_NODE to set the number of cores
n.cores <- Sys.getenv("SLURM_NTASKS_PER_NODE")
print(paste0("Using ", n.cores, " cores"))
registerDoParallel(cores=n.cores)

# 1. Open data and define hypothetical non-marginal changes
data <- fread(binned.data.tax)
data[, p_cml := p_m - tau]
data[, tauno := 0]
data[, tau5 := tau + log(1+0.05)]


## 2. Values of sigma to Tests and conduct parameters 
# In practice, this was done in batches to avoid running all cases and exceed the running time
sigmas.test <- c(0.25, 0.5, 0.75, 1)

## Conduct parameters
thetas <- fread(conduct.parameter.at.p)
thetas <- thetas[K==2] # We use the K=2 estimates

# Capture values of interest into a list
thetas.list <- list()
for (sig in sigmas.test) {
  
  # theta = 0 
  thetas.list[[paste0("s",sig*100,"-th.Inf")]] <- list(
    sigma = sig, theta = 0)
  # theta estimated when e_s = Inf
  thetas.list[[paste0("s",sig*100,"-Inf")]] <- list(
    sigma = sig, theta = av.theta[sigma==sig & is.infinite(es.val), mean(theta)])
  # theta estimated when e_s = 1
  thetas.list[[paste0("s",sig*100,"-1")]] <- list(
    sigma = sig, theta = av.theta[sigma==sig & es.val == 1, mean(theta)])
  
}


## 4. Set up IV estimates for each sigma
IVs <- fread(iv.output.salience.results.file)
IVs <- IVs[controls == "division_by_module_by_time"]

# Adjust accordingly
IVs2 <- dcast(IVs, n.groups + lev + sigma + iter ~ outcome,  fun=sum, value.var = c("Estimate"))
IVs2[, w.ln_cpricei2 := w.ln_cpricei2_sig0.25 + w.ln_cpricei2_sig0.5 + w.ln_cpricei2_sig0.75 + w.ln_cpricei2_sig1]
IVs2[, Estimate := w.ln_quantity3/w.ln_cpricei2]
IVs2 <- IVs2[, c("n.groups", "lev", "sigma", "iter", "Estimate")]
# Merge and Order appropiately
res.ivs <- IVs2[order(iter, sigma, n.groups, lev)]
rm(IVs, IVs2)

## 5. Open Min - Max files and min criteria
res.pq <- fread(pq.output.salience.results.file)
min.criteria <- fread(out.file.mc.welf)


## 6. Set up Ks, Ls, and scenarios (manually)
K.test <- c(2, 8)
L.test <- c(1, 2)
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
rep <- 0 # try first on baseline

for (sc in scenarios) {
  flog.info("Starting Scenario %s", sc)
  
  if (sc == "No Tax") {
    t0 <- "tauno"
    t1 <- "tau"
  } 
  if (sc == "plus 5 Tax")  {
    t0 <- "tau"
    t1 <- "tau5"
  }
  
  for (K in K.test) {
    flog.info("... Estimating for K=%s in Starting Scenario %s",K, sc)
    ## B.1. Load Matrix of gamma (this extrictly depends on K since the basis change)
    in.file <- paste0(theta.berstein.sal, K,"_bern.csv")
    gamma.full.data <- fread(in.file)
    
    ## C. Loop across sigmas x theta
    for (comb in thetas.list) {
      # Capture value of sigma and thetas to test
      sig = comb$sigma
      theta <- comb$theta
      ## C.1 Extract support to use
      p.min <- res.pq[extrap == sc & sigma == sig & iter == rep][["min.p"]]
      p.max <- res.pq[extrap == sc & sigma == sig & iter == rep][["max.p"]]
      
      ## C.2 Restrict gamma file
      gamma <- gamma.full.data[extrap == sc & n.groups < max(L.test) & sigma == sig & iter == rep][, c(paste0("b", 0:(K-1)), "n.groups"), with = F]           
      
      ## D Start Loop at number of groups
      for (D in L.test) { #unique(gamma$n.groups)
        flog.info("....  Focus now: L = %, sigma = %s and theta = %s", L, sig, theta)
        
        ## D1. Build the constraints matrix 
        constr <- as.matrix(gamma[n.groups == D][, -c("n.groups")]) 
        
        ## D2. Retrieve IVs
        IVs <- res.ivs[n.groups == D  & sigma == sig & iter == rep][["Estimate"]] 
        
        ## D3. Load min.criterion for case
        mc <- min.criteria[K == K & L == D & sigma == sig & extrap == sc & iter == rep,][["min.criteria"]]
        
        ## D4. Generate an initial value somewhere in the middle to test algorithms
        init.val0max <- init.val0min <- get.init.val(constr, IVs, mc)
        
        ## E. Estimate for each case
        ## Retrieve previous results
        # case <- data.table(sc, D , K, sigma = sig, theta)
        # target <- merge(prev.sol, case, by = c("sc", "sigma", "theta", "K", "D"))
        # 
        # init.val0min <- target[["sol1"]]
        # init.val0max <- target[["sol2"]]
        # target <- target[, .(it1 = mean(it1), it2 = mean(it2),
        #                      down = mean(down), up = mean(up),
        #                      s1 = mean(s1), s2 = mean(s2)), by = .(sc,sigma,theta,K,D)]
        
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
          
          # if (target[["it1"]] == 400) {
          #   res0 <- nloptr( x0=init.val0min,
          #                   eval_f= av.non.marginal.change.parallel,
          #                   eval_g_ineq = eval_restrictions_nmarg_av,
          #                   opts = nlo.opts.local.df,
          #                   data = data,
          #                   pp = "p_cml", 
          #                   t0 = t0, 
          #                   t1 = t1,
          #                   theta = theta,
          #                   sigma = sig,
          #                   w = "eta_m", 
          #                   st.code = "fips_state", 
          #                   min = p.min, 
          #                   max = p.max,
          #                   constr_mat = constr, 
          #                   IV_mat = IVs, 
          #                   min.crit = mc,
          #                   elas = T,
          #                   ub = rep(0, K),
          #                   lb = rep(min(IVs)/min(constr), K)
          #   )       
          #   # B4. Extract minimization results
          #   down <- res0$objective
          #   s1 <- res0$status
          #   it1 <- res0$iterations
          #   sol1 <- res0$solution
          # }
          # else {
          #   down <- target[["down"]]
          #   s1 <- target[["s1"]]
          #   it1 <- target[["it1"]]
          #   sol1 <- init.val0min
          # }
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
          
          
          # if (target[["it2"]] == 400) {
          #   # B5 Run maximization: derivative free 
          #   res0 <- nloptr( x0=init.val0max,
          #                   eval_f= max.av.non.marginal.change.parallel,
          #                   eval_g_ineq = eval_restrictions_nmarg_av,
          #                   opts = nlo.opts.local.df,
          #                   data = data,
          #                   pp = "p_cml", 
          #                   t0 = t0, 
          #                   t1 = t1,
          #                   theta = theta,
          #                   sigma = sig,
          #                   w = "eta_m", 
          #                   st.code = "fips_state", 
          #                   min = p.min, 
          #                   max = p.max,
          #                   constr_mat = constr, 
          #                   IV_mat = IVs, 
          #                   min.crit = mc,
          #                   elas = T,
          #                   ub = rep(0, K),
          #                   lb = rep(min(IVs)/min(constr), K)
          #   )       
          #   # B6. Extract minimization results
          #   up <- sol <- -res0$objective
          #   s2 <- res0$status
          #   it2 <- res0$iterations
          #   sol2 <- res0$solution
          # }
          # else {
          #   up <- target[["up"]]
          #   s2 <- target[["s2"]]
          #   it2 <- target[["it2"]]
          #   sol2 <- init.val0max
          # }
          
        }
        ## F2c Export
        welfare.theta <- data.table(down, up, sc, L=D , K, 
                                    sigma = sig, theta, s1, s2, 
                                    it1, it2, sol1, sol2)
        results <- rbind(results, welfare.theta)
      }
    }
  }
}


