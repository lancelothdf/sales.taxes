##### Santiago Lacouture
#' Sales Taxes
#' Replication File. Updated on 9/13/2022
#' Step 14(a): Welfare extrapolation. By state. Marginal changes
#' We estimate state-level welfare extrapolation these in different codes
#' This one focuses on marginal changes: it uses the original distribution only



library(data.table)
library(futile.logger)
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
# Output
out.welfare.state.marg <- "Data/Replication/state_welfare_extrapolation_marginal.csv"


# 0. Parallelize options
# use the environment variable SLURM_NTASKS_PER_NODE to set the number of cores
n.cores <- Sys.getenv("SLURM_NTASKS_PER_NODE")
print(paste0("Using ", n.cores, " cores"))
registerDoParallel(cores=n.cores)


# 1. Open data
data <- fread(binned.data.tax)
data[, p_cml := p_m - tau]

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
    sigma = sig, theta = thetas[sigma==sig & is.infinite(es.val), mean(theta)])
  # theta estimated when e_s = 1
  thetas.list[[paste0("s",sig*100,"-1")]] <- list(
    sigma = sig, theta = thetas[sigma==sig & es.val == 1, mean(theta)])
  
}


## 3. Set up IV estimates for each sigma
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



## 4. Open Min - Max files and min criteria
res.pq <- fread(pq.output.salience.results.file)
min.criteria <- fread(out.file.mc.welf)
setnames(min.criteria, "K", "Deg") # for some reason K is confused

## 5. Set up Ks, Ls (manually), and states (from data)
K.test <- c(2, 8)
L.test <- c(1, 2)
states.test <- unique(data$fips_state)

## 6. Set up Optimization Parameters (algorithm for now)
maxit <- 8000
nlo.opts.local.df <- list(
  "algorithm"="NLOPT_LN_COBYLA",
  "maxeval" = maxit,
  "xtol_rel"=1.0e-8
)

## 7. Source extrapolation functions
source("Code/sales.taxes/Replication/welfare_functions.R")


## 8. Capture all possible combinations
combinations.all.states <- data.table(NULL)
for (K in K.test) {
  for (comb in thetas.list) {
    for (L in L.test) {
      for (state in states.test) {
        combinations.all.states <- rbind(combinations.all.states,
                                  data.table(K = K, L = L,
                                             sigma = comb$sigma,
                                             theta = comb$theta,
                                             state)
        )
      }
    }
  }
}




### Estimation ----

rep <- 0 # try only on baseline
# We do it by batches of "maxeval" number of iterations.
done <- F
while (!done) {
  # Capture existing results
  new <- !file.exists(out.welfare.state.marg) 
  
  
  # First time? Capture all combinations
  if (new) {
    print("No previous results found. Starting from 0")
    combinations.states <- copy(combinations.all.states)
    combinations <- combinations.all.states[, .(N=.N), by = c("L", "K", "sigma", "theta")]
    combinations <- combinations[N>0]
    results <- data.table(NULL)
    attempt <- 1
    
    #### Estimates for Linear Case
    # FOR LINEAR Estimates we don't need to normalize!!! The coefficient is directly interpretable (contrary to non-linear, where matrices are normalized)
    min <- 0
    max <- 1
    for (state in states.test) {
      data.st <- data[fips_state == state,]
      for (comb in thetas.list) {
        theta <- comb$theta
        sig <- comb$sigma
        
        ## Capture min/max and coef in lin case
        lin <- res.ivs[n.groups == 1 & sigma == sig & iter == rep][["Estimate"]] 
        
        ## Marginal Change
        value <- marginal.change(lin, data.st, "p_cml", "tau", theta, sig, "eta_m", min, max, 0, 0)
        results<- rbind(results, data.table(state, value, est = "", theta, sigma = sig, K = 1, L = 1, it.n=1, s=4, attempt = 1))
        
        
      }
    }
    
  }
  else {
    
    ## Identify cases to be solved
    prev.res <- fread(out.welfare.state.marg)
    prev.res.K1 <-  prev.res[K==1]
    prev.res <- prev.res[K!=1]
    flog.info("Number of estimates in previous results is %s", nrow(prev.res))
    # make sure they are unique to avoid keeping extra
    prev.res <- prev.res[!duplicated(prev.res[, c('est', 'L', 'K', 'sigma', 'theta', 'state')]),]
    flog.info("Number of non-duplicated estimates in previous results is %s", nrow(prev.res))
    
    # which have both solutions?
    done.prev.res <- copy(prev.res)
    done.prev.res <- done.prev.res[s!=5, nest := .N, by = c('L', 'K', 'sigma', 'theta', 'state')]
    done.prev.res <- done.prev.res[nest == 2]
    flog.info("Number of complete cases estimates in previous results is %s", nrow(done.prev.res))
    # Collapse to merge with all and identify remaining cases
    done.prev.res <- done.prev.res[, .(complete = mean(nest)-1), by = c('L', 'K', 'sigma', 'theta', 'state')]
    flog.info("Number of complete cases in previous results is %s", nrow(done.prev.res))
    combinations.states <- merge(combinations.all.states, done.prev.res, 
                              by = c("L", "K", "sigma", "theta", 'state'),
                              all.x = T)
    
    combinations <- combinations.states[is.na(complete), .(N=.N), by = c("L", "K", "sigma", "theta")]
    combinations <- combinations[N>0]
    
    ## Keep previous progress
    results <- rbind(prev.res.K1, prev.res)
    results <- results[s!=5]
    
    # Capture prev. attempt number and add one
    attempt <- max(results$attempt) + 1
  }
  
  flog.info("Starting attempt %s", attempt)
  flog.info("Remaining combinations: %s", nrow(combinations))
  prog.results <- data.table(NULL)
  print(combinations)
  
  ### Run estimation for combinations: each row
  for (nr in 1:nrow(combinations)) {
    
    # Capture values
    K <- combinations[nr,][["K"]]
    D <- combinations[nr,][["L"]]
    sig <- combinations[nr,][["sigma"]]
    theta <- combinations[nr,][["theta"]]
    
    # A.1 Identify missing states for combination
    case <- merge(combinations[nr,], combinations.states, by = c("L", "K", "sigma", "theta"))
    states.case <- case[["state"]]

    # A.2 Modify maximum iterations: add maxit
    nlo.opts.local.df[["maxeval"]] <- maxit*attempt
    
    
    flog.info("Estimating case %s out of %s: K = %s, L = %s, sigma = %s, theta = %s, for %s states", 
              nr, nrow(combinations),  K, D, sig, theta, length(states.case))
  
    ## B.1. Load Matrix of gamma (this extrictly depends on K since the basis change)
    in.file <- paste0(theta.berstein.sal, K,"_bern.csv")
    gamma.full.data <- fread(in.file)
  
    ## C.1 Extract support to use
    p.min <- res.pq[extrap == "Original" & sigma == sig][["min.p"]]
    p.max <- res.pq[extrap == "Original" & sigma == sig][["max.p"]]
    
    ## C.2 Restrict gamma file. Constant across p
    gamma <- gamma.full.data[extrap == "Original" & n.groups <= max(L.test) & sigma == sig & iter == rep][, c(paste0("b", 0:(K-1)), "n.groups"), with = F]             ## For elasticity

    ## D1. Build the constraints matrix 
    constr <- as.matrix(gamma[n.groups == D][, -c("n.groups")])   ## For elasticity
    
    ## D2. Retrieve IVs
    IVs <- res.ivs[n.groups == D  & sigma == sig & iter == rep][["Estimate"]] 
    
    ## D3. Load min.criterion for case
    mc <- min.criteria[Deg == K & L == D & sigma == sig & extrap == "Original" & iter == rep,][["min.criteria"]]
      
    ## D3. Load min.criterion for case
    mc <- min.criteria[Deg == K & L == D & sigma == sig & extrap == sc & iter == rep,]
    mc <- mc[["min.criteria"]]
    
    ## D4. Reinitialize initial values
    init.val0max <- init.val0min <- get.init.val(constr, IVs, mc) 
    
    ## F Loop across states
    welfare.st <- foreach (state= states.case, .combine=rbind) %dopar% {
      
      # F1. Subset data
      st.data <- data[fips_state == state,]
      # F2. Marginal change
      # F2a1. Min calculation
      res0 <- nloptr( x0=init.val0,
                      eval_f= marginal.change,
                      eval_g_ineq = eval_restrictions_marg,
                      opts = nlo.opts.local.df,
                      data = st.data,
                      pp = "p_cml",
                      tau = "tau",
                      theta = theta,
                      sigma = sig,
                      w = "eta_m", 
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
      # F2b1. Max calculation
      res0 <- nloptr( x0=init.val0,
                      eval_f= max.marginal.change,
                      eval_g_ineq = eval_restrictions_marg,
                      opts = nlo.opts.local.df,
                      data = st.data,
                      pp = "p_cml",
                      tau = "tau",
                      theta = theta,
                      sigma = sig,
                      w = "eta_m", 
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
      
      ## F2c Export
      rbind(data.table(est = "LB", value = down, state, L=D , K, sigma = sig, theta, s = s1, it = it1 + maxit*(attempt-1)),
            data.table(est = "UB", value = up,   state, L=D , K, sigma = sig, theta, s = s2, it = it2 + maxit*(attempt-1))
            )
    }
    results <- rbind(results, welfare.st[, attempt := attempt])
    fwrite(results, out.welfare.state.marg)
  }
  # Check results, are we done?
  if (nrow(results[s==5]) == 0) done <- T
}
