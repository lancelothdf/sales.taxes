##### Santiago Lacouture
#' Sales Taxes
#' Replication File. Updated on 8/18/2022
#' Step 13: Welfare extrapolation. Nationwide average.
#' We estimate in loops: the function itself computes the average in parallel


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
out.welfare.nationwide.av <- "Data/Replication/average_nationwide_extrapolation.csv"


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

## 5. Set up Ks, Ls, and scenarios (manually)
K.test <- c(2, 8)
L.test <- c(1, 2)
scenarios <- c("Original", "No Tax", "plus 5 Tax")

## 6. Set up Optimization Parameters (algorithm for now)
maxit <- 600
nlo.opts.local.df <- list(
  "algorithm"="NLOPT_LN_COBYLA",
  "maxeval" = maxit,
  "xtol_rel"=1.0e-8
)

## 7. Source extrapolation functions
source("Code/sales.taxes/Replication/welfare_functions.R")


## 8. Capture all possible combinations
combinations.all <- data.table(NULL)
for (sc in scenarios) {
  for (K in K.test) {
    for (comb in thetas.list) {
      for (L in L.test) {
        combinations.all <- rbind(combinations.all,
                              data.table(K = K, L = L,
                                         sc = sc, 
                                         sigma = comb$sigma,
                                         theta = comb$theta)
        )
      }
    }
  }
}



### Estimation ----


results <- data.table(NULL)
rep <- 0 # try first on baseline
# We do it by batches of "maxeval" number of iterations.
done <- F
attempt <- 1
remaining.up <- remaining.down <- NULL
while (!done) {
  flog.info("Starting attempt %s", attempt)
  # Capture existing results
  prev.res <- try(fread(out.welfare.nationwide.av))
  print(prev.res)
  
  # First time? Capture all combinations
  if (is.na(prev.res)) {
    remaining.up <- remaining.down <- combinations <- combinations.all
  }
  else {
    
    # Make sure prev results contain all desired combinations
    prev.res <- merge(prev.res, combinations.all, 
                      by = c("sc", "L", "K", "sigma", "theta"), all = T)
    
    # Capture remaining combinations
    remaining.down <- prev.res[s1 != 4 | it1 == maxit]
    remaining.up <- prev.res[s2 != 4 | it2 == maxit]
    # Combinations
    combinations <- rbind(remaining.up[mean(sol1), by = .(sc, L, K, sigma, theta)], 
                          remaining.down[mean(sol1), by = .(sc, L, K, sigma, theta)])
    combinations <- combinations[, -c("sol1")]
    
    # Save correct prebious results
    results <- prev.res[(s1 == 4 & it1 < maxit) |(s2 == 4 & it2 < maxit)]
    
  }
  flog.info("Remaining combinations: %s", nrow(combinations))
  
  ### Run estimation for combinations: each row
  for (nr in 1:nrow(combinations)) {
    
    # Capture values
    sc <- combinations[nr,][["sc"]]
    K <- combinations[nr,][["K"]]
    D <- combinations[nr,][["L"]]
    sig <- combinations[nr,][["sigma"]]
    theta <- combinations[nr,][["theta"]]
    
    flog.info("Estimating case: K = %s, L = %s, sigma = %s, theta = %s for %s case", K, D, sig, theta, sc)
    
    # Capture Scenario variables
    if (sc == "No Tax") {
      t0 <- "tauno"
      t1 <- "tau"
    } 
    if (sc == "plus 5 Tax")  {
      t0 <- "tau"
      t1 <- "tau5"
    }  

    ## B.1. Load Matrix of gamma (this extrictly depends on K since the basis change)
    in.file <- paste0(theta.berstein.sal, K,"_bern.csv")
    gamma.full.data <- fread(in.file)
    
    ## C.1 Extract support to use
    p.min <- res.pq[extrap == sc & sigma == sig & iter == rep][["min.p"]]
    p.max <- res.pq[extrap == sc & sigma == sig & iter == rep][["max.p"]]
    
    ## C.2 Restrict gamma file
    gamma <- gamma.full.data[extrap == sc & n.groups <= max(L.test) & sigma == sig & iter == rep][, c(paste0("b", 0:(K-1)), "n.groups"), with = F]           
    
  
    ## D1. Build the constraints matrix 
    constr <- as.matrix(gamma[n.groups == D][, -c("n.groups")]) 
    
    ## D2. Retrieve IVs
    IVs <- res.ivs[n.groups == D  & sigma == sig & iter == rep][["Estimate"]] 
    
    ## D3. Load min.criterion for case
    mc <- min.criteria[Deg == K & L == D & sigma == sig & extrap == sc & iter == rep,]
    mc <- mc[["min.criteria"]]
    
    
    ## D4. Generate an initial value somewhere in the middle to test algorithms
    # max
    if (is.null(remaining.up)) {
      init.val0max <- NULL
      if (is.null(results)) init.val0max <- get.init.val(constr, IVs, mc)
    }
    else {
      ## Retrieve previous results
      case <- data.table(sc, L = D , K, sigma = sig, theta)
      target <- merge(remaining.up, case, by = c("sc", "sigma", "theta", "K", "L"))
      
      # Capture previous solution if existent
      init.val0max <- target[["sol2"]]
      if (is.na(init.val0max)) init.val0max <- get.init.val(constr, IVs, mc)
      
      prevmin <- target[, .(it1 = mean(it1), down = mean(down), s1 = mean(s1)),
                        by = .(sc,sigma,theta,K,L)]
      prevmin.sol <- target[["sol1"]]
    }
    # min
    if (is.null(remaining.down)) {
      init.val0min <- NULL
      if (is.null(results)) init.val0min <- get.init.val(constr, IVs, mc)
    }
    else {
      ## Retrieve previous results
      case <- data.table(sc, D , K, sigma = sig, theta)
      target <- merge(remaining.down, case, by = c("sc", "sigma", "theta", "K", "D"))
      
      # Capture previous solution if existent
      init.val0min <- target[["sol1"]]
      if (is.na(init.val0max)) init.val0min <- get.init.val(constr, IVs, mc)
      
      prevmax <- target[, .(it2 = mean(it2), up = mean(up), s2 = mean(s2)),
                        by = .(sc,sigma,theta,K,L)]
      prevmax.sol <- target[["sol2"]]
    }    
    if (!is.null(init.val0min)) {
      print("Initial value Min is")
      print("init.val0min")
    }
    if (!is.null(init.val0max)) {
      print("Initial value Max is")
      print("init.val0max")
    }
    
    ## E. Estimate for each case
    if (sc == "Original") {
      # F2. Marginal change
      # F2a1. Min calculation
      if (!is.null(init.val0min)) {
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
      }
      else {
        down <- prevmin$down
        s1 <- prevmin$s1
        it1 <- prevmin$it1
        sol1 <- prevmin.sol       
      }

      if (!is.null(init.val0max)) {
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
        up <- prevmax$up
        s2 <- prevmax$s2
        it2 <- prevmax$it2
        sol2 <- prevmax.sol       
      }      
    }
    else {
      # F2. Non Marginal change
      # B3 Run minimization: derivative free 
      if (!is.null(init.val0min)) {
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
        down <- prevmin$down
        s1 <- prevmin$s1
        it1 <- prevmin$it1
        sol1 <- prevmin.sol       
      }
      # B3 Run maximization: derivative free 
      if (!is.null(init.val0max)) {
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
        up <- prevmax$up
        s2 <- prevmax$s2
        it2 <- prevmax$it2
        sol2 <- prevmax.sol       
      }   
 
    }
    ## F2c Export
    welfare.theta <- data.table(down, up, sc, L=D , K, 
                                sigma = sig, theta, s1, s2, 
                                it1, it2, sol1, sol2)
    results <- rbind(results, welfare.theta)
    fwrite(results, out.welfare.nationwide.av)
  }
  # Check results
  if (nrow(results[s1 != 4 | it1 == maxit]) + nrow(results[s2 != 4 | it2 == maxit]) == 0) done <- T
  else attempt <- attempt + 1
}


