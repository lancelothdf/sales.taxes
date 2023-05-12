##### Santiago Lacouture
#' Sales Taxes
#' Replication File. Updated on 4/24/2023
#' Step 13a: Welfare extrapolation. Nationwide average.
#' We estimate in loops: the function itself computes the average in parallel
#' This version: produces 2 files, one with final results and another with results in progress


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


setwd("/project/igaarder")


### input and output paths ----
# inputs
binned.data.tax <- "Data/Replication_v4/extraction_state_binned_tax.csv"
iv.output.salience.results.file <- "Data/Replication_v4/Demand_iv_sat_initial_price_semester_salience.csv"
theta.berstein.sal <- "Data/Replication_v4/Demand_gamma_sat_initial_price_semester_salience_K_division"
pq.output.salience.results.file <- "Data/Replication_v4/Demand_pq_sat_initial_price_semester_salience_division.csv"
out.file.mc.welf <- "Data/Replication_v4/mincriteria_welfare_boot_division.csv"
conduct.parameter.at.p <- "Data/Replication_v4/salience_conduct_parameter_at_p_division.csv"
# Output
out.welfare.nationwide.av <- "Data/Replication_v4/average_nationwide_extrapolation_division_smallv3.csv"
sol.welfare.nationwide.av <- "Data/Replication_v4/average_nationwide_temp_progress_division_smallv3.csv"


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
#sigmas.test <- c(0.25, 0.5, 0.75, 1)
sigmas.test <- c(0.25, 0.5, 1)

## Conduct parameters
thetas <- fread(conduct.parameter.at.p)
thetas <- thetas[K==2] # We use the K=2 estimates

# Capture values of interest into a list
thetas.list <- list()

## HERE instead of getting implied theta at all levels of salience - only get estimates of theta for salience = 1
sig <- 1    
  # theta = 0 
  thetas.list[[paste0("s",sig*100,"-th.Inf")]] <- list(
    sigma = sig, theta = 0)
  # theta estimated when e_s = Inf
  thetas.list[[paste0("s",sig*100,"-Inf")]] <- list(
    sigma = sig, theta = round(thetas[sigma==sig & is.infinite(es.val), mean(theta)], 6))

  

#for (sig in sigmas.test) {
  
#  # theta = 0 
#  thetas.list[[paste0("s",sig*100,"-th.Inf")]] <- list(
#    sigma = sig, theta = 0)
#  # theta estimated when e_s = Inf
#  thetas.list[[paste0("s",sig*100,"-Inf")]] <- list(
#    sigma = sig, theta = round(thetas[sigma==sig & is.infinite(es.val), mean(theta)], 6))
#  ## theta estimated when e_s = 1
#  #thetas.list[[paste0("s",sig*100,"-1")]] <- list(
#  #  sigma = sig, theta = round(thetas[sigma==sig & es.val == 1, mean(theta)], 6))
  
#}

## 3. Set up IV estimates for each sigma
IVs <- fread(iv.output.salience.results.file)
IVs <- IVs[controls == "division_by_module_by_time"]

# Adjust accordingly
IVs2 <- dcast(IVs, n.groups + lev + sigma + iter ~ outcome,  fun=sum, value.var = c("Estimate"))
IVs2[, DL.ln_cpricei2 := DL.ln_cpricei2_sig0.25 + DL.ln_cpricei2_sig0.5 + DL.ln_cpricei2_sig0.75 + DL.ln_cpricei2_sig1]
IVs2[, Estimate := DL.ln_quantity3/DL.ln_cpricei2]
IVs2 <- IVs2[, c("n.groups", "lev", "sigma", "iter", "Estimate")]
# Merge and Order appropiately
res.ivs <- IVs2[order(iter, sigma, n.groups, lev)]
rm(IVs, IVs2)

## 4. Open Min - Max files and min criteria
res.pq <- fread(pq.output.salience.results.file)
min.criteria <- fread(out.file.mc.welf)
setnames(min.criteria, "K", "Deg") # for some reason K is confused

## 5. Set up Ks, Ls, and scenarios (manually)
K.test <- c(2, 4, 8)
L.test <- c(1, 2)
scenarios <- c("Original", "No Tax", "plus 5 Tax")

## 6. Set up Optimization Parameters (algorithm for now)
maxit <- 5000
nlo.opts.local.df <- list(
  "algorithm"="NLOPT_LN_COBYLA",
  "maxeval" = maxit,
  "xtol_rel"=1.0e-8
)

## 7. Source extrapolation functions
source("/project2/igaarder/Code/sales.taxes/Replication/Replication_v4/welfare_functions_v4.R")


## 8. Capture all possible combinations
#combinations.all <- data.table(NULL)
#for (sc in scenarios) {
#  for (K in K.test) {
#    for (comb in thetas.list) {
#      for (L in L.test) {
#        combinations.all <- rbind(combinations.all,
#                              data.table(K = K, L = L,
#                                         sc = sc, 
#                                         sigma = comb$sigma,
#                                         theta = comb$theta)
#         )
#      }
#    }
#  }
#}

combinations.all <- data.table(K = rep(8,7), L = c(1, 2, 1, 2, 2, 2, 1), sc = c(rep("Original", 2), rep("plus 5 Tax", 4), "Original"), sigma = c(0.75, 0.5, 0.5, 0.25, 0.5, 0.75, 0.5), theta = rep(0, 7))


### Add different salience scenarii (assume theta = 0 for all these)
#combinations.all.salience <- data.table(NULL)
#for (sc in scenarios) {
#  for (K in K.test) {
#    for (sig in c(0.25,0.5,0.75)) {
#      for (L in L.test) {
#        combinations.all.salience <- rbind(combinations.all.salience,
#                                      data.table(K=K, L=L,
#                                                 sc = sc,
#                                                 sigma = sig,
#                                                 theta = 0)
#          )
#      }
#    }
#  }
#}

#combinations.all <- rbind(combinations.all, combinations.all.salience)



### Estimation ----
rep <- 0 # try only on baseline


# We do it by batches of "maxeval" number of iterations.
done <- F
while (!done) {
  # Capture existing results
  new <- !file.exists(out.welfare.nationwide.av) | !file.exists(sol.welfare.nationwide.av)
  

  # First time? Capture all combinations
  if (new) {
    print("No previous results found. Starting from 0")
    combinations <- copy(combinations.all)
    results <- data.table(NULL)
    attempt <- 1
    
    #### Estimates for Linear Case
    # FOR LINEAR Estimates we don't need to normalize!!! The coefficient is directly interpretable (contrary to non-linear, where matrices are normalized)
    min <- 0
    max <- 1
    for (comb in thetas.list) {
      theta <- comb$theta
      sig <- comb$sigma
      
      ## Capture coef in lin case
      lin <- res.ivs[n.groups == 1 & sigma == sig & iter == rep][["Estimate"]] 
      
      # Marginal Change
      value <-av.marginal.change(mu = lin, data = data, "p_cml", "tau", theta, sig, "eta_m",
                                 "fips_state", min, max, 0, 0)
      results <- rbind(results, data.table(est = "", sc = "Original", value, theta, sigma = sig, K = 1, L = 1, it.n=1))
      
      # Non Marginal Change: No tax
      t0 <- "tauno"
      t1 <- "tau"
      sc <- "No Tax"
      value <- av.non.marginal.change.parallel(lin, data, "p_cml", t0, t1, theta, sig, "eta_m", 
                                               "fips_state", min, max, 0, 0)
      results <- rbind(results, data.table(est = "", sc, value, theta, sigma = sig, K = 1, L = 1, it.n=1))
      
      # Non Marginal Change: Plus 5 tax
      t0 <- "tau"
      t1 <- "tau5"
      sc <- "plus 5 Tax"
      value <- av.non.marginal.change.parallel(lin, data, "p_cml", t0, t1, theta, sig, "eta_m",  
                                               "fips_state", min, max, 0, 0)
      results <- rbind(results, data.table(est = "", sc, value, theta, sigma = sig, K = 1, L = 1, it.n=1))
      
    }

  }
  else {
    
    ## Identify cases to be solved
    prev.res <- fread(out.welfare.nationwide.av)
    flog.info("Number of estimates in previous results is %s", nrow(prev.res))
    # make sure they are unique to avoid keeping extra
    prev.res <- prev.res[!duplicated(prev.res[, c('est', 'sc', 'L', 'K', 'sigma', 'theta')]),]
    flog.info("Number of non-duplicated estimates in previous results is %s", nrow(prev.res))

    # which have both solutions?
    done.prev.res <- copy(prev.res)
    done.prev.res <- done.prev.res[, nest := .N, by = c('sc', 'L', 'K', 'sigma', 'theta')]
    done.prev.res <- done.prev.res[nest == 2]
    flog.info("Number of complete cases estimates in previous results is %s", nrow(done.prev.res))
    # Collapse to merge with all and identify remaining cases
    done.prev.res <- done.prev.res[, .(complete = mean(nest)-1), by = c('sc', 'L', 'K', 'sigma', 'theta')]
    flog.info("Number of complete cases in previous results is %s", nrow(done.prev.res))
    combinations.all.it <- merge(combinations.all, done.prev.res, 
                              by = c("sc", "L", "K", "sigma", "theta"),
                              all.x = T)
    
    combinations <- combinations.all.it[is.na(complete)]
    results <- copy(prev.res)
    
    ## Open previous attempt progress
    prev.sol <- fread(sol.welfare.nationwide.av)
    
    # Capture prev. attempt number and add one
    attempt <- max(prev.sol$attempt) + 1
  }
  
  flog.info("Starting attempt %s", attempt)
  flog.info("Remaining combinations: %s", nrow(combinations))
  prog.results <- data.table(NULL)
  print(combinations)
  
  ### Run estimation for combinations: each row
  for (nr in 1:nrow(combinations)) {
    
    # Capture values
    sc <- combinations[nr,][["sc"]]
    K <- combinations[nr,][["K"]]
    D <- combinations[nr,][["L"]]
    sig <- combinations[nr,][["sigma"]]
    theta <- combinations[nr,][["theta"]]
    

    flog.info("Estimating case %s out of %s: K = %s, L = %s, sigma = %s, theta = %s for %s", 
              nr, nrow(combinations),  K, D, sig, theta, sc)
    
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
    
    
    ## D4. Initial values: either capture previous attempt or generate an 
    # initial value somewhere in the middle to test algorithms
    # Reinitialize initial values
    init.val0min <- init.val0max <- NULL
    prev.min <- prev.max <- NULL
    
    # First attempt ever?
    if (new) {
      init.val0max <- init.val0min <- get.init.val(constr, IVs, mc)
      flog.info("Capturing starting values: using random initial value since new attempt")
    }
    # Recover previous solutions if not
    else {
      
      case <- data.table(sc, L = D , K, sigma = sig, theta)
      prev.attempt.case <- merge(case, prev.sol, 
                                 by =  c("sc", "sigma", "theta", "K", "L"),
                                 all.x = T)
      print(head(prev.attempt.case))
      # minimization
      prev.min <- prev.attempt.case[est == "LB"]
      # Check we have the solution if previous attempt not found
      if (nrow(prev.min) == 0) {
        prev.res.case <- merge(case, prev.res,
                               by =  c("sc", "sigma", "theta", "K", "L"),
                               all.x = T)
        print(head(prev.res.case))
        if (nrow(prev.res.case[est == "LB"]) == 0) {
          init.val0min <- get.init.val(constr, IVs, mc)
          flog.info("Capturing starting values: min missing from previous attempt")
        }
        else flog.info("Minimization already solved for this case")
        
      }
      else {
        init.val0min <- prev.min[["sol"]]
        flog.info("Capturing starting values: recovered previous attempt for min")
      }
      
      # maximization
      prev.max <- prev.attempt.case[est == "UB"]
      # Check we have the solution if previous attempt not found
      if (nrow(prev.max) == 0) {
        prev.res.case <- merge(case, prev.res,
                               by =  c("sc", "sigma", "theta", "K", "L"),
                               all.x = T)
        print(head(prev.res.case))
        if (nrow(prev.res.case[est == "UB"]) == 0) {
          init.val0max <- get.init.val(constr, IVs, mc)
          flog.info("Capturing starting values: max missing from previous attempt")
        }
        else flog.info("Maximization already solved for this case")
      }
      else {
        init.val0max <- prev.max[["sol"]]      
        flog.info("Capturing starting values: recovered previous attempt for max")
      }
    }
    

    ## E. Estimate for each case
    if (sc == "Original") {
      # E1. Marginal change
      # E1a1. Min calculation
      if (!is.null(init.val0min)) {
        flog.info("Running minimization")
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
        # E1a2. Extract and export minimization results
        if (res0$iterations == maxit) {
          progress.sol <- data.table(est = "LB", sc, L=D , K, 
                                     sigma = sig, theta, status = res0$status, 
                                     it.n = res0$iterations, 
                                     sol = res0$solution, attempt)
          prog.results <- rbind(prog.results, progress.sol)
          fwrite(prog.results, sol.welfare.nationwide.av)
        }
        else{
          welfare.theta <- data.table(est = "LB", value = res0$objective, 
                                      sc, L=D , K, 
                                      sigma = sig, theta, 
                                      it.n = res0$iterations + (attempt - 1)*600)
          results <- rbind(results, welfare.theta)
          fwrite(results, out.welfare.nationwide.av) 
        }
        
      }
      if (!is.null(init.val0max)) {
        # E1b1. Max calculation
        flog.info("Running maximization")
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
        # E1b2 Results extraction
        if (res0$iterations == maxit) {
          progress.sol <- data.table(est = "UB", sc, L=D , K, 
                                     sigma = sig, theta, status = res0$status, 
                                     it.n = res0$iterations, 
                                     sol = res0$solution, attempt)
          prog.results <- rbind(prog.results, progress.sol)
          fwrite(prog.results, sol.welfare.nationwide.av)
        }
        else{
          welfare.theta <- data.table(est = "UB", value = -res0$objective, 
                                      sc, L=D , K, 
                                      sigma = sig, theta, 
                                      it.n = res0$iterations + (attempt - 1)*600)
          results <- rbind(results, welfare.theta)
          fwrite(results, out.welfare.nationwide.av)
          
        }   
      }
    }
    else {
      # E2. Non Marginal change
      # E2a1 Run minimization: derivative free 
      if (!is.null(init.val0min)) {
        flog.info("Running minimization")
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
          # E2a2 Extract and export minimization results
          if (res0$iterations == maxit) {
            progress.sol <- data.table(est = "LB", sc, L=D , K, 
                                       sigma = sig, theta, status = res0$status, 
                                       it.n = res0$iterations, 
                                       sol = res0$solution, attempt)
            prog.results <- rbind(prog.results, progress.sol)
            fwrite(prog.results, sol.welfare.nationwide.av)
          }
          else{
            welfare.theta <- data.table(est = "LB", value = res0$objective, 
                                        sc, L=D , K, 
                                        sigma = sig, theta, 
                                        it.n = res0$iterations + (attempt - 1)*600)
            results <- rbind(results, welfare.theta)
            fwrite(results, out.welfare.nationwide.av) 
          }
      }
      if (!is.null(init.val0max)) {
          # E2b1 Run maximization: derivative free 
          flog.info("Running maximization")
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
          # E2b2 Extract and export maximization results
          if (res0$iterations == maxit) {
            progress.sol <- data.table(est = "UB", sc, L=D , K, 
                                       sigma = sig, theta, status = res0$status, 
                                       it.n = res0$iterations, 
                                       sol = res0$solution, attempt)
            prog.results <- rbind(prog.results, progress.sol)
            fwrite(prog.results, sol.welfare.nationwide.av)
          }
          else{
            welfare.theta <- data.table(est = "UB", value = -res0$objective, 
                                        sc, L=D , K, 
                                        sigma = sig, theta, 
                                        it.n = res0$iterations + (attempt - 1)*600)
            results <- rbind(results, welfare.theta)
            fwrite(results, out.welfare.nationwide.av)
            
          }
        }
    }
  }
  
  # Check results, are we done?
  print(head(prog.results[, .N , by = c("est", "sc", "sigma", "theta", "K", "L")]))
  if (nrow(prog.results) == 0) done <- T
}


