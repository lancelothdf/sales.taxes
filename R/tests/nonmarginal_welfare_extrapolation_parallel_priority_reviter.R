## Welfare Extrapolations actual data
# Non-Marginal Case
# Priority Non-convergent Cases

library(Matrix)
library(data.table)
library(ggplot2)
library(zoo)
library(tidyverse)
library(stringr)
library(nloptr)
library(doParallel)
library(MASS)
library(pracma)

setwd("/project2/igaarder")

# Load Code
source("Code/sales.taxes/R/tests/welfare_formulae_nlopt.R")

## Output files
out.file.nonmarginal.c <- "Data/nonmarginal_extrapolation_state_priority_notax_con_notax.csv"
out.file.nonmarginal.r <- "Data/nonmarginal_extrapolation_state_priority_notax_rev_notax.csv"


# 0. Parallelize options
# use the environment variable SLURM_NTASKS_PER_NODE to set the number of cores
registerDoParallel(cores=(Sys.getenv("SLURM_NTASKS_PER_NODE")))

# 1. Open data
data <- fread("Data/extraction_state_binned_price.csv")
data[, p_cml := p_m - tau]
data[, tauno := 0]
data[, tau5 := tau + log(1+0.05)]

# 2. Open IV estimates
IVs <- fread("Data/Demand_iv_sat_initial_price_semester_salience.csv")
IVs <- IVs[controls == "division_by_module_by_time"]

# 3. Values to test
prev.sol <- fread("Data/nonmarginal_extrapolation_state_priority_notax.csv")

sc <- "No Tax"
t0 <- "tauno"
t1 <- "tau"


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


## 5. Open Min - Max files
res.pq <- fread("Data/Demand_pq_sat_initial_price_semester_salience.csv")

## 6. Load min criteria
min.criteria <- fread("Data/mincriteria_all.csv")
setnames(min.criteria, c("K", "D"), c("Degree", "L"))

## 7. Set up Optimization Parameters (algorithm for now)
nlo.opts.local.df <- list(
  "algorithm"="NLOPT_LN_COBYLA",
  "maxeval" = 3000,
  "xtol_rel"=1.0e-8
)

# Prepare output
results.conv <- data.table(NULL)
results.rev <- data.table(NULL)


# A. Loop across failing xcases (up down)
for (case in c("down", "up")) {
  
  ## A.1 Identify cases
  if (case == "up") target <- prev.sol[itup == 2000, ]
  if (case == "down") target <- prev.sol[itdown == 2000, ]
  print(target)
  
  ## A.2. Load Matrix of gamma (in this case is K=8)
  in.file <- "Data/Demand_gamma_sat_initial_price_semester_salience_K8_bern.csv"
  gamma.full.data <- fread(in.file)
  
  ## B. Loop across unsolved cases
  all <- c(1:nrow(target))
  welfare.st <- foreach (unsolved= all, .combine=rbind) %dopar% {
      
    print(unsolved)
    ## Get case dta
    target.case <- target[unsolved,]
    print(target.case)
    
    
    ## Capture characteristics of case
    sig <- target.case[["sigma"]]
    D <- target.case[["D"]]
    K <- target.case[["K"]]
    theta <- target.case[["theta"]] 
    state <- target.case[["state"]] 
    

    ## C.1 Extract support to use
    p.min <- res.pq[extrap == sc & sigma == sig][["min.p"]]
    p.max <- res.pq[extrap == sc & sigma == sig][["max.p"]]
    
    ## C.2 Restrict gamma file. Constant across p
    gamma <- gamma.full.data[extrap == sc & n.groups < 3 & sigma == sig][, c(paste0("b", 0:(K-1)), "n.groups"), with = F]             ## For elasticity
    


    ## D1. Build the constraints matrix 
    constr <- as.matrix(gamma[n.groups == D][, -c("n.groups")])   ## For elasticity
    
    print("4")
    ## D2. Retrieve IVs
    IVs <- res.ivs[n.groups == D  & sigma == sig][["Estimate"]] 
    
    print("5")
    ## D3. Load min.criterion for case (note that if there is no value it is 0)
    mc <- min.criteria[Degree == K & L == D & sigma == sig & extrap == sc,][["min.criteria"]]
    
    ## D4. Generate an initial value somewhere in the middle to test algorithms
    init.old<- init.val0 <- get.init.val(constr, IVs, mc)
    print(init.val0)
    print(constr)
    print(IVs)
    print(mc)


    # F1. Subset data
    st.data <- data[fips_state == state,]
    
    # F2. Non Marginal change
    # B3 Run minimization: derivative free
    if (case == "down") {
      res0 <- nloptr( x0=init.val0,
                      eval_f= non.marginal.change,
                      eval_g_ineq = eval_restrictions_nmarg,
                      opts = nlo.opts.local.df,
                      data = st.data,
                      pp = "p_cml", 
                      t0 = t0, 
                      t1 = t1,
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
      # B4. Extract minimization results
      obj <- res0$objective
      it <- res0$iterations
      mu <- res0$solution
      
    }
    if (case == "up") {
    
      # B5 Run maximization: derivative free 
      res0 <- nloptr( x0=init.val0,
                      eval_f= max.non.marginal.change,
                      eval_g_ineq = eval_restrictions_nmarg,
                      opts = nlo.opts.local.df,
                      data = st.data,
                      pp = "p_cml", 
                      t0 = t0, 
                      t1 = t1,
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
      # B6. Extract minimization results
      obj <- res0$objective
      it <- res0$iterations
      mu <- res0$solution
      
    }
    
    data.table(state, sc, sigma = sig, theta, case, K, D, obj, it, mu)
    
  }
  res.conv <- welfare.st[it != 3000, ]
  res.conv<- res.conv[, lapply(.SD, mean), by = .(state, sigma, theta, sc, case, K, D), .SDcols = c("obj", "it")]
  

  results.conv <- rbind(results.conv, res.conv)
  fwrite(out.file.nonmarginal.c, out.file.nonmarginal)
  
  results.rev <- rbind(results.rev, welfare.st[it == 3000, ])
  fwrite(out.file.nonmarginal.r, out.file.nonmarginal)
        
}

    
