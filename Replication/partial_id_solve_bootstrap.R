##### Santiago Lacouture
#' Sales Taxes
#' Replication File. Updated on 8/12/2022
#' Step 8 and 9: Partial identification. 
#' This code produces the partially identified estimates (step 8) and averages (step 9),
#' taking as inputs the matrices for the Bernestein polynomials and the IVs.
#' Here we only run the division \times module \times time specification
#' Use Paralell for efficiency across iterations


library(data.table)
library(futile.logger)
library(Matrix)
library(gurobi)
library(zoo)
library(tidyverse)
library(stringr)
library(parallel)


### Set directy path and clean R
setwd("/project2/igaarder")
rm(list = ls())

# Detect number of Cores to parallel
numCores <- detectCores()


##### Part 0. Functions used throughout -----

# Functions for bernstein polynomial computation
bernstein <- function(x, k, K){
  choose(K, k) * x^k * (1 - x)^(K - k)
}
int.bernstein <- function(x,k,K) {
  
  result <- 0
  for (j in (k+1):(K+1)) {
    result <- result + bernstein(x,j,K+1)
  }
  return(result/(K+1))
  
}


# An mc-version of the sapply function.
mcsapply <- function (X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE) {
  FUN <- match.fun(FUN)
  answer <- parallel::mclapply(X = X, FUN = FUN, ...)
  if (USE.NAMES && is.character(X) && is.null(names(answer))) 
    names(answer) <- X
  if (!isFALSE(simplify) && length(answer)) 
    simplify2array(answer, higher = (simplify == "array"))
  else answer
}



## Function to obtain partially identified estimates for a given iteration. 
# The goal is to use this function in the multi core lapply 
obtain.bounds <- function(ests, prices, params, noise = F) {
  #' @param ests must be a list with $gamma, $beta, $desclist and  as components
  #' each $gamma and $beta are lists too: 
  #' Each list contains data.frames, for a given value of L (beta) or K (gamma)
  #' the data frame of the beta must contain 2 columns: L (with the value), beta (ordered)
  #' $desclist is also a list, but that only contains vectors with needed values
  #' 
  #' @param prices is a vector with the values of prices to compute extrapolated values
  #' @param params is a list containing options to pass to gurobi

  # unpack list of descriptives
  p.bar <- ests$desclist$p.bar
  q.bar <- ests$desclist$q.bar
  p.min <- ests$desclist$p.min
  p.max <- ests$desclist$p.max
  iter <- ests$desclist$iter
  
  
  # create results files
  elasticity <- data.table(NULL)
  # Loop over L
  for (dat.l in ests$beta) {
    
    # Capture value of L
    L <- unique(dat.l$L)
    # Capture beta 
    beta <- dat.l$beta
    
    if (noise) print(paste0("Starting loop for L=", L))

    # Loop over K
    for (dat.k in ests$gamma) {
      
      if (noise) {
        print(dat.k)
        print(K)
      }
      
      # Capture value of K
      K <- unique(dat.k$K)

      ## A1. Build the constraints matrix 
      constr <- as.matrix(dat.k[n.groups == L][, -c("n.groups", "K")])   ## For elasticity
      constr.dd <- cbind(constr, 
                         matrix(0, nrow = nrow(constr), ncol = 1)
                         )                                  ## For demand
      
      ## A2. Build RHS
      RHS <- beta   

      ## A3. Set monotonicity of bernstein polynomials. Elasticity < 0 
      constr.mono <- Diagonal(ncol(constr))              ## For elasticity
      constr.mono.dd <- cbind(constr.mono,
                              matrix(0, nrow = nrow(constr.mono), ncol = 1)
                              )             ## For demand
      RHS.mono <- rep(0, K)
      
      ## A4. Get intercept constraint. Demand
      constr.inter <- rep(0, K)
      for (i in 0:(K-1)) {
        constr.inter[i+1] <- -int.bernstein(p.bar,i,K-1)
      }
      constr.inter <- t(as.matrix(c(constr.inter,1))) ## Add the intercept and transform to matrix
      RHS.inter <- q.bar
      
      
      ## A4. If L > 1 we have to estimate the minimum criterion: min sum_s abs(gamma_s(theta) - beta_s)  
      # To do this I define a set of auxiliar variables a_s such that: 
      # a_s + gamma_s >= beta_s and a_s - gamma_s(theta) >= - beta_s
      # And I minimize over them - thetas are now 0s in the objective function:
      # obj is sum_s (1*a_s) + 0 *theta
      # Shape constraints still hold
      if (L > 1) {
        
        if (noise) {
          print(paste0("Solving min.criteria for K=", K, ", L=", L))
          print("constrint mat is")
          print(constr)
          print("constrint mono mat is")
          print(constr.mono)      
        }
        ## Define the problem
        min.crit <- list() 
        min.crit$A <- rbind(cbind(Diagonal(nrow(constr)), constr), 
                            cbind(Diagonal(nrow(constr)), -constr),
                            cbind(matrix(0, nrow = nrow(constr.mono), ncol = nrow(constr)), 
                                  constr.mono)
                            )
        min.crit$rhs <- c(RHS, -RHS, RHS.mono)
        min.crit$sense <- c( rep('>=', 2*length(RHS)), rep('<=',K))
        min.crit$obj <- c(rep(1, nrow(constr)), rep(0, ncol(constr)))
        min.crit$lb <- c(rep(0, nrow(constr)), rep(-Inf, ncol(constr)))  
        min.crit$modelsense <- 'min'
        
        print(min.crit)
        
        ## Solve for the minimum criteria
        min.crit.sol <- gurobi(min.crit)
        
        ## Get the minimum criterion estimated and modify the setting of the problem
        min.criteria <- min.crit.sol$objval
        tuning <- min.criteria*(1 + tolerance)
        
        
        if (noise) print(paste0("Min crit. succesful for K=",K, ", L =",L))
      }
      else min.criteria <- 0
      
      ## A5. Start loop at a given price
      if (noise) print(paste0("Starting loop for K=", K, " L=", L))
      for (p in prices) {
        
        ## B0. Normalize price
        n.p <- (p - round(p.min, 3))/(round(p.max, 3) - round(p.min, 3))
        
        ## B1. Specify objective function. Elasticity at p
        objec <- rep(0, K)
        for (i in 0:(K-1)) {
          objec[i+1] <- bernstein(n.p,i,K-1)
          if (is.nan(objec[i+1])) {objec[i+1] <- 0}
        }
        
        ## B2. Set-Up LP with all the inputs created. Elasticity
        model <- list()                                        ## Create
        model$A <- rbind(constr, constr.mono)                  ## Constraints
        model$rhs <- c(RHS, RHS.mono)                          ## RHS
        model$sense <- c(rep('=', length(RHS)), rep('<=',K))   ## Equalities
        model$obj <- objec                                     ## Objective function
        model$lb <- rep(-Inf, length(objec))                   ## Let theta be negative
        
        
        ## B2.A. If L > 1 we have to modify the problem to allow for the inequalities up to the estimated tuning parameter
        if (L > 1) {
          
          model$A <- rbind(constr, constr, constr.mono)                                  ## Constraints
          model$rhs <- c(c(RHS + tuning), c(RHS - tuning), RHS.mono)                     ## RHS
          model$sense <- c(rep('<=', length(RHS)), rep('>=', length(RHS)), rep('<=',K))  ## Equalities
          print(model)
        }
        
        ## B3. Upper bound. Elasticity
        model$modelsense <- 'max'
        result <- gurobi(model, params)
        elas.up <- result$objval
        theta.up <- result$x
        if(is.null(elas.up) | is_empty(elas.up)) {elas.up <- NA}
        
        
        ## B4. Lower bound. Elasticity
        model$modelsense <- 'min'
        result <- gurobi(model, params)
        elas.down <- result$objval
        theta.down <- result$x 
        if(is.null(elas.down) | is_empty(elas.down)) {elas.down <- NA}
        
        
        ## B5. Specify objective function. Demand at p
        objec <- rep(0, K)
        for (i in 0:(K-1)) {
          objec[i+1] <- int.bernstein(n.p,i,K-1)
          if (is.nan(objec[i+1])) {objec[i+1] <- 0}
        }
        objec <- c(objec, 1) ## Add the intercept
        
        
        ## B6. Set-Up LP with all the inputs created. Demand
        model <- list()                                            ## Create
        model$A <- rbind(constr.dd, constr.mono.dd, constr.inter)  ## Constraints
        model$rhs <- c(RHS, RHS.mono, RHS.inter)                   ## RHS
        model$sense <- c(rep('=', length(RHS)), rep('<=',K), '=')  ## Equalities
        model$obj <- objec                                         ## Objective function
        model$lb <- rep(-Inf, length(objec))                       ## Let theta be negative
        
        
        ## B6.A. If L > 1 we have to modify the problem to allow for the inequalities up to the estimated tuning parameter
        if (L > 1) {
          
          model$A <- rbind(constr.dd, constr.dd, constr.mono.dd, constr.inter)                   ## Constraints
          model$rhs <- c(c(RHS + tuning), c(RHS - tuning), RHS.mono, RHS.inter)               ## RHS
          model$sense <- c(rep('<=', length(RHS)), rep('>=', length(RHS)), rep('<=',K), '=')  ## Equalities
          
        }
        
        ## B3. Upper bound. Demand
        model$modelsense <- 'max'
        result <- gurobi(model, params)
        dd.up <- result$objval
        theta.up <- result$x
        if(is.null(dd.up) | is_empty(dd.up)) {dd.up <- NA}
        
        
        ## B4. Lower bound. Demand
        model$modelsense <- 'min'
        result <- gurobi(model, params)
        dd.down <- result$objval
        theta.down <- result$x 
        if(is.null(dd.down) | is_empty(dd.down)) {dd.down <- NA}
        
        ## B5. Save. Elasticity bounds
        elasticity.p <- data.table(elas.down, elas.up, 
                                   dd.down, dd.up, 
                                   p, L, K, 
                                   min.criteria, iter)
        elasticity <- rbind(elasticity, elasticity.p)
        
      }
      
      if (noise) print(paste0("Bounds succesful for K=",K, ", L =",L, ", at all p"))
      if (noise) print(head(elasticity[K == K & L == L]))
      
    }
  }
  return(elasticity)
}

  

#### Part 1. General set-up -------


## 1. Input and output files
# inputs
theta.bernestein <- "Data/Replication/Demand_gamma_sat_initial_price_semester_boot_r_K"
ivs.results.file <- "Data/Replication/Demand_iv_sat_initial_price_semester_boot_r.csv"
pq.output.results.file <- "Data/Replication/Demand_pq_sat_initial_price_semester_boot_r_partial.csv"
# output
partial.results.file <- "Data/Replication/partial_point_results_boot.csv"

## 2. Set up Optimization Parameters
# These options will make Gurobi think more about numerical issues
params <- list()
params$NumericFocus <- 3
params$ScaleFlag <- 2
params$Method <- 1
params$Presolve <- 0
params$OutputFlag <- 0

## 3. Set up Tolerance 
tolerance <- 1e-6
params$FeasibilityTol <- tolerance

## 4. range of p to bound elasticity
prices <- seq(-.25, .25, 0.001)

## 5. Load inputs used across iterations
# Load betas
res.ivs.all <- fread(ivs.results.file)
# Load p and qs
res.pq.all <- fread(pq.output.results.file)


#### Part 2. Capture elements across iterations and organize them -------
all.iters <- list()
for (rep in c(0:100)) {
  
  gamma <- list()
  beta <- list()
  desclist <- list()
  
  ## 1. Load average p and q's 
  res.pq <- res.pq.all[iter == rep]
  desclist$p.bar <- res.pq[["mean.p"]]
  desclist$q.bar <- res.pq[["mean.q"]]
  desclist$p.min <- res.pq[["min.p"]]
  desclist$p.max <- res.pq[["max.p"]]
  desclist$iter <- rep
  
  ## 2. Set up betas
  # 2.1 Keep iterest results
  res.ivs <- res.ivs.all[iter == rep & controls == "group_division_by_module_by_time"]
  # 2.2  dcast outcomes
  res.ivs <- dcast(res.ivs, n.groups + lev ~ outcome,  fun=sum, value.var = c("Estimate"))
  # 2.3 Calculate IV
  res.ivs[, estimate := w.ln_quantity3/w.ln_cpricei2]
  # 2.4 Order appropiately
  res.ivs <- res.ivs[order(n.groups, lev)]
  # 2.5 Save in list of betas
  for (L in unique(res.ivs$n.groups)) {
    
    # Keep relevant data
    data.L <- res.ivs[n.groups == L, c("estimate", "n.groups")]
    # Format
    setnames(data.L, c("estimate", "n.groups"), c("beta", "L"))
    # save in list
    beta[[L]] <- data.L
  }
  
  ## 3. Set up gammas
  for (K in 2:10) {
    
    ## 3.1. Load Matrix of gamma (this extrictly depends on K since the basis change)
    in.file <- paste0(theta.bernestein, K,"_bern.csv")
    gamma.full.data <- fread(in.file)
    
    ## 3.2 Restrict gamma file. Constant across p
    gamma.K <- gamma.full.data[iter == rep][, c(paste0("b", 0:(K-1)), "n.groups"), with = F]             ## For elasticity
    gamma.K[, "K" := K]
    
    ## save in list
    gamma[[K-1]] <- gamma.K
    
  }
  
  ## Part 4. Save together in iteration and then into all iterations results
  iter <- list(gamma = gamma,
               beta = beta,
               desclist = desclist)
  all.iters[[rep+1]] <- iter
    
}


#### Part 3. Estimation   -------

## Try iteration 0 to make sure things work well
iter0 <- all.iters[[1]]
res0 <- obtain.bounds(iter0, prices = prices, params = params, noise = T)
head(res0)

# Run sapply multicore
res.l <- mcsapply(all.iters, FUN = obtain.bounds, 
                  prices = prices, params = params, 
                  simplify = F, mc.cores = numCores)

# rbind results and save them
results = data.table::rbindlist(res.l, fill = T)
fwrite(LRdiff_boot, partial.results.file)

