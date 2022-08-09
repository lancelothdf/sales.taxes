##### Santiago Lacouture
#' Sales Taxes
#' Replication File. Updated on 8/9/2022
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


## Function to obtain partially identified estimates for a given iteration
obtain.bounds <- function(gamma, Kset, Lset) {
  
}

  

#### Part 1. General set-up

## 1. Input and output files
# inputs
theta.bernestein

## 2. Set up Optimization Parameters
# These options will make Gurobi think more about numerical issues
params <- list()
params$NumericFocus <- 3
params$ScaleFlag <- 2
params$Method <- 1
params$Presolve <- 0

## 3. Set up Tolerance
tolerance <- 1e-6
params$FeasibilityTol <- tolerance

## 4. Results
elasticity <- data.table(NULL)
mincriteria <- data.table(NULL)
for (rep in c(0:100)) {
  
  ## Part 1. Set up for iteration
  ## 1. Load IVs
  res.ivs <- fread(theta.output.results.file)
  ## Keep iterest results
  res.ivs <- res.ivs[iter == rep & controls == "group_division_by_module_by_time"]
  # dcast outcomes
  res.ivs <- dcast(res.ivs, n.groups + lev ~ outcome,  fun=sum, value.var = c("Estimate"))
  # Calculate IV
  res.ivs[, estimate := w.ln_quantity3/w.ln_cpricei2]
  # Order appropiately
  res.ivs <- res.ivs[order(n.groups, lev)]
  
  ## 2. Load average p and q's 
  res.pq <- fread(pq.output.results.file)
  res.pq[, iter := .I - 1]
  res.pq <- res.pq[iter == rep]
  p.bar <- res.pq[["mean.p"]]
  q.bar <- res.pq[["mean.q"]]
  p.min <- res.pq[["min.p"]]
  p.max <- res.pq[["max.p"]]
  
  ## 2. range of p to bound elasticity
  prices <- seq(-.25, .25, 0.001)
  
  elasticity.iter <- data.table(NULL)
  mincriteria.iter <- data.table(NULL)
  
  # Prepare relevant inputs, iter-specific
  gammaK <- list()
  for (K in 2:10) {
    ## 6.1. Load Matrix of gamma (this extrictly depends on K since the basis change)
    in.file <- paste0(theta.bernestein, K,"_bern.csv")
    gamma.full.data <- fread(in.file)
    
    ## 6.2 Restrict gamma file. Constant across p
    gammaK[[paste0("K",K)]] <- gamma.full.data[ iter == rep][, c(paste0("b", 0:(K-1)), "n.groups"), with = F]             ## For elasticity
    
  }
  
  
}
