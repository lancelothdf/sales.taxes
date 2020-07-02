#' Sales Taxes
#' Welfare Extrapolation
#' Government revenue: counterfactual sales under t'
#' This is a Non-Linear optimization. We use many inputs and functions
#' First, as functions we define all the definitions that are general enough to test different scenarios
#' Then we get several inputs: constraint matrices, minimum criterion's used before and estimated bounds
#' Finally, we create more functions (for the constraints, as they vary depending the scenario)
#' We put everything together and run the nonlinear optimization problem for each state in a given scenario, varying cases

library(Matrix)
library(data.table)
library(ggplot2)
library(zoo)
library(tidyverse)
library(stringr)
library(nloptr)

setwd("/project2/igaarder")



#### Objective Function and derivatives ----------------

## These functions are general enough to produce different scenarios
# They need, in the end:

# mu: the vector of parameters (control variables)
# data: the name of the data set 
# act.p: the name of the variable of log prices (producer)
# t: the value of the new sales tax to extrapolate
# w: variable of weights
# K: order of the polynomial used
# min: minimum value of the support of shape constraint
# max: maximum value of the support of shape constraint



# Normalization function for bernstein polynomial
normalize <- function(p, min, max) {
  (p - min)/(max - min)
}

# Bernstein polynomial
bernstein <- function(p, t, k, K, min, max){
  p <- normalize(p + t, min, max)
  choose(K, k) * p^k * (1 - p)^(K - k)
}
# Integral of the bernstein (demand is actually this: to use the previous results and don't change everything)
int.bernstein <- function(p, t, k, K, min, max) {
  
  j <- (k+1):(K+1)
  b_k <- sapply(j, function (j, t, p, k, K, min, max) bernstein(p = p, t = t, k = j, K = K, min = min, max = max), 
                p = p, t = t, K = K + 1, min = min, max = max)
  
  return(sum(b_k)/(K+1))
}

# Integrand: here is where parameters appear for the first time
# Objective
exp.sales <- function(p, t, mu, K, min, max) {
  polynomial <- rep(0,K)
  for (k in 1:K) {
    polynomial[k] <- int.bernstein(p, t, k, K-1, min, max)
  }
  return(exp(sum(mu*polynomial)))
}
common_term <- function(p, t, mu, K, min, max) {
  
  return(sapply(p, function(x, t, mu, K, min, max) 
    exp.sales(p = x, t = t, mu = mu, K = K, min = min, max = max), 
    t = t, mu = mu, K = K, min = min, max = max))
}

# Objective function: extrapolated sales
ext.sales <- function(mu, data, act.p, t, tax, w, min, max, K, constr_mat, IV_mat, min.crit = 0, elas = T) {
  
  # mu: the vector of parameters (control variables)
  # data: the name of the data set 
  # act.p: the name of the variable of log prices
  # t: the value of the new sales tax to extrapolate
  # w: variable of weights
  # K: order of the polynomial used
  # min: minimum value of the support of shape constraint
  # max: maximum value of the support of shape constraint
  
  # Use vectors 
  p.m <- data[[act.p]] - data[[tax]]
  p_m_t <- data[[act.p]]
  
  # Conterfactual
  extra <- common_term(p = p.m, t = t, mu = mu, K = K, min = min, max = max)
  
  # Substract initial current sales
  or <- common_term(p = p_m_t, t = 0, mu = mu, K = K, min = min, max = max)
  
  # Division
  d <- extra / or

  # get the weights
  w <- data[[w]]

  
  # Return weighted average
  return(weighted.mean(d, w = w))  
  
}
max_ext.sales <- function(mu, data, act.p, t, tax, w, min, max, K, constr_mat, IV_mat, min.crit = 0, elas = T) {
  return(-ext.sales(mu, data, act.p, t, tax, w, min, max, K, constr_mat, IV_mat, min.crit, elas))
}

# derivative w. respect to mu_k
d.mu.k.ext.sales <- function(mu, data, act.p, t, tax, w, min, max, K, k) {
  
  # k: mu_k degree for derivative
  # the rest as above
  
  # Use vectors 
  p.m <- data[[act.p]] - data[[tax]]
  p_m_t <- data[[act.p]]
  
  # Difference
  p.1 <- sapply(p.m, function(x,t,k,K,min,max)
    int.bernstein(p = x, t = t, k = k, K = K, min = min, max = max),
    t = t, k = k, K = K, min = min, max = max)
  p.0 <- sapply(p_m_t, function(x,t,k,K,min,max)
    int.bernstein(p = x, t = t, k = k, K = K, min = min, max = max),
    t = 0, k = k, K = K, min = min, max = max)
  
  # Conterfactual
  extra <- common_term(p = p.m, t = t, mu = mu, K = K, min = min, max = max)
  
  # Substract initial current sales
  or <- common_term(p = p_m_t, t = 0, mu = mu, K = K, min = min, max = max)
  
  # Division
  d <- extra / or
  
    # get the weights
  w <- data[[w]]
  
  
  # Return weighted average
  return(weighted.mean(p.1*d, w = w) - weighted.mean(p.0*d, w = w) )  
  
}


# Finally, a function that evaluates every gradient: include here arguments for constraint so it runs
eval_grad <- function(mu, data, act.p, t, tax, w, min, max, K, constr_mat, IV_mat, min.crit = 0, elas = T) {
  k <-1:K
  der <- sapply(k, function(x, data, act.p, t, tax, w, mu, K, min, max) 
    d.mu.k.ext.sales(mu = mu, data = data, act.p = act.p, 
                      t = t, tax = tax, w = w, min = min, 
                      max = max, K = K, k = x),
    mu = mu, data = data, act.p = act.p, t = t, tax = tax, w = w, K = K, min = min, max = max)
  return(t(t(der)))
}
max_eval_grad <- function(mu, data, act.p, t, tax, w, min, max, K, constr_mat, IV_mat, min.crit = 0, elas = T) {
  return(-eval_grad(mu, data, act.p, t, tax, w, min, max, K, constr_mat, IV_mat, min.crit, elas))
}

#### Constraints functions ----------

## Now, we put together functions that create the restrictions for the problem and will be used in the NLOPT program

# The final function takes 5 main inputs:
# mu: the control variables
# constr_mat = the matrix of constraint
# IV_mat = the vector of IVs, used for the RHS of the restriction
# elas = T (default) indicates we are imposing shape constraint on the elasticity (F is for demand)
# min.crit = (default 0) indicates the value of the minimum criterion for the problem (as solved earlier). If NULL the problem is on an equality

## Function without min.criterion
constraint <- function(mu, constr_mat, IV_mat) {
  
  if (dim(constr_mat)[1] != length(IV_mat)) { stop("constr_mat and IV_mat dimensions must match") }
  if (length(mu) != dim(constr_mat)[2]) { stop("constr_mat and mu dimensions must match") }
  
  constraints <- NULL
  for (r in 1:dim(constr_mat)[1]) {
    constraints <- rbind(constraints, c(sum(constr_mat[r,]*mu) - IV_mat[r]))
  }
  return(constraints)
}
## Function to add min.cretrion = value
constr.min.crit <- function(mu, constr_mat, IV_mat, min.crit) {
  
  if(length(min.crit) > 1){ stop("minimum criterion should be a value") }
  
  return(
    rbind(
      constraint(mu, constr_mat, IV_mat+min.crit),
      constraint(mu, -constr_mat, -IV_mat+min.crit)
    )
  )
}
## Function to create the shape constraint
shape.constr<-function(mu, elas) {
  constr.mono <- NULL
  if (elas) {
    for (k in 1:K) {
      
      constr.mono <- rbind(constr.mono,
                           c(mu[k]))
    }
  } else {
    for (k in 1:(K-1)) {
      
      constr.mono <- rbind(constr.mono,
                           c( -mu[k] + mu[k+1]))
    }
  }
  return(constr.mono)
}
## Function for constraint: includes the arguments from evaluation function even if not needed so it runs
eval_restrictions <- function(mu, data, act.p, t, tax, w, min, max, K, constr_mat, IV_mat, min.crit = 0, elas = T) {
  
  return(
    as.matrix(
      rbind(
        constr.min.crit(mu, constr_mat, IV_mat, min.crit),
        shape.constr(mu, elas)
      )
    )
  )
}
## Function for jacobian
eval_restrictions_j <- function(mu, data, act.p, t, tax, w, min, max, K, constr_mat, IV_mat, min.crit = 0, elas = T) {
  
  constr.jac <- NULL
  for (k in 1:K) {
    
    constr.jac <- cbind(
      constr.jac,
      rbind(
        constr.min.crit(c(rep(0,k-1),1,rep(0,K-k)), constr_mat, rep(0, dim(constr_mat)[1]), 0),
        shape.constr(c(rep(0,k-1),1,rep(0,K-k)), elas)
      )
    )
    
  }
  
  return(as.matrix(constr.jac))
  
}

#### Prepare and run optimizations -----

# 1. Open data
data <- fread("Data/extraction_state_binned_price.csv")

# 2. Open IVs: constant across K & D
res.ivs <- fread("Data/Demand_iv_sat_initial_price_semester_boot_r.csv")
## Keep iterest results
res.ivs <- res.ivs[controls == "group_division_by_module_by_time" & iter == 0][, -c("iter", "controls")]
# dcast outcomes
res.ivs <- dcast(res.ivs, n.groups + lev ~ outcome,  fun=sum, value.var = c("Estimate"))
# Calculate IV
res.ivs[, estimate := w.ln_quantity3/w.ln_cpricei2]
# Order appropiately
res.ivs <- res.ivs[order(n.groups, lev)]

## 2. Load min and max p for support
res.pq <- fread("Data/Demand_pq_sat_initial_price_semester_extrapolate.csv")

## 3. Load min criteria
min.criteria <- fread("Data/table_mincreteria_extrapolation.csv")
setnames(min.criteria, c("K", "D", "sc"), c("Degree", "L", "extrap"))

## 4. Load previous solutions of the linear problem, to start there
mus <- fread("Data/elasticity_mu_bounds_table_state_extrapolation.csv")
mus <- mus[target == "elas" & taxability == "taxable" & !is.na(mu.up)][, -c("up", "down", "target", "taxability")]
setnames(mus, c("K", "D", "sc"), c("Degree", "L", "extrap"))

# 5. Define output and Ks to test
out.file <- "Data/expected_sales_changes_exp5.csv"
K.test <- c(2,3,7,10)
#K.test <- c(10)

# 6. Set up Optimization Parameters (algorithm for now)
nlo.opts.global <- list(
  "algorithm"="NLOPT_GN_ISRES",
  "maxeval" = 40,
  "xtol_rel"=1.0e-8
)
nlo.opts.local <- list(
  "algorithm"="NLOPT_LD_SLSQP",
  "maxeval" = 200,
  "xtol_rel"=-1,
  "check_derivatives_print" = "all"
)



## 6. Loop acorss Scenarios
#scenarios <- c("No Tax", "plus 5 Tax")
scenarios <- c("plus 5 Tax")
welfare <- data.table(NULL)

for (sc in scenarios) {
  
  p.min <- res.pq[extrap == sc][["min.p"]]
  p.max <- res.pq[extrap == sc][["max.p"]]
  
  if (sc == "No Tax") {
    tax.cs <- "tau"
    t.cs <- 0
  } 
  if (sc == "plus 5 Tax")  {
    data[, tau.n := 0]
    tax.cs <- "tau.n"
    t.cs <- log(1+0.05)
    
  }
  ## Loop across K
  for (K in K.test) {
    
    ## 6.1. Load Matrix of gamma (this extrictly depends on K since the basis change)
    in.file <- paste0("Data/Demand_gamma_sat_initial_price_semester_extrapolate_K", K,"_bern.csv")
    gamma.full.data <- fread(in.file)
    
    ## 6.2 Restrict gamma file. Constant across p
    gamma <- gamma.full.data[extrap == sc & n.groups < 3][, c(paste0("b", 0:(K-1)), "n.groups"), with = F]             ## For elasticity
    
    ## 6.3 Start Loop at number of groups
    for (D in unique(gamma$n.groups)) {
      
      ## A1. Build the constraints matrix 
      constr <- as.matrix(gamma[n.groups == D][, -c("n.groups")])   ## For elasticity
      
      ## A2. Load min.criterion for case (note that if there is no value it is 0)
      mc <- min.criteria[Degree == K & L == D & extrap == sc,][["min.criteria"]]
      if (is_empty(mc)) mc <- 0
      
      ## A3. Retrieve IVs
      IVs <- res.ivs[n.groups == D][["estimate"]] 
      
      print(K)
      print(D)
      print(constr)
      print(IVs)
      
      ## A5. Loop across states
      for (state in unique(mus$st)) {
        print(state)
        
        
        ## Generate an initial value somewhere in the middle
        init.val.up <- mus[Degree == K & L == D & st == state & extrap == sc,][["mu.up"]]
        init.val.down <- mus[Degree == K & L == D & st == state & extrap == sc,][["mu.down"]]
        
        # B1. Subset data
        st.data <- data[fips_state == state,]
        
        print(st.data[, weighted.mean(p_m - get(tax.cs), w = eta_m)])
        print(st.data[, weighted.mean(p_m + t.cs, w = eta_m)])
        
        
        # B2.B1 Run minimization: Global 
        res0 <- nloptr( x0=init.val.down,
                        eval_f= ext.sales,
                        eval_g_ineq = eval_restrictions,
                        opts = nlo.opts.global,
                        data = st.data,
                        act.p = "p_m", 
                        t = t.cs, 
                        tax = tax.cs,
                        w = "eta_m", 
                        min = p.min, 
                        max = p.max, 
                        K = K,
                        constr_mat = constr, 
                        IV_mat = IVs, 
                        min.crit = mc,
                        elas = T,
                        ub = rep(0, K),
                        lb = rep(-1000, K),
        )       
        init.val.down <- res0$solution
        
        # B2.B2 Run minimization: Local 
        res0 <- nloptr( x0=init.val.down,
                        eval_f= ext.sales,
                        eval_grad_f = eval_grad,
                        eval_g_ineq = eval_restrictions,
                        eval_jac_g_ineq = eval_restrictions_j,
                        opts = nlo.opts.local,
                        data = st.data,
                        act.p = "p_m", 
                        t = t.cs, 
                        tax = tax.cs,
                        w = "eta_m", 
                        min = p.min, 
                        max = p.max, 
                        K = K,
                        constr_mat = constr, 
                        IV_mat = IVs, 
                        min.crit = mc,
                        elas = T
        )
        
        # B2.B1 Minimum
        down0 <- res0$objective
        s1 <- res0$status
        
        # B2.B1 Run minimization: Global 
        res0 <- nloptr( x0=init.val.up,
                        eval_f= max_ext.sales,
                        eval_g_ineq = eval_restrictions,
                        opts = nlo.opts.global,
                        data = st.data,
                        act.p = "p_m", 
                        t = t.cs, 
                        tax = tax.cs,
                        w = "eta_m", 
                        min = p.min, 
                        max = p.max, 
                        K = K,
                        constr_mat = constr, 
                        IV_mat = IVs, 
                        min.crit = mc,
                        elas = T,
                        ub = rep(0, K),
                        lb = rep(-1000, K),
        )       
        init.val.up <- res0$solution
        s2 <- res0$status
        
        # B3.B1 Run maximization: Local
        res0 <- nloptr( x0=init.val.up,
                        eval_f= max_ext.sales,
                        eval_grad_f = max_eval_grad,
                        eval_g_ineq = eval_restrictions,
                        eval_jac_g_ineq = eval_restrictions_j,
                        opts = nlo.opts.local,
                        data = st.data,
                        act.p = "p_m", 
                        t = t.cs, 
                        tax = tax.cs,
                        w = "eta_m", 
                        min = p.min, 
                        max = p.max, 
                        K = K,
                        constr_mat = constr, 
                        IV_mat = IVs, 
                        min.crit = mc,
                        elas = T
        )
        # B3.B2 Extract minimization results
        up0 <- -res0$objective
        
        up <- max(up0,down0)
        down <- min(up0,down0)
        
        # B6. Compile estimates
        welfare.st <- data.table(data.table(down, up, state, D , K, sc, s1, s2))
        welfare <- rbind(welfare, welfare.st)
        
        # B7. Export Results every case is done
        fwrite(welfare, out.file)
      }
    }
  }
}