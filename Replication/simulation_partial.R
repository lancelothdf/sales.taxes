##### Santiago Lacouture
#' Sales Taxes
#' Replication File. Updated on 9/21/2022
#' This Code Produces the simulation for partial identified cases. Relies on true data moments



library(data.table)
library(futile.logger)
library(lfe)
library(multcomp)
library(ggplot2)
library(boot)
library(gurobi)
library(purrr)

setwd("/project2/igaarder")



## input and output filepaths -----------------------------------------------
data.semester <- "Data/Nielsen/semester_nielsen_data.csv"
out.file.elast <- "Data/Replication/Simulation_bounds_table.csv"


## Bernstein basis Function -------------------------------------------

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


## input values ------------------------------
set.seed(1948)
#set.seed(2019)

# We first use estimates of rho and sigma we have previously gotten
rho <- 0.8089
sigma_2 <- 0.00585

# We set parameters for error of demand
alpha <- 0.2
sigma_d_2 <- 0.007

# Also we set parameters we aim to recover
beta <- c(2, -1.5, 0.8, -0.3)

# Finally we set the number of quantiles to implement the method. 
# This will indicate the number of target parameters to retrieve 
n.quantiles <- 3
Ks <- c(1:10) 
  
#### Simulation based on  Real data -----------------
all_pi <- fread(data.semester)


# Define level of sampling
ids <- unique(all_pi$store_by_module)

## Files Needed
LRdiff_res <- data.table(NULL)
target_res <- data.table(NULL)
dist_res <- data.table(NULL)
matKs <- list()
for (k in Ks) {
  matKs[[k]] <- data.table(NULL)
}

## Iteration
for (rep in 1:100) {
  
  flog.info("Iteration %s", rep)
  ##### Create fake Data based on real distribution of tax changes ----------
  
  # Sample (to keep "real" tax rates)
  sampled.ids <- data.table(sample(ids, replace = T))
  setnames(sampled.ids, old= "V1", new = "store_by_module")
  # Compute random initial prices
  sampled.ids$p0 <- rnorm(length(sampled.ids$store_by_module), 0, sigma_2)
  
  # Generate Prices
  for (time in 1:length(unique(all_pi$cal_time))) {
    sampled.ids$eps <- rnorm(length(sampled.ids$store_by_module), 0, sigma_2)
    sampled.ids[, paste0("p",time) := (rho)*get(paste0("p",time-1)) + eps  ]
  }
  
  # Generate Errors of demand from a random error of supply (for every period)
  for (time in 1:length(unique(all_pi$cal_time))) {
    sampled.ids[, xis := get(paste0("p",time)) - get(paste0("p",time-1))]
    sampled.ids$exid <- rnorm(length(sampled.ids$store_by_module), 0, sigma_d_2)
    sampled.ids[, paste0("xid",time) := (alpha)*xis + exid  ]
  }
  
  # Melt generated data to merge with tax rates
  sampled.ids <- melt(sampled.ids, "store_by_module", measure=patterns("p", "xid"),
                      variable.factor = T, variable.name = "cal_time",
                      value.name = c("p", "xid"), na.rm=TRUE)
  
  # Retrieve semester and year based on cal_time: 1 is 2008-2
  sampled.ids$cal_time <- as.integer(sampled.ids$cal_time)
  sampled.ids[, year:= floor(cal_time/2) + 2008]
  sampled.ids[, semester:= ifelse(round(cal_time/2) == cal_time/2, 1, 2)]
  sampled.ids <- sampled.ids[, -c("cal_time")]
  
  # Merge data to tax rates
  sampled.data <- merge(sampled.ids, all_pi, by = c("store_by_module", "semester", "year") , allow.cartesian = T, all.x = T)
  
  # Create consumer price
  sampled.data[, p_t := p + ln_sales_tax ]
  
  # Create quantity based on demand function
  for (n in 0:(length(beta)-1)){
    sampled.data[, paste0("p_t_",n) := (beta[n+1])*(p_t^(n))]
  }
  sampled.data[, q := rowSums(.SD) + xid, .SDcols = paste0("p_t_",0:(length(beta)-1))]
  
  
  ####### Set up to run our estimators for different initial values -----------
  
  sampled.data[, w.t := ln_sales_tax - mean(ln_sales_tax, na.rm = T), by = .(store_by_module)]
  sampled.data[, w.p_t := p_t - mean(p_t, na.rm = T), by = .(store_by_module)]
  sampled.data[, w.q := q - mean(q, na.rm = T), by = .(store_by_module)]
  
  sampled.data <- sampled.data[order(store_code_uc, product_module_code, cal_time),] ##Sort on store by year-quarter (in ascending order)
  sampled.data[, L.p_t := shift(p_t, n=1, type="lag"), by = .(store_code_uc, product_module_code)]
  sampled.data[, D.t := ln_sales_tax - shift(ln_sales_tax, n=1, type="lag"), by = .(store_code_uc, product_module_code)]
  sampled.data[, D.p_t := p_t - L.p_t]
  sampled.data[, D.q := q - shift(q, n=1, type="lag"), by = .(store_code_uc, product_module_code)]
  
  
  # Create lagged value (initial)
  sampled.data[, L.t := ln_sales_tax - D.t]
  
  ######## Defining common supports --------------------
  control <- sampled.data[D.t == 0,]
  treated <- sampled.data[D.t != 0,]
  
  # Tax rate
  pct1.control <- quantile(control$L.t, probs = 0.01, na.rm = T, weights = control$base.sales)
  pct1.treated <- quantile(treated$L.t, probs = 0.01, na.rm = T, weights = treated$base.sales)
  
  pct99.control <- quantile(control$L.t, probs = 0.99, na.rm = T, weights = control$base.sales)
  pct99treated <- quantile(treated$L.t, probs = 0.99, na.rm = T, weights = treated$base.sales)
  
  sampled.data[, cs_tax := ifelse(L.t > max(pct1.treated, pct1.control) & 
                                    L.t < min(pct99treated, pct99.control), 1, 0)]
  # Make sure missings are 0s
  sampled.data[, cs_tax := ifelse(is.na(L.t), 0, cs_tax)]
  
  ## Keep within the common support
  sampled.data_cstax <- sampled.data[cs_tax == 1,]
  
  
  # Price 
  pct1.control <- quantile(control$L.p_t, probs = 0.01, na.rm = T, weights = control$base.sales)
  pct1.treated <- quantile(treated$L.p_t, probs = 0.01, na.rm = T, weights = treated$base.sales)
  
  pct99.control <- quantile(control$L.p_t, probs = 0.99, na.rm = T, weights = control$base.sales)
  pct99treated <- quantile(treated$L.p_t, probs = 0.99, na.rm = T, weights = treated$base.sales)
  
  sampled.data[, cs_price := ifelse(L.p_t > max(pct1.treated, pct1.control) & 
                                      L.p_t < min(pct99treated, pct99.control), 1, 0)]
  # Make sure missings are 0s
  sampled.data[, cs_price := ifelse(is.na(L.p_t), 0, cs_price)]
  
  ## Keep within the common support
  sampled.data_csprice <- sampled.data[cs_price == 1,]
  
  
  outcomes <- c("D.p_t", "D.q")
  
  ## Create normalized prices
  min.p <- sampled.data_csprice[, min(p_t)]
  max.p <- sampled.data_csprice[, max(p_t)]
  sampled.data_csprice[, r.p_t := (p_t - min.p)/(max.p - min.p) ]
  
  # Extract important distribution variables (min, max, mean)
  mean.p <- sampled.data_csprice[, mean(p_t)]
  mean.q <- sampled.data_csprice[, mean(q)]
  dist_res <- rbind(dist_res, data.table(min.p, max.p, mean.p, mean.q, rep))
  
  rm(control, treated)

  ###### Estimation by initial price -----------------
  for (L in 1:n.quantiles) {
    sampled.data_csprice[, quantile := 1]
    if (L ==1) {
      
      for (Y in outcomes) {
        formula1 <- as.formula(paste0(
          Y, " ~ D.t "
        ))
        res1 <- lm(formula = formula1, data = sampled.data_csprice,
                     weights = sampled.data_csprice$base.sales)
        
        
        ## attach results
        res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
        res1.dt[, outcome := Y]
        res1.dt[, n.groups := L]
        res1.dt[, lev := 1]
        res1.dt[, iter := rep]
        
        LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
      }
      
    } else {
      # Create groups of initial values of tax rate
      sampled.data_csprice <- sampled.data_csprice[, quantile := cut(L.p_t,
                                                                     breaks = quantile(L.p_t, probs = seq(0, 1, by = 1/L), na.rm = T, 
                                                                                       weights = sampled.data_cstax$base.sales),
                                                                     labels = 1:L, right = FALSE)]
      quantlab <- round(quantile(sampled.data_csprice$L.p_t, probs = seq(0, 1, by = 1/L), na.rm = T, weights = sampled.data_cstax$base.sales), digits = 4)
      
      ## Estimate FS and RF
      for (Y in outcomes) {
        formula1 <- as.formula(paste0(
          Y, " ~ D.t:quantile |  quantile "
        ))
        res1 <- felm(formula = formula1, data = sampled.data_csprice,
                     weights = sampled.data_csprice$base.sales)
        
        
        ## attach results
        res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
        res1.dt[, outcome := Y]
        res1.dt[, n.groups := L]
        res1.dt[, lev := quantlab[-1]]
        res1.dt[, iter := rep]
        
        LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
      }
    }
    
    ## Do partial identification: use normalized to 0-1
    ## Estimate the matrix of the implied system of equations. For each possible polynomial degree and compute 
    # Get the empirical distribution of prices by quantile
    sampled.data_csprice[, base.sales.q := base.sales/sum(base.sales), by = .(quantile)]
    sampled.data_csprice[, p_group := floor((r.p_t - min(r.p_t, na.rm = T))/((max(r.p_t, na.rm = T)-min(r.p_t, na.rm = T))/100)), by = .(quantile)]
    sampled.data_csprice[, p_ll := p_group*((max(r.p_t, na.rm = T)-min(r.p_t, na.rm = T))/100), by = .(quantile)]
    sampled.data_csprice[, p_ll := p_ll + min(r.p_t, na.rm = T), by = .(quantile)]
    sampled.data_csprice[, p_ul := p_ll + ((max(r.p_t, na.rm = T)-min(r.p_t, na.rm = T))/100), by = .(quantile)]
    
    ed.price.quantile <- sampled.data_csprice[, .(w1 = (sum(base.sales.q))), by = .(p_ul, p_ll, quantile)]
    ed.price.quantile[, p_m := (p_ul+p_ll)/2]
    
    for (K in L:max(Ks)) {
      
      # Create the derivative of the polynomial of prices and multiplicate by weights
      for (n in 0:(K-1)){
        ed.price.quantile[, paste0("b",n) := w1*(bernstein(p_m,n,K-1))]
      }
      
      # Calculate integral
      gamma <- ed.price.quantile[ , lapply(.SD, sum), by = .(quantile), .SDcols = paste0("b",0:(K-1))]
      gamma <- gamma[!is.na(quantile),][order(quantile)][, -c("quantile")]
      
      # Export Calculation
      gamma[, n.groups := L]
      gamma[, iter := rep]
      
      ## Read and save Previous and write
      prev <- matKs[[K]]
      prev <- rbind(prev, gamma)
      matKs[[K]] <- prev
      
    }
    
  }
  
}



## Partial identification Set-Up --------------------

## 1. Remove data to free disk space
rm(all_pi, sampled.data_csprice, sampled.data)


## 2. Load IVs and clean estimates
## Modify case D = 1 (delete intercept)
LRdiff_res <- LRdiff_res[rn != "(Intercept)"]
# dcast outcomes
res.ivs <- dcast(LRdiff_res, n.groups + lev + iter ~ outcome,  fun=sum, value.var = c("Estimate"))
# Calculate IV
res.ivs[, estimate := D.q/D.p_t]


## 3. range of p to bound elasticity
prices <- seq(-0.15, .15, 0.002)

## 4. Oberved distributions of prices
setnames(dist_res, "rep", "iter") # Modify for loop to run

## 5. Set up Optimization Parameters
# This options will make Gurobi think more about numerical issues
params <- list()
params$NumericFocus <- 3
params$ScaleFlag <- 2
params$Method <- 1
params$Presolve <- 0

## 5. Set up Tolerance
tolerance <- 1e-6
params$FeasibilityTol <- tolerance

## 6. Loop Across iterations -------------------------
bounds.file <- data.table(NULL)

for (rep in 1:100) {
  
  flog.info("Iteration %s", rep)
  ## A. Load IVs
  # Keep iter and Order appropiately
  res.iter <- res.ivs[iter == rep]
  res.iter <- res.iter[order(n.groups, lev)]
  
  ## B. Load support and means
  max.p <- dist_res[iter == rep][["max.p"]]
  min.p <- dist_res[iter == rep][["min.p"]]
  q.bar <- dist_res[iter == rep][["mean.q"]]
  p.bar <- dist_res[iter == rep][["mean.p"]]
  p.bar <- (p.bar - min.p)/(max.p - min.p) # normalize p.bar to make constraint sense
  
  ## C. Start Loop for K
  elasticity <- data.table(NULL)
  mincriteria <- data.table(NULL)
  for (K in 2:10) {
    
    ## 6.1. Load Matrix of gamma (this extrictly depends on K since the basis change)
    gamma.full.data <- matKs[[K]]
    
    ## 6.2 Restrict gamma file. Constant across p
    gamma <- gamma.full.data[ iter == rep][, c(paste0("b", 0:(K-1)), "n.groups"), with = F]             ## For elasticity
    
    ## 6.3 Start Loop at number of groups
    for (D in unique(gamma$n.groups)) {
      
      ## A1. Build the constraints matrix 
      constr <- as.matrix(gamma[n.groups == D][, -c("n.groups")])   ## For elasticity
      constr.dd <- cbind(constr,0)                                  ## For demand
      
      ## A2. Build RHS
      RHS <- res.iter[n.groups == D][["estimate"]]    
      
      ## A3. Set monotonicity of bernstein polynomials. Elasticity < 0 
      constr.mono <- Diagonal(ncol(constr))              ## For elasticity
      constr.mono.dd <- cbind(constr.mono,0)             ## For demand
      RHS.mono <- rep(0, K)
      
      ## A4. Get intercept constraint. Demand
      constr.inter <- rep(0, K)
      for (i in 0:(K-1)) {
        constr.inter[i+1] <- int.bernstein(p.bar,i,K-1)
      }
      constr.inter <- t(as.matrix(c(constr.inter,1))) ## Add the intercept and transform to matrix
      RHS.inter <- q.bar
      
      
      ## A5. If D > 1 we have to estimate the minimum criterion: min sum_s abs(gamma_s(theta) - beta_s)  
      # To do this I have to define a set of auxiliar variables a_s such that: 
      # a_s + gamma_s >= beta_s and a_s - gamma_s(theta) >= - beta_s
      # And I minimize over them - thetas are now 0s in the objective function:
      # obj is sum_s (1*a_s) + 0 *theta
      # Shape constraints still hold
      if (D > 1) {
        
        ## Define the problem
        min.crit <- list() 
        min.crit$A <- rbind(cbind(Diagonal(nrow(constr)), constr), 
                            cbind(Diagonal(nrow(constr)), -constr),
                            cbind(matrix(0, nrow(constr.mono), nrow(constr)), constr.mono)
        )
        min.crit$rhs <- c(RHS, -RHS, RHS.mono)
        min.crit$sense <- c( rep('>=', 2*length(RHS)), rep('<=',K))
        min.crit$obj <- c(rep(1, nrow(constr)), rep(0, ncol(constr)))
        min.crit$lb <- c(rep(0, nrow(constr)), rep(-Inf, ncol(constr)))  
        min.crit$modelsense <- 'min'
        
        ## Solve for the minimum criteria
        min.crit.sol <- gurobi(min.crit)
        
        ## Get the minimum criterion estimated and modify the setting of the problem
        min.criteria <- min.crit.sol$objval
        tuning <- min.criteria*(1 + tolerance)
      }
      
      ## A6. Start loop at a given price
      for (p in prices) {
        
        ## B0. Normalize price
        n.p <- (p - min.p)/(max.p - min.p)
        
        
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
        
        
        ## B2.A. If D > 1 we have to modify the problem to allow for the inequalities up to the estimated tuning parameter
        if (D > 1) {
          
          model$A <- rbind(constr, constr, constr.mono)                                  ## Constraints
          model$rhs <- c(c(RHS + tuning), c(RHS - tuning), RHS.mono)                     ## RHS
          model$sense <- c(rep('<=', length(RHS)), rep('>=', length(RHS)), rep('<=',K))  ## Equalities
          
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
        
        
        ## B6.A. If D > 1 we have to modify the problem to allow for the inequalities up to the estimated tuning parameter
        if (D > 1) {
          
          model$A <- rbind(constr.dd, constr.dd, constr.mono.dd, constr.inter)                   ## Constraints
          model$rhs <- c(c(RHS + tuning), c(RHS - tuning), RHS.mono, RHS.inter)               ## RHS
          model$sense <- c(rep('<=', length(RHS)), rep('>=', length(RHS)), rep('<=',K), '=')  ## Equalities
          
        }
        
        ## B7. Upper bound. Demand
        model$modelsense <- 'max'
        result <- gurobi(model, params)
        dd.up <- result$objval
        theta.up <- result$x
        if(is.null(dd.up) | is_empty(dd.up)) {dd.up <- NA}
        
        
        ## B7. Lower bound. Demand
        model$modelsense <- 'min'
        result <- gurobi(model, params)
        dd.down <- result$objval
        theta.down <- result$x 
        if(is.null(dd.down) | is_empty(dd.down)) {dd.down <- NA}
        
        ## B8. Save. Elasticity bounds
        elasticity.p <- data.table(elas.down, elas.up, dd.down, dd.up, p, L=D, K, min.criteria)
        elasticity <- rbind(elasticity, elasticity.p)
        
      }
    }
  }
  
  # Save
  elasticity[, iter := rep]

  bounds.file <- rbind(bounds.file, elasticity)

  ## 7. Export Results
  fwrite(bounds.file, out.file.elast)

}



