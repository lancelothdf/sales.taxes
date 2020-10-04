## Welfare Extrapolations actual data
# Non-Marginal Case
# Priority Cases

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
#out.file.nonmarginal <- "Data/nonmarginal_extrapoaltion_state_priority.csv"
#out.file.nonmarginal <- "Data/nonmarginal_extrapoaltion_state_priorityA.csv"
out.file.nonmarginal <- "Data/nonmarginal_extrapoaltion_state_priorityB.csv"
#out.file.nonmarginal <- "Data/nonmarginal_extrapoaltion_state_priorityC.csv"
#out.file.nonmarginal <- "Data/nonmarginal_extrapoaltion_state_priorityD.csv"


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

# 3. Values to Tests

#sigmas.test <- c(0.25, 0.5, 0.75, 1)
sigmas.test <- c(0.5)
#sigmas.test <- c(0.75, 1)

thetas.list <- list()
## All
# thetas.list$s25 <- c(0, 0.058897778, 0.564015475)
# thetas.list$s50 <- c(0, 0.018501935, 0.202776786)
# thetas.list$s75 <- c(0, 0.008007016, 0.102212766)
# thetas.list$s100 <- c(0, 0.004861793, 0.066616166)
## Priority
# thetas.list$s25 <- c(0)
 thetas.list$s50 <- c(0)
# thetas.list$s75 <- c(0)
# thetas.list$s100 <- c(0, 0.066616166)

states.test <- unique(data$fips_state)

#### Estimates for Linear Case
results.nonmarginal <- data.table(NULL)
#FOR LINEAR Estimates we don't need to normalize!!! The coefficient is directly interpretable (contrary to non-linear, where matrices are normalized)
min <- 0
max <- 1
for (state in states.test) {
  data.st <- data[fips_state == state,]
  i <- 0
  for (sig in sigmas.test) {
    i <- i + 1
    thetas.test <- thetas.list[[i]]
    for (theta in thetas.test) {
      ## Capture min/max and coef in lin case
      lin <- IVs[outcome == "IV" & sigma == sig][["Estimate"]]

      ## Non Marginal Change
      t0 <- "tauno"
      t1 <- "tau"
      sc <- "No Tax"
      up <- down <- non.marginal.change(lin, data.st, "p_cml", t0, t1, theta, sig, "eta_m", min, max, 0, 0)
      results.nonmarginal<- rbind(results.nonmarginal, data.table(state, down, up, sc, theta, sigma = sig, K = 1, D = 1, s1 = 1, s2 = 1, it1 = 0, it2 = 0, ConsChckUp = 1, ConsChckDown = 1))


      t0 <- "tau"
      t1 <- "tau5"
      sc <- "plus 5 Tax"
      up <- down <- non.marginal.change(lin, data.st, "p_cml", t0, t1, theta, sig, "eta_m", min, max, 0, 0)
      results.nonmarginal<- rbind(results.nonmarginal, data.table(state, down, up, sc, theta, sigma = sig, K = 1, D = 1, s1 = 1, s2 = 1, it1 = 0, it2 = 0, ConsChckUp = 1, ConsChckDown = 1))

    }
  }
}

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

## 5. Load min criteria
min.criteria <- fread("Data/mincriteria_all.csv")
setnames(min.criteria, c("K", "D"), c("Degree", "L"))


## 6. Set up Ks
K.test <- c(2, 8)
#K.test <- 8
#scenarios <- c("No Tax", "plus 5 Tax")
scenarios <- "No Tax"

## 7. Set up Optimization Parameters (algorithm for now)
nlo.opts.local.df <- list(
  "algorithm"="NLOPT_LN_COBYLA",
  "maxeval" = 2000,
  "xtol_rel"=1.0e-8
)


for (sc in scenarios) {
  
  if (sc == "No Tax") {
    t0 <- "tauno"
    t1 <- "tau"
  } 
  if (sc == "plus 5 Tax")  {
    t0 <- "tau"
    t1 <- "tau5"
  }
  
  ## B. Loop across K
  for (K in K.test) {
    
    ## B.1. Load Matrix of gamma (this extrictly depends on K since the basis change)
    in.file <- paste0("Data/Demand_gamma_sat_initial_price_semester_salience_K", K,"_bern.csv")
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
      for (D in 2) { #unique(gamma$n.groups)
        
        ## D1. Build the constraints matrix 
        constr <- as.matrix(gamma[n.groups == D][, -c("n.groups")])   ## For elasticity
        
        ## D2. Retrieve IVs
        IVs <- res.ivs[n.groups == D  & sigma == sig][["Estimate"]] 
        
        ## D3. Load min.criterion for case (note that if there is no value it is 0)
        mc <- min.criteria[Degree == K & L == D & sigma == sig & extrap == sc,][["min.criteria"]]
        
        ## D4. Generate an initial value somewhere in the middle to test algorithms
        init.old<- init.val0 <- get.init.val(constr, IVs, mc)
        print(init.val0)
        print(constr)
        print(IVs)
        print(mc)
        
        
        ## E. Loop across thetas
        for (theta in thetas.test) {
          
          ## F Loop across states
          ## A4. Loop across states
          welfare.st <- foreach (state= states.test, .combine=rbind) %dopar% {
            
            # F1. Subset data
            st.data <- data[fips_state == state,]
            
            # F2. Non Marginal change
            # B3 Run minimization: derivative free 
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
            down <- res0$objective
            s1 <- res0$status
            it1 <- res0$iterations
            
            
            ConsChckDown <- all.equal(sum(abs(constr%*%(as.matrix(res0$solution)) - IVs) - mc), 0)
            # B5. Check constraint is met
            
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
            up <- sol <- -res0$objective
            s2 <- res0$status
            it2 <- res0$iterations
            ConsChckUp <- all.equal(sum(abs(constr%*%(as.matrix(res0$solution)) - IVs) - mc), 0)
            
            
            
            data.table(down, up, state, sc, D , K, sigma = sig, theta, s1, s2, it1, it2, ConsChckDown, ConsChckUp)
            
          }
          results.nonmarginal <- rbind(results.nonmarginal, welfare.st)
          fwrite(results.nonmarginal, out.file.nonmarginal)
          
        }
      }    
    }  
  }
}
