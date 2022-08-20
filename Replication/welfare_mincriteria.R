##### Santiago Lacouture
#' Sales Taxes
#' Replication File. Updated on 8/18/2022
#' Step 13: Welfare extrapolation. Minimum criteria
#' This code estimates the minimum criteria for each combination of 
#' values so that any non-linear optimization takes these as inputs


library(data.table)
library(futile.logger)
library(Matrix)
library(gurobi)

setwd("/project2/igaarder")
rm(list = ls())

## Inputs
iv.output.salience.results.file <- "Data/Replication/Demand_iv_sat_initial_price_semester_salience.csv"
theta.berstein.sal <- "Data/Replication/Demand_gamma_sat_initial_price_semester_salience_K"

## Ouputs
out.file.mc.welf <- "Data/Replication/mincriteria_welfare_boot.csv"

### Part 1. Set up Extrapolation ----

sigmas.test <- c(0.25, 0.5, 0.75, 1)

## 1. Set up IV estimates for each sigma
IVs <- fread(iv.output.salience.results.file)
IVs <- IVs[controls == "division_by_module_by_time"]

# For L = 1
IVs1 <- IVs[outcome == "IV", -c("Std. Error", "controls", "rn", "Cluster s.e.", "t value", "Pr(>|t|)", "outcome")]
# For L > 1
IVs2 <- dcast(IVs[n.groups > 1], n.groups + lev + sigma + iter ~ outcome,  fun=sum, value.var = c("Estimate"))
IVs2[, w.ln_cpricei2 := w.ln_cpricei2_sig0.25 + w.ln_cpricei2_sig0.5 + w.ln_cpricei2_sig0.75 + w.ln_cpricei2_sig1]
IVs2[, Estimate := w.ln_quantity3/w.ln_cpricei2]
colsout <- c(paste0("w.ln_cpricei2_sig", sigmas.test), "w.ln_quantity3", "w.ln_cpricei2")
print(colsout)
print(colnames(IVs2))
IVs2 <- IVs2[, -colsout]
# Merge and Order appropiately
res.ivs <- rbind(IVs1, IVs2)
res.ivs <- res.ivs[order(iter, sigma, n.groups, lev)]
rm(IVs, IVs1, IVs2)

## 2. Set up Ks and scenarios (non-marginal) to test
K.test <- c(2:8)
scenarios <- c("Original", "No Tax", "plus 5 Tax")

## 3. Set up Optimization Parameters (algorithm for now)
params <- list()
params$NumericFocus <- 3
params$ScaleFlag <- 2
params$Method <- 1
params$Presolve <- 0
tolerance <- 1e-6
params$FeasibilityTol <- tolerance


### Part 2. Run estimation -------
mincriteria <- data.table(NULL)
for (rep in 0:100) {
  flog.info("Iteration %s", rep)
  ## A. Loop across scenarios
  for (sc in scenarios) {
    
    flog.info("...Starting Scenario %s", sc)
    ## B. Loop across K
    for (K in K.test) {
      
      ## B.1. Load Matrix of gamma (this extrictly depends on K since the basis change)
      in.file <- paste0(theta.berstein.sal, K,"_bern.csv")
      gamma.full.data <- fread(in.file)
      
      ## C. Loop across sigmas
      for (sig in sigmas.test) {
        ## C.1 Extract support to use (mot needed)
        
        ## C.2 Restrict gamma file. Constant across p
        gamma <- gamma.full.data[iter == rep & extrap == sc & n.groups < 3 & sigma == sig][, c(paste0("b", 0:(K-1)), "n.groups"), with = F]             ## For elasticity
        
        ## D Start Loop at number of groups
        for (D in unique(gamma$n.groups)) {
          
          flog.info("...... K = %s, \sigma = %s, L = %s", K, sig, D)
          ## D1. Build the constraints matrix 
          constr <- as.matrix(gamma[n.groups == D][, -c("n.groups")])   ## For elasticity
          
          ## D2. Retrieve IVs
          IVs <- res.ivs[iter == rep & n.groups == D  & sigma == sig][["Estimate"]] 
          
          ## D3. Estimate min.criterion for case (note that if there is no value it is 0)
          constr.mono <- Diagonal(ncol(constr))
          RHS.mono <- rep(0, K)
          
          # D3a. Define the problem
          min.crit <- list() 
          min.crit$A <- rbind(cbind(Diagonal(nrow(constr)), constr), 
                              cbind(Diagonal(nrow(constr)), -constr),
                              cbind(matrix(0, nrow(constr.mono), nrow(constr)), constr.mono)
          )
          min.crit$rhs <- c(IVs, -IVs, RHS.mono)
          min.crit$sense <- c( rep('>=', 2*length(IVs)), rep('<=',K))
          min.crit$obj <- c(rep(1, nrow(constr)), rep(0, ncol(constr)))
          min.crit$lb <- c(rep(0, nrow(constr)), rep(-Inf, ncol(constr)))  
          min.crit$modelsense <- 'min'
          
          # D3b. Solve for the minimum criterion
          min.crit.sol <- gurobi(min.crit)
          
          # D3c. Get the minimum criterion estimated and modify the setting of the problem
          mc <- min.crit.sol$objval
          
          # D3d. Export the min creterion for each case to check
          mincriteria <- rbind(mincriteria, 
                               data.table(min.criteria = mc, L= D, K, 
                                          sigma = sig, extrap = sc, iter = rep))
          fwrite(mincriteria, out.file.mc.welf)
          
        }    
      }  
    }
  }
}
