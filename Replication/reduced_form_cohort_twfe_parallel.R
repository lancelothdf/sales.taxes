#' Author: Lancelot Henry de Frahan and Santiago Lacouture
#'
# Run similar regressions as "reduced_form_evidence.R", but with county X module data 
# + Importantly run the regression cohort-by-cohort then average the effect across cohorts
# Here we define a cohort based on time of ``treatment'': the second FE in the TWFE
# This is the parallelized version of the code, to improve computational efficiency and recover s.e. via bootstrap

library(data.table)
library(futile.logger)
library(lfe)
library(multcomp)
library(parallel)
library(MASS)

########## !!!!!!! ###########
setwd("/project2/igaarder")


## output filepaths ----------------------------------------------
# output.results.file.fs <- "Data/Cohort_TWFE_fs.csv" ### Main results
# output.results.file.fsc <- "Data/Cohort_TWFE_fsc.csv" ### Main results county 
output.results.file <- "Data/Replication/Cohort_TWFE_div.csv" ### Main results county separate
boot.results.file <- "Data/Replication/Boot_cohort_TWFE_div.csv" ### Bootstrap county separate


numCores <- detectCores()

##### Open directly the county level data: otherwise will be killed -----
all_pi <- fread("Data/Replication/all_pi_county.csv", showProgress= T)

## By-cohort TWFE
outcomes <- c("w.ln_cpricei2", "w.ln_quantity3", "w.ln_pricei2")
FE_opts <- c("region_by_module_by_time", "division_by_module_by_time")

####### Alternative: separate by-cohort county-level ------

# Function to capture estimates for given cohort, to use in apply environment
reg.output.co <- function(X, dep.var, indep.var, data, FE, w) {
  
  # Capture formula
  formula1 <- as.formula(paste0(dep.var, " ~ ", indep.var))
  # Capture subset of data relevant
  co.data <- data[get(FE) == X,]

  # Check number of observations. Don't even estimate if N < 3
  if (nrow(co.data[!is.na(get(dep.var)) & !is.na(get(w))]) < 3) {
    
    res1.dt <- data.table(
      Estimate = NA,
      `Std. Error` = NA,
      `Pr(>|t|)` = NA,
      outcome = dep.var,
      cohort = X,
      `FE` = FE)
    
  }
  else {
    # Run regression
    res1 <- lm(formula = formula1, 
               data = co.data,
               weights = co.data[[w]])
    
    ## Store results
    if(!is.na(coef(res1)[2])) {
      
      res1.dt <- data.table(
        Estimate = coef(summary(res1))[ indep.var, "Estimate"],
        `Std. Error` = coef(summary(res1))[ indep.var, "Std. Error"],
        `Pr(>|t|)` = coef(summary(res1))[ indep.var, "Pr(>|t|)"],
        outcome = dep.var,
        cohort = X,
        `FE` = FE)
      
    } else { # just in case...
      
      res1.dt <- data.table(
        Estimate = NA,
        `Std. Error` = NA,
        `Pr(>|t|)` = NA,
        outcome = dep.var,
        cohort = X,
        `FE` = FE)
      
    }
    
    
    res1.dt[, paste0(w) := sum(co.data[[w]])]
    if ("base.sales" %in% colnames(co.data) & "sales" %in% colnames(co.data)) {
      res1.dt[, sales := sum(co.data$sales)]
    }
    res1.dt[, "wVAR" := weighted.mean((co.data[[indep.var]] - 
                                            weighted.mean(co.data[[indep.var]], 
                                                          w = co.data[[w]], na.rm = T))^2,
                                         w = co.data[[w]], na.rm = T)]
    
  }
  
  return(res1.dt)
}


# Use "try" to paralleling
reg.output.co.par <- function(X, dep.var, indep.var, data, FE, w) {
  res <- try(reg.output.co.par(X = X, dep.var = dep.var, 
                               indep.var = indep.var, 
                               data = data, FE = FE, w = w),
             silent = T)
  if (class(res) == "try-error") { 
    res1.dt <- data.table(
      Estimate = NA,
      `Std. Error` = NA,
      `Pr(>|t|)` = NA,
      outcome = dep.var,
      cohort = X,
      `FE` = FE,
      wVAR = 0
      )
    res1.dt[, paste0(w) := 0]
  }
  else return(res)
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


LRdiff_res <- data.table(NULL)
for (fe in FE_opts) {

  c_ids <- unique(sort(all_pi[[fe]])) ## Define cohorts based on YearXsemesterXmoduleXCensus Region/division
  for (y in c(outcomes)) {
    flog.info("Iteration 0. Estimating on %s using %s as FE", y, fe)
    res.l <- mcsapply(c_ids, FUN = reg.output.co, 
                      dep.var = y, indep.var = "w.ln_sales_tax", 
                      data = all_pi, FE = fe, w = "base.sales",
                      simplify = F, mc.cores = numCores)
    flog.info("Writing results...")
    res1.dt = as.data.table(data.table::rbindlist(res.l, fill = T))
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)
  }
}



#### Now bootstrap the results
### Start manual bootstrap
set.seed(1941)
ids <- unique(all_pi$module_by_state)
LRdiff_boot <- data.table(NULL)
for (rep in 1:200) {

  flog.info("Iteration %s", rep)

  # Sample by block
  sampled.ids <- data.table(module_by_state = sample(ids, replace = T))
  # Merge data to actual data
  sampled.data <- merge(sampled.ids, all_pi, by = c("module_by_state") , allow.cartesian = T, all.x = T)
  
  for (fe in FE_opts) {
    ## Remake list of unique division/region by module by time
    c_ids <- unique(sort(sampled.data[[fe]])) ## Define cohorts based on YearXsemesterXmoduleXCensus division/Region
    for (y in c(outcomes)) {
  
      flog.info("Estimating on %s using %s as FE", y, fe)
      res.l <- mcsapply(c_ids, FUN = reg.output.co.par, 
                        dep.var = y, indep.var = "w.ln_sales_tax", 
                        data = sampled.data, FE = fe, w = "base.sales",
                        simplify = F, mc.cores = numCores)
      flog.info("Writing results...")
      data = as.data.table(data.table::rbindlist(res.l, fill = T))
      # LRdiff_boot <- rbind(LRdiff_boot, res1.dt, fill = T) # We used to save everything. This is a memory killer
      ## Produce a table of mean estimates
      setnames(data, old = c("Estimate", "Std. Error"), new = c("estimate", "se"))
      data[, invse := 1/se]
      data[is.na(invse) | is.infinite(invse), invse := 0]
      data[, invse_bs := invse*base.sales]
      data[, invvar := invse^2]
      data[, invvar_bs := invvar*base.sales]
      data[, varX := ifelse(wVAR == 0 | is.na(wVAR), 0, wVAR)]
      data[, seX := varX^.5]
      data[, varX_bs := varX*base.sales]
      data[, seX_bs := seX*base.sales]
      
      mean.est <- data[, .(est = mean(estimate, na.rm = T),
                           west.bs = weighted.mean(estimate, w = base.sales, na.rm = T),
                           west.invvar = weighted.mean(estimate, w = invvar, na.rm = T),
                           west.invse = weighted.mean(estimate, w = invse, na.rm = T),
                           west.invvar_bs = weighted.mean(estimate, w = invvar_bs, na.rm = T),
                           west.invse_bs = weighted.mean(estimate, w = invse_bs, na.rm = T),
                           west.varX = weighted.mean(estimate, w = varX, na.rm = T),
                           west.seX = weighted.mean(estimate, w = seX, na.rm = T),
                           west.varX_bs = weighted.mean(estimate, w = varX_bs, na.rm = T),
                           west.seX_bs = weighted.mean(estimate, w = seX_bs, na.rm = T)
      ), 
      by = .(outcome, FE)]
      
      mean.est[, iter := rep]
      LRdiff_boot <- rbind(LRdiff_boot, mean.est, fill = T)
      fwrite(LRdiff_boot, boot.results.file)
    }
  }
}

