#' Author: Lancelot Henry de Frahan and John Bonney
#'
#'Same as LRdiff_diff_semesterly_preferred_part1 but we plot the cumulative effect as opposed to changes

library(data.table)
library(futile.logger)
library(lfe)
library(multcomp)

setwd("/project2/igaarder")


## input filepaths -----------------------------------------------
#' This data set contains quarterly Laspeyres indices and sales from 2006 to
#' 2014. It also contains sales tax rates from 2008-2014.
all_goods_pi_path <- "Data/Nielsen/price_quantity_indices_allitems_2006-2016_notaxinfo.csv"
#' This data set contains an old price index that Lance constructed, from
old_pi_path <- "Data/Nielsen/Quarterly_old_pi.csv"
#' This data is the same as all_goods_pi_path, except it has 2015-2016 data as well.
data.full.path <- "Data/Nielsen/semester_nielsen_data.csv"



## output filepaths ----------------------------------------------
output.results.file <- "Data/Main_semesterly_regressions_IV.csv"
output.covv.mat1 <- "Data/Main_semesterly_regressions_IV_vcov1.csv"
output.covv.mat2 <- "Data/Main_semesterly_regressions_IV_vcov2.csv"



### Estimation ---------------------------------------------------
all_pi <- fread(data.full.path)


formula_lags <- paste0("L", 1:4, ".D.ln_sales_tax", collapse = "+")
formula_leads <- paste0("F", 1:4, ".D.ln_sales_tax", collapse = "+")
formula_RHS <- paste0("D.ln_sales_tax + ", formula_lags, "+", formula_leads)



outcomes <- c("D.ln_cpricei", "D.ln_quantity")
#econ.outcomes <- c("D.ln_unemp", "D.ln_home_price")
#FE_opts <- c("module_by_time", "region_by_module_by_time", "division_by_module_by_time")
FE_opts <- c("region_by_module_by_time")


## for linear hypothesis tests
lead.vars <- paste(paste0("F", 4:1, ".D.ln_sales_tax"), collapse = " + ")
lag.vars <- paste(paste0("L", 4:1, ".D.ln_sales_tax"), collapse = " + ")
lead.lp.restr <- paste(lead.vars, "= 0")
lag.lp.restr <- paste(lag.vars, "+ D.ln_sales_tax = 0")
total.lp.restr <- paste(lag.vars, "+", lead.vars, "+ D.ln_sales_tax = 0")



### First: run regression and estimate leads and lags directly (without imposing smoothness)
## No Econ controls (but we run the placebos)
LRdiff_res <- data.table(NULL)
  for (FE in FE_opts) {

    ### First: Quantity regression
    formula1 <- as.formula(paste0(
      "D.ln_quantity ~", formula_RHS, "| ", FE, " | 0 | module_by_state"
    ))
    flog.info("Estimating with D.ln_quantity as outcome with %s FE.", FE)
    res1 <- felm(formula = formula1, data = all_pi,
                 weights = all_pi$base.sales, keepCX = TRUE)
    flog.info("Finished estimating with %s as outcome with %s FE.", Y, FE)


    ## attach results
    flog.info("Writing results...")
    res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
    res1.dt[, outcome := "ln_quantity"]
    res1.dt[, controls := FE]
    res1.dt[, econ := "none"]
    res1.dt[, parametric := "No"]
    res1.dt[, Rsq := summary(res1)$r.squared]
    res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)


    ## Compute covariance matrix

    #Start with getting the matrix of residualized regressors
    resid.quantity <- as.data.frame(res1$cX)  ##Get the expanded matrix (centered = residualized after FEs)
    xx.names <- names(resid.quantity)
    xx.mat <- as.matrix(resid.quantity) ## Create the X'X matrix that will be used in the standard errors

    #Create a dataframe that is going to contain the sums of residuals*X (*weights = base.sales) for each cluster (stateXmodule)
    resid.quantity$module_by_state <- all_pi$module_by_state
    resid.quantity$weights <- all_pi$base.sales
    resid.quantity$residuals <- res1$residuals

    setDT(resid.quantity)
    resid.quantity <- resid.quantity[, list(D.ln_sales_tax = sum(weights*D.ln_sales_tax*residuals), L1.D.ln_sales_tax = sum(weights*L1.D.ln_sales_tax*residuals), L2.D.ln_sales_tax = sum(weights*L2.D.ln_sales_tax*residuals), L3.D.ln_sales_tax = sum(weights*L3.D.ln_sales_tax*residuals), L4.D.ln_sales_tax = sum(weights*L4.D.ln_sales_tax*residuals), F1.D.ln_sales_tax = sum(weights*F1.D.ln_sales_tax*residuals), F2.D.ln_sales_tax = sum(weights*F2.D.ln_sales_tax*residuals), F3.D.ln_sales_tax = sum(weights*F3.D.ln_sales_tax*residuals), F4.D.ln_sales_tax = sum(weights*F4.D.ln_sales_tax*residuals), weights = sum(weights)), by = "module_by_state"]


    # Create "Sandwich" (X'WX) - where W is the diagonal matrix with the weights
    xx.mat <- t(xx.mat*as.vector(all_pi$base.sales))%*%xx.mat ## At some point, we need to make sure that variables in xx.mat and resid are ordered the same way -
    xx.mat <- solve(xx.mat)
    names(xx.mat) <- xx.names



    ### Second: Price regression
    formula1 <- as.formula(paste0(
      "D.ln_cpricei ~", formula_RHS, "| ", FE, " | 0 | module_by_state"
    ))
    flog.info("Estimating with D.ln_cpricei as outcome with %s FE.", FE)
    res1 <- felm(formula = formula1, data = all_pi,
                 weights = all_pi$base.sales, keepCX = TRUE)
    flog.info("Finished estimating with %s as outcome with %s FE.", Y, FE)


    ## attach results
    flog.info("Writing results...")
    res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
    res1.dt[, outcome := "ln_cpricei"]
    res1.dt[, controls := FE]
    res1.dt[, econ := "none"]
    res1.dt[, parametric := "No"]
    res1.dt[, Rsq := summary(res1)$r.squared]
    res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)


    #Create matrix of residuals for cpricei
    resid.cpricei <- as.data.frame(res1$cX)
    resid.cpricei$module_by_state <- all_pi$module_by_state
    resid.cpricei$weights <- all_pi$base.sales
    resid.cpricei$residuals <- res1$residuals

    setDT(resid.cpricei)
    resid.cpricei <- resid.cpricei[, list(c.D.ln_sales_tax = sum(weights*D.ln_sales_tax*residuals), c.L1.D.ln_sales_tax = sum(weights*L1.D.ln_sales_tax*residuals), c.L2.D.ln_sales_tax = sum(weights*L2.D.ln_sales_tax*residuals), c.L3.D.ln_sales_tax = sum(weights*L3.D.ln_sales_tax*residuals), c.L4.D.ln_sales_tax = sum(weights*L4.D.ln_sales_tax*residuals), c.F1.D.ln_sales_tax = sum(weights*F1.D.ln_sales_tax*residuals), c.F2.D.ln_sales_tax = sum(weights*F2.D.ln_sales_tax*residuals), c.F3.D.ln_sales_tax = sum(weights*F3.D.ln_sales_tax*residuals), c.F4.D.ln_sales_tax = sum(weights*F4.D.ln_sales_tax*residuals), weights = sum(weights)), by = "module_by_state"]


    ### Combine these matrices into a covariance matrix for all parameters
    resid <- merge(resid.quantity, resid.cpricei, all.x = TRUE, by = "module_by_state")
    B.clu <- t(as.matrix(resid[, c("D.ln_sales_tax", "L1.D.ln_sales_tax", "L2.D.ln_sales_tax", "L3.D.ln_sales_tax", "L4.D.ln_sales_tax", "F1.D.ln_sales_tax", "F2.D.ln_sales_tax", "F3.D.ln_sales_tax", "F4.D.ln_sales_tax", "c.D.ln_sales_tax", "c.L1.D.ln_sales_tax", "c.L2.D.ln_sales_tax", "c.L3.D.ln_sales_tax", "c.L4.D.ln_sales_tax", "c.F1.D.ln_sales_tax", "c.F2.D.ln_sales_tax", "c.F3.D.ln_sales_tax", "c.F4.D.ln_sales_tax")]))%*%as.matrix(resid[, c("D.ln_sales_tax", "L1.D.ln_sales_tax", "L2.D.ln_sales_tax", "L3.D.ln_sales_tax", "L4.D.ln_sales_tax", "F1.D.ln_sales_tax", "F2.D.ln_sales_tax", "F3.D.ln_sales_tax", "F4.D.ln_sales_tax", "c.D.ln_sales_tax", "c.L1.D.ln_sales_tax", "c.L2.D.ln_sales_tax", "c.L3.D.ln_sales_tax", "c.L4.D.ln_sales_tax", "c.F1.D.ln_sales_tax", "c.F2.D.ln_sales_tax", "c.F3.D.ln_sales_tax", "c.F4.D.ln_sales_tax")])

    nvar <- dim(B.clu)[1]/2
    sandwich <- matrix(0, nrow = 2*nvar, ncol = 2*nvar)
    sandwich[1:nvar, 1:nvar] <- xx.mat
    sandwich[(nvar + 1):(2*nvar), (nvar + 1):(2*nvar)] <- xx.mat

    covv.mat <- sandwich%*%B.clu
    covv.mat <- covv.mat%*%sandwich


    output.temp <- paste(output.covv.mat1, "_", FE, ".csv", sep = "")
    fwrite(covv.mat, output.temp)
    rm(covv.mat)
    }


##### New quantity and price measures
LRdiff_res <- data.table(NULL)
for (FE in FE_opts) {
  
  ### First: Quantity regression
  formula1 <- as.formula(paste0(
    "D.ln_quantity3 ~", formula_RHS, "| ", FE, " | 0 | module_by_state"
  ))
  flog.info("Estimating with D.ln_quantity3 as outcome with %s FE.", FE)
  res1 <- felm(formula = formula1, data = all_pi,
               weights = all_pi$base.sales, keepCX = TRUE)
  flog.info("Finished estimating with %s as outcome with %s FE.", Y, FE)
  
  
  ## attach results
  flog.info("Writing results...")
  res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
  res1.dt[, outcome := "ln_quantity3"]
  res1.dt[, controls := FE]
  res1.dt[, econ := "none"]
  res1.dt[, parametric := "No"]
  res1.dt[, Rsq := summary(res1)$r.squared]
  res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
  LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
  fwrite(LRdiff_res, output.results.file)
  
  
  ## Compute covariance matrix
  
  #Start with getting the matrix of residualized regressors
  resid.quantity <- as.data.frame(res1$cX)  ##Get the expanded matrix (centered = residualized after FEs)
  xx.names <- names(resid.quantity)
  xx.mat <- as.matrix(resid.quantity) ## Create the X'X matrix that will be used in the standard errors
  
  #Create a dataframe that is going to contain the sums of residuals*X (*weights = base.sales) for each cluster (stateXmodule)
  resid.quantity$module_by_state <- all_pi$module_by_state
  resid.quantity$weights <- all_pi$base.sales
  resid.quantity$residuals <- res1$residuals
  
  setDT(resid.quantity)
  resid.quantity <- resid.quantity[, list(D.ln_sales_tax = sum(weights*D.ln_sales_tax*residuals), L1.D.ln_sales_tax = sum(weights*L1.D.ln_sales_tax*residuals), L2.D.ln_sales_tax = sum(weights*L2.D.ln_sales_tax*residuals), L3.D.ln_sales_tax = sum(weights*L3.D.ln_sales_tax*residuals), L4.D.ln_sales_tax = sum(weights*L4.D.ln_sales_tax*residuals), F1.D.ln_sales_tax = sum(weights*F1.D.ln_sales_tax*residuals), F2.D.ln_sales_tax = sum(weights*F2.D.ln_sales_tax*residuals), F3.D.ln_sales_tax = sum(weights*F3.D.ln_sales_tax*residuals), F4.D.ln_sales_tax = sum(weights*F4.D.ln_sales_tax*residuals), weights = sum(weights)), by = "module_by_state"]
  
  
  # Create "Sandwich" (X'WX) - where W is the diagonal matrix with the weights
  xx.mat <- t(xx.mat*as.vector(all_pi$base.sales))%*%xx.mat ## At some point, we need to make sure that variables in xx.mat and resid are ordered the same way -
  xx.mat <- solve(xx.mat)
  names(xx.mat) <- xx.names
  
  
  
  ### Second: Price regression
  formula1 <- as.formula(paste0(
    "D.ln_cpricei2 ~", formula_RHS, "| ", FE, " | 0 | module_by_state"
  ))
  flog.info("Estimating with D.ln_cpricei2 as outcome with %s FE.", FE)
  res1 <- felm(formula = formula1, data = all_pi,
               weights = all_pi$base.sales, keepCX = TRUE)
  flog.info("Finished estimating with %s as outcome with %s FE.", Y, FE)
  
  
  ## attach results
  flog.info("Writing results...")
  res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
  res1.dt[, outcome := "ln_cpricei2"]
  res1.dt[, controls := FE]
  res1.dt[, econ := "none"]
  res1.dt[, parametric := "No"]
  res1.dt[, Rsq := summary(res1)$r.squared]
  res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
  LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
  fwrite(LRdiff_res, output.results.file)
  
  
  #Create matrix of residuals for cpricei
  resid.cpricei <- as.data.frame(res1$cX)
  resid.cpricei$module_by_state <- all_pi$module_by_state
  resid.cpricei$weights <- all_pi$base.sales
  resid.cpricei$residuals <- res1$residuals
  
  setDT(resid.cpricei)
  resid.cpricei <- resid.cpricei[, list(c.D.ln_sales_tax = sum(weights*D.ln_sales_tax*residuals), c.L1.D.ln_sales_tax = sum(weights*L1.D.ln_sales_tax*residuals), c.L2.D.ln_sales_tax = sum(weights*L2.D.ln_sales_tax*residuals), c.L3.D.ln_sales_tax = sum(weights*L3.D.ln_sales_tax*residuals), c.L4.D.ln_sales_tax = sum(weights*L4.D.ln_sales_tax*residuals), c.F1.D.ln_sales_tax = sum(weights*F1.D.ln_sales_tax*residuals), c.F2.D.ln_sales_tax = sum(weights*F2.D.ln_sales_tax*residuals), c.F3.D.ln_sales_tax = sum(weights*F3.D.ln_sales_tax*residuals), c.F4.D.ln_sales_tax = sum(weights*F4.D.ln_sales_tax*residuals), weights = sum(weights)), by = "module_by_state"]
  
  
  ### Combine these matrices into a covariance matrix for all parameters
  resid <- merge(resid.quantity, resid.cpricei, all.x = TRUE, by = "module_by_state")
  B.clu <- t(as.matrix(resid[, c("D.ln_sales_tax", "L1.D.ln_sales_tax", "L2.D.ln_sales_tax", "L3.D.ln_sales_tax", "L4.D.ln_sales_tax", "F1.D.ln_sales_tax", "F2.D.ln_sales_tax", "F3.D.ln_sales_tax", "F4.D.ln_sales_tax", "c.D.ln_sales_tax", "c.L1.D.ln_sales_tax", "c.L2.D.ln_sales_tax", "c.L3.D.ln_sales_tax", "c.L4.D.ln_sales_tax", "c.F1.D.ln_sales_tax", "c.F2.D.ln_sales_tax", "c.F3.D.ln_sales_tax", "c.F4.D.ln_sales_tax")]))%*%as.matrix(resid[, c("D.ln_sales_tax", "L1.D.ln_sales_tax", "L2.D.ln_sales_tax", "L3.D.ln_sales_tax", "L4.D.ln_sales_tax", "F1.D.ln_sales_tax", "F2.D.ln_sales_tax", "F3.D.ln_sales_tax", "F4.D.ln_sales_tax", "c.D.ln_sales_tax", "c.L1.D.ln_sales_tax", "c.L2.D.ln_sales_tax", "c.L3.D.ln_sales_tax", "c.L4.D.ln_sales_tax", "c.F1.D.ln_sales_tax", "c.F2.D.ln_sales_tax", "c.F3.D.ln_sales_tax", "c.F4.D.ln_sales_tax")])
  
  nvar <- dim(B.clu)[1]/2
  sandwich <- matrix(0, nrow = 2*nvar, ncol = 2*nvar)
  sandwich[1:nvar, 1:nvar] <- xx.mat
  sandwich[(nvar + 1):(2*nvar), (nvar + 1):(2*nvar)] <- xx.mat
  
  covv.mat <- sandwich%*%B.clu
  covv.mat <- covv.mat%*%sandwich
  
  
  output.temp <- paste(output.covv.mat2, "_", FE, ".csv", sep = "")
  fwrite(covv.mat, output.temp)
  rm(covv.mat)
}



## summary values --------------------------------------------------------------
LRdiff_res$N_obs <- nrow(all_pi)
LRdiff_res$N_modules <- length(unique(all_pi$product_module_code))
LRdiff_res$N_stores <- length(unique(all_pi$store_code_uc))
LRdiff_res$N_counties <- uniqueN(all_pi, by = c("fips_state", "fips_county"))
LRdiff_res$N_years <- uniqueN(all_pi, by = c("year")) # should be 6 (we lose one because we difference)
LRdiff_res$N_county_modules <- uniqueN(all_pi, by = c("fips_state", "fips_county",
                                                           "product_module_code"))
LRdiff_res$N_state_modules <- length(unique(all_pi$module_by_state))
LRdiff_res$N_module_time <- length(unique(all_pi$module_by_time))
LRdiff_res$N_module_region_time <- length(unique(all_pi$region_by_module_by_time))
LRdiff_res$N_module_division_time <- length(unique(all_pi$division_by_module_by_time))

fwrite(LRdiff_res, output.results.file)


