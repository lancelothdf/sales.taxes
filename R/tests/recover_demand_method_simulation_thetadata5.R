#' Sales Taxes Project
#' In this code I make the first attempts of simulations.Following Lance's instructions:
#' We are going to first assume that supply is perfectly elastic.  
#' 1) This implies a passthrough of 1 - which is close to what we find in the data anyway and 
#' 2) under perfectly elastic supply, the price is directly pinned down by supply. 
#' In this file we approximate the results from the data



library(data.table)
library(futile.logger)
library(lfe)
library(multcomp)
library(ggplot2)
library(boot)
library(dplyr)


setwd("/project2/igaarder")



## input and output filepaths -----------------------------------------------
data.semester <- "Data/Nielsen/semester_nielsen_data.csv"
output.results.file <- "Data/SimulationIV_100_thetadata3.csv"
output.target.file <- "Data/SimulationTarget_100_thetadata3.csv"


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
beta <- c(7.712, -0.554, 5.174, -3.373)

# Finally we set the number of quantiles to implement the method. 
# This will indicate the number of target parameters to retrieve 
n.quantiles <- 3
  
#### Open Real data -----------------
all_pi <- fread(data.semester)


# Define level of sampling
ids <- unique(all_pi$store_by_module)

## Files Needed
LRdiff_res <- data.table(NULL)
target_res <- data.table(NULL)

## Iteration
for (rep in 1:100) {
  
  flog.info("Iteration %s", rep)
  ##### Create fake Data based on real distribution of tax changes ----------
  
  # Sample (to keep "real" tax rates)
  sampled.ids <- data.table(sample(ids, replace = T))
  setnames(sampled.ids, old= "V1", new = "store_by_module")
  # Compute random initial prices
  sampled.ids$p0 <- rnorm(length(sampled.ids$store_by_module), -0.25, sigma_2)
  
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
  
  
  ###### Estimation by initial tax rate -----------------
  
  # Create groups of initial values of tax rate
  sampled.data_cstax <- sampled.data_cstax[, quantile := cut(L.t,
                                                             breaks = quantile(L.t, probs = seq(0, 1, by = 1/n.quantiles), na.rm = T,
                                                                               weights = base.sales),
                                                             labels = 1:n.quantiles, right = FALSE)]
  quantlab <- round(quantile(sampled.data_cstax$L.t, probs = seq(0, 1, by = 1/n.quantiles), na.rm = T, weights = sampled.data_cstax$base.sales), digits = 4)
  
  ## Estimate FS and RF
  for (Y in outcomes) {
    formula1 <- as.formula(paste0(
      Y, " ~ D.t:quantile |  quantile "
    ))
    res1 <- felm(formula = formula1, data = sampled.data_cstax,
                 weights = sampled.data_cstax$base.sales)
    
    
    ## attach results
    res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
    res1.dt[, outcome := Y]
    res1.dt[, n.groups := n.quantiles]
    res1.dt[, lev := quantlab[-1]]
    res1.dt[, initial := "tax"]
    res1.dt[, iter := rep]
    
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)
  }
  
  ## Estimate IV and retrieve in vector
  IV <- LRdiff_res[outcome == "D.q" & initial == "tax" & iter == rep,][["Estimate"]]/LRdiff_res[outcome == "D.p_t" & initial == "tax" & iter == rep,][["Estimate"]]
  
  ## Estimate the matrix of the implied system of equations
  
  # Get the empirical distribution of prices by quantile
  sampled.data_cstax[, base.sales.q := base.sales/sum(base.sales), by = .(quantile)]
  sampled.data_cstax[, p_group := floor((p_t - min(p_t, na.rm = T))/((max(p_t, na.rm = T)-min(p_t, na.rm = T))/100)), by = .(quantile)]
  sampled.data_cstax[, p_ll := p_group*((max(p_t, na.rm = T)-min(p_t, na.rm = T))/100), by = .(quantile)]
  sampled.data_cstax[, p_ll := p_ll + min(p_t, na.rm = T), by = .(quantile)]
  sampled.data_cstax[, p_ul := p_ll + ((max(p_t, na.rm = T)-min(p_t, na.rm = T))/100), by = .(quantile)]
  
  ed.price.quantile <- sampled.data_cstax[, .(w1 = (sum(base.sales.q))), by = .(p_ul, p_ll, quantile)]
  ed.price.quantile[, p_m := (p_ul+p_ll)/2]
  
  
  # Create the derivative of the polynomial of prices and multiplicate by weights
  for (n in 1:n.quantiles){
    ed.price.quantile[, paste0("b",n) := (n)*w1*(p_m^(n-1))]
  }
  # Calculate integral
  gamma <- ed.price.quantile[ , lapply(.SD, sum), by = .(quantile), .SDcols = paste0("b",1:n.quantiles)]
  gamma <- gamma[!is.na(quantile),][order(quantile)][, -c("quantile")]
  
  
  
  ## Repeat matrix estimation using only distirbutions on the treated
  
  # Get the empirical distribution of prices by quantile
  ed.price.quantile <- sampled.data_cstax[D.t != 0][, .(w1 = (sum(base.sales.q))), by = .(p_ul, p_ll, quantile)]
  ed.price.quantile[, p_m := (p_ul+p_ll)/2]
  
  
  # Create the derivative of the polynomial of prices and multiplicate by weights
  for (n in 1:n.quantiles){
    ed.price.quantile[, paste0("b",n) := (n)*w1*(p_m^(n-1))]
  }
  # Calculate integral
  
  # Calculate sum
  gamma.t <- ed.price.quantile[ , lapply(.SD, sum), by = .(quantile), .SDcols = paste0("b",1:n.quantiles)]
  gamma.t <- gamma.t[!is.na(quantile),][order(quantile)][, -c("quantile")]
  
  ## Retrieve target parameters
  beta_hat <- as.vector(solve(as.matrix(gamma))%*%(as.matrix(IV)))
  # Estimate intercept
  mean.q <- sampled.data_cstax[, mean(q, weights = base.sales)]
  mean.p <- sampled.data_cstax[, mean(p_t, weights = base.sales)]
  beta_0_hat <- mean.q - sum((beta_hat)*(mean.p^(1:n.quantiles)))
  beta_hat <- c(beta_0_hat, beta_hat)
  
  ## Repeat using only treated
  beta_hat.t <- as.vector(solve(as.matrix(gamma.t))%*%(as.matrix(IV)))
  # Estimate intercept
  beta_0_hat <- mean.q - sum((beta_hat.t)*(mean.p^(1:n.quantiles)))
  beta_hat.t <- c(beta_0_hat, beta_hat.t)
  
  ## Export estimated target parameters
  estimated.target <- data.table(beta_hat, beta_hat.t)
  estimated.target[, beta_n := .I-1]
  estimated.target[, initial := "tax"]
  estimated.target[, iter := rep]
  target_res <- rbind(target_res, estimated.target)
  fwrite(target_res, output.target.file)
  
  ###### Estimation by initial price -----------------
  
  # Create groups of initial values of tax rate
  sampled.data_csprice <- sampled.data_csprice[, quantile := cut(L.p_t,
                                                                 breaks = quantile(L.p_t, probs = seq(0, 1, by = 1/n.quantiles), na.rm = T, 
                                                                                   weights = base.sales),
                                                                 labels = 1:n.quantiles, right = FALSE)]
  quantlab <- round(quantile(sampled.data_csprice$L.p_t, probs = seq(0, 1, by = 1/n.quantiles), na.rm = T, weights = sampled.data_cstax$base.sales), digits = 4)
  
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
    res1.dt[, n.groups := n.quantiles]
    res1.dt[, lev := quantlab[-1]]
    res1.dt[, initial := "price"]
    res1.dt[, iter := rep]
    
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)
  }
  
  ## Estimate IV and retrieve in vector
  IV <- LRdiff_res[outcome == "D.q" & initial == "price" & iter == rep,][["Estimate"]]/LRdiff_res[outcome == "D.p_t" & initial == "price" & iter == rep,][["Estimate"]]
  
  ## Estimate the matrix of the implied system of equations
  
  # Get the empirical distribution of prices by quantile
  sampled.data_csprice[, base.sales.q := base.sales/sum(base.sales), by = .(quantile)]
  sampled.data_csprice[, p_group := floor((p_t - min(p_t, na.rm = T))/((max(p_t, na.rm = T)-min(p_t, na.rm = T))/100)), by = .(quantile)]
  sampled.data_csprice[, p_ll := p_group*((max(p_t, na.rm = T)-min(p_t, na.rm = T))/100), by = .(quantile)]
  sampled.data_csprice[, p_ll := p_ll + min(p_t, na.rm = T), by = .(quantile)]
  sampled.data_csprice[, p_ul := p_ll + ((max(p_t, na.rm = T)-min(p_t, na.rm = T))/100), by = .(quantile)]
  
  ed.price.quantile <- sampled.data_csprice[, .(w1 = (sum(base.sales.q))), by = .(p_ul, p_ll, quantile)]
  ed.price.quantile[, p_m := (p_ul+p_ll)/2]
  
  
  # Create the derivative of the polynomial of prices and multiplicate by weights
  for (n in 1:n.quantiles){
    ed.price.quantile[, paste0("b",n) := (n)*w1*(p_m^(n-1))]
  }
  # Calculate integral
  
  # Calculate sum
  gamma <- ed.price.quantile[ , lapply(.SD, sum), by = .(quantile), .SDcols = paste0("b",1:n.quantiles)]
  gamma <- gamma[!is.na(quantile),][order(quantile)][, -c("quantile")]
  
  
  
  ## Repeat matrix estimation using only distirbutions on the treated
  
  # Get the empirical distribution of prices by quantile
  ed.price.quantile <- sampled.data_csprice[D.t != 0][, .(w1 = (sum(base.sales.q))), by = .(p_ul, p_ll, quantile)]
  ed.price.quantile[, p_m := (p_ul+p_ll)/2]
  
  
  # Create the derivative of the polynomial of prices and multiplicate by weights
  for (n in 1:n.quantiles){
    ed.price.quantile[, paste0("b",n) := (n)*w1*(p_m^(n-1))]
  }
  # Calculate integral
  
  
  # Calculate sum
  gamma.t <- ed.price.quantile[ , lapply(.SD, sum), by = .(quantile), .SDcols = paste0("b",1:n.quantiles)]
  gamma.t <- gamma.t[!is.na(quantile),][order(quantile)][, -c("quantile")]
  
  ## Retrieve target parameters
  beta_hat <- as.vector(solve(as.matrix(gamma))%*%(as.matrix(IV)))
  # Estimate intercept
  mean.q <- sampled.data_csprice[, mean(q, weights = base.sales)]
  mean.p <- sampled.data_csprice[, mean(p_t, weights = base.sales)]
  beta_0_hat <- mean.q - sum((beta_hat)*(mean.p^(1:n.quantiles)))
  beta_hat <- c(beta_0_hat, beta_hat)
  
  ## Repeat using only treated
  beta_hat.t <- as.vector(solve(as.matrix(gamma.t))%*%(as.matrix(IV)))
  # Estimate intercept
  beta_0_hat <- mean.q - sum((beta_hat.t)*(mean.p^(1:n.quantiles)))
  beta_hat.t <- c(beta_0_hat, beta_hat.t)
  
  ## Export estimated target parameters
  estimated.target <- data.table(beta_hat, beta_hat.t)
  estimated.target[, beta_n := .I-1]
  estimated.target[, initial := "Price"]
  estimated.target[, iter := rep]
  target_res <- rbind(target_res, estimated.target)
  fwrite(target_res, output.target.file)
  
  

}
