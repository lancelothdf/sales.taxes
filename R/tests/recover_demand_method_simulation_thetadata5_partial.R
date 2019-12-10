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


## Bernstein basis Function -------------------------------------------

bernstein <- function(x, k, K){
  choose(K, k) * x^k * (1 - x)^(K - k)
}

## input and output filepaths -----------------------------------------------
data.semester <- "Data/Nielsen/semester_nielsen_data.csv"


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
  
  ######## Defining commo support --------------------

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
  
  ## Define normalized price for bernstein polynomial
  min.p <- sampled.data_csprice[, min(p_t)]
  max.p <- sampled.data_csprice[, max(p_t)]
  sampled.data_csprice[, r.p_t := (p_t - min.p)/(max.p - min.p) ]
  

  ###### Estimation by initial price -----------------
  
  ## Estimate the matrix of the implied system of equations by diff number of quantiles
  
  for (n.g in 1:7) {
    # Create groups of initial values of tax rate
    sampled.data_csprice <- sampled.data_csprice[, quantile := cut(L.p_t,
                                                                 breaks = quantile(L.p_t, probs = seq(0, 1, by = 1/n.g), na.rm = T, 
                                                                                   weights = base.sales),
                                                                 labels = 1:n.g, right = FALSE)]
    quantlab <- round(quantile(sampled.data_csprice$L.p_t, probs = seq(0, 1, by = 1/n.g), na.rm = T, weights = sampled.data_cstax$base.sales), digits = 4)
  
    ## Don't estimate IV, we already have these results
  
    # Get the empirical distribution of prices by quantile
    sampled.data_csprice[, base.sales.q := base.sales/sum(base.sales), by = .(quantile)]
    sampled.data_csprice[, p_group := floor((r.p_t - min(r.p_t, na.rm = T))/((max(r.p_t, na.rm = T)-min(r.p_t, na.rm = T))/100)), by = .(quantile)]
    sampled.data_csprice[, p_ll := p_group*((max(r.p_t, na.rm = T)-min(r.p_t, na.rm = T))/100), by = .(quantile)]
    sampled.data_csprice[, p_ll := p_ll + min(r.p_t, na.rm = T), by = .(quantile)]
    sampled.data_csprice[, p_ul := p_ll + ((max(r.p_t, na.rm = T)-min(r.p_t, na.rm = T))/100), by = .(quantile)]
    
    ed.price.quantile <- sampled.data_csprice[, .(w1 = (sum(base.sales.q))), by = .(p_ul, p_ll, quantile)]
    ed.price.quantile[, p_m := (p_ul+p_ll)/2]
    
    
    for (K in (n.g+1):12) {
      
      # Create the derivative of the polynomial of prices and multiplicate by weights
      for (n in 0:(K-1)){
        ed.price.quantile[, paste0("b",n) := w1*(bernstein(p_m,n,K-1))]
      }
      
      # Calculate integral
      gamma <- ed.price.quantile[ , lapply(.SD, sum), by = .(quantile), .SDcols = paste0("b",0:(K-1))]
      gamma <- gamma[!is.na(quantile),][order(quantile)][, -c("quantile")]
      
      # Export Calculation
      gamma[, n.groups := n.g]
      gamma[, iter := rep]
      
      ## Read Previous and write
      theta.output.results.file <- paste0("Data/SimulationTarget_100_thetadata3_berns_K", K,".csv")
      
      if (n.g == 1 & rep == 1) {
        fwrite(gamma, theta.output.results.file)
      } else {
        previous.data <- fread(theta.output.results.file)
        previous.data <- rbind(previous.data, gamma)
        fwrite(previous.data, theta.output.results.file)
      }
      
    } 
  }
}
