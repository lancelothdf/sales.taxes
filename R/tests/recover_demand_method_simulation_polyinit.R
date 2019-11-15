#' Sales Taxes Project
#' In this code I make the first attempts of simulations.Following Lance's instructions:
#' We are going to first assume that supply is perfectly elastic.  
#' 1) This implies a passthrough of 1 - which is close to what we find in the data anyway and 
#' 2) under perfectly elastic supply, the price is directly pinned down by supply. 
#' 



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
output.results.file <- "Data/SimulationIV_poly.csv"


## input values ------------------------------
#set.seed(1948)
set.seed(2019)

# We first use estimates of rho and sigma we have previously gotten
rho <- 0.8089
sigma_2 <- 0.00585

# We set parameters for error of demand
alpha <- 0.2
sigma_d_2 <- 0.007

# Also we set parameters we aim to recover
beta <- c(2, -1.5, 0.8, -0.3)

# Finally we set the polynomial order to retrieve 
n.poly <- 3

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
  
  
  ###### Estimation by initial tax rate: polynomial -----------------
  
  RHS <- "0 + D.t"
  # Create higher orders and add to RHS formula
  for (k in 1:n.poly) {
    sampled.data_cstax[, paste0("D.t_L.t_",k) := D.t*(L.t^k)]
    RHS <- paste0(RHS, " + ", paste0("D.t_L.t_",k))
  }
  
  ## Estimate FS and RF
  for (Y in outcomes) {
    formula1 <- as.formula(paste0(
      Y, " ~ ", RHS
    ))
    res1 <- lm(formula = formula1, data = sampled.data_cstax,
                 weights = sampled.data_cstax$base.sales)
    
    
    ## attach results
    res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
    res1.dt[, outcome := Y]
    res1.dt[, degree := n.poly]
    res1.dt[, initial := "tax"]
    res1.dt[, iter := rep]
    
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)
  }
  
  ###### Estimation by initial price -----------------
  
  RHS <- "0 + D.t"
  # Create higher orders and add to RHS formula
  for (k in 1:n.poly) {
    sampled.data_csprice[, paste0("D.t_L.p_t_",k) := D.t*(L.p_t^k)]
    RHS <- paste0(RHS, " + ", paste0("D.t_L.p_t_",k))
  }
  
  
  ## Estimate FS and RF
  for (Y in outcomes) {
    formula1 <- as.formula(paste0(
      Y, " ~ ", RHS
    ))
    res1 <- lm(formula = formula1, data = sampled.data_csprice,
               weights = sampled.data_csprice$base.sales)
    
    
    ## attach results
    res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
    res1.dt[, outcome := Y]
    res1.dt[, degree := n.poly]
    res1.dt[, initial := "price"]
    res1.dt[, iter := rep]
    
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)
  }
}
