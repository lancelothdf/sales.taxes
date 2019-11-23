#' Sales Taxes Project
#' This code estimates the demand using the proposed method. First, we 
#' run a Basic DiD model by initial price level and estimate the "long run" models 
#' splitting the sample by quantiles increasing the number of groups.
#' Here initial level means previous period and we divide by groups within the "common" support
#' In this case, we run a fully saturated model (instead of splitting the sample)
#' We do partial identification in this case, so we extract the gamma matrix plus the mean q and demean log p. 
#' We Bootstrap to get CIs


library(data.table)
library(futile.logger)
library(lfe)
library(multcomp)


setwd("/project2/igaarder")


## input filepaths -----------------------------------------------
#' This data is the same as all_goods_pi_path, except it has 2015-2016 data as well.
data.semester <- "Data/Nielsen/semester_nielsen_data.csv"
data.year <- "Data/Nielsen/yearly_nielsen_data.csv"


## output filepaths ----------------------------------------------
pq.output.results.file <- "Data/Demand_pq_sat_initial_price_semester_boot_r.csv"

### Set up Semester Data ---------------------------------
all_pi <- fread(data.semester)
all_pi[, w.ln_sales_tax := ln_sales_tax - mean(ln_sales_tax), by = .(store_by_module)]
all_pi[, w.ln_cpricei2 := ln_cpricei2 - mean(ln_cpricei2), by = .(store_by_module)]
all_pi[, w.ln_quantity3 := ln_quantity3 - mean(ln_quantity3), by = .(store_by_module)]

# Need to demean
all_pi[, module_by_time := .GRP, by = .(product_module_code, semester, year)]
all_pi[, L.ln_cpricei2 := ln_cpricei2 - D.ln_cpricei2]
all_pi[, dm.L.ln_cpricei2 := L.ln_cpricei2 - mean(L.ln_cpricei2, na.rm = T), by = module_by_time]
all_pi[, dm.ln_cpricei2 := ln_cpricei2 - mean(ln_cpricei2, na.rm = T), by = module_by_time]
all_pi[, dm.ln_quantity3 := ln_quantity3 - mean(ln_quantity3, na.rm = T), by = module_by_time]


# Defining common support
control <- all_pi[D.ln_sales_tax == 0,]
treated <- all_pi[D.ln_sales_tax != 0,]

# Price 
pct1.control <- quantile(control$dm.L.ln_cpricei2, probs = 0.01, na.rm = T, weight=control$base.sales)
pct1.treated <- quantile(treated$dm.L.ln_cpricei2, probs = 0.01, na.rm = T, weight=treated$base.sales)

pct99.control <- quantile(control$dm.L.ln_cpricei2, probs = 0.99, na.rm = T, weight=control$base.sales)
pct99treated <- quantile(treated$dm.L.ln_cpricei2, probs = 0.99, na.rm = T, weight=treated$base.sales)

all_pi[, cs_price := ifelse(dm.L.ln_cpricei2 > max(pct1.treated, pct1.control) & 
                              dm.L.ln_cpricei2 < min(pct99treated, pct99.control), 1, 0)]
# Make sure missings are 0s
all_pi[, cs_price := ifelse(is.na(dm.L.ln_cpricei2), 0, cs_price)]

## Keep within the common support
all_pi <- all_pi[cs_price == 1,]



outcomes <- c("w.ln_cpricei2", "w.ln_quantity3")
FE_opts <- c("group_region_by_module_by_time", "group_division_by_module_by_time")


LRdiff_res <- data.table(NULL)
pq_res <- data.table(NULL)
## Run within
flog.info("Iteration 0")

## To estimate the intercept
mean.q <- all_pi[, mean(ln_quantity3, weights = base.sales, na.rm = T)]
mean.p <- all_pi[, mean(dm.ln_cpricei2, weights = base.sales, na.rm = T)]

estimated.pq <- data.table(mean.q, mean.p)
pq_res <- rbind(pq_res, estimated.pq)
fwrite(pq_res, pq.output.results.file)


for (n.g in 2:7) {
    
  # Create groups of initial values of tax rate
  # We use the full weighted distribution
  all_pi <- all_pi[, quantile := cut(dm.L.ln_cpricei2,
                                     breaks = quantile(dm.L.ln_cpricei2, probs = seq(0, 1, by = 1/n.g), na.rm = T, weight = base.sales),
                                     labels = 1:n.g, right = FALSE)]
  quantlab <- round(quantile(all_pi$dm.L.ln_cpricei2, 
                            probs = seq(0, 1, by = 1/n.g), na.rm = T, 
                            weight = all_pi$base.sales), digits = 4)
  
  ## Do partial identification
  for (K in (n.g + 1):10) {
    
    ## Estimate the matrix of the implied system of equations. For each possible polynomial degree and compute 
    # Get the empirical distribution of prices by quantile
    all_pi[, base.sales.q := base.sales/sum(base.sales), by = .(quantile)]
    all_pi[, p_group := floor((dm.ln_cpricei2 - min(dm.ln_cpricei2, na.rm = T))/((max(dm.ln_cpricei2, na.rm = T)-min(dm.ln_cpricei2, na.rm = T))/100)), by = .(quantile)]
    all_pi[, p_ll := p_group*((max(dm.ln_cpricei2, na.rm = T)-min(dm.ln_cpricei2, na.rm = T))/100), by = .(quantile)]
    all_pi[, p_ll := p_ll + min(dm.ln_cpricei2, na.rm = T), by = .(quantile)]
    all_pi[, p_ul := p_ll + ((max(dm.ln_cpricei2, na.rm = T)-min(dm.ln_cpricei2, na.rm = T))/100), by = .(quantile)]
    
    ed.price.quantile <- all_pi[, .(w1 = (sum(base.sales.q))), by = .(p_ul, p_ll, quantile)]
    ed.price.quantile[, p_m := (p_ul+p_ll)/2]
    
    # Create the derivative of the polynomial of prices and multiplicate by weights
    for (n in 1:K){
      ed.price.quantile[, paste0("b",n) := (n)*w1*(p_m^(n-1))]
    }
    
    # Calculate integral
    gamma <- ed.price.quantile[ , lapply(.SD, sum), by = .(quantile), .SDcols = paste0("b",1:K)]
    gamma <- gamma[!is.na(quantile),][order(quantile)][, -c("quantile")]
    
    # Export Calculation
    gamma[, n.groups := n.g]
    gamma[, iter := 0]
    
    theta.output.results.file <- paste0("Data/Demand_gamma_sat_initial_price_semester_boot_r_K", K,".csv")
    if (n.g == 2) {
      fwrite(gamma, theta.output.results.file)
    } else {
      previous.data <- fread(theta.output.results.file)
      previous.data <- rbind(previous.data, gamma)
      fwrite(previous.data, theta.output.results.file)
    }
  } 
}





### Start manual bootstrap
set.seed(2019)
ids <- unique(all_pi$module_by_state)

for (rep in 1:100) {
  
  flog.info("Iteration %s", rep)

  # Sample by block
  sampled.ids <- data.table(sample(ids, replace = T))
  setnames(sampled.ids, old= "V1", new = "module_by_state")
  
  # Merge data to actual data
  sampled.data <- merge(sampled.ids, all_pi, by = c("module_by_state") , allow.cartesian = T, all.x = T)
  
  
  ## To estimate the intercept
  mean.q <- sampled.data[, mean(ln_quantity3, weights = base.sales, na.rm = T)]
  mean.p <- sampled.data[, mean(dm.ln_cpricei2, weights = base.sales, na.rm = T)]
  
  estimated.pq <- data.table(mean.q, mean.p)
  pq_res <- rbind(pq_res, estimated.pq)
  fwrite(pq_res, pq.output.results.file)
  
  for (n.g in 2:7) {
    
    # Create groups of initial values of tax rate
    # We use the full weighted distribution
    sampled.data <- sampled.data[, quantile := cut(dm.L.ln_cpricei2,
                                       breaks = quantile(dm.L.ln_cpricei2, probs = seq(0, 1, by = 1/n.g), na.rm = T, weight = base.sales),
                                       labels = 1:n.g, right = FALSE)]
    quantlab <- round(quantile(sampled.data$dm.L.ln_cpricei2, 
                               probs = seq(0, 1, by = 1/n.g), na.rm = T, 
                               weight = sampled.data$base.sales), digits = 4)
      ## Do partial identification
    for (K in (n.g + 1):10) {
      
      ## Estimate the matrix of the implied system of equations. For each possible polynomial degree and compute 
      # Get the empirical distribution of prices by quantile
      sampled.data[, base.sales.q := base.sales/sum(base.sales), by = .(quantile)]
      sampled.data[, p_group := floor((dm.ln_cpricei2 - min(dm.ln_cpricei2, na.rm = T))/((max(dm.ln_cpricei2, na.rm = T)-min(dm.ln_cpricei2, na.rm = T))/100)), by = .(quantile)]
      sampled.data[, p_ll := p_group*((max(dm.ln_cpricei2, na.rm = T)-min(dm.ln_cpricei2, na.rm = T))/100), by = .(quantile)]
      sampled.data[, p_ll := p_ll + min(dm.ln_cpricei2, na.rm = T), by = .(quantile)]
      sampled.data[, p_ul := p_ll + ((max(dm.ln_cpricei2, na.rm = T)-min(dm.ln_cpricei2, na.rm = T))/100), by = .(quantile)]
      
      ed.price.quantile <- sampled.data[, .(w1 = (sum(base.sales.q))), by = .(p_ul, p_ll, quantile)]
      ed.price.quantile[, p_m := (p_ul+p_ll)/2]
      
      # Create the derivative of the polynomial of prices and multiplicate by weights
      for (n in 1:K){
        ed.price.quantile[, paste0("b",n) := (n)*w1*(p_m^(n-1))]
      }
      
      # Calculate integral
      gamma <- ed.price.quantile[ , lapply(.SD, sum), by = .(quantile), .SDcols = paste0("b",1:K)]
      gamma <- gamma[!is.na(quantile),][order(quantile)][, -c("quantile")]
      
      # Export Calculation
      gamma[, n.groups := n.g]
      gamma[, iter := rep]
     
      theta.output.results.file <- paste0("Data/Demand_gamma_sat_initial_price_semester_boot_r_K", K,".csv")
      previous.data <- fread(theta.output.results.file)
      previous.data <- rbind(previous.data, gamma)
      fwrite(previous.data, theta.output.results.file)
      
    } 
  }
}

