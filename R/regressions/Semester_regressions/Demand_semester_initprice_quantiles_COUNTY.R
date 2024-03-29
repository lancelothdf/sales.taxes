#' Sales Taxes Project
#' This code estimates the demand using the proposed method. First, we 
#' run a Basic DiD model by initial price level and estimate the "long run" models 
#' splitting the sample by quantiles increasing the number of groups.
#' Here initial level means previous period and we divide by groups within the "common" support
#' In this case, we run a fully saturated model (instead of splitting the sample)
#' We get the Implied IV and recover the implied demand function varying the degree estimated (no. of quantiles)


library(data.table)
library(futile.logger)
library(lfe)
library(multcomp)


setwd("/project2/igaarder")


## input filepaths -----------------------------------------------
#' This data is the same as all_goods_pi_path, except it has 2015-2016 data as well.
data.semester <- "Data/Nielsen/semester_nielsen_data_county.csv"
#data.year <- "Data/Nielsen/yearly_nielsen_data.csv"


## output filepaths ----------------------------------------------
iv.output.results.file <- "Data/Demand_iv_sat_initial_price_semester_county.csv"
theta.output.results.file <- "Data/Demand_theta_sat_initial_price_semester_county.csv"

### Set up Semester Data ---------------------------------
all_pi <- fread(data.semester)

# Create a categorical variable for county_by_module
all_pi[, county_by_module := .GRP, by = .(fips_state, fips_county, product_module_code)]

all_pi[, w.ln_sales_tax := ln_sales_tax - mean(ln_sales_tax), by = .(county_by_module)]
all_pi[, w.ln_cpricei2 := ln_cpricei2 - mean(ln_cpricei2), by = .(county_by_module)]
all_pi[, w.ln_quantity3 := ln_quantity3 - mean(ln_quantity3), by = .(county_by_module)]

# Need to demean
all_pi[, module_by_time := .GRP, by = .(product_module_code, semester, year)]
all_pi[, L.ln_cpricei2 := ln_cpricei2 - D.ln_cpricei2]
all_pi[, dm.L.ln_cpricei2 := L.ln_cpricei2 - mean(L.ln_cpricei2, na.rm = T), by = module_by_time]
all_pi[, dm.ln_cpricei2 := ln_cpricei2 - mean(ln_cpricei2, na.rm = T), by = module_by_time]
all_pi[, dm.ln_quantity3 := ln_quantity3 - mean(ln_quantity3, na.rm = T), by = module_by_time]


## Defining common support
control <- all_pi[D.ln_sales_tax == 0,]
treated <- all_pi[D.ln_sales_tax != 0,]

## Price 
#pct1.control <- quantile(control$dm.L.ln_cpricei2, probs = 0.01, na.rm = T, weight=control$base.sales)
#pct1.treated <- quantile(treated$dm.L.ln_cpricei2, probs = 0.01, na.rm = T, weight=treated$base.sales)

#pct99.control <- quantile(control$dm.L.ln_cpricei2, probs = 0.99, na.rm = T, weight=control$base.sales)
#pct99treated <- quantile(treated$dm.L.ln_cpricei2, probs = 0.99, na.rm = T, weight=treated$base.sales)
pct5.control <- quantile(control$dm.ln_cpricei2, probs = 0.05, na.rm = T, weight=control$base.sales)
pct5.treated <- quantile(treated$dm.ln_cpricei2, probs = 0.05, na.rm = T, weight=treated$base.sales)

pct95.control <- quantile(control$dm.ln_cpricei2, probs = 0.95, na.rm = T, weight=control$base.sales)
pct95.treated <- quantile(treated$dm.ln_cpricei2, probs = 0.95, na.rm = T, weight=treated$base.sales)


pct5.control
pct5.treated
pct95.control
pct95.treated

max(control$dm.ln_cpricei2, na.rm = T)
min(control$dm.ln_cpricei2, na.rm = T)

#all_pi[, cs_price := ifelse(dm.L.ln_cpricei2 > max(pct1.treated, pct1.control) & 
#                              dm.L.ln_cpricei2 < min(pct99treated, pct99.control), 1, 0)]
# Make sure missings are 0s
#all_pi[, cs_price := ifelse(is.na(dm.L.ln_cpricei2), 0, cs_price)]

## Keep within the common support
#all_pi <- all_pi[cs_price == 1,]

all_pi[, cs_price := ifelse(dm.ln_cpricei2 > max(pct5.treated, pct5.control) & 
                              dm.ln_cpricei2 < min(pct95.treated, pct95.control), 1, 0)]
# Make sure missings are 0s
all_pi[, cs_price := ifelse(is.na(dm.ln_cpricei2), 0, cs_price)]

## Keep within the common support
all_pi <- all_pi[cs_price == 1,]



outcomes <- c("w.ln_cpricei2", "w.ln_quantity3")
FE_opts <- c("group_region_by_module_by_time", "group_division_by_module_by_time")


LRdiff_res <- data.table(NULL)
target_res <- data.table(NULL)
## Run within
for (n.g in 2:7) {
    
  # Create groups of initial values of tax rate
  # We use the full weighted distribution
  all_pi <- all_pi[, quantile := cut(dm.L.ln_cpricei2,
                                     breaks = quantile(dm.L.ln_cpricei2, probs = seq(0, 1, by = 1/n.g), na.rm = T, weight = base.sales),
                                     labels = 1:n.g, right = FALSE)]
  quantlab <- round(quantile(all_pi$dm.L.ln_cpricei2, 
                            probs = seq(0, 1, by = 1/n.g), na.rm = T, 
                            weight = all_pi$base.sales), digits = 4)
  # Saturate fixed effects
  all_pi[, group_region_by_module_by_time := .GRP, by = .(region_by_module_by_time, quantile)]
  all_pi[, group_division_by_module_by_time := .GRP, by = .(division_by_module_by_time, quantile)]
  
  ## Estimate RF and FS
  for (FE in FE_opts) {
    for (Y in outcomes) {
        formula1 <- as.formula(paste0(
        Y, " ~ w.ln_sales_tax:quantile | ", FE, "+ quantile | 0 | module_by_state"
      ))
      res1 <- felm(formula = formula1, data = all_pi,
                   weights = all_pi$base.sales)
      
      
      ## attach results
      res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
      res1.dt[, outcome := Y]
      res1.dt[, controls := FE]
      res1.dt[, n.groups := n.g]
      res1.dt[, lev := quantlab[-1]]
  
      LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
      fwrite(LRdiff_res, iv.output.results.file)
    }
  
    ## Estimate IVs and retrieve in vector
    IV <- LRdiff_res[outcome == "w.ln_quantity3" & n.groups == n.g & controls == FE,][["Estimate"]]/LRdiff_res[outcome == "w.ln_cpricei2" & n.groups == n.g & controls == FE,][["Estimate"]]
    
    ## Estimate the matrix of the implied system of equations
    
    # Get the empirical distribution of prices by quantile
    all_pi[, base.sales.q := base.sales/sum(base.sales), by = .(quantile)]
    all_pi[, p_group := floor((dm.ln_cpricei2 - min(dm.ln_cpricei2, na.rm = T))/((max(dm.ln_cpricei2, na.rm = T)-min(dm.ln_cpricei2, na.rm = T))/100)), by = .(quantile)]
    all_pi[, p_ll := p_group*((max(dm.ln_cpricei2, na.rm = T)-min(dm.ln_cpricei2, na.rm = T))/100), by = .(quantile)]
    all_pi[, p_ll := p_ll + min(dm.ln_cpricei2, na.rm = T), by = .(quantile)]
    all_pi[, p_ul := p_ll + ((max(dm.ln_cpricei2, na.rm = T)-min(dm.ln_cpricei2, na.rm = T))/100), by = .(quantile)]
    
    ed.price.quantile <- all_pi[, .(w1 = (sum(base.sales.q))), by = .(p_ul, p_ll, quantile)]
    ed.price.quantile[, p_m := (p_ul+p_ll)/2]
    
    
    # Create the derivative of the polynomial of prices and multiplicate by weights
    for (n in 1:n.g){
      ed.price.quantile[, paste0("b",n) := (n)*w1*(p_m^(n-1))]
    }
    # Calculate integral
    gamma <- ed.price.quantile[ , lapply(.SD, sum), by = .(quantile), .SDcols = paste0("b",1:n.g)]
    gamma <- gamma[!is.na(quantile),][order(quantile)][, -c("quantile")]
    
    ## Retrieve target parameters
    beta_hat <- as.vector(solve(as.matrix(gamma))%*%(as.matrix(IV)))
    # Estimate intercept
    mean.q <- all_pi[, mean(dm.ln_quantity3, weights = base.sales)]
    mean.p <- all_pi[, mean(dm.ln_cpricei2, weights = base.sales)]
    beta_0_hat <- mean.q - sum((beta_hat)*(mean.p^(1:n.g)))
    beta_hat <- c(beta_0_hat, beta_hat)
    
    ## Export estimated target parameters
    estimated.target <- data.table(beta_hat)
    estimated.target[, beta_n := .I-1]
    estimated.target[, n.groups := n.g]
    estimated.target[, controls := FE]
    target_res <- rbind(target_res, estimated.target)
    fwrite(target_res, theta.output.results.file)
  } 
}


