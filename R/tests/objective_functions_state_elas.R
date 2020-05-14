#' Sales Taxes Project. 
#' In this code we extract the objective functions to run the Linear program for each state
#' 

library(data.table)
library(futile.logger)
library(lfe)
library(multcomp)

setwd("/project2/igaarder")

## inputs -----------------------------------------------
data.semester <- "Data/Nielsen/semester_nielsen_data.csv"

## Outputs ----------------------------------------------
output.table <- "Data/objective_state_bernestein.csv"


bernstein <- function(x, k, K){
  choose(K, k) * x^k * (1 - x)^(K - k)
}

all_pi <- fread(data.semester)

# Need to demean
all_pi[, module_by_time := .GRP, by = .(product_module_code, semester, year)]
all_pi[, L.ln_cpricei2 := ln_cpricei2 - D.ln_cpricei2]
all_pi[, dm.L.ln_cpricei2 := L.ln_cpricei2 - mean(L.ln_cpricei2, na.rm = T), by = module_by_time]
all_pi[, dm.ln_cpricei2 := ln_cpricei2 - mean(ln_cpricei2, na.rm = T), by = module_by_time]

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

## cut the tails (keep between 1st and 99th percentile)
pct1 <- quantile(all_pi$dm.ln_cpricei2, probs = 0.01, na.rm = T, weight=base.sales)
pct99 <- quantile(all_pi$dm.ln_cpricei2, probs = 0.99, na.rm = T, weight=base.sales)
all_pi <- all_pi[(dm.ln_cpricei2 > pct1 & dm.ln_cpricei2 < pct99),]

## Keep only taxable items as those are whose responses we care
all_pi <- all_pi[ln_sales_tax > 0]

data.objective <- data.table(NULL)
for (K in 10:2) {
  
  ### Consumer Price
  
  ## Objective for average elasticity 
  data <- all_pi
  # Calculate berstein polynomials
  for (n in 0:(K-1)){
    data[, paste0("b",n) := bernstein(dm.ln_cpricei2, n, K-1)]
  }
  av.elas <- data[ , lapply(.SD, weighted.mean, w = base.sales), by = .(fips_state), .SDcols = paste0("b",0:(K-1))]
  av.elas[, K := K]
  av.elas[, obj := "elas"]

  ## Objective for fiscal externality
  for (n in 0:(K-1)){
    data[, paste0("b",n) := (get(paste0("b",n)))*(exp(ln_sales_tax)-1)/exp(ln_sales_tax)]
  }
  av.fe <- data[ , lapply(.SD, weighted.mean, w = base.sales), by = .(fips_state), .SDcols = paste0("b",0:(K-1))]
  av.fe[, K := K]
  av.fe[, obj := "fe"]

  data.objective <- rbind(data.objective, av.elas, av.fe, fill = T)
  fwrite(data.objective, output.table)
  
  
}

