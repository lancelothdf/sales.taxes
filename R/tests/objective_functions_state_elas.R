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

## Define re-scaled prices to use Bernstein polynomials in that range
min.p <- all_pi[, min(dm.ln_cpricei2)]
max.p <- all_pi[, max(dm.ln_cpricei2)]
all_pi[, r.dm.ln_cpricei2 := (dm.ln_cpricei2 - min.p)/(max.p - min.p) ]


## Keep only taxable items as those are whose responses we care
all_pi_t <- all_pi[ln_sales_tax > 0]

data.objective <- data.table(NULL)
for (K in 10:2) {
  
  ### Consumer Price
  
  ## Objective for average elasticity 
  data <- all_pi_t
  # Calculate berstein polynomials
  for (n in 0:(K-1)){
    data[, paste0("b",n) := bernstein(r.dm.ln_cpricei2, n, K-1)]
  }
  av.elas <- data[ , lapply(.SD, weighted.mean, w = base.sales), by = .(fips_state), .SDcols = paste0("b",0:(K-1))]
  av.elas[, K := K]
  av.elas[, obj := "elas"]
  av.elas[, type := "t>0"]
  
  ## Objective for fiscal externality
  for (n in 0:(K-1)){
    data[, paste0("b",n) := (get(paste0("b",n)))*(exp(ln_sales_tax)-1)/exp(ln_sales_tax)]
  }
  av.fe <- data[ , lapply(.SD, weighted.mean, w = base.sales), by = .(fips_state), .SDcols = paste0("b",0:(K-1))]
  av.fe[, K := K]
  av.fe[, obj := "fe"]
  av.fe[, type := "t>0"]
  
  data.objective <- rbind(data.objective, av.elas, av.fe, fill = T)
  fwrite(data.objective, output.table)
  
}
rm(all_pi_t)
## Identify taxability as of 2014-2

# Identify taxability of module: import
taxability_panel <- fread("/project2/igaarder/Data/taxability_state_panel.csv")
# For now, make reduced rate another category
taxability_panel[, taxability := ifelse(!is.na(reduced_rate), 2, taxability)]
# We will use taxability as of December 2014
taxability_panel <- taxability_panel[(month==12 & year==2014),][, .(product_module_code, product_group_code,
                                                                    fips_state, taxability)]

## Merge to products
all_pi<- merge(all_pi, taxability_panel, by = c("product_module_code", "fips_state"))

## Run by taxability

# 1. Taxable

all_pi_t <- all_pi[taxability == 1, ]

for (K in 10:2) {
  
  ### Consumer Price
  
  ## Objective for average elasticity 
  data <- all_pi_t
  # Calculate berstein polynomials
  for (n in 0:(K-1)){
    data[, paste0("b",n) := bernstein(r.dm.ln_cpricei2, n, K-1)]
  }
  av.elas <- data[ , lapply(.SD, weighted.mean, w = base.sales), by = .(fips_state), .SDcols = paste0("b",0:(K-1))]
  av.elas[, K := K]
  av.elas[, obj := "elas"]
  av.elas[, type := "taxable"]
  
  ## Objective for fiscal externality
  for (n in 0:(K-1)){
    data[, paste0("b",n) := (get(paste0("b",n)))*(exp(ln_sales_tax)-1)/exp(ln_sales_tax)]
  }
  av.fe <- data[ , lapply(.SD, weighted.mean, w = base.sales), by = .(fips_state), .SDcols = paste0("b",0:(K-1))]
  av.fe[, K := K]
  av.fe[, obj := "fe"]
  av.fe[, type := "taxable"]
  
  data.objective <- rbind(data.objective, av.elas, av.fe, fill = T)
  fwrite(data.objective, output.table)
  
}
               
# 2. Reduced rate

all_pi_t <- all_pi[taxability == 2, ]

for (K in 10:2) {
  
  ### Consumer Price
  
  ## Objective for average elasticity 
  data <- all_pi_t
  # Calculate berstein polynomials
  for (n in 0:(K-1)){
    data[, paste0("b",n) := bernstein(r.dm.ln_cpricei2, n, K-1)]
  }
  av.elas <- data[ , lapply(.SD, weighted.mean, w = base.sales), by = .(fips_state), .SDcols = paste0("b",0:(K-1))]
  av.elas[, K := K]
  av.elas[, obj := "elas"]
  av.elas[, type := "reduced"]
  
  ## Objective for fiscal externality
  for (n in 0:(K-1)){
    data[, paste0("b",n) := (get(paste0("b",n)))*(exp(ln_sales_tax)-1)/exp(ln_sales_tax)]
  }
  av.fe <- data[ , lapply(.SD, weighted.mean, w = base.sales), by = .(fips_state), .SDcols = paste0("b",0:(K-1))]
  av.fe[, K := K]
  av.fe[, obj := "fe"]
  av.fe[, type := "reduced"]
  
  data.objective <- rbind(data.objective, av.elas, av.fe, fill = T)
  fwrite(data.objective, output.table)
  
}

               
