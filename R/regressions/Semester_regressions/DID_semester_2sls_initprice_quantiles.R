# Sales Taxes PRoject
# In this code we re-do estimate of evidence on non-linearities, but also using 2SLS
# We do this for consumer price and pre-tax and splitting them by both
# We only show evidence for all sample and 5 quintiles


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
iv.output.results.file <- "Data/IV_2sls_initial_price_semester.csv"


## Bernstein basis Function -------------------------------------------

bernstein <- function(x, k, K){
  choose(K, k) * x^k * (1 - x)^(K - k)
}

### Set up Semester Data ---------------------------------
all_pi <- fread(data.semester)
all_pi[, w.ln_sales_tax := ln_sales_tax - mean(ln_sales_tax), by = .(store_by_module)]
all_pi[, w.ln_cpricei2 := ln_cpricei2 - mean(ln_cpricei2), by = .(store_by_module)]
all_pi[, w.ln_pricei2 := ln_pricei2 - mean(ln_pricei2), by = .(store_by_module)]
all_pi[, w.ln_quantity3 := ln_quantity3 - mean(ln_quantity3), by = .(store_by_module)]

# Need to demean
all_pi[, module_by_time := .GRP, by = .(product_module_code, semester, year)]
all_pi[, L.ln_cpricei2 := ln_cpricei2 - D.ln_cpricei2]
all_pi[, L.ln_pricei2 := ln_pricei2 - D.ln_pricei2]
all_pi[, dm.L.ln_cpricei2 := L.ln_cpricei2 - mean(L.ln_cpricei2, na.rm = T), by = module_by_time]
all_pi[, dm.L.ln_pricei2 := L.ln_pricei2 - mean(L.ln_pricei2, na.rm = T), by = module_by_time]
all_pi[, dm.ln_cpricei2 := ln_cpricei2 - mean(ln_cpricei2, na.rm = T), by = module_by_time]
all_pi[, dm.ln_pricei2 := ln_pricei2 - mean(ln_pricei2, na.rm = T), by = module_by_time]
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

## cut the tails (keep between 1st and 99th percentile)
pct1 <- quantile(all_pi$dm.ln_cpricei2, probs = 0.01, na.rm = T, weight=base.sales)
pct99 <- quantile(all_pi$dm.ln_cpricei2, probs = 0.99, na.rm = T, weight=base.sales)
all_pi <- all_pi[(dm.ln_cpricei2 > pct1 & dm.ln_cpricei2 < pct99),]


outcomes.sep <- c("w.ln_cpricei2", "w.ln_pricei2", "w.ln_quantity3")
endogenous.2sls <- c("w.ln_cpricei2", "w.ln_pricei2")
FE_opts <- c("group_region_by_module_by_time", "group_division_by_module_by_time")

LRdiff_res <- data.table(NULL)

##### Full sample IV

for (FE in FE_opts) {
  for (X in endogenous.2sls) {
    formula1 <- as.formula(paste0(
      "w.ln_quantity3 ~ 0 | ", FE, " | (", X," ~ w.ln_sales_tax) | module_by_state"
    ))
    res1 <- felm(formula = formula1, all_pi = all_pi,
                 weights = all_pi$base.sales)
    
    
    ## attach results
    res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
    res1.dt[, outcome := X]
    res1.dt[, controls := FE]
    res1.dt[, lev := 0]
    res1.dt[, init := "all"]
    res1.dt[, spec := "IV"]
    print(res1.dt)
    
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, iv.output.results.file)
  }  
}


#### Splitting by CONSUMER prices

# Create groups of initial values of tax rate (consumer)
# We use the full weighted distribution
all_pi <- all_pi[, quantile := cut(dm.L.ln_cpricei2,
                                   breaks = quantile(dm.L.ln_cpricei2, probs = seq(0, 1, by = 1/5), na.rm = T, weight = base.sales),
                                   labels = 1:5, right = FALSE)]
quantlab <- round(quantile(all_pi$dm.L.ln_cpricei2, 
                           probs = seq(0, 1, by = 1/5), na.rm = T, 
                           weight = all_pi$base.sales), digits = 4)
# Saturate fixed effects
all_pi[, group_region_by_module_by_time := .GRP, by = .(region_by_module_by_time, quantile)]
all_pi[, group_division_by_module_by_time := .GRP, by = .(division_by_module_by_time, quantile)]


### RF/FS manual version
for (FE in FE_opts) {
  for (Y in outcomes.sep) {
    formula1 <- as.formula(paste0(
      Y, " ~ w.ln_sales_tax:quantile | ", FE, "+ quantile | 0 | module_by_state"
    ))
    res1 <- felm(formula = formula1, data = all_pi,
                 weights = all_pi$base.sales)
    
    
    ## attach results
    res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
    res1.dt[, outcome := Y]
    res1.dt[, controls := FE]
    res1.dt[, lev := quantlab[-1]]
    res1.dt[, init := "cons"]
    res1.dt[, spec := "Sep"]
    print(res1.dt)
    
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, iv.output.results.file)
  }
}

### 2SLS estimate
for (i in 1:5) {
  data <- all_pi[quantile == i]
  for (FE in FE_opts) {
    for (X in endogenous.2sls) {
      formula1 <- as.formula(paste0(
        "w.ln_quantity3 ~ 0 | ", FE, " | (", X," ~ w.ln_sales_tax) | module_by_state"
      ))
      res1 <- felm(formula = formula1, data = data,
                   weights = data$base.sales)
      
      
      ## attach results
      res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
      res1.dt[, outcome := X]
      res1.dt[, controls := FE]
      res1.dt[, lev := quantlab[i+1]]
      res1.dt[, init := "cons"]
      res1.dt[, spec := "IV"]
      print(res1.dt)
      
      LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
      fwrite(LRdiff_res, iv.output.results.file)
    }  
  }
}

#### Splitting by PRODUCER prices

# Create groups of initial values of tax rate (producer)
# We use the full weighted distribution
all_pi <- all_pi[, quantile := cut(dm.L.ln_pricei2,
                                   breaks = quantile(dm.L.ln_pricei2, probs = seq(0, 1, by = 1/5), na.rm = T, weight = base.sales),
                                   labels = 1:5, right = FALSE)]
quantlab <- round(quantile(all_pi$dm.L.ln_pricei2, 
                           probs = seq(0, 1, by = 1/5), na.rm = T, 
                           weight = all_pi$base.sales), digits = 4)
# Saturate fixed effects
all_pi[, group_region_by_module_by_time := .GRP, by = .(region_by_module_by_time, quantile)]
all_pi[, group_division_by_module_by_time := .GRP, by = .(division_by_module_by_time, quantile)]


### RF/FS manual version
for (FE in FE_opts) {
  for (Y in outcomes.sep) {
    formula1 <- as.formula(paste0(
      Y, " ~ w.ln_sales_tax:quantile | ", FE, "+ quantile | 0 | module_by_state"
    ))
    res1 <- felm(formula = formula1, data = all_pi,
                 weights = all_pi$base.sales)
    
    
    ## attach results
    res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
    res1.dt[, outcome := Y]
    res1.dt[, controls := FE]
    res1.dt[, lev := quantlab[-1]]
    res1.dt[, init := "prod"]
    res1.dt[, spec := "Sep"]
    print(res1.dt)
    
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, iv.output.results.file)
  }
}

### 2SLS estimate
for (i in 1:5) {
  data <- all_pi[quantile == i]
  for (FE in FE_opts) {
    for (X in endogenous.2sls) {
      formula1 <- as.formula(paste0(
        "w.ln_quantity3 ~ 0 | ", FE, " | (", X," ~ w.ln_sales_tax) | module_by_state"
      ))
      res1 <- felm(formula = formula1, data = data,
                   weights = data$base.sales)
      
      
      ## attach results
      res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
      res1.dt[, outcome := X]
      res1.dt[, controls := FE]
      res1.dt[, lev := quantlab[i+1]]
      res1.dt[, init := "prod"]
      res1.dt[, spec := "IV"]
      print(res1.dt)
      
      LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
      fwrite(LRdiff_res, iv.output.results.file)
    }  
  }
}