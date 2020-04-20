#' Sales Taxes Project
#' Explore doble de-meaned distributions in the common support
#' 

library(tidyverse)
library(data.table)
library(futile.logger)
library(lfe)
library(multcomp)
library(ggplot2)



setwd("/project2/igaarder")


## input filepaths -----------------------------------------------
#' This data is the same as all_goods_pi_path, except it has 2015-2016 data as well.
data.semester <- "Data/Nielsen/semester_nielsen_data.csv"


### Set up Semester Data ---------------------------------
all_pi <- fread(data.semester)
all_pi[, w.ln_sales_tax := ln_sales_tax - mean(ln_sales_tax), by = .(store_by_module)]
all_pi[, w.ln_cpricei2 := ln_cpricei2 - mean(ln_cpricei2), by = .(store_by_module)]
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

## Demean again variables by division x module x time
all_pi[, dd.ln_sales_tax := w.ln_sales_tax - mean(w.ln_sales_tax), by = .(division_by_module_by_time)]
all_pi[, dd.ln_cpricei2 := w.ln_cpricei2 - mean(w.ln_cpricei2), by = .(division_by_module_by_time)]
all_pi[, dd.ln_quantity3 := w.ln_quantity3 - mean(w.ln_quantity3), by = .(division_by_module_by_time)]


# Do this for each sample of interest
control <- all_pi[D.ln_sales_tax == 0,]
treated <- all_pi[D.ln_sales_tax != 0,]

vars <- c("dd.ln_sales_tax", "dd.ln_cpricei2", "dd.ln_quantity3")


dist <- data.table(NULL)
for (var in vars) {
  
  ## Full sample
  obs <- all_pi[[var]]
  step <- (max(obs, na.rm = T) - min(obs, na.rm = T) )/1500
  min <- min(obs, na.rm = T)
  all_pi[, st := floor((get(var) - min)/step)]
  dens <-  all_pi[, .(dens.v = sum(base.sales)), by = .(st)]
  dens[, dens.v := dens.v/sum(dens.v)]
  dens[, var := st*step + min + step/2]
  setnames(dens, "dens.v", paste0("dens.", var))
  if (is.null(dist)) {
    dist <- dens
  } else {
    dist <- merge(dist, dens, by = "st")
  }
  
  # Treated
  obs <- treated[[var]]
  step <- (max(obs, na.rm = T) - min(obs, na.rm = T) )/1500
  min <- min(obs, na.rm = T)
  treated[, st := floor((get(var) - min)/step)]
  dens <-  treated[, .(dens.v = sum(base.sales)), by = .(st)]
  dens[, dens.v := dens.v/sum(dens.v)]
  dens[, paste0(var, ".T") := st*step + min + step/2]
  setnames(dens, "dens.v", paste0("dens.", var,".T"))
  dist <- merge(dist, dens, by = "st")
  
  # Control
  obs <- control[[var]]
  step <- (max(obs, na.rm = T) - min(obs, na.rm = T) )/1500
  min <- min(obs, na.rm = T)
  control[, st := floor((get(var) - min)/step)]
  dens <-  control[, .(dens.v = sum(base.sales)), by = .(st)]
  dens[, dens.v := dens.v/sum(dens.v)]
  dens[, paste0(var, ".C") := st*step + min + step/2]
  setnames(dens, "dens.v", paste0("dens.", var,".C"))
  dist <- merge(dist, dens, by = "dens.v")
  
  
}
## Treated
fwrite(dist, "../../home/slacouture/NLP/Ddemeaned_dens.csv")



