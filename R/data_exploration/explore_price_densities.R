## Sales Taxes
#' Extract distributions of de-meaned prices to plot them
#' using different number of bins. Semesterly data
#' 

library(data.table)
library(ggplot2)
library(zoo)
library(tidyverse)
library(stringr)

setwd("/project2/igaarder")


## input filepaths -----------------------------------------------
data.semester <- "Data/Nielsen/semester_nielsen_data.csv"


## output filepaths ----------------------------------------------
output.results.file <- "Data/Demean_Price_Distribution.csv"

### Set up Semester Data ---------------------------------
all_pi <- fread(data.semester)

## Demean Prices
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

bins <- c(30, 50, 100, 150)
histogram.data <- data.table(NULL)
for (bin.n in bins) {
  
  binwidth <- (max(all_pi$dm.ln_cpricei2, na.rm = T)-min(all_pi$dm.ln_cpricei2, na.rm = T))/bin.n
  all_pi[, p_group := floor(((dm.ln_cpricei2 - min(dm.ln_cpricei2, na.rm = T))/binwidth))]
  all_pi[, p_ll := p_group*binwidth]
  all_pi[, p_ll := p_ll + min(dm.ln_cpricei2, na.rm = T)]
  all_pi[, p_ul := p_ll + binwidth]
  
  bin.range <- all_pi[, .(density = sum(base.sales)), by = .(p_ul, p_ll, p_group)]
  bin.range[, density := density/(sum(density))]
  bin.range[, n.bins := bin.n]
  histogram.data <- rbind(histogram.data, bin.range)
}

fwrite(histogram.data, output.results.file)