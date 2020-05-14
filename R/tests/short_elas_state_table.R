#' Sales Taxes Project. Extract short infor to compute MVPFs using bounds on different scenarios
#' This version: extract also average Fiscal Externality to compute MVPF using actual tax rate

library(data.table)
library(futile.logger)
library(lfe)
library(multcomp)

setwd("/project2/igaarder")


## inputs -----------------------------------------------
data.semester <- "Data/Nielsen/semester_nielsen_data.csv"

## Outputs ----------------------------------------------
output.table <- "Data/short_elas_state.csv"


##### 0. Open Data and clean as needed -----------
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



###### 1. Calculate information-------------

## Calculate all elasticities
all_pi <- all_pi[, .(av.dm.ln_cpricei2 = weighted.mean(dm.ln_cpricei2 , w = base.sales),
                    av.ln_sales_tax = weighted.mean(ln_sales_tax , w = base.sales),
                    av.d_sales_tax = weighted.mean((exp(ln_sales_tax)-1)/(exp(ln_sales_tax)), w = base.sales),
                    av.d_sales_tax_theta0117 = weighted.mean((exp(ln_sales_tax)-1 + 0.117)/(exp(ln_sales_tax)), w = base.sales),
                    av.d_sales_tax_theta0100 = weighted.mean((exp(ln_sales_tax)-1 + 0.1)/(exp(ln_sales_tax)), w = base.sales),
                    av.d_sales_tax_theta0050 = weighted.mean((exp(ln_sales_tax)-1 + 0.05)/(exp(ln_sales_tax)), w = base.sales),
                    N = .N) , by = .(fips_state)]

###### 3. Export Results ---------------------
fwrite(all_pi, output.table)
