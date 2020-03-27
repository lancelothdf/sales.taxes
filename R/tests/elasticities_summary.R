#' Sales Taxes Project. Compare estimates
#' In this code we produce a summary table of the results. We pick some states based on their average prices and
#' compare the estimated elasticities under different scenarios using our base estimates
#' To do this we take some inputs:
#' 1) Our final data
#' 2) Our estimates:
#'   i)   Elasticity in log-linear
#'   ii)  Elasticity in log-quadratic (log-cubic) using subsamples
#'   iii) Elasticity in log-quadratic (log-cubic) using partial-id
#' 1) and 2ii) are included as csv and 2i) and 2iii) are written
#' We use the estimates to compute average elasticities under different scenarios for each State

library(data.table)
library(futile.logger)
library(lfe)
library(multcomp)

setwd("/project2/igaarder")


## inputs -----------------------------------------------
data.semester <- "Data/Nielsen/semester_nielsen_data.csv"
bounds.data <- "Data/elasticity_bounds_table_berns_monot_mincreterion_d.csv"
linear.elas <- -0.5760/1.0508
quad.elas <- c(-0.513, 2*1.746)
cubic.elas <- c(-0.641, 2*1.705, 3*8.423)
tetra.elas <- c(-0.627, 2*4.865, 3*8.869, -112.32*4)
#states <- c()

## Outputs ----------------------------------------------
output.table <- "Data/summary_elasticity_states.csv"


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


### Open estimated elasticities bounds
bounds <- fread(bounds.data)

## Keep all bounds for d <=5 and K <= 7
bounds <- bounds[ D <= 5 & K %in% c(2,7)]

## dcast data (long to wide)
bounds <- dcast(bounds, "p + D ~ K", value.var = c("elas.down", "elas.up"), fun = sum)
  
###### 1. Calculate eslaticities -------------

## Keep interest states
#elasticities <- all_pi[ fips_state %in% states]
elasticities <- all_pi

## Keep only taxable items as those are whose responses we care
elasticities <- elasticities[ln_sales_tax > 0]

## Need to round to the third decimal point to match the bounds we have estimated
elasticities[, p := round(dm.ln_cpricei2, 3)]

## Merge estimated bounds
elasticities <- merge(elasticities, bounds, by = "p")

## Calculate all elasticities
elasticities <- elasticities[, .( av.elas_1 = linear.elas,
                                  av.elas_2 = weighted.mean(quad.elas[1] + quad.elas[2]*dm.ln_cpricei2, w = base.sales),
                                  av.elas_3 = weighted.mean(cubic.elas[1] + cubic.elas[2]*dm.ln_cpricei2 + cubic.elas[3]*dm.ln_cpricei2^2, w = base.sales),
                                  av.elas_4 = weighted.mean(tetra.elas[1] + tetra.elas[2]*dm.ln_cpricei2 + tetra.elas[3]*dm.ln_cpricei2^2 + tetra.elas[4]*dm.ln_cpricei2^3, w = base.sales),
                                  av.elas.down_2 = weighted.mean(elas.down_2 , w = base.sales),
                                  av.elas.down_3 = weighted.mean(elas.down_3 , w = base.sales),
                                  av.elas.down_4 = weighted.mean(elas.down_4 , w = base.sales),
                                  av.elas.down_5 = weighted.mean(elas.down_5 , w = base.sales),
                                  av.elas.down_6 = weighted.mean(elas.down_6 , w = base.sales),
                                  av.elas.down_7 = weighted.mean(elas.down_7 , w = base.sales),
                                  av.elas.up_2 = weighted.mean(elas.up_2 , w = base.sales),
                                  av.elas.up_3 = weighted.mean(elas.up_3 , w = base.sales),
                                  av.elas.up_4 = weighted.mean(elas.up_4 , w = base.sales),
                                  av.elas.up_5 = weighted.mean(elas.up_5 , w = base.sales),
                                  av.elas.up_6 = weighted.mean(elas.up_6 , w = base.sales),
                                  av.elas.up_7 = weighted.mean(elas.up_7 , w = base.sales),
                                  N = .N
                                  ) , by = .(fips_state, D)]

###### 2. Export Results ---------------------
fwrite(elasticities, output.table)



