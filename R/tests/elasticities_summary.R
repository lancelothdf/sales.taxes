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
#' This version: extract also average Fiscal Externality to compute MVPF using actual tax rate

library(data.table)
library(futile.logger)
library(lfe)
library(multcomp)

setwd("/project2/igaarder")


## inputs -----------------------------------------------
data.semester <- "Data/Nielsen/semester_nielsen_data.csv"
bounds.data <- "Data/elasticity_bounds_table_berns_monot_mincreterion_d.csv"
bounds.data.pretax <- "Data/elasticity_bounds_table_berns_monot_mincreterion_d_pretax.csv"
linear.elas <- -0.5760/1.0508
quad.elas <- c(-0.513, 2*1.746)
cubic.elas <- c(-0.641, 2*1.705, 3*8.423)
tetra.elas <- c(-0.627, 2*4.865, 3*8.869, -112.32*4)
linear.elas.pt <- linear.elas
quad.elas.pt <- c(-0.5004, 2*0.9512)
cubic.elas.pt <- c(-0.6709, 2*0.2974, 3*11.1323)
tetra.elas.pt <- c(-0.5928, 2*5.0307, 3*9.2581, 4*-172.9887)
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


## Keep only taxable items as those are whose responses we care
elasticities <- all_pi[ln_sales_tax > 0]

## Need to round to the third decimal point to match the bounds we have estimated
elasticities[, p := round(dm.ln_cpricei2, 3)]


###### 1. Calculate eslaticities: perfect salience -------------

### Open estimated elasticities bounds
bounds <- fread(bounds.data)

## Keep all bounds for d <=3 and K <= 10
bounds <- bounds[ D <= 3 & K %in% seq(2,10,1)]

## dcast data (long to wide)
bounds <- dcast(bounds, "p + D ~ K", value.var = c("elas.down", "elas.up"), fun = sum)
setnames(bounds, "D", "L")
  
## Merge estimated bounds
elasticities.1 <- merge(elasticities, bounds, by = "p", allow.cartesian=T)

## Calculate all elasticities
elasticities.1 <- elasticities.1[, .( av.elas_1 = linear.elas,
                                  av.elas_2 = weighted.mean(quad.elas[1] + quad.elas[2]*dm.ln_cpricei2, w = base.sales),
                                  av.elas_3 = weighted.mean(cubic.elas[1] + cubic.elas[2]*dm.ln_cpricei2 + cubic.elas[3]*dm.ln_cpricei2^2, w = base.sales),
                                  av.elas_4 = weighted.mean(tetra.elas[1] + tetra.elas[2]*dm.ln_cpricei2 + tetra.elas[3]*dm.ln_cpricei2^2 + tetra.elas[4]*dm.ln_cpricei2^3, w = base.sales),
                                  av.elas.down_2 = weighted.mean(elas.down_2 , w = base.sales),
                                  av.elas.down_3 = weighted.mean(elas.down_3 , w = base.sales),
                                  av.elas.down_4 = weighted.mean(elas.down_4 , w = base.sales),
                                  av.elas.down_5 = weighted.mean(elas.down_5 , w = base.sales),
                                  av.elas.down_6 = weighted.mean(elas.down_6 , w = base.sales),
                                  av.elas.down_7 = weighted.mean(elas.down_7 , w = base.sales),
                                  av.elas.down_10 = weighted.mean(elas.down_10 , w = base.sales),
                                  av.elas.up_2 = weighted.mean(elas.up_2 , w = base.sales),
                                  av.elas.up_3 = weighted.mean(elas.up_3 , w = base.sales),
                                  av.elas.up_4 = weighted.mean(elas.up_4 , w = base.sales),
                                  av.elas.up_5 = weighted.mean(elas.up_5 , w = base.sales),
                                  av.elas.up_6 = weighted.mean(elas.up_6 , w = base.sales),
                                  av.elas.up_7 = weighted.mean(elas.up_7 , w = base.sales),
                                  av.elas.up_10 = weighted.mean(elas.up_10 , w = base.sales),
                                  av.fe_2 = weighted.mean((quad.elas[1] + quad.elas[2]*dm.ln_cpricei2)*(exp(ln_sales_tax)-1)/(exp(ln_sales_tax)), w = base.sales),
                                  av.fe_3 = weighted.mean((cubic.elas[1] + cubic.elas[2]*dm.ln_cpricei2 + cubic.elas[3]*dm.ln_cpricei2^2)*(exp(ln_sales_tax)-1)/(exp(ln_sales_tax)), w = base.sales),
                                  av.fe_4 = weighted.mean((tetra.elas[1] + tetra.elas[2]*dm.ln_cpricei2 + tetra.elas[3]*dm.ln_cpricei2^2+ tetra.elas[4]*dm.ln_cpricei2^3)*(exp(ln_sales_tax)-1)/(exp(ln_sales_tax)), w = base.sales),
                                  av.fe.down_2 = weighted.mean(elas.down_2*(exp(ln_sales_tax)-1)/(exp(ln_sales_tax)), w = base.sales),
                                  av.fe.down_3 = weighted.mean(elas.down_3*(exp(ln_sales_tax)-1)/(exp(ln_sales_tax)), w = base.sales),
                                  av.fe.down_4 = weighted.mean(elas.down_4*(exp(ln_sales_tax)-1)/(exp(ln_sales_tax)), w = base.sales),
                                  av.fe.down_5 = weighted.mean(elas.down_5*(exp(ln_sales_tax)-1)/(exp(ln_sales_tax)), w = base.sales),
                                  av.fe.down_6 = weighted.mean(elas.down_6*(exp(ln_sales_tax)-1)/(exp(ln_sales_tax)), w = base.sales),
                                  av.fe.down_7 = weighted.mean(elas.down_7*(exp(ln_sales_tax)-1)/(exp(ln_sales_tax)), w = base.sales),
                                  av.fe.down_10 = weighted.mean(elas.down_10*(exp(ln_sales_tax)-1)/(exp(ln_sales_tax)), w = base.sales),
                                  av.fe.up_2 = weighted.mean(elas.up_2*(exp(ln_sales_tax)-1)/(exp(ln_sales_tax)), w = base.sales),
                                  av.fe.up_3 = weighted.mean(elas.up_3*(exp(ln_sales_tax)-1)/(exp(ln_sales_tax)), w = base.sales),
                                  av.fe.up_4 = weighted.mean(elas.up_4*(exp(ln_sales_tax)-1)/(exp(ln_sales_tax)), w = base.sales),
                                  av.fe.up_5 = weighted.mean(elas.up_5*(exp(ln_sales_tax)-1)/(exp(ln_sales_tax)), w = base.sales),
                                  av.fe.up_6 = weighted.mean(elas.up_6*(exp(ln_sales_tax)-1)/(exp(ln_sales_tax)), w = base.sales),
                                  av.fe.up_7 = weighted.mean(elas.up_7*(exp(ln_sales_tax)-1)/(exp(ln_sales_tax)), w = base.sales),
                                  av.fe.up_10 = weighted.mean(elas.up_10*(exp(ln_sales_tax)-1)/(exp(ln_sales_tax)), w = base.sales),
                                  av.dm.ln_cpricei2 = weighted.mean(dm.ln_cpricei2 , w = base.sales),
                                  av.ln_sales_tax = weighted.mean(ln_sales_tax , w = base.sales),
                                  av.d_sales_tax = weighted.mean((exp(ln_sales_tax)-1)/(exp(ln_sales_tax)), w = base.sales),
                                  av.d_sales_tax_theta0117 = weighted.mean((exp(ln_sales_tax)-1 + 0.117)/(exp(ln_sales_tax)), w = base.sales),
                                  av.d_sales_tax_theta0100 = weighted.mean((exp(ln_sales_tax)-1 + 0.1)/(exp(ln_sales_tax)), w = base.sales),
                                  av.d_sales_tax_theta0050 = weighted.mean((exp(ln_sales_tax)-1 + 0.05)/(exp(ln_sales_tax)), w = base.sales),
                                  N = .N
                                  ) , by = .(fips_state, L)]
elasticities.1[, weight := "consumer price"]


###### 2. Calculate eslaticities: imperfect salience -------------

### Open estimated elasticities bounds
bounds <- fread(bounds.data.pretax)

## Keep all bounds for d <=3 and K <= 10
bounds <- bounds[ D <= 3 & K %in% seq(2,10,1)]

## dcast data (long to wide)
bounds <- dcast(bounds, "p + D ~ K", value.var = c("elas.down", "elas.up"), fun = sum)
setnames(bounds, "D", "L")

## Merge estimated bounds
elasticities.2 <- merge(elasticities, bounds, by = "p", allow.cartesian=T)

## Calculate all elasticities
elasticities.2 <- elasticities.2[, .(av.elas_1 = linear.elas.pt,
                                    av.elas_2 = weighted.mean(quad.elas.pt[1] + quad.elas.pt[2]*dm.ln_cpricei2, w = base.sales),
                                    av.elas_3 = weighted.mean(cubic.elas.pt[1] + cubic.elas.pt[2]*dm.ln_cpricei2 + cubic.elas.pt[3]*dm.ln_cpricei2^2, w = base.sales),
                                    av.elas_4 = weighted.mean(tetra.elas.pt[1] + tetra.elas.pt[2]*dm.ln_cpricei2 + tetra.elas.pt[3]*dm.ln_cpricei2^2 + tetra.elas.pt[4]*dm.ln_cpricei2^3, w = base.sales),
                                    av.elas.down_2 = weighted.mean(elas.down_2 , w = base.sales),
                                    av.elas.down_3 = weighted.mean(elas.down_3 , w = base.sales),
                                    av.elas.down_4 = weighted.mean(elas.down_4 , w = base.sales),
                                    av.elas.down_5 = weighted.mean(elas.down_5 , w = base.sales),
                                    av.elas.down_6 = weighted.mean(elas.down_6 , w = base.sales),
                                    av.elas.down_7 = weighted.mean(elas.down_7 , w = base.sales),
                                    av.elas.down_10 = weighted.mean(elas.down_10 , w = base.sales),
                                    av.elas.up_2 = weighted.mean(elas.up_2 , w = base.sales),
                                    av.elas.up_3 = weighted.mean(elas.up_3 , w = base.sales),
                                    av.elas.up_4 = weighted.mean(elas.up_4 , w = base.sales),
                                    av.elas.up_5 = weighted.mean(elas.up_5 , w = base.sales),
                                    av.elas.up_6 = weighted.mean(elas.up_6 , w = base.sales),
                                    av.elas.up_7 = weighted.mean(elas.up_7 , w = base.sales),
                                    av.elas.up_10 = weighted.mean(elas.up_10 , w = base.sales),
                                    av.fe_2 = weighted.mean((quad.elas.pt[1] + quad.elas.pt[2]*dm.ln_cpricei2)*(exp(ln_sales_tax)-1)/(exp(ln_sales_tax)), w = base.sales),
                                    av.fe_3 = weighted.mean((cubic.elas.pt[1] + cubic.elas.pt[2]*dm.ln_cpricei2 + cubic.elas.pt[3]*dm.ln_cpricei2^2)*(exp(ln_sales_tax)-1)/(exp(ln_sales_tax)), w = base.sales),
                                    av.fe_4 = weighted.mean((tetra.elas.pt[1] + tetra.elas.pt[2]*dm.ln_cpricei2 + tetra.elas.pt[3]*dm.ln_cpricei2^2+ tetra.elas.pt[4]*dm.ln_cpricei2^3)*(exp(ln_sales_tax)-1)/(exp(ln_sales_tax)), w = base.sales),
                                    av.fe.down_2 = weighted.mean(elas.down_2*(exp(ln_sales_tax)-1)/(exp(ln_sales_tax)), w = base.sales),
                                    av.fe.down_3 = weighted.mean(elas.down_3*(exp(ln_sales_tax)-1)/(exp(ln_sales_tax)), w = base.sales),
                                    av.fe.down_4 = weighted.mean(elas.down_4*(exp(ln_sales_tax)-1)/(exp(ln_sales_tax)), w = base.sales),
                                    av.fe.down_5 = weighted.mean(elas.down_5*(exp(ln_sales_tax)-1)/(exp(ln_sales_tax)), w = base.sales),
                                    av.fe.down_6 = weighted.mean(elas.down_6*(exp(ln_sales_tax)-1)/(exp(ln_sales_tax)), w = base.sales),
                                    av.fe.down_7 = weighted.mean(elas.down_7*(exp(ln_sales_tax)-1)/(exp(ln_sales_tax)), w = base.sales),
                                    av.fe.down_10 = weighted.mean(elas.down_10*(exp(ln_sales_tax)-1)/(exp(ln_sales_tax)), w = base.sales),
                                    av.fe.up_2 = weighted.mean(elas.up_2*(exp(ln_sales_tax)-1)/(exp(ln_sales_tax)), w = base.sales),
                                    av.fe.up_3 = weighted.mean(elas.up_3*(exp(ln_sales_tax)-1)/(exp(ln_sales_tax)), w = base.sales),
                                    av.fe.up_4 = weighted.mean(elas.up_4*(exp(ln_sales_tax)-1)/(exp(ln_sales_tax)), w = base.sales),
                                    av.fe.up_5 = weighted.mean(elas.up_5*(exp(ln_sales_tax)-1)/(exp(ln_sales_tax)), w = base.sales),
                                    av.fe.up_6 = weighted.mean(elas.up_6*(exp(ln_sales_tax)-1)/(exp(ln_sales_tax)), w = base.sales),
                                    av.fe.up_7 = weighted.mean(elas.up_7*(exp(ln_sales_tax)-1)/(exp(ln_sales_tax)), w = base.sales),
                                    av.fe.up_10 = weighted.mean(elas.up_10*(exp(ln_sales_tax)-1)/(exp(ln_sales_tax)), w = base.sales),
                                    av.dm.ln_cpricei2 = weighted.mean(dm.ln_cpricei2 , w = base.sales),
                                    av.ln_sales_tax = weighted.mean(ln_sales_tax , w = base.sales),
                                    av.d_sales_tax = weighted.mean((exp(ln_sales_tax)-1)/(exp(ln_sales_tax)), w = base.sales),
                                    N = .N) , by = .(fips_state, L)]

elasticities.2[, weight := "pretax price"]

###### 3. Export Results ---------------------
elasticities <- rbind(elasticities.1, elasticities.2)
fwrite(elasticities, output.table)



