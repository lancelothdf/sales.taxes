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


# Identify taxability of module: import
taxability_panel <- fread("/project2/igaarder/Data/taxability_state_panel.csv")
# For now, make reduced rate another category
taxability_panel[, taxability := ifelse(!is.na(reduced_rate), 2, taxability)]
# We will use taxability as of December 2014
taxability_panel <- taxability_panel[(month==12 & year==2014),][, .(product_module_code, product_group_code,
                                                                    fips_state, taxability, FoodNonfood)]

## Merge to products
all_pi<- merge(all_pi, taxability_panel, by = c("product_module_code", "fips_state"))


## Keep only taxable items as those are whose responses we care


output <- data.table(NULL)
###### 1. Calculate information-------------

## Calculate all elasticities: t>0
all_pi_t <- all_pi[ln_sales_tax > 0, .(av.dm.ln_cpricei2 = weighted.mean(dm.ln_cpricei2 , w = base.sales),
                                       av.fe2 = weighted.mean((exp(ln_sales_tax)-1)/(exp(ln_sales_tax))*dm.ln_cpricei2 , w = base.sales),
                                       av.fe3 = weighted.mean((exp(ln_sales_tax)-1)/(exp(ln_sales_tax))*dm.ln_cpricei2^2, w = base.sales),
                                       av.sales_tax = weighted.mean(exp(ln_sales_tax) -1 , w = base.sales),
                                       av.ln_sales_tax = weighted.mean(ln_sales_tax , w = base.sales),
                                       av.d_sales_tax = weighted.mean((exp(ln_sales_tax)-1)/(exp(ln_sales_tax)), w = base.sales),
                                       av.1_d_sales_tax = weighted.mean(1/(exp(ln_sales_tax)), w = base.sales),
                    N = .N) , by = .(fips_state)]
all_pi_t[, type := "t>0"]

output <- rbind(output, all_pi_t)

## Calculate all elasticities: taxable
all_pi_t <- all_pi[taxability == 1, .(av.dm.ln_cpricei2 = weighted.mean(dm.ln_cpricei2 , w = base.sales),
                                      av.fe2 = weighted.mean((exp(ln_sales_tax)-1)/(exp(ln_sales_tax))*dm.ln_cpricei2 , w = base.sales),
                                      av.fe3 = weighted.mean((exp(ln_sales_tax)-1)/(exp(ln_sales_tax))*dm.ln_cpricei2^2, w = base.sales),
                                      av.sales_tax = weighted.mean(exp(ln_sales_tax) -1 , w = base.sales),
                                      av.ln_sales_tax = weighted.mean(ln_sales_tax , w = base.sales),
                                         av.d_sales_tax = weighted.mean((exp(ln_sales_tax)-1)/(exp(ln_sales_tax)), w = base.sales),
                                      av.1_d_sales_tax = weighted.mean(1/(exp(ln_sales_tax)), w = base.sales),
                                      N = .N) , by = .(fips_state)]
all_pi_t[, type := "taxable"]

output <- rbind(output, all_pi_t)

## Calculate all elasticities: reduced
all_pi_t <- all_pi[taxability == 2, .(av.dm.ln_cpricei2 = weighted.mean(dm.ln_cpricei2 , w = base.sales),
                                      av.fe2 = weighted.mean((exp(ln_sales_tax)-1)/(exp(ln_sales_tax))*dm.ln_cpricei2 , w = base.sales),
                                      av.fe3 = weighted.mean((exp(ln_sales_tax)-1)/(exp(ln_sales_tax))*dm.ln_cpricei2^2, w = base.sales),
                                      av.sales_tax = weighted.mean(exp(ln_sales_tax) -1 , w = base.sales),
                                      av.ln_sales_tax = weighted.mean(ln_sales_tax , w = base.sales),
                                        av.d_sales_tax = weighted.mean((exp(ln_sales_tax)-1)/(exp(ln_sales_tax)), w = base.sales),
                                      av.1_d_sales_tax = weighted.mean(1/(exp(ln_sales_tax)), w = base.sales),
                                      N = .N) , by = .(fips_state)]
all_pi_t[, type := "reduced"]

output <- rbind(output, all_pi_t)


## Calculate all elasticities: Food
all_pi_t <- all_pi[FoodNonfood == 1, .(av.dm.ln_cpricei2 = weighted.mean(dm.ln_cpricei2 , w = base.sales),
                                       av.fe2 = weighted.mean((exp(ln_sales_tax)-1)/(exp(ln_sales_tax))*dm.ln_cpricei2 , w = base.sales),
                                       av.fe3 = weighted.mean((exp(ln_sales_tax)-1)/(exp(ln_sales_tax))*dm.ln_cpricei2^2, w = base.sales),
                                       av.sales_tax = weighted.mean(exp(ln_sales_tax) -1 , w = base.sales),
                                       av.ln_sales_tax = weighted.mean(ln_sales_tax , w = base.sales),
                                        av.d_sales_tax = weighted.mean((exp(ln_sales_tax)-1)/(exp(ln_sales_tax)), w = base.sales),
                                       av.1_d_sales_tax = weighted.mean(1/(exp(ln_sales_tax)), w = base.sales),
                                       N = .N) , by = .(fips_state)]
all_pi_t[, type := "Food"]

output <- rbind(output, all_pi_t)

## Calculate all elasticities: NonFood
all_pi_t <- all_pi[FoodNonfood == 0, .(av.dm.ln_cpricei2 = weighted.mean(dm.ln_cpricei2 , w = base.sales),
                                       av.fe2 = weighted.mean((exp(ln_sales_tax)-1)/(exp(ln_sales_tax))*dm.ln_cpricei2 , w = base.sales),
                                       av.fe3 = weighted.mean((exp(ln_sales_tax)-1)/(exp(ln_sales_tax))*dm.ln_cpricei2^2, w = base.sales),
                                       av.sales_tax = weighted.mean(exp(ln_sales_tax) -1 , w = base.sales),
                                       av.ln_sales_tax = weighted.mean(ln_sales_tax , w = base.sales),
                                       av.d_sales_tax = weighted.mean((exp(ln_sales_tax)-1)/(exp(ln_sales_tax)), w = base.sales),
                                       av.1_d_sales_tax = weighted.mean(1/(exp(ln_sales_tax)), w = base.sales),
                                       N = .N) , by = .(fips_state)]
all_pi_t[, type := "NonFood"]

output <- rbind(output, all_pi_t)


###### 3. Export Results ---------------------
fwrite(output, output.table)

# Capture this value
all_pi[taxability == 1, weighted.mean(dm.ln_cpricei2 , w = base.sales)]
