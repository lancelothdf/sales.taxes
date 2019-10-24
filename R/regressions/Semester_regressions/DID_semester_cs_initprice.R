#' Sales Taxes Project
#' This code run a Basic DiD model by initial tax rate or price level
#' The idea is to try to see whether we observe non linearities in this sense
#' We estimate both the "short run" and the "long run" models splitting the sample
#' by quantiles increasing the number of groups.
#' Here initial level means previous period and we divide by groups within the "common" support
#' 


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
output.results.file <- "Data/DID_split_initial_price_semester.csv"

### Set up Semester Data ---------------------------------
all_pi <- fread(data.semester)
all_pi[, w.ln_sales_tax := ln_sales_tax - mean(ln_sales_tax), by = .(store_by_module)]
all_pi[, w.ln_cpricei2 := ln_cpricei2 - mean(ln_cpricei2), by = .(store_by_module)]
all_pi[, w.ln_quantity3 := ln_quantity3 - mean(ln_quantity3), by = .(store_by_module)]

# Need to demean
all_pi[, module_by_time := .GRP, by = .(product_module_code, year)]
all_pi[, L.ln_cpricei2 := ln_cpricei2 - D.ln_cpricei2]
all_pi[, dm.L.ln_cpricei2 := L.ln_cpricei2 - mean(L.ln_cpricei2, na.rm = T), by = module_by_time]


# Defining common support
control <- all_pi[D.ln_sales_tax == 0,]
treated <- all_pi[D.ln_sales_tax != 0,]

# Price 
pct1.control <- quantile(control$dm.L.ln_cpricei2, probs = 0.01, na.rm = T, weight=all_pi$base.sales)
pct1.treated <- quantile(treated$dm.L.ln_cpricei2, probs = 0.01, na.rm = T, weight=all_pi$base.sales)

pct99.control <- quantile(control$dm.L.ln_cpricei2, probs = 0.99, na.rm = T, weight=all_pi$base.sales)
pct99treated <- quantile(treated$dm.L.ln_cpricei2, probs = 0.99, na.rm = T, weight=all_pi$base.sales)

all_pi[, cs_price := ifelse(dm.L.ln_cpricei2 > max(pct1.treated, pct1.control) & 
                              dm.L.ln_cpricei2 < min(pct99treated, pct99.control), 1, 0)]
# Make sure missings are 0s
all_pi[, cs_price := ifelse(is.na(dm.L.ln_cpricei2), 0, cs_price)]

## Keep within the common support
all_pi <- all_pi[cs_price == 1,]



outcomes.changes <- c("D.ln_cpricei2", "D.ln_quantity3")
outcomes.within <- c("w.ln_cpricei2", "w.ln_quantity3")
FE_opts <- c("region_by_module_by_time", "division_by_module_by_time")


LRdiff_res <- data.table(NULL)
## Run in Diffs
for (Y in c(outcomes.changes)) {
  for (n.g in 2:5) {
    
    # Create groups of initial values of tax rate
    # We use the full weighted distribution
    all_pi <- all_pi[, quantile := cut(dm.L.ln_cpricei2,
                                        breaks = quantile(dm.L.ln_cpricei2, probs = seq(0, 1, by = 1/n.g), na.rm = T, weight = base.sales),
                                        labels = 1:n.g, right = FALSE)]
    for (group in 1:n.g) {
      group.data <- all_pi[quantile == group]
      for (FE in FE_opts) {
        
        formula1 <- as.formula(paste0(
          Y, "~ D.ln_sales_tax | ", FE, " | 0 | module_by_state"
        ))
        flog.info("Estimating with %s as outcome with %s FE.", Y, FE)
        res1 <- felm(formula = formula1, data = group.data,
                     weights = group.data$base.sales)
        flog.info("Finished estimating with %s as outcome with %s FE.", Y, FE)
        
        
        ## attach results
        flog.info("Writing results...")
        res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
        res1.dt[, outcome := Y]
        res1.dt[, controls := FE]
        res1.dt[, window := "semester"]
        res1.dt[, spec := "changes"]
        res1.dt[, n.groups := n.g]
        res1.dt[, group.n := group]
        # Add summary values
        res1.dt[, Rsq := summary(res1)$r.squared]
        res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
        res1.dt[, N_obs := nrow(group.data)]
        res1.dt[, N_modules := length(unique(group.data$product_module_code))]
        res1.dt[, N_stores :=  length(unique(group.data$store_code_uc))]
        res1.dt[, N_counties := uniqueN(group.data, by = c("fips_state", "fips_county"))]
        res1.dt[, N_years := uniqueN(group.data, by = c("year"))]
        res1.dt[, N_county_modules := uniqueN(group.data, by = c("fips_state", "fips_county",
                                                             "product_module_code"))]
        LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
        fwrite(LRdiff_res, output.results.file)
        
        
      }
    }
  }
}


## Run within

## Run in Diffs
for (Y in c(outcomes.within)) {
  for (n.g in 2:5) {
    
    # Create groups of initial values of tax rate
    # We use the full weighted distribution
    all_pi <- all_pi[, quantile := cut(dm.L.ln_cpricei2,
                                       breaks = quantile(dm.L.ln_cpricei2, probs = seq(0, 1, by = 1/n.g), na.rm = T, weight = base.sales),
                                       labels = 1:n.g, right = FALSE)]
    for (group in 1:n.g) {
      group.data <- all_pi[quantile == group]
      for (FE in FE_opts) {
        
        formula1 <- as.formula(paste0(
          Y, "~ w.ln_sales_tax | ", FE, " | 0 | module_by_state"
        ))
        flog.info("Estimating with %s as outcome with %s FE.", Y, FE)
        res1 <- felm(formula = formula1, data = group.data,
                     weights = group.data$base.sales)
        flog.info("Finished estimating with %s as outcome with %s FE.", Y, FE)
        
        
        ## attach results
        flog.info("Writing results...")
        res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
        res1.dt[, outcome := Y]
        res1.dt[, controls := FE]
        res1.dt[, window := "semester"]
        res1.dt[, spec := "changes"]
        res1.dt[, n.groups := n.g]
        res1.dt[, group.n := group]
        # Add summary values
        res1.dt[, Rsq := summary(res1)$r.squared]
        res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
        res1.dt[, N_obs := nrow(group.data)]
        res1.dt[, N_modules := length(unique(group.data$product_module_code))]
        res1.dt[, N_stores :=  length(unique(group.data$store_code_uc))]
        res1.dt[, N_counties := uniqueN(group.data, by = c("fips_state", "fips_county"))]
        res1.dt[, N_years := uniqueN(group.data, by = c("year"))]
        res1.dt[, N_county_modules := uniqueN(group.data, by = c("fips_state", "fips_county",
                                                                 "product_module_code"))]
        LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
        fwrite(LRdiff_res, output.results.file)
        
        
      }
    }
  }
}

