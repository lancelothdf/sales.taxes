#' Sales Taxes Project
#' This code run a Basic DiD model by initial tax rate or price level
#' The idea is to try to see whether we observe non linearities in this sense
#' We estimate both the "short run" and the "long run" models splitting the sample
#' by quantiles increasing the number of groups.
#' Here initial level means previous period and we divide by groups within the "common" support
#' In this case, we run a fully saturated model (instead of splitting the sample)


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
output.results.file <- "Data/DID_sat_initial_tax_semester_pretrends.csv"

### Set up Semester Data ---------------------------------
all_pi <- fread(data.semester)
all_pi[, w.ln_sales_tax := ln_sales_tax - mean(ln_sales_tax), by = .(store_by_module)]
all_pi[, w.ln_cpricei2 := ln_cpricei2 - mean(ln_cpricei2), by = .(store_by_module)]
all_pi[, w.ln_quantity3 := ln_quantity3 - mean(ln_quantity3), by = .(store_by_module)]

all_pi <- all_pi[order(store_by_module, cal_time),] ##Sort on store by year-quarter (in ascending order)
all_pi[, L1.w.ln_cpricei2 := shift(w.ln_cpricei2, n=1, type="lag"), by = .(store_by_module)]
all_pi[, L2.w.ln_cpricei2 := shift(w.ln_cpricei2, n=2, type="lag"), by = .(store_by_module)]
all_pi[, L4.w.ln_cpricei2 := shift(w.ln_cpricei2, n=4, type="lag"), by = .(store_by_module)]
all_pi[, L1.w.ln_quantity3 := shift(w.ln_quantity3, n=1, type="lag"), by = .(store_by_module)]
all_pi[, L2.w.ln_quantity3 := shift(w.ln_quantity3, n=2, type="lag"), by = .(store_by_module)]
all_pi[, L4.w.ln_quantity3 := shift(w.ln_quantity3, n=4, type="lag"), by = .(store_by_module)]
all_pi[, L1.D.ln_cpricei2 := shift(D.ln_cpricei2, n=1, type="lag"), by = .(store_by_module)]
all_pi[, L2.D.ln_cpricei2 := shift(D.ln_cpricei2, n=2, type="lag"), by = .(store_by_module)]
all_pi[, L4.D.ln_cpricei2 := shift(D.ln_cpricei2, n=4, type="lag"), by = .(store_by_module)]
all_pi[, L1.D.ln_quantity3 := shift(D.ln_quantity3, n=1, type="lag"), by = .(store_by_module)]
all_pi[, L2.D.ln_quantity3 := shift(D.ln_quantity3, n=2, type="lag"), by = .(store_by_module)]
all_pi[, L4.D.ln_quantity3 := shift(D.ln_quantity3, n=4, type="lag"), by = .(store_by_module)]




# Create lagged value (initial)
all_pi[, L.ln_sales_tax := ln_sales_tax - D.ln_sales_tax]
# Need to keep data that has initial tax rate level
all_pi <- all_pi[!is.na(L.ln_sales_tax)]

# Defining common support
control <- all_pi[D.ln_sales_tax == 0,]
treated <- all_pi[D.ln_sales_tax != 0,]

# Tax rate
pct1.control <- quantile(control$L.ln_sales_tax, probs = 0.01, na.rm = T, weight=all_pi$base.sales)
pct1.treated <- quantile(treated$L.ln_sales_tax, probs = 0.01, na.rm = T, weight=all_pi$base.sales)

pct99.control <- quantile(control$L.ln_sales_tax, probs = 0.99, na.rm = T, weight=all_pi$base.sales)
pct99treated <- quantile(treated$L.ln_sales_tax, probs = 0.99, na.rm = T, weight=all_pi$base.sales)

all_pi[, cs_tax := ifelse(L.ln_sales_tax > max(pct1.treated, pct1.control) & 
                            L.ln_sales_tax < min(pct99treated, pct99.control), 1, 0)]
# Make sure missings are 0s
all_pi[, cs_tax := ifelse(is.na(L.ln_sales_tax), 0, cs_tax)]

## Keep within the common support
all_pi <- all_pi[cs_tax == 1,]



outcomes.changes <- c("L1.D.ln_cpricei2", "L2.D.ln_cpricei2", "L4.D.ln_cpricei2", "L1.D.ln_quantity3", "L2.D.ln_quantity3", "L4.D.ln_quantity3")
outcomes.within <- c("L1.w.ln_cpricei2", "L2.w.ln_cpricei2", "L4.w.ln_cpricei2", "L1.w.ln_quantity3", "L2.w.ln_quantity3", "L4.w.ln_quantity3")
FE_opts <- c("group_region_by_module_by_time", "group_division_by_module_by_time")


LRdiff_res <- data.table(NULL)
## Run in Diffs
for (Y in c(outcomes.changes)) {
  for (n.g in 2:5) {
    
    # Create groups of initial values of tax rate
    # We use the full weighted distribution
    all_pi <- all_pi[, quantile := cut(L.ln_sales_tax,
                                        breaks = quantile(L.ln_sales_tax, probs = seq(0, 1, by = 1/n.g), na.rm = T, weight = base.sales),
                                        labels = 1:n.g, right = FALSE)]
    quantlab <- round(quantile(all_pi$L.ln_sales_tax, 
                               probs = seq(0, 1, by = 1/n.g), na.rm = T, 
                               weight = all_pi$base.sales), digits = 4)
    # Saturate fixed effects
    all_pi[, group_region_by_module_by_time := .GRP, by = .(region_by_module_by_time, quantile)]
    all_pi[, group_division_by_module_by_time := .GRP, by = .(division_by_module_by_time, quantile)]
    
    for (FE in FE_opts) {
      
      formula1 <- as.formula(paste0(
        Y, "~ D.ln_sales_tax:quantile | ", FE, "+ quantile | 0 | module_by_state"
      ))
      flog.info("Estimating with %s as outcome with %s FE.", Y, FE)
      res1 <- felm(formula = formula1, data = all_pi,
                   weights = all_pi$base.sales)
      flog.info("Finished estimating with %s as outcome with %s FE.", Y, FE)
      
      
      ## attach results
      flog.info("Writing results...")
      res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
      res1.dt[, outcome := Y]
      res1.dt[, controls := FE]
      res1.dt[, window := "semester"]
      res1.dt[, spec := "changes"]
      res1.dt[, n.groups := n.g]
      res1.dt[, lev := quantlab[-1]]
      # Add summary values
      res1.dt[, Rsq := summary(res1)$r.squared]
      res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
      res1.dt[, N_obs := nrow(all_pi)]
      res1.dt[, N_modules := length(unique(all_pi$product_module_code))]
      res1.dt[, N_stores :=  length(unique(all_pi$store_code_uc))]
      res1.dt[, N_counties := uniqueN(all_pi, by = c("fips_state", "fips_county"))]
      res1.dt[, N_years := uniqueN(all_pi, by = c("year"))]
      res1.dt[, N_county_modules := uniqueN(all_pi, by = c("fips_state", "fips_county",
                                                           "product_module_code"))]
      LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
      fwrite(LRdiff_res, output.results.file)
      
      
      
    }
  }
}



## Run in Diffs
for (Y in c(outcomes.within)) {
  for (n.g in 2:5) {
    
    # Create groups of initial values of tax rate
    # We use the full weighted distribution
    all_pi <- all_pi[, quantile := cut(L.ln_sales_tax,
                                       breaks = quantile(L.ln_sales_tax, probs = seq(0, 1, by = 1/n.g), na.rm = T, weight = base.sales),
                                       labels = 1:n.g, right = FALSE)]
    quantlab <- round(quantile(all_pi$L.ln_sales_tax, 
                               probs = seq(0, 1, by = 1/n.g), na.rm = T, 
                               weight = all_pi$base.sales), digits = 4)
    # Saturate fixed effects
    all_pi[, group_region_by_module_by_time := .GRP, by = .(region_by_module_by_time, quantile)]
    all_pi[, group_division_by_module_by_time := .GRP, by = .(division_by_module_by_time, quantile)]
    
    for (FE in FE_opts) {
      
      formula1 <- as.formula(paste0(
        Y, "~ w.ln_sales_tax:quantile | ", FE, "+ quantile | 0 | module_by_state"
      ))
      flog.info("Estimating with %s as outcome with %s FE.", Y, FE)
      res1 <- felm(formula = formula1, data = all_pi,
                   weights = all_pi$base.sales)
      flog.info("Finished estimating with %s as outcome with %s FE.", Y, FE)
      
      
      ## attach results
      flog.info("Writing results...")
      res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
      res1.dt[, outcome := Y]
      res1.dt[, controls := FE]
      res1.dt[, window := "semester"]
      res1.dt[, spec := "within"]
      res1.dt[, n.groups := n.g]
      res1.dt[, lev := quantlab[-1]]
      # Add summary values
      res1.dt[, Rsq := summary(res1)$r.squared]
      res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
      res1.dt[, N_obs := nrow(all_pi)]
      res1.dt[, N_modules := length(unique(all_pi$product_module_code))]
      res1.dt[, N_stores :=  length(unique(all_pi$store_code_uc))]
      res1.dt[, N_counties := uniqueN(all_pi, by = c("fips_state", "fips_county"))]
      res1.dt[, N_years := uniqueN(all_pi, by = c("year"))]
      res1.dt[, N_county_modules := uniqueN(all_pi, by = c("fips_state", "fips_county",
                                                           "product_module_code"))]
      LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
      fwrite(LRdiff_res, output.results.file)
      
      
      
    }
  }
}

