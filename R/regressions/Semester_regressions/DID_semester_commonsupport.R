#' Sales Taxes Project
#' This code run a Basic DiD model on the common support
#' The idea is to try to see how much our estimates change
#' We estimate both the "short run" and the "long run" models 
#' Here initial level means previous period 
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
output.results.file <- "Data/DID_common_support_semester.csv"


### Set up Semester Data ---------------------------------
all_pi <- fread(data.semester)
all_pi[, w.ln_sales_tax := ln_sales_tax - mean(ln_sales_tax), by = .(store_by_module)]
all_pi[, w.ln_cpricei2 := ln_cpricei2 - mean(ln_cpricei2), by = .(store_by_module)]
all_pi[, w.ln_quantity3 := ln_quantity3 - mean(ln_quantity3), by = .(store_by_module)]

# Create lagged value (initial)
all_pi[, L.ln_sales_tax := ln_sales_tax - D.ln_sales_tax]

# need to demean lag price to compare appropiately
all_pi[, module_by_time := .GRP, by = .(product_module_code, year)]
all_pi[, L.ln_cpricei2 := ln_cpricei2 - D.ln_cpricei2]
all_pi[, dm.L.ln_cpricei2 := L.ln_cpricei2 - mean(L.ln_cpricei2, na.rm = T), by = module_by_time]



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



# Price: 
pct1.control <- quantile(control$dm.L.ln_cpricei2, probs = 0.01, na.rm = T, weight=all_pi$base.sales)
pct1.treated <- quantile(treated$dm.L.ln_cpricei2, probs = 0.01, na.rm = T, weight=all_pi$base.sales)

pct99.control <- quantile(control$dm.L.ln_cpricei2, probs = 0.99, na.rm = T, weight=all_pi$base.sales)
pct99treated <- quantile(treated$dm.L.ln_cpricei2, probs = 0.99, na.rm = T, weight=all_pi$base.sales)

all_pi[, cs_price := ifelse(dm.L.ln_cpricei2 > max(pct1.treated, pct1.control) & 
                            dm.L.ln_cpricei2 < min(pct99treated, pct99.control), 1, 0)]
# Make sure missings are 0s
all_pi[, cs_price := ifelse(is.na(dm.L.ln_cpricei2), 0, cs_price)]



### Set up loop

outcomes.changes <- c("D.ln_cpricei2", "D.ln_quantity3")
outcomes.within <- c("w.ln_cpricei2", "w.ln_quantity3")
common.supports <- c("cs_price", "cs_tax")
FE_opts <- c("region_by_module_by_time", "division_by_module_by_time")
LRdiff_res <- data.table(NULL)

## Start loop

for (cs in common.supports) {
  
  all_pi_cs <- all_pi[get(cs) == 1,]
  for (FE in FE_opts) {
    
    ## Run in Diffs
    for (Y in c(outcomes.changes)) {
      
      formula1 <- as.formula(paste0(
        Y, "~ D.ln_sales_tax | ", FE, " | 0 | module_by_state"
      ))
      flog.info("Estimating with %s as outcome with %s FE.", Y, FE)
      res1 <- felm(formula = formula1, data = all_pi_cs,
                   weights = all_pi_cs$base.sales)
      flog.info("Finished estimating with %s as outcome with %s FE.", Y, FE)
      
      
      ## attach results
      flog.info("Writing results...")
      res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
      res1.dt[, outcome := Y]
      res1.dt[, controls := FE]
      res1.dt[, window := "semester"]
      res1.dt[, spec := "changes"]
      res1.dt[, support := cs]
      # Add summary values
      res1.dt[, Rsq := summary(res1)$r.squared]
      res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
      res1.dt[, N_obs := nrow(all_pi_cs)]
      res1.dt[, N_modules := length(unique(all_pi_cs$product_module_code))]
      res1.dt[, N_stores :=  length(unique(all_pi_cs$store_code_uc))]
      res1.dt[, N_counties := uniqueN(all_pi_cs, by = c("fips_state", "fips_county"))]
      res1.dt[, N_years := uniqueN(all_pi_cs, by = c("year"))]
      res1.dt[, N_county_modules := uniqueN(all_pi_cs, by = c("fips_state", "fips_county",
                                                           "product_module_code"))]
      LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
      fwrite(LRdiff_res, output.results.file)
    }
    
    ## Run within
    for (Y in c(outcomes.within)) {

      formula1 <- as.formula(paste0(
        Y, "~ w.ln_sales_tax | ", FE, " | 0 | module_by_state"
      ))
      flog.info("Estimating with %s as outcome with %s FE.", Y, FE)
      res1 <- felm(formula = formula1, data = all_pi_cs,
                   weights = all_pi_cs$base.sales)
      flog.info("Finished estimating with %s as outcome with %s FE.", Y, FE)
      
      
      ## attach results
      flog.info("Writing results...")
      res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
      res1.dt[, outcome := Y]
      res1.dt[, controls := FE]
      res1.dt[, window := "semester"]
      res1.dt[, spec := "within"]
      res1.dt[, support := cs]
      # Add summary values
      res1.dt[, Rsq := summary(res1)$r.squared]
      res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
      res1.dt[, N_obs := nrow(all_pi_cs)]
      res1.dt[, N_modules := length(unique(all_pi_cs$product_module_code))]
      res1.dt[, N_stores :=  length(unique(all_pi_cs$store_code_uc))]
      res1.dt[, N_counties := uniqueN(all_pi_cs, by = c("fips_state", "fips_county"))]
      res1.dt[, N_years := uniqueN(all_pi_cs, by = c("year"))]
      res1.dt[, N_county_modules := uniqueN(all_pi_cs, by = c("fips_state", "fips_county",
                                                           "product_module_code"))]
      LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
      fwrite(LRdiff_res, output.results.file)
      
    }
  }  
}

