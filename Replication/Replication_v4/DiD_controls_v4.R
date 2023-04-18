##### Wesley Janson and Santiago Lacouture
#' Sales Taxes
#' Replication File. Updated on 03/07/2023
#' Step 2: Reduced Form Evidence with controls portion of replication

library(data.table)
library(futile.logger)
library(lfe)
library(multcomp)
library(Matrix)
library(zoo)
library(tidyverse)
library(stringr)
library(nloptr)
library(doParallel)
library(MASS)
library(pracma)

setwd("/project/igaarder")
rm(list = ls())

## input filepath ----------------------------------------------
all_pi_econ <- fread("Data/Replication_v4/all_pi_econ.csv")

## output filepath ----------------------------------------------
output.results.file.econ <- "Data/Replication_v4/LRdiff_semesterly_w_econ.csv"
output.pretrends.file.econ <- "Data/Replication_v4/LRdiff_semesterly_w_econ_pretrends.csv"


### 2. Reduced Form Evidence with Controls -----------------
formula_RHS <- "DL.ln_sales_tax"

outcomes <- c("DL.ln_cpricei", "DL.ln_cpricei2", "DL.ln_quantity", "DL.ln_quantity2", "DL.ln_quantity3")
FE_opts <- c("region_by_module_by_time", "division_by_module_by_time")


# Define samples
samples <- c("non_imp_tax_strong")


LRdiff_res <- data.table(NULL)
## Run
for (s in samples) {
  data.est <- all_pi_econ[get(s) == 1,]
  
  for (Y in c(outcomes)) {
    for (FE in FE_opts) {

        
        
        formula1 <- as.formula(paste0(
          Y, "~", formula_RHS, " + DL.ln_home_price + DL.ln_unemp | ", FE, " | 0 | module_by_state"
        ))
        flog.info("Estimating with %s as outcome with %s FE in sample %s.", Y, FE, s)
        res1 <- felm(formula = formula1, data = data.est,
                     weights = data.est$base.sales)
        flog.info("Finished estimating with %s as outcome with %s FE in sample %s.", Y, FE, s)
        
        
        ## attach results
        flog.info("Writing results...")
        res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
        res1.dt[, outcome := Y]
        res1.dt[, controls := FE]
        res1.dt[, sample := s]
        #res1.dt[, econ := i]
        res1.dt[, Rsq := summary(res1)$r.squared]
        res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
        LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
        fwrite(LRdiff_res, output.results.file.econ)
        
    }
  }
  
  LRdiff_res[sample == s, N_obs := nrow(data.est)]
  LRdiff_res[sample == s, N_modules := length(unique(data.est$product_module_code))]
  LRdiff_res[sample == s, N_stores := length(unique(data.est$store_code_uc))]
  LRdiff_res[sample == s, N_counties := uniqueN(data.est, by = c("fips_state", "fips_county"))]
  LRdiff_res[sample == s, N_years := uniqueN(data.est, by = c("year"))] # should be 7 (we lose one because we difference)
  LRdiff_res[sample == s, N_county_modules := uniqueN(data.est, by = c("fips_state", "fips_county",
                                                                       "product_module_code"))]
  fwrite(LRdiff_res, output.results.file.econ)
}



### Pre-trends
formula_RHS <- "FL.ln_sales_tax"
LRdiff_res <- data.table(NULL)
## Run
for (s in samples) {
  data.est <- all_pi_econ[get(s) == 1,]
  
  for (Y in c(outcomes)) {
    for (FE in FE_opts) {
      
      
      
      formula1 <- as.formula(paste0(
        Y, "~", formula_RHS, " + DL.ln_home_price + DL.ln_unemp | ", FE, " | 0 | module_by_state"
      ))
      flog.info("Estimating with %s as outcome with %s FE in sample %s.", Y, FE, s)
      res1 <- felm(formula = formula1, data = data.est,
                   weights = data.est$base.sales)
      flog.info("Finished estimating with %s as outcome with %s FE in sample %s.", Y, FE, s)
      
      
      ## attach results
      flog.info("Writing results...")
      res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
      res1.dt[, outcome := Y]
      res1.dt[, controls := FE]
      res1.dt[, sample := s]
      #res1.dt[, econ := i]
      res1.dt[, Rsq := summary(res1)$r.squared]
      res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
      LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
      fwrite(LRdiff_res, output.pretrends.file.econ)
      
    }
  }
  
  LRdiff_res[sample == s, N_obs := nrow(data.est)]
  LRdiff_res[sample == s, N_modules := length(unique(data.est$product_module_code))]
  LRdiff_res[sample == s, N_stores := length(unique(data.est$store_code_uc))]
  LRdiff_res[sample == s, N_counties := uniqueN(data.est, by = c("fips_state", "fips_county"))]
  LRdiff_res[sample == s, N_years := uniqueN(data.est, by = c("year"))] # should be 7 (we lose one because we difference)
  LRdiff_res[sample == s, N_county_modules := uniqueN(data.est, by = c("fips_state", "fips_county",
                                                                       "product_module_code"))]
  fwrite(LRdiff_res, output.pretrends.file.econ)
}

