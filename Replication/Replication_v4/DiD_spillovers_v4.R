##### Wesley Janson and Santiago Lacouture
#' Sales Taxes
#' Replication File. Updated on 04/18/2022
#' Step 3: Reduced Form Evidence with spillovers portion of replication

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
all_pi_spill <- fread("Data/Replication_v4/all_pi_spill.csv")
all_pi_spill_econ <- fread("Data/Replication_v4/all_pi_spill_econ.csv")

## output filepath ----------------------------------------------
results.file.spillovers <- "Data/Replication_v4/LRdiff_semesterly_spillovers.csv"
results.file.spillovers.controls <- "Data/Replication_v4/LRdiff_semesterly_spillovers_controls.csv"

### 3. Reduced Form Evidence spillovers -----------------

LRdiff_res <- data.table(NULL)



####### Long-Difference specification ------

## De-mean statutory tax rate
outcomes <- c("DL.ln_cpricei2", "DL.ln_quantity3", "DL.ln_pricei2", "DL.ln_sales")
FE_opts <- c("region_by_module_by_time", "division_by_module_by_time")

# Define samples
subsamples <- c("all_taxable", "all_taxexempt")
samples <- c("non_imp_tax_strong")


## Run
for (s in samples) {
  
  for (sam in subsamples) {
    all_pi_spill[, sample := get(sam)]
    data.est <- all_pi_spill[sample == 1 & get(s) == 1]
    
    for (Y in c(outcomes)) {
      for (FE in FE_opts) {
        
        formula1 <- as.formula(paste0(
          Y, "~ DL.ln_statutory_tax | ", FE, " | 0 | module_by_state"
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
        res1.dt[, subsample := sam]
        res1.dt[, sample := s]
        res1.dt[, spec := "DL"]
        # Add summary values
        res1.dt[, Rsq := summary(res1)$r.squared]
        res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
        res1.dt[, N_obs := nrow(data.est)]
        res1.dt[, N_modules := length(unique(data.est$product_module_code))]
        res1.dt[, N_stores :=  length(unique(data.est$store_code_uc))]
        res1.dt[, N_counties := uniqueN(data.est, by = c("fips_state", "fips_county"))]
        res1.dt[, N_years := uniqueN(data.est, by = c("year"))]
        res1.dt[, N_county_modules := uniqueN(data.est, by = c("fips_state", "fips_county",
                                                               "product_module_code"))]
        LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
        fwrite(LRdiff_res, results.file.spillovers)
        
      }
    }
  }
}




####### Spillovers with econ controls  ------
LRdiff_res <- data.table(NULL)

## Run
for (s in samples) {
  
  for (sam in subsamples) {
    all_pi_spill_econ[, sample := get(sam)]
    data.est <- all_pi_spill_econ[sample == 1 & get(s) == 1]
    
    for (Y in c(outcomes)) {
      for (FE in FE_opts) {
        
        formula1 <- as.formula(paste0(
          Y, "~ DL.ln_statutory_tax + DL.ln_home_price + DL.ln_unemp | ", FE, " | 0 | module_by_state"
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
        res1.dt[, subsample := sam]
        res1.dt[, sample := s]
        res1.dt[, spec := "DL"]
        # Add summary values
        res1.dt[, Rsq := summary(res1)$r.squared]
        res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
        res1.dt[, N_obs := nrow(data.est)]
        res1.dt[, N_modules := length(unique(data.est$product_module_code))]
        res1.dt[, N_stores :=  length(unique(data.est$store_code_uc))]
        res1.dt[, N_counties := uniqueN(data.est, by = c("fips_state", "fips_county"))]
        res1.dt[, N_years := uniqueN(data.est, by = c("year"))]
        res1.dt[, N_county_modules := uniqueN(data.est, by = c("fips_state", "fips_county",
                                                               "product_module_code"))]
        LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
        fwrite(LRdiff_res, results.file.spillovers.econ)
        
      }
    }
  }
}



