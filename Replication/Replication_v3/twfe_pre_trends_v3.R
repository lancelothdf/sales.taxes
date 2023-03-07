##### Wesley Janson
#' Sales Taxes
#' Replication File. Updated on 5/21/2022
#' Step 5: TWFE estimates and pre-trends portion of replication

library(data.table)
library(futile.logger)
library(lfe)
library(multcomp)
library(Matrix)

setwd("/project2/igaarder")
rm(list = ls())

## input filepath ----------------------------------------------
all_pi <- fread("Data/Replication/all_pi.csv")

## output filepath ----------------------------------------------
output.results.file.TWFE <- "Data/Replication_v2/LR_Diff_design.csv"


### 5. Two-way FE estimates and pre-trends ---------------
outcomes <- c("w.ln_cpricei2", "w.ln_quantity3", "w.ln_pricei2", "w.ln_sales")
FE_opts <- c("region_by_module_by_time", "division_by_module_by_time")

# Define samples
samples <- c("all", "non_imp_tax")


LRdiff_res <- data.table(NULL)
## Run
for (s in samples) {
  
  data.est <- all_pi[get(s) == 1,]
  
  for (Y in c(outcomes)) {
    for (FE in FE_opts) {
      
      formula1 <- as.formula(paste0(
        Y, "~ w.ln_sales_tax | ", FE, " | 0 | module_by_state"
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
      fwrite(LRdiff_res, output.results.file.TWFE)
      
      ## Run pre-trends
      for (k in 2:4) {
        
        formula1 <- as.formula(paste0(
          Y, "~ F", k,".w.ln_sales_tax | ", FE, " | 0 | module_by_state"
        ))
        flog.info("Estimating pretrend %s with %s as outcome with %s FE in sample %s.", k, Y, FE, s)
        res1 <- felm(formula = formula1, data = data.est,
                     weights = data.est$base.sales)
        flog.info("Finished estimating pretrend %s with %s as outcome with %s FE in sample %s.", k, Y, FE, s)
        
        
        ## attach results
        flog.info("Writing results...")
        res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
        res1.dt[, outcome := Y]
        res1.dt[, controls := FE]
        res1.dt[, sample := s]
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
        fwrite(LRdiff_res, output.results.file.TWFE)
      }
    }
  }
}