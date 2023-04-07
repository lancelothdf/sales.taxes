##### Wesley Janson
#' Sales Taxes
#' Replication File. Updated on 03/07/2023
#' Step 5: TWFE estimates and pre-trends portion of replication

library(data.table)
library(futile.logger)
library(lfe)
library(multcomp)
library(Matrix)

setwd("/project/igaarder")
rm(list = ls())

## input filepath ----------------------------------------------
all_pi <- fread("Data/Replication_v2/all_pi.csv")

## output filepath ----------------------------------------------
output.results.file.TWFE <- "Data/Replication_v2/LR_Diff_design.csv"


### 5. Long DiD estimates  ---------------
#outcomes <- c("DL.ln_cpricei", "DL.ln_cpricei2", "DL.ln_quantity", "DL.ln_quantity2", "DL.ln_quantity3", "DL.ln_pricei2", "DLL.ln_cpricei", "DLL.ln_cpricei2", "DLL.ln_quantity", "DLL.ln_quantity2", "DLL.ln_quantity3", "DLL.ln_pricei2")
DL.outcomes <- c("DL.ln_cpricei", "DL.ln_cpricei2", "DL.ln_quantity", "DL.ln_quantity2", "DL.ln_quantity3", "DL.ln_pricei2")
DLL.outcomes <- c("DLL.ln_cpricei", "DLL.ln_cpricei2", "DLL.ln_quantity", "DLL.ln_quantity2", "DLL.ln_quantity3", "DLL.ln_pricei2")

FE_opts <- c("region_by_module_by_time", "division_by_module_by_time")

# Define samples
samples <- c("all", "non_imp_tax", "non_imp_tax_strong")  ## This order is important because restrictions are logically stronger and stronger (all > non_imp_tax > non_imp_tax_strong)


LRdiff_res <- data.table(NULL)
## Run
for (s in samples) {
  
  data.est <- all_pi[get(s) == 1,]
  #data <- all_pi[get(s) == 1,] ## If samples is defined in the right order, then we get the right samples (all > non_imp_tax > non_imp_tax_strong)
  
  for (Y in c(DL.outcomes)) {
    for (FE in FE_opts) {
      
      formula1 <- as.formula(paste0(
        Y, "~ DL.ln_sales_tax | ", FE, " | 0 | module_by_state"
      ))
      flog.info("Estimating with %s as outcome with %s FE in sample %s.", Y, FE, s)
      #res1 <- felm(formula = formula1, data = data,
      #             weights = data$base.sales)
      #flog.info("Finished estimating with %s as outcome with %s FE in sample %s.", Y, FE, s)
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
      #res1.dt[, N_obs := nrow(data)]
      #res1.dt[, N_modules := length(unique(data$product_module_code))]
      #res1.dt[, N_stores :=  length(unique(data$store_code_uc))]
      #res1.dt[, N_counties := uniqueN(data, by = c("fips_state", "fips_county"))]
      #res1.dt[, N_years := uniqueN(data, by = c("year"))]
      #res1.dt[, N_county_modules := uniqueN(data, by = c("fips_state", "fips_county",
      #                                                       "product_module_code"))]
      LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
      fwrite(LRdiff_res, output.results.file.TWFE)
      flog.info("Finished writing csv with %s as outcome with %s FE in sample %s.", Y, FE, s)
      
    }
  }
  
  for (Y in c(DLL.outcomes)) {
    for (FE in FE_opts) {
      
      formula1 <- as.formula(paste0(
        Y, "~ DLL.ln_sales_tax | ", FE, " | 0 | module_by_state"
      ))
      flog.info("Estimating with %s as outcome with %s FE in sample %s.", Y, FE, s)
      #res1 <- felm(formula = formula1, data = data,
      #             weights = data$base.sales)
      #flog.info("Finished estimating with %s as outcome with %s FE in sample %s.", Y, FE, s)
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
      #res1.dt[, N_obs := nrow(data)]
      #res1.dt[, N_modules := length(unique(data$product_module_code))]
      #res1.dt[, N_stores :=  length(unique(data$store_code_uc))]
      #res1.dt[, N_counties := uniqueN(data, by = c("fips_state", "fips_county"))]
      #res1.dt[, N_years := uniqueN(data, by = c("year"))]
      #res1.dt[, N_county_modules := uniqueN(data, by = c("fips_state", "fips_county",
      #                                                       "product_module_code"))]
      LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
      fwrite(LRdiff_res, output.results.file.TWFE)
      flog.info("Finished writing csv with %s as outcome with %s FE in sample %s.", Y, FE, s)
      
    }
  }
}