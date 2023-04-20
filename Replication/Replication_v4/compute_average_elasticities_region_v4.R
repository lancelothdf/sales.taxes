##### Santiago Lacouture
#' Sales Taxes
#' Replication File. Updated on 04/20/2023
#' Step 9: Extract average elasticities previously estimated. Bootstrapped version
#' This version takes prices as given (only bootstrap estimates)

library(data.table)
library(futile.logger)
library(lfe)
library(Matrix)
library(zoo)
library(tidyverse)
library(stringr)

setwd("/project/igaarder")
rm(list = ls())

## input filepath ----------------------------------------------
theta.output.results.file <- "Data/Replication_v4/Demand_theta_sat_initial_price_semester_boot_r_region.csv"
iv.output.results.file <- "Data/Replication_v4/Demand_iv_sat_initial_price_semester_boot_r.csv"
out.file.elast <- "Data/Replication_v4/partial_point_results_region.csv"
out.file.elast.boot <- "Data/Replication_v4/partial_point_results_boot_region.csv"

## output filepath ----------------------------------------------
output.table.avelas <- "Data/Replication_v4/summary_elasticity_states_region.csv"


## Open data ----------------------------------------------
all_pi <- fread("Data/Replication_v4/all_pi.csv")
# restrict to relevant sample
all_pi <- all_pi[non_imp_tax_strong == 1,]

## Prepare bootstrap
res.all.full <- fread(theta.output.results.file)
ivs.full <- fread(iv.output.results.file)
bounds <- fread(out.file.elast)
bounds.boot <- fread(out.file.elast.boot)

set.seed(2019)
ids <- unique(all_pi$module_by_state)

elasticities.all <- data.table(NULL)
for (rep in 0:100) {
  
  flog.info("Iteration %s", rep)
  ### Part 1. Recover Point identified case  ------------
  flog.info("Recovering estimates for %s...", rep)
  res.all <- res.all.full[iter == rep & controls == "group_region_by_module_by_time"]
  iv1 <- ivs.full[iter == rep & controls == "group_region_by_module_by_time" & n.groups == 1]
  
  linear.elas <- (iv1[outcome == "DL.ln_quantity3"][["Estimate"]])/iv1[outcome == "DL.ln_cpricei2"][["Estimate"]]
  quad.elas <- res.all[n.groups ==2][["beta_hat"]][-1]*c(1,2)
  cubic.elas <- res.all[n.groups ==3][["beta_hat"]][-1]*c(1,2,3)
  
  ### Part 2. Recover Partial identified case -------
  flog.info("Recovering matrices for %s...", rep)
  if (rep == 0) bounds.iter <- bounds
  else bounds.iter <- bounds.boot[iter == rep]
  
  bounds.iter <- bounds.iter[, -c("iter", "min.criterion")]
  
  ## dcast data (long to wide)
  bounds.iter <- dcast(bounds.iter, "p + L ~ K", 
                       value.var = c("elas.down", "elas.up"), fun = sum)
  
  if (rep == 0) print(head(bounds.iter))
  
  ### Part 3. Open data (relevant subsample) -------
  flog.info("Recovering data for %s...", rep)
  if (rep > 0) {
    # Sample by block
    sampled.ids <- data.table(sample(ids, replace = T))
    setnames(sampled.ids, old= "V1", new = "module_by_state")

    # Merge data to actual data
    sampled.data <- merge(sampled.ids, all_pi, by = c("module_by_state") , allow.cartesian = T, all.x = T)

  }
  else {
    sampled.data <- copy(all_pi)
  }
  flog.info("Computing average for %s...", rep)
  ## Keep only taxable items as those are whose responses we care
  elasticities <- sampled.data[ln_sales_tax > 0]
  rm(sampled.data)
  
  ## Need to round to the third decimal point to match the bounds we have estimated
  elasticities[, p := round(dm.ln_cpricei2, 3)]
  
  ## Merge estimated bounds
  elasticities <- merge(elasticities, bounds.iter, by = "p", allow.cartesian=T)
  

  ## Calculate all elasticities
  elasticities <- elasticities[, .( av.elas_1 = linear.elas,
                                    av.elas_2 = weighted.mean(quad.elas[1] + 
                                                                quad.elas[2]*dm.ln_cpricei2, 
                                                              w = base.sales),
                                    av.elas_3 = weighted.mean(cubic.elas[1] + 
                                                                cubic.elas[2]*dm.ln_cpricei2 + 
                                                                cubic.elas[3]*dm.ln_cpricei2^2, 
                                                              w = base.sales),
                                    av.elas.down_2 = weighted.mean(elas.down_2 , w = base.sales),
                                    av.elas.down_3 = weighted.mean(elas.down_3 , w = base.sales),
                                    av.elas.down_4 = weighted.mean(elas.down_4 , w = base.sales),
                                    av.elas.down_5 = weighted.mean(elas.down_5 , w = base.sales),
                                    av.elas.down_6 = weighted.mean(elas.down_6 , w = base.sales),
                                    av.elas.down_7 = weighted.mean(elas.down_7 , w = base.sales),
                                    av.elas.down_8 = weighted.mean(elas.down_8 , w = base.sales),
                                    av.elas.up_2 = weighted.mean(elas.up_2 , w = base.sales),
                                    av.elas.up_3 = weighted.mean(elas.up_3 , w = base.sales),
                                    av.elas.up_4 = weighted.mean(elas.up_4 , w = base.sales),
                                    av.elas.up_5 = weighted.mean(elas.up_5 , w = base.sales),
                                    av.elas.up_6 = weighted.mean(elas.up_6 , w = base.sales),
                                    av.elas.up_7 = weighted.mean(elas.up_7 , w = base.sales),
                                    av.elas.up_8 = weighted.mean(elas.up_8 , w = base.sales),
                                    N = .N
  ) , by = .(fips_state, L)]
  elasticities[, iter := rep]
  if (rep == 0) print(head(elasticities))
  
  flog.info("Writing results of %s...", rep)
  elasticities.all <- rbind(elasticities.all, elasticities)
  ## Export results
  fwrite(elasticities.all, output.table.avelas)
  
}


