##### Wesley Janson and Santiago Lacouture
#' Sales Taxes
#' Replication File. Updated on 7/30/2022
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

setwd("/project2/igaarder")
rm(list = ls())

## input filepath ----------------------------------------------
all_pi_spill <- fread("Data/Replication/all_pi_spill.csv")
all_pi_spill_econ <- fread("Data/Replication/all_pi_spill_econ.csv")

## output filepath ----------------------------------------------
results.file.spillovers <- "Data/Replication/LRdiff_semesterly_spillovers.csv"


### 3. Reduced Form Evidence spillovers -----------------
formula_lags <- paste0("L", 1:4, "D.ln_statutory_tax", collapse = "+")
formula_leads <- paste0("F", 1:4, "D.ln_statutory_tax", collapse = "+")
formula_RHS <- paste0("D.ln_statutory_tax + ", formula_lags, "+", formula_leads)

FE_opts <- c("region_by_module_by_time", "division_by_module_by_time")
outcomes <- c("D.ln_cpricei2", "D.ln_quantity3")
subsamples <- c("all_taxable", "all_taxexempt")
samples <- c("all", "non_imp_tax")

## for linear hypothesis tests
lead.vars <- paste(paste0("F", 4:1, "D.ln_statutory_tax"), collapse = " + ")
lag.vars <- paste(paste0("L", 4:1, "D.ln_statutory_tax"), collapse = " + ")
lead.lp.restr <- paste(lead.vars, "= 0")
lag.lp.restr <- paste(lag.vars, "+ D.ln_statutory_tax = 0")
total.lp.restr <- paste(lag.vars, "+", lead.vars, "+ D.ln_statutory_tax = 0")


LRdiff_res <- data.table(NULL)
## FE vary across subsamples
for (s in samples) {
  for (sam in subsamples) {
    all_pi_spill[, sample := get(sam)]
    all_pi_spill_econ[, sample := get(sam)]
    sample <- all_pi_spill_econ[sample == 1 & get(s) == 1]
    
    # Print basic stats of sample as double check
    print(nrow(sample))
    print(nrow(sample[!is.na(D.ln_statutory_tax) & is.finite(D.ln_statutory_tax) ]))
    print(mean(sample[is.finite(D.ln_statutory_tax)]$D.ln_statutory_tax, na.rm = T))
    
    for (Y in c(outcomes)) {
      for (FE in FE_opts) {
        
        for(i in 0:4) {
          
          if (i>0) {
            
            # Create list of economic controls  
            lag.home <- paste(paste0("L", i:1, ".D.ln_home_price"), collapse = " + ")
            lag.unemp <- paste(paste0("L", i:1, ".D.ln_unemp"), collapse = " + ")
            lag.econ <- paste(lag.home, lag.unemp, sep = " + ")
            
            
            formula1 <- as.formula(paste0(
              Y, "~", formula_RHS, " + ", lag.econ, "| ", FE, " | 0 | module_by_state"
            ))
            flog.info("Estimating with %s as outcome with %s FE in samples %s and %s. Econ control %s", Y, FE, s, sam, i)
            res1 <- felm(formula = formula1, data = sample,
                         weights = sample$base.sales)
            flog.info("Finished estimating with %s as outcome with %s FE in samples %s and %s.", Y, FE, s, sam)
            
            
            
          } else {
            
            formula1 <- as.formula(paste0(
              Y, "~", formula_RHS, "| ", FE, " | 0 | module_by_state"
            ))
            flog.info("Estimating with %s as outcome with %s FE in samples %s and %s.", Y, FE, s, sam)
            res1 <- felm(formula = formula1, data = sample,
                         weights = sample$base.sales)
            flog.info("Finished estimating with %s as outcome with %s FE in samples %s and %s.", Y, FE, s, sam)
            
            
          }
          
          
          ## attach results
          flog.info("Writing results...")
          res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
          res1.dt[, outcome := Y]
          res1.dt[, controls := FE]
          res1.dt[, sample := s]
          res1.dt[, subsample := sam]
          res1.dt[, econ := i]
          res1.dt$Rsq <- summary(res1)$r.squared
          res1.dt$adj.Rsq <- summary(res1)$adj.r.squared
          
          res1.dt$N_obs <- nrow(sample)
          res1.dt$N_stores <- uniqueN(sample, by = c("store_code_uc") )
          res1.dt$N_modules <- length(unique(sample$product_module_code))
          res1.dt$N_counties <- uniqueN(sample, by = c("fips_state", "fips_county"))
          res1.dt$N_years <- uniqueN(sample, by = c("year"))
          res1.dt$N_county_modules <- uniqueN(sample, by = c("fips_state", "fips_county",
                                                             "product_module_code"))
          LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
          fwrite(LRdiff_res, results.file.spillovers)
          
          ## sum leads
          flog.info("Summing leads...")
          lead.test <- glht(res1, linfct = lead.lp.restr)
          lead.test.est <- coef(summary(lead.test))[[1]]
          lead.test.se <- sqrt(vcov(summary(lead.test)))[[1]]
          lead.test.pval <- 2*(1 - pnorm(abs(lead.test.est/lead.test.se)))
          
          ## sum lags
          flog.info("Summing lags...")
          lag.test <- glht(res1, linfct = lag.lp.restr)
          lag.test.est <- coef(summary(lag.test))[[1]]
          lag.test.se <- sqrt(vcov(summary(lag.test)))[[1]]
          lag.test.pval <- 2*(1 - pnorm(abs(lag.test.est/lag.test.se)))
          
          ## sum all
          flog.info("Summing all...")
          total.test <- glht(res1, linfct = total.lp.restr)
          total.test.est <- coef(summary(total.test))[[1]]
          total.test.se <- sqrt(vcov(summary(total.test)))[[1]]
          total.test.pval <- 2*(1 - pnorm(abs(total.test.est/total.test.se)))
          
          ## linear hypothesis results
          lp.dt <- data.table(
            rn = c("Pre.D.ln_statutory_tax", "Post.D.ln_statutory_tax", "All.D.ln_statutory_tax"),
            Estimate = c(lead.test.est, lag.test.est, total.test.est),
            `Cluster s.e.` = c(lead.test.se, lag.test.se, total.test.se),
            `Pr(>|t|)` = c(lead.test.pval, lag.test.pval, total.test.pval),
            outcome = Y,
            controls = FE,
            sample = s,
            subsample = sam,
            econ = i,
            Rsq = summary(res1)$r.squared,
            adj.Rsq = summary(res1)$adj.r.squared
          )
          
          
          ##### Add the cumulative effect at each lead/lag (relative to -1)
          cumul.lead1.est <- 0
          cumul.lead1.se <- NA
          cumul.lead1.pval <- NA
          
          #cumul.lead3.est is just equal to minus the change between -2 and -1
          cumul.lead2.est <- - coef(summary(res1))[ "F1D.ln_statutory_tax", "Estimate"]
          cumul.lead2.se <- coef(summary(res1))[ "F1D.ln_statutory_tax", "Cluster s.e."]
          cumul.lead2.pval <- coef(summary(res1))[ "F1D.ln_statutory_tax", "Pr(>|t|)"]
          
          ##LEADS
          for(j in 3:5) {
            
            ## Create a name for estimate, se and pval of each lead
            cumul.test.est.name <- paste("cumul.lead", j, ".est", sep = "")
            cumul.test.se.name <- paste("cumul.lead", j, ".se", sep = "")
            cumul.test.pval.name <- paste("cumul.lead", j, ".pval", sep = "")
            
            ## Create the formula to compute cumulative estimate at each lead/lag
            cumul.test.form <- paste0("-", paste(paste0("F", (j-1):1, "D.ln_statutory_tax"), collapse = " - "))
            cumul.test.form <- paste(cumul.test.form, " = 0")
            
            ## Compute estimate and store in variables names
            cumul.test <- glht(res1, linfct = cumul.test.form)
            
            assign(cumul.test.est.name, coef(summary(cumul.test))[[1]])
            assign(cumul.test.se.name, sqrt(vcov(summary(cumul.test)))[[1]])
            assign(cumul.test.pval.name, 2*(1 - pnorm(abs(coef(summary(cumul.test))[[1]]/sqrt(vcov(summary(cumul.test)))[[1]]))))
          }
          
          
          ##LAGS
          ## On Impact --> Effect = coefficient on D.ln_sales_tax + F1.D.ln_sales_tax
          cumul.lag0.est <- coef(summary(res1))[ "D.ln_statutory_tax", "Estimate"]
          cumul.lag0.se <- coef(summary(res1))[ "D.ln_statutory_tax", "Cluster s.e."]
          cumul.lag0.pval <- coef(summary(res1))[ "D.ln_statutory_tax", "Pr(>|t|)"]
          
          
          for(j in 1:4) {
            
            ## Create a name for estimate, se and pval of each lead
            cumul.test.est.name <- paste("cumul.lag", j, ".est", sep = "")
            cumul.test.se.name <- paste("cumul.lag", j, ".se", sep = "")
            cumul.test.pval.name <- paste("cumul.lag", j, ".pval", sep = "")
            
            ## Create the formula to compute cumulative estimate at each lead/lag
            cumul.test.form <- paste("D.ln_statutory_tax + ", paste(paste0("L", 1:j, "D.ln_statutory_tax"), collapse = " + "), sep = "")
            cumul.test.form <- paste(cumul.test.form, " = 0")
            
            ## Compute estimate and store in variables names
            cumul.test <- glht(res1, linfct = cumul.test.form)
            
            assign(cumul.test.est.name, coef(summary(cumul.test))[[1]])
            assign(cumul.test.se.name, sqrt(vcov(summary(cumul.test)))[[1]])
            assign(cumul.test.pval.name, 2*(1 - pnorm(abs(coef(summary(cumul.test))[[1]]/sqrt(vcov(summary(cumul.test)))[[1]]))))
          }    
          
          
          ## linear hypothesis results
          lp.dt <- data.table(
            rn = c("cumul.lead5.D.ln_statutory_tax", "cumul.lead4.D.ln_statutory_tax", "cumul.lead3.D.ln_statutory_tax", "cumul.lead2.D.ln_statutory_tax", 
                   "cumul.lead1.D.ln_statutory_tax", "cumul.lag0.D.ln_statutory_tax", "cumul.lag1.D.ln_statutory_tax", "cumul.lag2.D.ln_statutory_tax", "cumul.lag3.D.ln_statutory_tax", "cumul.lag4.D.ln_statutory_tax"),
            Estimate = c(cumul.lead5.est, cumul.lead4.est, cumul.lead3.est, cumul.lead2.est, cumul.lead1.est, cumul.lag0.est, cumul.lag1.est, cumul.lag2.est, cumul.lag3.est, cumul.lag4.est),
            `Cluster s.e.` = c(cumul.lead5.se, cumul.lead4.se, cumul.lead3.se, cumul.lead2.se, cumul.lead1.se, cumul.lag0.se, cumul.lag1.se, cumul.lag2.se, cumul.lag3.se, cumul.lag4.se),
            `Pr(>|t|)` = c(cumul.lead5.pval, cumul.lead4.pval, cumul.lead3.pval, cumul.lead2.pval, cumul.lead1.pval, cumul.lag0.pval, cumul.lag1.pval, cumul.lag2.pval, cumul.lag3.pval, cumul.lag4.pval),
            outcome = Y,
            controls = FE,
            sample = s,
            subsample = sam,
            econ = i,
            Rsq = summary(res1)$r.squared,
            adj.Rsq = summary(res1)$adj.r.squared
          )
          lp.dt$N_obs <- nrow(sample)
          lp.dt$N_stores <- uniqueN(sample, by = c("store_code_uc") )
          lp.dt$N_modules <- length(unique(sample$product_module_code))
          lp.dt$N_counties <- uniqueN(sample, by = c("fips_state", "fips_county"))
          lp.dt$N_years <- uniqueN(sample, by = c("year"))
          lp.dt$N_county_modules <- uniqueN(sample, by = c("fips_state", "fips_county",
                                                           "product_module_code"))
          
          LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T)
          fwrite(LRdiff_res, results.file.spillovers)
          
          
          
        }
      }
    }
  }
  
}
