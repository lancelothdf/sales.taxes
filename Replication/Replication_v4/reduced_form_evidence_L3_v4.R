##### Wesley Janson and Santiago Lacouture
#' Sales Taxes
#' Replication File. Updated on 03/07/2023
#' Step 1: Reduced Form Evidence portion of replication

library(data.table)
library(futile.logger)
library(lfe)
library(multcomp)


setwd("/project/igaarder")
rm(list = ls())

## input filepaths ----------------------------------------------
all_pi <- fread("/project/igaarder/Data/Replication_v2/all_pi.csv")

## output filepaths ----------------------------------------------
output.results.file <- "/project/igaarder/Data/Replication_v2/LRdiff_semesterly_main_L3.csv"

########## Estimations -----------------

### 1. Reduced Form Evidence -----------------
## Set up
formula_lags <- paste0("L", 1:3, ".D.ln_sales_tax", collapse = "+")
formula_leads <- paste0("F", 1:4, ".D.ln_sales_tax", collapse = "+")
formula_RHS <- paste0("D.ln_sales_tax + ", formula_lags, "+", formula_leads)
outcomes <- c("D.ln_cpricei", "D.ln_cpricei2", "D.ln_quantity", "D.ln_quantity2", "D.ln_quantity3")
FE_opts <- c("region_by_module_by_time", "division_by_module_by_time")

## for linear hypothesis tests
lead.vars <- paste(paste0("F", 4:1, ".D.ln_sales_tax"), collapse = " + ")
lag.vars <- paste(paste0("L", 3:1, ".D.ln_sales_tax"), collapse = " + ")
lead.lp.restr <- paste(lead.vars, "= 0")
lag.lp.restr <- paste(lag.vars, "+ D.ln_sales_tax = 0")
total.lp.restr <- paste(lag.vars, "+", lead.vars, "+ D.ln_sales_tax = 0")

# Define samples
samples <- c("all", "non_imp_tax", "non_imp_tax_strong")


LRdiff_res <- data.table(NULL)
## Run
for (s in samples) {
  data.est <- all_pi[get(s) == 1,]
  
  for (Y in c(outcomes)) {
    for (FE in FE_opts) {
      
      formula1 <- as.formula(paste0(
        Y, "~", formula_RHS, "| ", FE, " | 0 | module_by_state"
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
      res1.dt[, econ := "none"]
      res1.dt[, Rsq := summary(res1)$r.squared]
      res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
      LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
      fwrite(LRdiff_res, output.results.file)
      
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
        rn = c("Pre.D.ln_sales_tax", "Post.D.ln_sales_tax", "All.D.ln_sales_tax"),
        Estimate = c(lead.test.est, lag.test.est, total.test.est),
        `Cluster s.e.` = c(lead.test.se, lag.test.se, total.test.se),
        `Pr(>|t|)` = c(lead.test.pval, lag.test.pval, total.test.pval),
        outcome = Y,
        controls = FE,
        sample = s,
        econ = "none",
        Rsq = summary(res1)$r.squared,
        adj.Rsq = summary(res1)$adj.r.squared)
      LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T)
      fwrite(LRdiff_res, output.results.file)
      
      
      ##### Add the cumulative effect at each lead/lag (relative to -1)
      cumul.lead1.est <- 0
      cumul.lead1.se <- NA
      cumul.lead1.pval <- NA
      
      #cumul.lead3.est is just equal to minus the change between -2 and -1
      cumul.lead2.est <- - coef(summary(res1))[ "F1.D.ln_sales_tax", "Estimate"]
      cumul.lead2.se <- coef(summary(res1))[ "F1.D.ln_sales_tax", "Cluster s.e."]
      cumul.lead2.pval <- coef(summary(res1))[ "F1.D.ln_sales_tax", "Pr(>|t|)"]
      
      ##LEADS
      for(j in 3:5) {
        
        ## Create a name for estimate, se and pval of each lead
        cumul.test.est.name <- paste("cumul.lead", j, ".est", sep = "")
        cumul.test.se.name <- paste("cumul.lead", j, ".se", sep = "")
        cumul.test.pval.name <- paste("cumul.lead", j, ".pval", sep = "")
        
        ## Create the formula to compute cumulative estimate at each lead/lag
        cumul.test.form <- paste0("-", paste(paste0("F", (j-1):1, ".D.ln_sales_tax"), collapse = " - "))
        cumul.test.form <- paste(cumul.test.form, " = 0")
        
        ## Compute estimate and store in variables names
        cumul.test <- glht(res1, linfct = cumul.test.form)
        
        assign(cumul.test.est.name, coef(summary(cumul.test))[[1]])
        assign(cumul.test.se.name, sqrt(vcov(summary(cumul.test)))[[1]])
        assign(cumul.test.pval.name, 2*(1 - pnorm(abs(coef(summary(cumul.test))[[1]]/sqrt(vcov(summary(cumul.test)))[[1]]))))
      }
      
      
      ##LAGS
      ## On Impact --> Effect = coefficient on D.ln_sales_tax + F1.D.ln_sales_tax
      cumul.lag0.est <- coef(summary(res1))[ "D.ln_sales_tax", "Estimate"]
      cumul.lag0.se <- coef(summary(res1))[ "D.ln_sales_tax", "Cluster s.e."]
      cumul.lag0.pval <- coef(summary(res1))[ "D.ln_sales_tax", "Pr(>|t|)"]
      
      
      for(j in 1:3) {
        
        ## Create a name for estimate, se and pval of each lead
        cumul.test.est.name <- paste("cumul.lag", j, ".est", sep = "")
        cumul.test.se.name <- paste("cumul.lag", j, ".se", sep = "")
        cumul.test.pval.name <- paste("cumul.lag", j, ".pval", sep = "")
        
        ## Create the formula to compute cumulative estimate at each lead/lag
        cumul.test.form <- paste("D.ln_sales_tax + ", paste(paste0("L", 1:j, ".D.ln_sales_tax"), collapse = " + "), sep = "")
        cumul.test.form <- paste(cumul.test.form, " = 0")
        
        ## Compute estimate and store in variables names
        cumul.test <- glht(res1, linfct = cumul.test.form)
        
        assign(cumul.test.est.name, coef(summary(cumul.test))[[1]])
        assign(cumul.test.se.name, sqrt(vcov(summary(cumul.test)))[[1]])
        assign(cumul.test.pval.name, 2*(1 - pnorm(abs(coef(summary(cumul.test))[[1]]/sqrt(vcov(summary(cumul.test)))[[1]]))))
      }    
      
      ## linear hypothesis results
      lp.dt <- data.table(
        rn = c("cumul.lead5.D.ln_sales_tax", "cumul.lead4.D.ln_sales_tax", "cumul.lead3.D.ln_sales_tax", "cumul.lead2.D.ln_sales_tax", "cumul.lead1.D.ln_sales_tax", "cumul.lag0.D.ln_sales_tax", "cumul.lag1.D.ln_sales_tax", "cumul.lag2.D.ln_sales_tax", "cumul.lag3.D.ln_sales_tax"),
        Estimate = c(cumul.lead5.est, cumul.lead4.est, cumul.lead3.est, cumul.lead2.est, cumul.lead1.est, cumul.lag0.est, cumul.lag1.est, cumul.lag2.est, cumul.lag3.est),
        `Cluster s.e.` = c(cumul.lead5.se, cumul.lead4.se, cumul.lead3.se, cumul.lead2.se, cumul.lead1.se, cumul.lag0.se, cumul.lag1.se, cumul.lag2.se, cumul.lag3.se),
        `Pr(>|t|)` = c(cumul.lead5.pval, cumul.lead4.pval, cumul.lead3.pval, cumul.lead2.pval, cumul.lead1.pval, cumul.lag0.pval, cumul.lag1.pval, cumul.lag2.pval, cumul.lag3.pval),
        outcome = Y,
        controls = FE,
        sample = s,
        econ = "none",
        Rsq = summary(res1)$r.squared,
        adj.Rsq = summary(res1)$adj.r.squared)
      LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T)
      fwrite(LRdiff_res, output.results.file)
      
      
    }
  }

  LRdiff_res[sample == s, N_obs := nrow(data.est)]
  LRdiff_res[sample == s, N_modules := length(unique(data.est$product_module_code))]
  LRdiff_res[sample == s, N_stores := length(unique(data.est$store_code_uc))]
  LRdiff_res[sample == s, N_counties := uniqueN(data.est, by = c("fips_state", "fips_county"))]
  LRdiff_res[sample == s, N_years := uniqueN(data.est, by = c("year"))] # should be 7 (we lose one because we difference)
  LRdiff_res[sample == s, N_county_modules := uniqueN(data.est, by = c("fips_state", "fips_county",
                                                        "product_module_code"))]
  fwrite(LRdiff_res, output.results.file)
}


