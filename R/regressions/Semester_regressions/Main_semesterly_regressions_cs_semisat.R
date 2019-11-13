#' Author: Lancelot Henry de Frahan and John Bonney
#'
#'Same as Main_semesterly_regressions_commonsupport but we split by quantiles of initial levels

library(data.table)
library(futile.logger)
library(lfe)
library(multcomp)

setwd("/project2/igaarder")


## input filepaths -----------------------------------------------
#' This data set contains quarterly Laspeyres indices and sales from 2006 to
#' 2014. It also contains sales tax rates from 2008-2014.
all_goods_pi_path <- "Data/Nielsen/price_quantity_indices_allitems_2006-2016_notaxinfo.csv"
#' This data set contains an old price index that Lance constructed, from
old_pi_path <- "Data/Nielsen/Quarterly_old_pi.csv"
#' This data is the same as all_goods_pi_path, except it has 2015-2016 data as well.
data.full.path <- "Data/Nielsen/semester_nielsen_data.csv"




## output filepaths ----------------------------------------------
output.results.file <- "Data/LRdiff_semesterly_main_commonsupport_semisat.csv"



##### 
all_pi <- fread(data.full.path)


# Create lagged value (initial)
all_pi[, L.ln_sales_tax := ln_sales_tax - D.ln_sales_tax]

# need to demean lag price to compare appropiately
all_pi[, module_by_time := .GRP, by = .(product_module_code, semester, year)]
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
pct1.control <- quantile(control$dm.L.ln_cpricei2, probs = 0.01, na.rm = T, weight=control$base.sales)
pct1.treated <- quantile(treated$dm.L.ln_cpricei2, probs = 0.01, na.rm = T, weight=treated$base.sales)

pct99.control <- quantile(control$dm.L.ln_cpricei2, probs = 0.99, na.rm = T, weight=control$base.sales)
pct99treated <- quantile(treated$dm.L.ln_cpricei2, probs = 0.99, na.rm = T, weight=treated$base.sales)

all_pi[, cs_price := ifelse(dm.L.ln_cpricei2 > max(pct1.treated, pct1.control) & 
                              dm.L.ln_cpricei2 < min(pct99treated, pct99.control), 1, 0)]
# Make sure missings are 0s
all_pi[, cs_price := ifelse(is.na(dm.L.ln_cpricei2), 0, cs_price)]

## Setting up loop to estimate

formula_lags <- paste0("L", 1:4, ".D.ln_sales_tax:quantile", collapse = "+")
formula_leads <- paste0("F", 1:4, ".D.ln_sales_tax:quantile", collapse = "+")
formula_RHS <- paste0("D.ln_sales_tax:quantile + ", formula_lags, "+", formula_leads)


outcomes <- c("D.ln_cpricei2", "D.ln_quantity3")
FE_opts <- c("region_by_module_by_time", "division_by_module_by_time")
common.supports <- c("cs_price", "cs_tax")



### First: run regression and estimate leads and lags directly (without imposing smoothness)
## No Econ controls (but we run the placebos)
LRdiff_res <- data.table(NULL)


## Start loop

for (cs in common.supports) {
  
  all_pi_cs <- all_pi[get(cs) == 1,]
  for (Y in c(outcomes)) {
    for (n.g in 2:7) {
      
      # Create groups of initial values of covariate
      if (cs == "cs_price") {
        all_pi_cs <- all_pi_cs[, quantile := cut(dm.L.ln_cpricei2,
                                           breaks = quantile(dm.L.ln_cpricei2, probs = seq(0, 1, by = 1/n.g), na.rm = T, weight = base.sales),
                                           labels = 1:n.g, right = FALSE)]
      } 
      if(cs == "cs_tax") {
        all_pi_cs <- all_pi_cs[, quantile := cut(L.ln_sales_tax,
                                           breaks = quantile(L.ln_sales_tax, probs = seq(0, 1, by = 1/n.g), na.rm = T, weight = base.sales),
                                           labels = 1:n.g, right = FALSE)]
      } 
      for (FE in FE_opts) {
        
        formula1 <- as.formula(paste0(
          Y, "~", formula_RHS, "| ", FE, " | 0 | module_by_state"
        ))
        flog.info("Estimating with %s as outcome with %s FE for %s qs.", Y, FE, n.g)
        res1 <- felm(formula = formula1, data = all_pi_cs,
                     weights = all_pi_cs$base.sales)
        flog.info("Finished estimating with %s as outcome with %s FE for %s qs.", Y, FE, n.g)
        
        
        ## attach results
        flog.info("Writing results...")
        res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
        res1.dt[, outcome := Y]
        res1.dt[, controls := FE]
        res1.dt[, support := cs]
        res1.dt[, n.groups := n.g]
        res1.dt[, Rsq := summary(res1)$r.squared]
        res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
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
        
        LRdiff_res <- rbind(LRdiff_res, res1.dt)
        fwrite(LRdiff_res, output.results.file)
        
        
        ##### Add the cumulative effect at each lead/lag (relative to -1) for each quantile
        for (lev in 1:n.g) {
          flog.info("Estimating cumulative effect for %s q.", lev)
          cumul.lead1.est <- 0
          cumul.lead1.se <- NA
          cumul.lead1.pval <- NA
          
          #cumul.lead2.est is just equal to minus the change between -2 and -1
          cumul.lead2.est <- - coef(summary(res1))[paste0("quantile", lev,":F1.D.ln_sales_tax"), "Estimate"]
          cumul.lead2.se <- coef(summary(res1))[paste0("quantile", lev,":F1.D.ln_sales_tax"), "Cluster s.e."]
          cumul.lead2.pval <- coef(summary(res1))[paste0("quantile", lev,":F1.D.ln_sales_tax"), "Pr(>|t|)"]
          
          ##LEADS
          for(j in 3:5) {
            
            ## Create a name for estimate, se and pval of each lead
            cumul.test.est.name <- paste("cumul.lead", j, ".est", sep = "")
            cumul.test.se.name <- paste("cumul.lead", j, ".se", sep = "")
            cumul.test.pval.name <- paste("cumul.lead", j, ".pval", sep = "")
            
            ## Create the formula to compute cumulative estimate at each lead/lag
            cumul.test.form <- paste0("-", paste(paste0("quantile", lev,":F", (j-1):1, ".D.ln_sales_tax"), collapse = " - "))
            cumul.test.form <- paste(cumul.test.form, " = 0")
            
            ## Compute estimate and store in variables names
            cumul.test <- glht(res1, linfct = cumul.test.form)
            
            assign(cumul.test.est.name, coef(summary(cumul.test))[[1]])
            assign(cumul.test.se.name, sqrt(vcov(summary(cumul.test)))[[1]])
            assign(cumul.test.pval.name, 2*(1 - pnorm(abs(coef(summary(cumul.test))[[1]]/sqrt(vcov(summary(cumul.test)))[[1]]))))
          }
          
          
          ##LAGS
          ## On Impact --> Effect = coefficient on D.ln_sales_tax
          cumul.lag0.est <- coef(summary(res1))[paste0("D.ln_sales_tax:quantile", lev), "Estimate"]
          cumul.lag0.se <- coef(summary(res1))[paste0("D.ln_sales_tax:quantile", lev), "Cluster s.e."]
          cumul.lag0.pval <- coef(summary(res1))[paste0("D.ln_sales_tax:quantile", lev), "Pr(>|t|)"]
          
          for(j in 1:4) {
            
            ## Create a name for estimate, se and pval of each lead
            cumul.test.est.name <- paste("cumul.lag", j, ".est", sep = "")
            cumul.test.se.name <- paste("cumul.lag", j, ".se", sep = "")
            cumul.test.pval.name <- paste("cumul.lag", j, ".pval", sep = "")
            
            ## Create the formula to compute cumulative estimate at each lead/lag
            cumul.test.form <- paste("D.ln_sales_tax:quantile", lev," + ", paste(paste0("quantile", lev,":L", 1:j, ".D.ln_sales_tax"), collapse = " + "), sep = "")
            cumul.test.form <- paste(cumul.test.form, " = 0")
            
            ## Compute estimate and store in variables names
            cumul.test <- glht(res1, linfct = cumul.test.form)
            
            assign(cumul.test.est.name, coef(summary(cumul.test))[[1]])
            assign(cumul.test.se.name, sqrt(vcov(summary(cumul.test)))[[1]])
            assign(cumul.test.pval.name, 2*(1 - pnorm(abs(coef(summary(cumul.test))[[1]]/sqrt(vcov(summary(cumul.test)))[[1]]))))
          }
          
          
          ## linear hypothesis results
          lp.dt <- data.table(
            rn = c(paste0("cumul.lead",c(5:1), ".D.ln_sales_tax", lev), paste0("cumul.lag",c(0:4), ".D.ln_sales_tax", lev)),
            Estimate = c(cumul.lead5.est, cumul.lead4.est, cumul.lead3.est, cumul.lead2.est, cumul.lead1.est, cumul.lag0.est, cumul.lag1.est, cumul.lag2.est, cumul.lag3.est, cumul.lag4.est),
            `Cluster s.e.` = c(cumul.lead5.se, cumul.lead4.se, cumul.lead3.se, cumul.lead2.se, cumul.lead1.se, cumul.lag0.se, cumul.lag1.se, cumul.lag2.se, cumul.lag3.se, cumul.lag4.se),
            `Pr(>|t|)` = c(cumul.lead5.pval, cumul.lead4.pval, cumul.lead3.pval, cumul.lead2.pval, cumul.lead1.pval, cumul.lag0.pval, cumul.lag1.pval, cumul.lag2.pval, cumul.lag3.pval, cumul.lag4.pval),
            outcome = Y,
            controls = FE,
            support = cs,
            n.groups = n.g,
            Rsq = summary(res1)$r.squared,
            adj.Rsq = summary(res1)$adj.r.squared)
          LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T)
          fwrite(LRdiff_res, output.results.file)
        } 
      }
    }
  }
}



