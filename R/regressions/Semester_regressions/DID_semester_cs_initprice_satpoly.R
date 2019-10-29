#' Sales Taxes Project
#' This code run a Basic DiD model by initial tax rate or price level
#' The idea is to try to see whether we observe non linearities in this sense
#' We estimate both the "short run" and the "long run" models splitting the sample
#' by quantiles increasing the number of groups.
#' Here initial level means previous period and we use polynomials and fully interact the model.
#' However, instead of using the polynomial, we interact with quantiles


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
output.results.file <- "Data/DID_satpoly_initial_price_semester.csv"
output.path <- "../../home/slacouture/NLP/satpoly/byprices"

### Set up Semester Data ---------------------------------
all_pi <- fread(data.semester)
all_pi[, w.ln_sales_tax := ln_sales_tax - mean(ln_sales_tax), by = .(store_by_module)]
all_pi[, w.ln_cpricei2 := ln_cpricei2 - mean(ln_cpricei2), by = .(store_by_module)]
all_pi[, w.ln_quantity3 := ln_quantity3 - mean(ln_quantity3), by = .(store_by_module)]

# Need to demean
all_pi[, module_by_time := .GRP, by = .(product_module_code, year)]
all_pi[, L.ln_cpricei2 := ln_cpricei2 - D.ln_cpricei2]
all_pi[, dm.L.ln_cpricei2 := L.ln_cpricei2 - mean(L.ln_cpricei2, na.rm = T), by = module_by_time]


# Defining common support
control <- all_pi[D.ln_sales_tax == 0,]
treated <- all_pi[D.ln_sales_tax != 0,]

# Price 
pct1.control <- quantile(control$dm.L.ln_cpricei2, probs = 0.01, na.rm = T, weight=all_pi$base.sales)
pct1.treated <- quantile(treated$dm.L.ln_cpricei2, probs = 0.01, na.rm = T, weight=all_pi$base.sales)

pct99.control <- quantile(control$dm.L.ln_cpricei2, probs = 0.99, na.rm = T, weight=all_pi$base.sales)
pct99treated <- quantile(treated$dm.L.ln_cpricei2, probs = 0.99, na.rm = T, weight=all_pi$base.sales)

all_pi[, cs_price := ifelse(dm.L.ln_cpricei2 > max(pct1.treated, pct1.control) & 
                              dm.L.ln_cpricei2 < min(pct99treated, pct99.control), 1, 0)]
# Make sure missings are 0s
all_pi[, cs_price := ifelse(is.na(dm.L.ln_cpricei2), 0, cs_price)]

## Keep within the common support
all_pi <- all_pi[cs_price == 1,]


# Discretize taxrate: common support
price_values <-seq(min(all_pi$dm.L.ln_cpricei2, na.rm = T), max(all_pi$dm.L.ln_cpricei2, na.rm = T), length.out = 15)
# The value of 0 is problematic: replace it for a very small value
replace(price_values, price_values==0, 0.001)
# average tax changes (for prediction)
av.tax.ch <- 1


outcomes.changes <- c("D.ln_cpricei2", "D.ln_quantity3")
outcomes.within <- c("w.ln_cpricei2", "w.ln_quantity3")
FE_opts <- c("group_region_by_module_by_time", "group_division_by_module_by_time")

LRdiff_res <- data.table(NULL)
## Run in Diffs
for (Y in c(outcomes.changes)) {
  RHS1 <- "D.ln_sales_tax"
  for (n in 1:3) {
    
    # Create polynomial of initial tax rate of degree "n"
    all_pi[, paste0("D.ln_sales_tax_init_",n) := D.ln_sales_tax*(dm.L.ln_cpricei2^(n))]
    # Add to formula 1
    RHS1 <- paste(RHS1, paste0("D.ln_sales_tax_init_",n), sep = " + ")
    
    for (n.g in 2:5) {
      
      # Create groups of initial values of tax rate
      # We use the full weighted distribution
      all_pi <- all_pi[, quantile := cut(dm.L.ln_cpricei2,
                                         breaks = quantile(dm.L.ln_cpricei2, probs = seq(0, 1, by = 1/n.g), na.rm = T, weight = base.sales),
                                         labels = 1:n.g, right = FALSE)]
      quantlab <- round(quantile(all_pi$dm.L.ln_cpricei2, 
                                 probs = seq(0, 1, by = 1/n.g), na.rm = T, 
                                 weight = all_pi$base.sales), digits = 4)
      # Saturate fixed effects
      all_pi[, group_region_by_module_by_time := .GRP, by = .(region_by_module_by_time, quantile)]
      all_pi[, group_division_by_module_by_time := .GRP, by = .(division_by_module_by_time, quantile)]
      
      for (FE in FE_opts) {
        
        formula1 <- as.formula(paste0(
          Y, "~ ", RHS1,"| ", FE, " + quantile | 0 | module_by_state"
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
        res1.dt[, degree := n]
        res1.dt[, quant.control := n.g]
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
          
        # Compute predicted values of the derivative. Common support
        pred_b <- rep(0,15)
        pred_se <- rep(0,15)
        for (i in 1:15) {
          plc.formula1 <- paste0(av.tax.ch, "*(D.ln_sales_tax +", paste0(paste0(paste0(paste0((price_values[i]),"^",1:(n))), "*D.ln_sales_tax_init_",1:n), collapse = " + "), ") = 0")
          # Predictred
          pplc.test1 <- glht(res1, linfct = c(plc.formula1))
          pred_b[i] <- coef(summary(pplc.test1))[[1]]
          pred_se[i] <- sqrt(vcov(summary(pplc.test1)))[[1]]
        }
        # Create data
        coef.dt <- data.table(price_values, pred_b, pred_se)
        out.pred.file <- paste0(output.path,"/predict_", Y, "_", n,"_", FE,"_c", n.g,".csv")
        fwrite(coef.dt, out.pred.file)      
      
      }
    }
  }
}


## Run within

## Run in Diffs
for (Y in c(outcomes.within)) {
  RHS1 <- "w.ln_sales_tax"
  for (n in 1:3) {
    
    # Create polynomial of initial tax rate of degree "n"
    all_pi[, paste0("w.ln_sales_tax_init_",n) := w.ln_sales_tax*(dm.L.ln_cpricei2^(n))]
    # Add to formula 1
    RHS1 <- paste(RHS1, paste0("w.ln_sales_tax_init_",n), sep = " + ")
    
    for (n.g in 2:5) {
      
      # Create groups of initial values of tax rate
      # We use the full weighted distribution
      all_pi <- all_pi[, quantile := cut(dm.L.ln_cpricei2,
                                         breaks = quantile(dm.L.ln_cpricei2, probs = seq(0, 1, by = 1/n.g), na.rm = T, weight = base.sales),
                                         labels = 1:n.g, right = FALSE)]
      quantlab <- round(quantile(all_pi$dm.L.ln_cpricei2, 
                                 probs = seq(0, 1, by = 1/n.g), na.rm = T, 
                                 weight = all_pi$base.sales), digits = 4)
      # Saturate fixed effects
      all_pi[, group_region_by_module_by_time := .GRP, by = .(region_by_module_by_time, quantile)]
      all_pi[, group_division_by_module_by_time := .GRP, by = .(division_by_module_by_time, quantile)]
      
      for (FE in FE_opts) {
        
        formula1 <- as.formula(paste0(
          Y, "~ ", RHS1,"| ", FE, " + quantile | 0 | module_by_state"
        ))
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
        res1.dt[, degree := n]
        res1.dt[, quant.control := n.g]
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
        
        # Compute predicted values of the derivative. Common support
        pred_b <- rep(0,15)
        pred_se <- rep(0,15)
        for (i in 1:15) {
          plc.formula1 <- paste0(av.tax.ch, "*(w.ln_sales_tax +", paste0(paste0(paste0(paste0((price_values[i]),"^",1:(n))), "*w.ln_sales_tax_init_",1:n), collapse = " + "), ") = 0")
          # Predictred
          pplc.test1 <- glht(res1, linfct = c(plc.formula1))
          pred_b[i] <- coef(summary(pplc.test1))[[1]]
          pred_se[i] <- sqrt(vcov(summary(pplc.test1)))[[1]]*((res1$N - res1$p)/(res1$N - res1$p - length(unique(all_pi$store_by_module))))^(1/2)
        }
        # Create data
        coef.dt <- data.table(price_values, pred_b, pred_se)
        out.pred.file <- paste0(output.path,"/predict_", Y, "_", n,"_", FE,"_c", n.g,".csv")
        fwrite(coef.dt, out.pred.file)   
      }
    }
  }
}

