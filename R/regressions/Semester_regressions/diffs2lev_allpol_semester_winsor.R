#' Run Non linear estimation using polynomials (parametric) in the levels counterpart
#' of the model in FD
#' Semester data
#' We do not run all polynomials 2-6 but only those we know will run
#' We plot the estimated response function (the expected value). We plot this in 
#' the full support
#' In this case the standard errors must be adjusted to the fact they are not taking into accout the FE.

#### This is the preferred specification: 10/17/19

### In this code we winsorize using three methods:
# 1) drop the top and bottom 5th percentiles of the full outcomes (in levels) distribution
# 2) drop the top and bottom 5th percentiles of the demeaned outcomes (in levels) distribution
# 3) drop the top and bottom 5th percentiles of the outcomes (in levels) by tax bin

## Also, we run the estimation dropping modules in states that change taxability status

library(data.table)
library(futile.logger)
library(lfe)
library(multcomp)
library(ggplot2)


setwd("/project2/igaarder")



## input filepaths -----------------------------------------------
#' This data is the same as all_goods_pi_path, except it has 2015-2016 data as well.
data.semester <- "Data/Nielsen/semester_nielsen_data.csv"
data.year <- "Data/Nielsen/yearly_nielsen_data.csv"


## output filepaths ----------------------------------------------
output.results.file <- "Data/allpol_semesterly_difftolev_trimmed.csv"
output.path <- "../../home/slacouture/NLP"

### Set up Semester Data ---------------------------------
all_pi <- fread(data.semester)

## Create the demeaned outcome and tax rates (within)

all_pi[, w.ln_sales_tax := ln_sales_tax - mean(ln_sales_tax), by = .(store_by_module)]
all_pi[, w.ln_cpricei2 := ln_cpricei2 - mean(ln_cpricei2), by = .(store_by_module)]
all_pi[, w.ln_quantity3 := ln_quantity3 - mean(ln_quantity3), by = .(store_by_module)]

# Create lagged value (initial)
all_pi[, L.ln_sales_tax := ln_sales_tax - D.ln_sales_tax]

# Create interaction term 
all_pi[, w.ln_sales_tax_init := w.ln_sales_tax*L.ln_sales_tax]

############ Winsorizing ----------------

## Method 1. cut in between 5th and 95th percentiles of the (weighted) distribution 
all_pi[, w.ln_cpricei2_3 := ifelse(ln_cpricei2 < quantile(ln_cpricei2, probs = 0.05, weights = base.sales) |  
                                     ln_cpricei2 > quantile(ln_cpricei2, probs = 0.95, weights = base.sales), NA, w.ln_cpricei2)]

all_pi[, w.ln_quantity3_3 := ifelse(ln_quantity3 < quantile(ln_quantity3, probs = 0.05, weights = base.sales) |  
                                      ln_quantity3 > quantile(ln_quantity3, probs = 0.95, weights = base.sales), NA, w.ln_quantity3)]


### Method 2. cut in between 5th and 95th percentiles of DEMEANED outcomes (on weighted distribution)
all_pi[, dem.ln_cpricei2 := ln_cpricei2 - mean(ln_cpricei2, weights = base.sales), by = .(store_by_module)]
all_pi[, w.ln_cpricei2_2 := ifelse(dem.ln_cpricei2 < quantile(dem.ln_cpricei2, probs = 0.05, weights = base.sales) |
                                   dem.ln_cpricei2 > quantile(dem.ln_cpricei2, probs = 0.95, weights = base.sales),
                               NA, w.ln_cpricei2)]

all_pi[, dem.ln_quantity3 := ln_quantity3 - mean(ln_quantity3, weights = base.sales), by = .(store_by_module)]
all_pi[, w.ln_quantity3_2 := ifelse(dem.ln_quantity3 < quantile(dem.ln_quantity3, probs = 0.05, weights = base.sales) |
                                    dem.ln_quantity3 > quantile(dem.ln_quantity3, probs = 0.95, weights = base.sales),
                                NA, w.ln_quantity3)]



## Method 3. cut in between 5th and 95th percentiles of the (weighted) distribution within tax rate bin
# first create tax rate bins 0.0125 (to have exactly 10 bins)
all_pi[, tax_bin := floor(ln_sales_tax/0.0125)]
# now windsorize outcomes 
all_pi[, w.ln_cpricei2_3 := ifelse(ln_cpricei2 < quantile(ln_cpricei2, probs = 0.05, weights = base.sales) |  
                                   ln_cpricei2 > quantile(ln_cpricei2, probs = 0.95, weights = base.sales), NA, w.ln_cpricei2), by = .(tax_bin)]

all_pi[, w.ln_quantity3_3 := ifelse(ln_quantity3 < quantile(ln_quantity3, probs = 0.05, weights = base.sales) |  
                                    ln_quantity3 > quantile(ln_quantity3, probs = 0.95, weights = base.sales), NA, w.ln_quantity3), by = .(tax_bin)]


##### Drop taxability changes -------------
# First, identify modules that change taxability status
all_pi[, taxable := ifelse(ln_sales_tax == 1,1,0)]
all_pi[, perc_taxable := sum(taxable)/.N , by = .(store_by_module)]
# Those that change taxability status are not 0 nor 1 in perc_taxable
all_pi[, w.ln_cpricei2_4 := ifelse(perc_taxable !=1 & perc_taxable !=0, NA, w.ln_cpricei2)]
all_pi[, w.ln_quantity3_4 := ifelse(perc_taxable !=1 & perc_taxable !=0, NA, w.ln_quantity3)]

### setup
outcomes <- c("w.ln_cpricei2_1", "w.ln_quantity3_1", "w.ln_cpricei2_2", "w.ln_quantity3_2", 
              "w.ln_cpricei2_3", "w.ln_quantity3_3", "w.ln_cpricei2_4", "w.ln_quantity3_4")
FE_opts <- c("region_by_module_by_time", "division_by_module_by_time")

RHS <- "w.ln_sales_tax + w.ln_sales_tax_init"

## For predicted values: plot only relevant support
# Discretize taxrate: relevant support
all_pi[, ln_sales_tax_r := ifelse(D.ln_sales_tax == 0, NA, L.ln_sales_tax)]
tax_values <-seq(quantile(all_pi$ln_sales_tax_r, probs = 0.05, na.rm = T, weight=all_pi$base.sales),
                 quantile(all_pi$ln_sales_tax_r, probs = 0.95, na.rm = T, weight=all_pi$base.sales),
                 length.out = 15)
# tax_values <-seq(min(all_pi$L.ln_sales_tax, na.rm = T), max(all_pi$L.ln_sales_tax, na.rm = T), length.out = 15)
# # The value of 0 is problematic: replace it for a very small value
# if (tax_values[1] == 0) tax_values[1] <- 0.001
# average tax changes (for prediction)
av.tax.ch <- 1

### Run level twoway FE semester data --------------------------------
LRdiff_res <- data.table(NULL)
for (n in 2:3) {
  # First create power
  all_pi[, paste0("w.ln_sales_tax_init_",n) := w.ln_sales_tax*(L.ln_sales_tax^(n))]
  # Add to formula
  RHS <- paste(RHS, paste0("w.ln_sales_tax_init_",n), sep = " + ")
  
  for (Y in c(outcomes)) {
    for (FE in FE_opts) {
      
      formula1 <- as.formula(paste0(
        Y, "~", RHS ," | ", FE, " | 0 | module_by_state"
      ))
      flog.info("Estimating with %s as outcome with %s FE.", Y, FE)
      res1 <- felm(formula = formula1, data = all_pi,
                   weights = all_pi$base.sales)
      flog.info("Finished estimating with %s as outcome with %s FE.", Y, FE)
      
      
      ## attach results
      flog.info("Writing results...")
      res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
      res1.dt[, c("Cluster s.e.") := get("Cluster s.e.")*((res1$N - res1$p)/(res1$N - res1$p - length(unique(all_pi$store_by_module))))^(1/2)]
      res1.dt[, c("t value") := Estimate/get("Cluster s.e.")]
      res1.dt[, outcome := Y]
      res1.dt[, controls := FE]
      res1.dt[, poly := "standard"]
      res1.dt[, degree.or := n]
      # Add summary values
      res1.dt[, Rsq := summary(res1)$r.squared]
      res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
      res1.dt[, N_obs := nrow(all_pi[!is.na(get(Y))])]
      res1.dt[, N_modules := length(unique(all_pi$product_module_code))]
      res1.dt[, N_stores :=  length(unique(all_pi$store_code_uc))]
      res1.dt[, N_counties := uniqueN(all_pi, by = c("fips_state", "fips_county"))]
      res1.dt[, N_years := uniqueN(all_pi, by = c("year"))]
      res1.dt[, N_county_modules := uniqueN(all_pi, by = c("fips_state", "fips_county",
                                                           "product_module_code"))]
      LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
      fwrite(LRdiff_res, output.results.file)
      
      
      
      # Compute predicted values of the derivative. Relevant support
      pred_b <- rep(0,15)
      pred_se <- rep(0,15)
      for (i in 1:15) {
        plc.formula1 <- paste0(av.tax.ch, "*(w.ln_sales_tax + ", (tax_values[i]),"*w.ln_sales_tax_init +", paste0(paste0(paste0(paste0((tax_values[i]),"^",2:(n))), "*w.ln_sales_tax_init_",2:n), collapse = " + "), ") = 0")
        # Predictred
        pplc.test1 <- glht(res1, linfct = c(plc.formula1))
        pred_b[i] <- coef(summary(pplc.test1))[[1]]
        pred_se[i] <- sqrt(vcov(summary(pplc.test1)))[[1]]*((res1$N - res1$p)/(res1$N - res1$p - length(unique(all_pi$store_by_module))))^(1/2)
      }
      # Create data
      coef.dt <- data.table(tax_values, pred_b, pred_se)
      out.pred.file <- paste0(output.path,"/D standard tolev w/predict", Y, "_", n,"_", FE,".csv")
      fwrite(coef.dt, out.pred.file)
      
      # Output file
      graphout <- paste0(output.path,"/D standard tolev w/", Y, "_", n,"_", FE, ".png")
      # Plot
      ggplot(data = coef.dt, mapping = aes(x = tax_values, y = pred_b)) +
        geom_point(size = 2, alpha = .5) +
        geom_line(linetype = "dashed") +
        geom_errorbar(aes(ymin = pred_b - 1.96 * pred_se,
                          ymax = pred_b + 1.96 * pred_se), width = .005) +
        theme_bw() +
        labs(x = "Sales Tax", y = paste0("Predicted response on ",Y), color = NULL) +
        geom_hline(yintercept = 0, color = "red", linetype = "dashed", alpha = .8)
      ggsave(graphout)
      
      
    }
  }
}
