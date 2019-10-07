#' Run Non linear estimation using polynomials (parametric) in differences
#' Semester Only
#' We do not run all polynomials 2-6 but only those we know will run
#' We plot the estimated response function: the estimated expected value for a given change

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
output.results.file <- "Data/allpol_semesterly_diff.csv"
output.path <- "../../home/slacouture/NLP"

### Set up Semester Data ---------------------------------
all_pi <- fread(data.semester)


# Create interaction term 
# First lagged value (initial)
all_pi[, L.ln_sales_tax := ln_sales_tax - D.ln_sales_tax]
all_pi[, D.ln_sales_tax_init := L.ln_sales_tax*D.ln_sales_tax]

# setup
outcomes <- c("D.ln_cpricei2", "D.ln_quantity3")
FE_opts <- c("region_by_module_by_time", "division_by_module_by_time")

RHS <- "D.ln_sales_tax_init"

## For predicted values
# Discretize taxrate: relevant support
all_pi[, ln_sales_tax_r := ifelse(D.ln_sales_tax == 0, NA, ln_sales_tax)]
tax_values <-seq(quantile(all_pi$ln_sales_tax_r, probs = 0.05, na.rm = T, weight=all_pi$base.sales),
                 quantile(all_pi$ln_sales_tax_r, probs = 0.95, na.rm = T, weight=all_pi$base.sales),
                 length.out = 15)
tax_values <-seq(min(all_pi$ln_sales_tax, na.rm = T), max(all_pi$ln_sales_tax, na.rm = T), length.out = 15)
# The value of 0 is problematic: replace it for a very small value
if (tax_values[1] == 0) tax_values[1] <- 0.001
# average tax changes (from positive changes)
av.tax.ch <- as.vector(all_pi[D.ln_sales_tax != 0,][, mean(D.ln_sales_tax)])
av.tax.ch <- 1

### Run level twoway FE semester data --------------------------------
LRdiff_res <- data.table(NULL)
for (n in 2:3) {
  # First create power
  all_pi[, paste0("D.ln_sales_tax_init_",n) := D.ln_sales_tax*(L.ln_sales_tax^(n))]
  # Add to formula
  RHS <- paste(RHS, paste0("D.ln_sales_tax_init_",n), sep = " + ")

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
      res1.dt[, outcome := Y]
      res1.dt[, controls := FE]
      res1.dt[, poly := "standard"]
      res1.dt[, degree.or := n]
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
      

      
      # Compute predicted values of the derivative
      pred_b <- rep(0,15)
      pred_se <- rep(0,15)
      for (i in 1:15) {
        plc.formula1 <- paste0(av.tax.ch, "*( ", (tax_values[i]),"*D.ln_sales_tax_init +", paste0(paste0(paste0(paste0((tax_values[i]),"^",2:(n))), "*D.ln_sales_tax_init_",2:n), collapse = " + "), ") = 0")
        # Predictred
        pplc.test1 <- glht(res1, linfct = c(plc.formula1))
        pred_b[i] <- coef(summary(pplc.test1))[[1]]
        pred_se[i] <- sqrt(vcov(summary(pplc.test1)))[[1]]
      }
      # Create data
      coef.dt <- data.table(tax_values, pred_b, pred_se)
      out.pred.file <- paste0(output.path,"/D standard/predict", Y, "_", n,"_", FE,".csv")
      fwrite(coef.dt, out.pred.file)
      
      # Output file
      graphout <- paste0(output.path,"/D standard/", Y, "_", n,"_", FE, ".png")
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

### Set up Hermite Polynomials ----------------------------------------------

## Hermite Function -------------------------------------------

hermite <-function(x, n, prob = T) {
  if (n < 0) {stop("sequence number n must be non-negative")}
  if (prob) {
    if (n == 0) {return(seq(1,1, length.out = length(x)))}
    if (n == 1) {return(x)} 
    if (n > 1) {return(x*hermite(x, n-1) - (n-1)*hermite(x, n-2))}  
  } else {
    if (n == 0) {return(seq(1,1, length.out = length(x)))}
    if (n == 1) {return(2*x)} 
    if (n > 1) {return(2*x*hermite(x, n-1, prob = F) - 2*(n-1)*hermite(x, n-2, prob = F))}     
  }
}


## For predicted values

# Compute hermite polinomials on values and create table to extract interest values
tax_values_2 <- hermite(tax_values, 2)
tax_values_3 <- hermite(tax_values, 3)
tax_values_4 <- hermite(tax_values, 4)
tax_values_5 <- hermite(tax_values, 5)
tax_hermite <- rbind(tax_values, tax_values_2, tax_values_3, tax_values_4, tax_values_5)

RHS <- "D.ln_sales_tax_init"

### Run level twoway FE semester data: hermite polynomials --------------------------------
for (n in 2:4) {
  # First create power
  all_pi[, paste0("D.ln_sales_tax_init_",n) := D.ln_sales_tax*(hermite(L.ln_sales_tax, n))]
  # Add to formula
  RHS <- paste(RHS, paste0("D.ln_sales_tax_init_",n), sep = " + ")
  
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
      res1.dt[, outcome := Y]
      res1.dt[, controls := FE]
      res1.dt[, poly := "hermite"]
      res1.dt[, degree.or := n]
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
      
      
      # Compute predicted values of the derivative
      pred_b <- rep(0,15)
      pred_se <- rep(0,15)
      for (i in 1:15) {
        plc.formula1 <- paste0(av.tax.ch, "*(", (tax_values[i]),"*D.ln_sales_tax_init +", paste0(paste0(paste0((tax_hermite)[2:n,i]), "*D.ln_sales_tax_init_",2:n), collapse = " + "), ") = 0")
        # Predictred
        pplc.test1 <- glht(res1, linfct = c(plc.formula1))
        pred_b[i] <- coef(summary(pplc.test1))[[1]]
        pred_se[i] <- sqrt(vcov(summary(pplc.test1)))[[1]]
      }
      # Create data
      coef.dt <- data.table(tax_values, pred_b, pred_se)
      out.pred.file <- paste0(output.path,"/D hermite prob/predict", Y, "_", n,"_", FE,".csv")
      fwrite(coef.dt, out.pred.file)
      
      # Output file
      graphout <- paste0(output.path,"/D hermite prob/", Y, "_", n,"_", FE, ".png")
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


### Run level twoway FE semester data: hermite polynomials physicists --------------------------------
tax_values <-seq(min(all_pi$ln_sales_tax), max(all_pi$ln_sales_tax), length.out = 15)
# The value of 0 is problematic: replace it for a very small value
if (tax_values[1] == 0) tax_values[1] <- 0.001
# Compute hermite polinomials on values and create table to extract interest values
tax_values_2 <- hermite(tax_values, 2, prob = F)
tax_values_3 <- hermite(tax_values, 3, prob = F)
tax_values_4 <- hermite(tax_values, 4, prob = F)
tax_values_5 <- hermite(tax_values, 5, prob = F)
tax_hermite <- rbind(tax_values, tax_values_2, tax_values_3, tax_values_4, tax_values_5)

RHS <- "D.ln_sales_tax_init"
for (n in 2:6) {
  # First create power
  all_pi[, paste0("D.ln_sales_tax_init_",n) := D.ln_sales_tax*(hermite(ln_sales_tax, n, prob = F))]
  # Add to formula
  RHS <- paste(RHS, paste0("D.ln_sales_tax_init_",n), sep = " + ")
  
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
      res1.dt[, outcome := Y]
      res1.dt[, controls := FE]
      res1.dt[, poly := "hermite ph"]
      res1.dt[, degree.or := n]
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
      
      
      # Compute predicted values of the derivative
      pred_b <- rep(0,15)
      pred_se <- rep(0,15)
      for (i in 1:15) {
        plc.formula1 <- paste0(av.tax.ch, "*(", (tax_values[i]),"*D.ln_sales_tax_init +", paste0(paste0(paste0((tax_hermite)[2:n,i]), "*D.ln_sales_tax_init_",2:n), collapse = " + "), ") = 0")
        # Predictred
        pplc.test1 <- glht(res1, linfct = c(plc.formula1))
        pred_b[i] <- coef(summary(pplc.test1))[[1]]
        pred_se[i] <- sqrt(vcov(summary(pplc.test1)))[[1]]
      }
      # Create data
      coef.dt <- data.table(tax_values, pred_b, pred_se)
      out.pred.file <- paste0(output.path,"/D hermite phy/predict", Y, "_", n,"_", FE,".csv")
      fwrite(coef.dt, out.pred.file)
      
      # Output file
      graphout <- paste0(output.path,"/D hermite phy/", Y, "_", n,"_", FE, ".png")
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