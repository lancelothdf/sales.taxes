#' Run Non linear estimation using polynomials (parametric) in the levels counterpart
#' of the model in FD
#' Semester data
#' We do not run all polynomials 2-6 but only those we know will run
#' We plot the estimated response function (the expected value). We plot this in 
#' the full support
#' In this case the standard errors must be adjusted to the fact they are not taking into accout the FE.


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
output.results.file <- "Data/splines_semesterly_difftolev.csv"
output.knots.file <- "Data/splines_semesterly_difftolev_knots.csv"
output.path <- "../../home/slacouture/NLP"

### Set up Semester Data ---------------------------------
all_pi <- fread(data.semester)

## Create the demeaned outcome and tax rates (within)

all_pi[, w.ln_sales_tax := ln_sales_tax - mean(ln_sales_tax), by = .(store_by_module)]
all_pi[, w.ln_cpricei2 := ln_cpricei2 - mean(ln_cpricei2), by = .(store_by_module)]
all_pi[, w.ln_quantity3 := ln_quantity3 - mean(ln_quantity3), by = .(store_by_module)]


# Create interaction term 
all_pi[, w.ln_sales_tax_init := w.ln_sales_tax*mean(ln_sales_tax), by = .(store_by_module)]

#Create mean value
all_pi[, m.ln_sales_tax := mean(ln_sales_tax), by = .(store_by_module)]

# setup
outcomes <- c("w.ln_cpricei2", "w.ln_quantity3")
FE_opts <- c("region_by_module_by_time", "division_by_module_by_time")

RHS <- "w.ln_sales_tax + w.ln_sales_tax_init"

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
knots_out <- data.table(NULL)
# K is the number of knots
# n is the polynomial degree of spline
for (k in 3:10) {
  knots <- (max(all_pi$m.ln_sales_tax) - min(all_pi$m.ln_sales_tax))* (1:k) / (k+1)
  knots_out <- cbind(data.table(knots), knots_out)
  fwrite(knots_out, output.knots.file)
  
  for (n in 1:2) {
    # Restart formula
    RHS <- "w.ln_sales_tax + w.ln_sales_tax_init"
    # First create power
    if (n > 1) {
      # First create power
      all_pi[, paste0("w.ln_sales_tax_init_",n) := w.ln_sales_tax*(mean(ln_sales_tax)^(n)), by = .(store_by_module)]
      # Add to formula
      RHS <- paste(RHS, paste0(paste0("w.ln_sales_tax_init_",2:n), collapse = " + "), sep = " + ")

    }
    # Second create truncated function
    d <-1
    for (ep in knots) {
      all_pi[, paste0("w.ln_sales_tax_init_k",d) := (m.ln_sales_tax > ep)*w.ln_sales_tax*(m.ln_sales_tax - ep)^(n)]
      d <- d+1
    }
    
    # Add trunctaed terms to formula
    RHS <- paste(RHS, paste0(paste0("w.ln_sales_tax_init_k",1:k), collapse = " + "), sep = " + ")
    
    
    for (Y in c(outcomes)) {
      for (FE in FE_opts) {
        
        formula1 <- as.formula(paste0(
          Y, "~", RHS ," | ", FE, " | 0 | module_by_state"
        ))
        flog.info("Estimating spline with %s knots of degree %s with %s as outcome with %s FE", k, n, Y, FE)
        res1 <- felm(formula = formula1, data = all_pi,
                     weights = all_pi$base.sales)
        flog.info("Finished estimating spline with %s knots of degree %s with %s as outcome with %s FE", k, n, Y, FE)
        
        
        ## attach results
        flog.info("Writing results...")
        res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
        res1.dt[, outcome := Y]
        res1.dt[, controls := FE]
        res1.dt[, spec := "spline"]
        res1.dt[, degree.or := n]
        res1.dt[, n.knots := k]
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
          
          if (n >1) {
            plc.formula1pk <- paste0((tax_values[i]), "*w.ln_sales_tax_init +", 
                                     paste0(paste0(paste0((tax_values[i]),"^",2:n), "*w.ln_sales_tax_init_",2:n), 
                                            collapse = " + "))
          } else {
            
            plc.formula1pk <- paste0((tax_values[i]), "*w.ln_sales_tax_init")
            
          }
          
          # Can't Add directly: have to add if is taken into account
          d <-1
          for (ep in knots) {
            if ((tax_values[i]) > ep) {
              plc.formula1pk <- paste0(plc.formula1pk, "+", paste0(((tax_values[i]) - ep)^(n), "*","w.ln_sales_tax_init_k",d))
            }
            d <- d+1
          }
          plc.formula1 <- paste0(av.tax.ch, "*(w.ln_sales_tax + ", plc.formula1pk, ") = 0")
          
          # Predictred
          pplc.test1 <- glht(res1, linfct = c(plc.formula1))
          pred_b[i] <- coef(summary(pplc.test1))[[1]]
          pred_se[i] <- sqrt(vcov(summary(pplc.test1)))[[1]]
        }
        # Create data
        coef.dt <- data.table(tax_values, pred_b, pred_se)
        out.pred.file <- paste0(output.path,"/D splines tolev/predict",  Y, "_", n,"_", FE, "_", k,".csv")
        fwrite(coef.dt, out.pred.file)
        
        # Output file
        graphout <- paste0(output.path,"/D splines tolev/", Y, "_", n,"_", FE, "_", k, ".png")
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
}

