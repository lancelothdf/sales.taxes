#' File that bootstraps estimates of elasticity of demand
#' Create a function for every variation of the main spec (so we can bootstrap that)
#' The function will be for the 1st order approx in the long run
#' We export the estimated elasticities based on the "relevant support" (see previous codes)
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



## Function that estimates the IV ----------------------------------
estimate.iv <- function(data, quantity, price, taxrate, lagtax, FE_opts, weights, 
                                       prediction, nonlinear = "polynomial", pol.degree = 3,
                                       knots = NULL, boot.run = T) {
  
  #' data: the dataset used
  #' quantity: the name of the variable of quantities
  #' price: the name of the variable of price
  #' taxrate: the name of the variables of tax rates
  #' lagtax: the name of the variable of the lag value of the taxrate
  #' FE_opts: a vector of FE that will be used
  #' weight: the name of the vector of weights
  #' prediction: the vector of the values of tax rates for which we want predictions
  #' nonlinear: type of nonlinearity to bbe estimated ("polynomial" -def.- or "splines")
  #' pol.degree: if polynomial, the order to run (Def: 3)
  #' knots: if splines, the number of knots that will be used. Must be specified
  #' boot.run: whether we want nice results or bootstrap output
  
  ##
  output <- data.table(NULL)
  
  if (nonlinear == "polynomial") {
    RHS <- as.character(taxrate)
    
    data[, (paste0("tau_",1:pol.degree)) := get(taxrate)*(get(lagtax)^(1:pol.degree))]
    a <- paste0(taxrate, "*",paste0(lagtax, "^", 1:pol.degree ))
    flog.info("Created %s", a)
    
    
    # Add to formula
    RHS <- paste(RHS, paste0("tau_",1:pol.degree, collapse = " + "), sep = " + ")
    for (FE in FE_opts) {
      
      ## Price
      formula1 <- as.formula(paste0(
        price, "~", RHS ," | ", FE, " | 0 | module_by_state"
      ))
      flog.info("Estimating %s", RHS)
      res1 <- felm(formula = formula1, data = data,
                   weights = data[, get(weights)])

      # Get the predicted values
      pred_b_p <- rep(0,length(prediction))
      for (i in 1:length(prediction)) {
        plc.formula1 <- paste0(taxrate, "+ ", paste0(paste0(paste0(paste0((prediction[i]),"^",1:(pol.degree))), "*tau_",1:pol.degree), collapse = " + "), " = 0")
        # Predictred
        pplc.test1 <- glht(res1, linfct = c(plc.formula1))
        pred_b_p[i] <- coef(summary(pplc.test1))[[1]]
      }

      
      ## Quantity
      formula1 <- as.formula(paste0(
        quantity, "~", RHS ," | ", FE, " | 0 | module_by_state"
      ))
      flog.info("Estimating %s", RHS)
      res1 <- felm(formula = formula1, data = data,
                   weights = data[, get(weights)])

      # Get the predicted values
      pred_b_q <- rep(0,length(prediction))
      for (i in 1:length(prediction)) {
        plc.formula1 <- paste0(taxrate, "+", paste0(paste0(paste0(paste0((prediction[i]),"^",1:(pol.degree))), "*tau_",1:pol.degree), collapse = " + "), " = 0")
        # Predictred
        pplc.test1 <- glht(res1, linfct = c(plc.formula1))
        pred_b_q[i] <- coef(summary(pplc.test1))[[1]]
      }    
    
      predict <- pred_b_q/pred_b_p
      coef.dt <- data.table(tax_values, predict, FE)
      output <- rbind(output, coef.dt)
      
    }
  }
  
  if (nonlinear == "splines") {
    
    if (length(knots) < 3 ) {
      return("You must specify a number of knots higher than 3")
    } else {
      RHS <- as.character(taxrate)

      data[, (paste0("tau_",1:pol.degree)) := get(taxrate)*(get(lagtax)^(1:pol.degree))]
      # Add to formula
      RHS <- paste(RHS, paste0("tau_",1:pol.degree, collapse = " + "), sep = " + ")
      # Second create truncated function
      d <-1
      for (ep in knots) {
        all_pi[, paste0("tau_k",d) := (get(lagtax) > ep)*get(taxrate)*(get(lagtax) - ep)^(pol.degree)]
        d <- d+1
      }
      # Add trunctaed terms to formula
      RHS <- paste(RHS, paste0(paste0("tau_k",1:d), collapse = " + "), sep = " + ")
      
      for (FE in FE_opts) {
          
        ## Price
        formula1 <- as.formula(paste0(
          price, "~", RHS ," | ", FE, " | 0 | module_by_state"
        ))
        flog.info("Estimating %s", RHS)
        res1 <- felm(formula = formula1, data = data,
                     weights = data[, get(weights)])
  
        # Compute predicted values of the derivative
        pred_b_p <- rep(0,length(prediction))
        for (i in 1:length(prediction)) {
          
          plc.formula1pk <- paste0(paste0(paste0((prediction[i]),"^",1:pol.degree), "*tau_",1:pol.degree), collapse = " + ")
           
          # Can't Add directly: have to add if is taken into account
          d <-1
          for (ep in knots) {
            if ((tax_values[i]) > ep) {
              plc.formula1pk <- paste0(plc.formula1pk, "+", paste0(((prediction[i]) - ep)^(n), "*tau_k",d))
            }
            d <- d+1
          }
          plc.formula1 <- paste0(taxrate, " + ", plc.formula1pk, " = 0")
          
          # Predictred
          pplc.test1 <- glht(res1, linfct = c(plc.formula1))
          pred_b_p[i] <- coef(summary(pplc.test1))[[1]]
        }
        
        
        ## Quatity
        formula1 <- as.formula(paste0(
          quantity, "~", RHS ," | ", FE, " | 0 | module_by_state"
        ))
        flog.info("Estimating %s", RHS)
        res1 <- felm(formula = formula1, data = data,
                     weights = data[, get(weights)])
        
        # Compute predicted values of the derivative
        pred_b_q <- rep(0,length(prediction))
        for (i in 1:length(prediction)) {
          
          plc.formula1pk <- paste0(paste0(paste0((prediction[i]),"^",1:pol.degree), "*tau_",1:pol.degree), collapse = " + ")
          
          # Can't Add directly: have to add if is taken into account
          d <-1
          for (ep in knots) {
            if ((tax_values[i]) > ep) {
              plc.formula1pk <- paste0(plc.formula1pk, "+", paste0(((prediction[i]) - ep)^(pol.degree), "*tau_k",d))
            }
            d <- d+1
          }
          plc.formula1 <- paste0(taxrate, " + ", plc.formula1pk, " = 0")
          
          # Predictred
          pplc.test1 <- glht(res1, linfct = c(plc.formula1))
          pred_b_q[i] <- coef(summary(pplc.test1))[[1]]
        }        
        
        # Create data
        predict <- pred_b_q/pred_b_p
        coef.dt <- data.table(tax_values, predict, FE)
        output <- rbind(output, coef.dt)

      }
    }
  }
  if (boot.run) {return(output[["predict"]])} else{return(output)}
}


### Set up Semester Data ---------------------------------
all_pi <- fread(data.semester)

## Create the demeaned outcome and tax rates (within)

all_pi[, w.ln_sales_tax := ln_sales_tax - mean(ln_sales_tax), by = .(store_by_module)]
all_pi[, w.ln_cpricei2 := ln_cpricei2 - mean(ln_cpricei2), by = .(store_by_module)]
all_pi[, w.ln_quantity3 := ln_quantity3 - mean(ln_quantity3), by = .(store_by_module)]

# Create lagged value (initial)
all_pi[, L.ln_sales_tax := ln_sales_tax - D.ln_sales_tax]

FE_opts <- c("region_by_module_by_time", "division_by_module_by_time")

# Prediction vector
all_pi[, ln_sales_tax_r := ifelse(D.ln_sales_tax == 0, NA, L.ln_sales_tax)]
tax_values <-seq(quantile(all_pi$ln_sales_tax_r, probs = 0.05, na.rm = T, weight=all_pi$base.sales),
                 quantile(all_pi$ln_sales_tax_r, probs = 0.95, na.rm = T, weight=all_pi$base.sales),
                 length.out = 15)

## Try function and export -------------------------------
t <- estimate.iv(data = all_pi,
                 quantity = "w.ln_quantity3", 
                 price = "w.ln_cpricei2", 
                 taxrate = "w.ln_sales_tax",
                 lagtax = "L.ln_sales_tax", 
                 FE_opts = FE_opts,
                 weights = "base.sales", 
                 prediction = tax_values, 
                 nonlinear = "polynomial", 
                 pol.degree = 3,
                 boot.run = F)
fwrite(t, "../../home/slacouture/NLP/beta_IV/try_IVest.csv")
t <- estimate.iv(data = all_pi,
                 quantity = "w.ln_quantity3", 
                 price = "w.ln_cpricei2", 
                 taxrate = "w.ln_sales_tax",
                 lagtax = "L.ln_sales_tax", 
                 FE_opts = FE_opts,
                 weights = "base.sales", 
                 prediction = tax_values, 
                 nonlinear = "polynomial", 
                 pol.degree = 3)
fwrite(t, "../../home/slacouture/NLP/beta_IV/try_IVest_out.csv")





# ############## Run bootstrap: Weighted using base.sales -----------------

block.boot <- function(x, i) {
  bootdata <- merge(data.table(state_by_module=x[i]), all_pi, by = "state_by_module", allow.cartesian = T)
  rep_count <<- rep_count + 1
  flog.info("Iteration %s", rep_count)
  estimate.iv(data = all_pi,
              quantity = "w.ln_quantity3", 
              price = "w.ln_cpricei2", 
              taxrate = "w.ln_sales_tax",
              lagtax = "L.ln_sales_tax", 
              FE_opts = FE_opts,
              weights = "base.sales", 
              prediction = tax_values, 
              nonlinear = "polynomial", 
              pol.degree = 3)
}

### Run essay bootstrap

# Define level of block bootstrap
state_by_module_ids <- unique(all_pi$state_by_module)
# Improve
# Run bootstrap
rep_count = 0
b0 <- boot(state_by_module_ids, block.boot, 100)

# Export: observed and distribution
t <- data.table(b0$t0)
mat.t <- data.table(b0$t)
fwrite(t, "../../home/slacouture/NLP/beta_IV/IV_est_pol3_100.csv")
fwrite(mat.t, "../../home/slacouture/NLP/beta_IV/mat_IV_est_pol3_100.csv")