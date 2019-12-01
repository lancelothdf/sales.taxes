#' Sales Taxes Project
#' Solve for the Parameter of competition
#' This codes solves the problem of competition parameter NOT assuming perfect elasticity
#' It takes as an input 3 things
#' 1) Data. In particular we need 1) log prices, 2) log demeaned prices and 3) weights
#' 2) The parameters of the demand function
#' 3) The value of the pass-through observed
#' This code first defines the function that calculates the passtrhough for a given competition parameter, a given data and a given parameter
#' Then it opens and cleans the data to use it
#' Finally uses a unique root procedure to  solve the problem
#' 

library(data.table)
library(futile.logger)
library(lfe)
library(multcomp)
library(ggplot2)


setwd("/project2/igaarder")

## Function to be used to solve the problem --------------


pasthrough.to.0.es <- function(theta, data, d.p, p, weights, function.param, rho.hat, es){
  
  #' data: data set used (data.table)
  #' d.p: demeaned log prices variable (character)
  #' p: log price variable (character)
  #' weights: name of the variable of weights (character)
  #' function.param: vector of coefficients of the polynomial demand function (beta_0, beta_1 ,..., beta_K) (vector) 
  #' rho.hat: the estimated pasthrough (number)
  #' theta: perfect competition parameter. Used in uniroot
  #' es: elasticity of supply
  
  # 0. check data is data.table
  data <- as.data.table(data)
  
  # 1. calculate the elasticity for each observation
  data[, q1 := 0]
  for (k in 2:length(function.param)) {
    data[, q1 := q1 + (k-1)*function.param[k]*(get(d.p))^(k-2)]
  }
  
  # 2. Calculate the second derivative for each observation
  data[, q2 := 0]
  for (k in 3:length(function.param)) {
    data[, q2 := q2 + (k-1)*(k-2)*function.param[k]*(get(d.p))^(k-3)]
  }  
  
  # 3. Calculate the contribution of that observation
  data[, integrand := (1-(theta/q1))/(1+theta*(1-((q1*(exp(get(p))-1)+q2)/(q1^2)))+(1-theta/q1)*(q1/es)) ]
  
  # 4. Calculate the integral as the (weighted) average
  integral <- data[, mean(integrand, weights = get(weights))]
  
  # 5. Return the value of the calculation
  return(rho.hat - integral)
}








## input filepaths -----------------------------------------------
#' This data is the same as all_goods_pi_path, except it has 2015-2016 data as well.
data.semester <- "Data/Nielsen/semester_nielsen_data.csv"


## output filepaths ----------------------------------------------
output.results.file <- "Data/Comp_Demand_sat_initial_price_semester_partial.csv"


### Set up Semester Data ---------------------------------
all_pi <- fread(data.semester)

# Need to demean
all_pi[, module_by_time := .GRP, by = .(product_module_code, semester, year)]
all_pi[, L.ln_cpricei2 := ln_cpricei2 - D.ln_cpricei2]
all_pi[, dm.L.ln_cpricei2 := L.ln_cpricei2 - mean(L.ln_cpricei2, na.rm = T), by = module_by_time]
all_pi[, dm.ln_cpricei2 := ln_cpricei2 - mean(ln_cpricei2, na.rm = T), by = module_by_time]
all_pi[, dm.ln_quantity3 := ln_quantity3 - mean(ln_quantity3, na.rm = T), by = module_by_time]


# Defining common support
control <- all_pi[D.ln_sales_tax == 0,]
treated <- all_pi[D.ln_sales_tax != 0,]

# Price 
pct1.control <- quantile(control$dm.L.ln_cpricei2, probs = 0.01, na.rm = T, weight=control$base.sales)
pct1.treated <- quantile(treated$dm.L.ln_cpricei2, probs = 0.01, na.rm = T, weight=treated$base.sales)

pct99.control <- quantile(control$dm.L.ln_cpricei2, probs = 0.99, na.rm = T, weight=control$base.sales)
pct99treated <- quantile(treated$dm.L.ln_cpricei2, probs = 0.99, na.rm = T, weight=treated$base.sales)

all_pi[, cs_price := ifelse(dm.L.ln_cpricei2 > max(pct1.treated, pct1.control) & 
                              dm.L.ln_cpricei2 < min(pct99treated, pct99.control), 1, 0)]
# Make sure missings are 0s
all_pi[, cs_price := ifelse(is.na(dm.L.ln_cpricei2), 0, cs_price)]

## Keep within the common support
all_pi <- all_pi[cs_price == 1,]


## Keep the needed variables only
all_pi_short <- all_pi[, c("dm.ln_cpricei2", "ln_cpricei2", "base.sales", "D.ln_sales_tax"), with = FALSE]

## Treated
all_pi_short_t <- all_pi_short[D.ln_sales_tax == 0]








## ---------- Inputs to the program ----------------

theta.function.2 <- c(7.712, -0.521, 1.546)
theta.function.3 <- c(7.712, -0.636, 1.506, 7.072)
theta.function.4 <- c(7.712, -0.620, 5.158, 7.382, -107.352)
theta.function.5 <- c(7.712, -0.554, 5.174, -3.373, -110.691, 172.613)

rho <- 1.0567


## ---- plot problems ---------
# 
LRdiff_res_full <- data.table(NULL)
for (es.value in c(0.5, 1, 2, 5, 10, 50, 100)) {
  
  
  
  
  ## K = 2
  graphout <- paste0("../../home/slacouture/NLP/ic_t_2_", es.value,".png")
  data.plot <- data.table(NULL)
  for (theta in seq(-0.5,1.5,0.025)) {
    value <- pasthrough.to.0.es(theta, data = all_pi_short_t,
                             d.p = "dm.ln_cpricei2", p = "ln_cpricei2", weights = "base.sales", function.param = theta.function.2, rho.hat = rho, es = es.value)
    data.plot <- rbind(data.plot, data.table(theta, value))
  }
  ggplot(data.plot, aes(theta, value)) + geom_line() +
    theme_bw(base_size = 14) +
    labs(x = "theta", y = paste0("Evaluated Root"), color = NULL)
  ggsave(graphout)
  
  graphout <- paste0("../../home/slacouture/NLP/ic_f_2_", es.value,".png")
  data.plot <- data.table(NULL)
  for (theta in seq(-0.5,1.5,0.025)) {
    value <- pasthrough.to.0.es(theta, data = all_pi_short,
                             d.p = "dm.ln_cpricei2", p = "ln_cpricei2", weights = "base.sales", function.param = theta.function.2, rho.hat = rho, es = es.value)
    data.plot <- rbind(data.plot, data.table(theta, value))
  }
  ggplot(data.plot, aes(theta, value)) + geom_line() +
    theme_bw(base_size = 14) +
    labs(x = "theta", y = paste0("Evaluated Root"), color = NULL)
  ggsave(graphout)
  
  ## K = 3
  graphout <- paste0("../../home/slacouture/NLP/ic_t_3_", es.value,".png")
  data.plot <- data.table(NULL)
  for (theta in seq(-0.5,1.5,0.025)) {
    value <- pasthrough.to.0.es(theta, data = all_pi_short_t,
                             d.p = "dm.ln_cpricei2", p = "ln_cpricei2", weights = "base.sales", function.param = theta.function.3, rho.hat = rho, es = es.value)
    data.plot <- rbind(data.plot, data.table(theta, value))
  }
  ggplot(data.plot, aes(theta, value)) + geom_line() +
    theme_bw(base_size = 14) +
    labs(x = "theta", y = paste0("Evaluated Root"), color = NULL)
  ggsave(graphout)
  
  graphout <- paste0("../../home/slacouture/NLP/ic_f_3_", es.value,".png")
  data.plot <- data.table(NULL)
  for (theta in seq(-0.5,1.5,0.025)) {
    value <- pasthrough.to.0.es(theta, data = all_pi_short,
                             d.p = "dm.ln_cpricei2", p = "ln_cpricei2", weights = "base.sales", function.param = theta.function.3, rho.hat = rho, es = es.value)
    data.plot <- rbind(data.plot, data.table(theta, value))
  }
  ggplot(data.plot, aes(theta, value)) + geom_line() +
    theme_bw(base_size = 14) +
    labs(x = "theta", y = paste0("Evaluated Root"), color = NULL)
  ggsave(graphout)
  
  ## K = 4
  graphout <- paste0("../../home/slacouture/NLP/ic_t_4_", es.value,".png")
  data.plot <- data.table(NULL)
  for (theta in seq(-0.5,1.5,0.025)) {
    value <- pasthrough.to.0.es(theta, data = all_pi_short_t,
                             d.p = "dm.ln_cpricei2", p = "ln_cpricei2", weights = "base.sales", function.param = theta.function.4, rho.hat = rho, es = es.value)
    data.plot <- rbind(data.plot, data.table(theta, value))
  }
  ggplot(data.plot, aes(theta, value)) + geom_line() +
    theme_bw(base_size = 14) +
    labs(x = "theta", y = paste0("Evaluated Root"), color = NULL)
  ggsave(graphout)
  
  graphout <- paste0("../../home/slacouture/NLP/ic_f_4_", es.value,".png")
  data.plot <- data.table(NULL)
  for (theta in seq(-0.5,1.5,0.025)) {
    value <- pasthrough.to.0.es(theta, data = all_pi_short,
                             d.p = "dm.ln_cpricei2", p = "ln_cpricei2", weights = "base.sales", function.param = theta.function.4, rho.hat = rho, es = es.value)
    data.plot <- rbind(data.plot, data.table(theta, value))
  }
  ggplot(data.plot, aes(theta, value)) + geom_line() +
    theme_bw(base_size = 14) +
    labs(x = "theta", y = paste0("Evaluated Root"), color = NULL)
  ggsave(graphout)
  
  ## K = 5
  graphout <- paste0("../../home/slacouture/NLP/ic_t_5_", es.value,".png")
  data.plot <- data.table(NULL)
  for (theta in seq(-0.5,1.5,0.025)) {
    value <- pasthrough.to.0.es(theta, data = all_pi_short_t,
                             d.p = "dm.ln_cpricei2", p = "ln_cpricei2", weights = "base.sales", function.param = theta.function.5, rho.hat = rho, es = es.value)
    data.plot <- rbind(data.plot, data.table(theta, value))
  }
  ggplot(data.plot, aes(theta, value)) + geom_line() +
    theme_bw(base_size = 14) +
    labs(x = "theta", y = paste0("Evaluated Root"), color = NULL)
  ggsave(graphout)
  
  graphout <- paste0("../../home/slacouture/NLP/ic_f_5_", es.value,".png")
  data.plot <- data.table(NULL)
  for (theta in seq(-0.5,1.5,0.025)) {
    value <- pasthrough.to.0.es(theta, data = all_pi_short,
                             d.p = "dm.ln_cpricei2", p = "ln_cpricei2", weights = "base.sales", function.param = theta.function.5, rho.hat = rho, es = es.value)
    data.plot <- rbind(data.plot, data.table(theta, value))
  }
  ggplot(data.plot, aes(theta, value)) + geom_line() +
    theme_bw(base_size = 14) +
    labs(x = "theta", y = paste0("Evaluated Root"), color = NULL)
  ggsave(graphout)
  
  
  
  
  ## ---------- Solve the unique root program in each scenario program ----------------
  
  
  LRdiff_res <- data.table(NULL)
  ## K = 2
  # Treated
  root.problem <- uniroot(pasthrough.to.0.es, c(0,1), extendInt = "yes", tol = .Machine$double.eps^0.4, data = all_pi_short_t, 
                          d.p = "dm.ln_cpricei2", p = "ln_cpricei2", weights = "base.sales", function.param = theta.function.2, rho.hat = rho, es = es.value)
  theta <- root.problem$root
  f <- root.problem$f.root
  results <- data.table(theta, f)
  results[, K := 2]
  results[, sample := "treated"]
  LRdiff_res <- rbind(LRdiff_res, results)
  
  # Full
  root.problem <- uniroot(pasthrough.to.0.es, c(0,1), extendInt = "yes", tol = .Machine$double.eps^0.4, data = all_pi_short, 
                          d.p = "dm.ln_cpricei2", p = "ln_cpricei2", weights = "base.sales", function.param = theta.function.2, rho.hat = rho, es = es.value)
  theta <- root.problem$root
  f <- root.problem$f.root
  results <- data.table(theta, f)
  results[, K := 2]
  results[, sample := "full"]
  LRdiff_res <- rbind(LRdiff_res, results)
  
  ## K = 3
  # Treated
  root.problem <- uniroot(pasthrough.to.0.es, c(0,1), extendInt = "yes", tol = .Machine$double.eps^0.4, data = all_pi_short_t, 
                          d.p = "dm.ln_cpricei2", p = "ln_cpricei2", weights = "base.sales", function.param = theta.function.3, rho.hat = rho, es = es.value)
  theta <- root.problem$root
  f <- root.problem$f.root
  results <- data.table(theta, f)
  results[, K := 3]
  results[, sample := "treated"]
  LRdiff_res <- rbind(LRdiff_res, results)
  
  # Full
  root.problem <- uniroot(pasthrough.to.0.es, c(0,1), extendInt = "yes", tol = .Machine$double.eps^0.4, data = all_pi_short, 
                          d.p = "dm.ln_cpricei2", p = "ln_cpricei2", weights = "base.sales", function.param = theta.function.3, rho.hat = rho, es = es.value)
  theta <- root.problem$root
  f <- root.problem$f.root
  results <- data.table(theta, f)
  results[, K := 3]
  results[, sample := "full"]
  LRdiff_res <- rbind(LRdiff_res, results)
  
  ## K = 4
  # Treated
  root.problem <- uniroot(pasthrough.to.0.es, c(0,1), extendInt = "yes", tol = .Machine$double.eps^0.4, data = all_pi_short_t, 
                          d.p = "dm.ln_cpricei2", p = "ln_cpricei2", weights = "base.sales", function.param = theta.function.4, rho.hat = rho, es = es.value)
  theta <- root.problem$root
  f <- root.problem$f.root
  results <- data.table(theta, f)
  results[, K := 4]
  results[, sample := "treated"]
  LRdiff_res <- rbind(LRdiff_res, results)
  
  # Full
  root.problem <- uniroot(pasthrough.to.0.es, c(0,1), extendInt = "yes", tol = .Machine$double.eps^0.4, data = all_pi_short, 
                          d.p = "dm.ln_cpricei2", p = "ln_cpricei2", weights = "base.sales", function.param = theta.function.4, rho.hat = rho, es = es.value)
  theta <- root.problem$root
  f <- root.problem$f.root
  results <- data.table(theta, f)
  results[, K := 4]
  results[, sample := "full"]
  LRdiff_res <- rbind(LRdiff_res, results)
  
  ## K = 5
  # Treated
  root.problem <- uniroot(pasthrough.to.0.es, c(0,1), extendInt = "yes", tol = .Machine$double.eps^0.4, data = all_pi_short_t, 
                          d.p = "dm.ln_cpricei2", p = "ln_cpricei2", weights = "base.sales", function.param = theta.function.5, rho.hat = rho, es = es.value)
  theta <- root.problem$root
  f <- root.problem$f.root
  results <- data.table(theta, f)
  results[, K := 5]
  results[, sample := "treated"]
  LRdiff_res <- rbind(LRdiff_res, results)
  
  # Full
  root.problem <- uniroot(pasthrough.to.0.es, c(0,1), extendInt = "yes", tol = .Machine$double.eps^0.4, data = all_pi_short, 
                          d.p = "dm.ln_cpricei2", p = "ln_cpricei2", weights = "base.sales", function.param = theta.function.5, rho.hat = rho, es = es.value)
  theta <- root.problem$root
  f <- root.problem$f.root
  results <- data.table(theta, f)
  results[, K := 5]
  results[, sample := "full"]
  LRdiff_res <- rbind(LRdiff_res, results)
  
  
  LRdiff_res[, es := es.value]

  ### Export
  LRdiff_res_full <- rbind(LRdiff_res_full, LRdiff_res)
  fwrite(LRdiff_res, output.results.file)
  
}

