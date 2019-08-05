## Sales taxes Project. Household Panel
# Run distributed lag regression in differences on (household x quarter x module) by type of module
# The file is modified so it can perform estimations without using all memory
# Author: John Bonney & Santiago Lacouture

library(data.table)
library(lfe)
library(futile.logger)
library(AER)
library(multcomp)
library(psych)
library(ggplot2)
library(DescTools)

setwd("/project2/igaarder/Data/Nielsen/Household_panel")


                                        ####### Type or Taxability ########

## Use previously build data set HH x Quarter to build sales tax lags and merge them on new data HH x module x quarter 
household.quarter <- fread("cleaning/consumer_panel_q_hh_2006-2016.csv")
# Keep only "non-magnet" households
household.quarter <- household.quarter[!is.na(projection_factor)]
household.quarter <- household.quarter[, .(ln_sales_tax, household_code, year, quarter)]

# impute tax rates prior to 2008 and after 2014
household.quarter[, ln_sales_tax := ifelse(year < 2008, ln_sales_tax[year == 2008 & quarter == 1], ln_sales_tax),
                 by = .(household_code)]
household.quarter[, ln_sales_tax := ifelse(year > 2014, ln_sales_tax[year == 2014 & quarter == 4], ln_sales_tax),
                 by = .(household_code)]

# Time
household.quarter[, cal_time := 4 * year + quarter]

## take first differences of outcomes
setkey(household.quarter, household_code, year, quarter)
household.quarter <- household.quarter[order(household_code, cal_time),] ##Sort on hh by year-quarter (in ascending order)

household.quarter[, D.ln_sales_tax := ln_sales_tax - shift(ln_sales_tax, n=1, type="lag"),
                 by = .(household_code)]

## generate lags and leads of ln_sales_tax
for (lag.val in 1:8) {
  lag.X <- paste0("L", lag.val, ".D.ln_sales_tax")
  household.quarter[, (lag.X) := shift(D.ln_sales_tax, n=lag.val, type="lag"),
                   by = .(household_code)]
  
  lead.X <- paste0("F", lag.val, ".D.ln_sales_tax")
  household.quarter[, (lead.X) := shift(D.ln_sales_tax, n=lag.val, type="lead"),
                   by = .(household_code)]
}

## Open Data
purchases.full <- fread("cleaning/consumer_panel_q_hh_mod_2006-2016.csv")

purchases.full$time <- factor(with(purchases.full, interaction(year, quarter)))


## Constraining Data set for estimations ------------ 
# Keep only "projection no-magnet" households: 
purchases.full[, sum(is.na(projection_factor))]
purchases.sample <- purchases.full[!is.na(projection_factor)]
rm(purchases.full)

## Create Necessary variables -----------------------

## Variables to restrict sample by taxability of product
# type or taxability
purchases.sample[, expenditures_taxable := ifelse(taxability == 1, expenditures, NA)]
purchases.sample[, expenditures_non_taxable := ifelse(taxability == 0, expenditures, NA)]
purchases.sample[, expenditures_unknown := ifelse(taxability == 2, expenditures, NA)]

## shares
purchases.sample[, share_taxable := expenditures_taxable/sum_total_exp_quarter]
purchases.sample[, share_non_taxable := expenditures_non_taxable/sum_total_exp_quarter]
purchases.sample[, share_unknown := expenditures_unknown/sum_total_exp_quarter]
purchases.sample[, share_same3 := expenditures_same3/sum_total_exp_quarter]
purchases.sample[, share_diff3 := expenditures_diff3/sum_total_exp_quarter]


# Delete variables for efficiency
purchases.sample <- purchases.sample[, -c("taxability", "expenditures")]

## Logarithms of variables
# type or taxability
purchases.sample <- purchases.sample[, ln_expenditure_taxable := log(expenditures_taxable)]
purchases.sample$ln_expenditure_taxable[is.infinite(purchases.sample$ln_expenditure_taxable)] <- NA
purchases.sample <- purchases.sample[, ln_expenditure_non_taxable := log(expenditures_non_taxable)]
purchases.sample$ln_expenditure_non_taxable[is.infinite(purchases.sample$ln_expenditure_non_taxable)] <- NA
purchases.sample <- purchases.sample[, ln_expenditure_unknown := log(expenditures_unknown)]
purchases.sample$ln_expenditure_unknown[is.infinite(purchases.sample$ln_expenditure_unknown)] <- NA
purchases.sample <- purchases.sample[, ln_expenditure_same3 := log(expenditures_same3)]
purchases.sample$ln_expenditure_same3[is.infinite(purchases.sample$ln_expenditure_same3)] <- NA
purchases.sample <- purchases.sample[, ln_expenditure_diff3 := log(expenditures_diff3)]
purchases.sample$ln_expenditure_diff3[is.infinite(purchases.sample$ln_expenditure_diff3)] <- NA

# Delete variables for efficiency
purchases.sample <- purchases.sample[, -c("expenditures_non_taxable", "expenditures_taxable", "expenditures_unknown", "expenditures_same3",
                      "expenditures_diff3")]


# shares type or taxability
purchases.sample <- purchases.sample[, ln_share_taxable := log(share_taxable)]
purchases.sample$ln_share_taxable[is.infinite(purchases.sample$ln_share_taxable)] <- NA
purchases.sample <- purchases.sample[, ln_share_non_taxable := log(share_non_taxable)]
purchases.sample$ln_share_non_taxable[is.infinite(purchases.sample$ln_share_non_taxable)] <- NA
purchases.sample <- purchases.sample[, ln_share_unknown := log(share_unknown)]
purchases.sample$ln_share_unknown[is.infinite(purchases.sample$ln_share_unknown)] <- NA
purchases.sample <- purchases.sample[, ln_share_same3 := log(share_same3)]
purchases.sample$ln_share_same3[is.infinite(purchases.sample$ln_share_same3)] <- NA
purchases.sample <- purchases.sample[, ln_share_diff3 := log(share_diff3)]
purchases.sample$ln_share_diff3[is.infinite(purchases.sample$ln_share_diff3)] <- NA

# Delete variables for efficiency
purchases.sample <- purchases.sample[, -c("share_taxable", "share_taxable", "share_unknown", "share_same3",
                      "share_diff3")]


# Time
purchases.sample[, cal_time := 4 * year + quarter]

# Module X Time FE
purchases.sample[, module_by_time := .GRP, by = .(product_module_code, cal_time)]

# Module X Region X Time FE
purchases.sample[, module_by_region_time := .GRP, by = .(product_module_code, region_code, cal_time)]


## take first differences of outcomes
setkey(purchases.sample, household_code, product_module_code, year, quarter)
purchases.sample <- purchases.sample[order(household_code, product_module_code, cal_time),] ##Sort on hh by year-quarter (in ascending order)


# type or taxability logs
purchases.sample[, D.ln_expenditure_taxable := ln_expenditure_taxable - shift(ln_expenditure_taxable, n=1, type="lag"),
                 by = .(product_module_code, household_code)]
purchases.sample[, D.ln_expenditure_non_taxable := ln_expenditure_non_taxable - shift(ln_expenditure_non_taxable, n=1, type="lag"),
                 by = .(product_module_code, household_code)]
purchases.sample[, D.ln_expenditure_unknown := ln_expenditure_unknown - shift(ln_expenditure_unknown, n=1, type="lag"),
                 by = .(product_module_code, household_code)]
purchases.sample[, D.ln_expenditure_diff3 := ln_expenditure_diff3 - shift(ln_expenditure_diff3, n=1, type="lag"),
                 by = .(product_module_code, household_code)]
purchases.sample[, D.ln_expenditure_same3 := ln_expenditure_same3 - shift(ln_expenditure_same3, n=1, type="lag"),
                 by = .(product_module_code, household_code)]

# Delete variables for efficiency
purchases.sample <- purchases.sample[, -c("ln_expenditure_taxable", "ln_expenditure_non_taxable", 
                                          "ln_expenditure_unknown", "ln_expenditure_diff3", 
                                          "ln_expenditure_same3")]


setkey(purchases.sample, household_code, product_module_code, year, quarter)
purchases.sample <- purchases.sample[order(household_code, product_module_code, cal_time),] ##Sort on hh by year-quarter (in ascending order)

# type or taxability shares logs
purchases.sample[, D.ln_share_taxable := ln_share_taxable - shift(ln_share_taxable, n=1, type="lag"),
                 by = .(product_module_code, household_code)]
purchases.sample[, D.ln_share_non_taxable := ln_share_non_taxable - shift(ln_share_non_taxable, n=1, type="lag"),
                 by = .(product_module_code, household_code)]
purchases.sample[, D.ln_share_unknown := ln_share_unknown - shift(ln_share_unknown, n=1, type="lag"),
                 by = .(product_module_code, household_code)]
purchases.sample[, D.ln_share_diff3 := ln_share_diff3 - shift(ln_share_diff3, n=1, type="lag"),
                 by = .(product_module_code, household_code)]
purchases.sample[, D.ln_share_same3 := ln_share_same3 - shift(ln_share_same3, n=1, type="lag"),
                 by = .(product_module_code, household_code)]

purchases.sample <- purchases.sample[, -c("ln_share_taxable", "ln_share_non_taxable", 
                                          "ln_share_unknown", "ln_share_diff3", 
                                          "ln_share_same3")]


# Merge sales taxes and lags
purchases.sample <- merge(purchases.sample, household.quarter,
  by = c("household_code", "year", "quarter"),
  all.x = T
)

# Restrict data to interest window
purchases.sample <- purchases.sample[between(year, 2008, 2014)]
purchases.sample <- purchases.sample[ year >= 2009 | (year == 2008 & quarter >= 2)] ## First quarter of 2008, the difference was imputed not real data - so we drop it


## Estimations: Expenditure on type of module --------
output.results.file <- "../../../../../home/slacouture/HMS/HH_mod_quarter_leadslags_cumulative.csv"

outcomes <- c("D.ln_expenditure_taxable", "D.ln_expenditure_non_taxable", "D.ln_expenditure_unknown",
              "D.ln_expenditure_diff3", "D.ln_expenditure_same3", "D.ln_share_taxable",
              "D.ln_share_non_taxable", "D.ln_share_unknown", "D.ln_share_same3", "D.ln_share_diff3")

FE_opts <- c("module_by_region_time", "module_by_time")


formula_lags <- paste0("L", 1:8, ".D.ln_sales_tax", collapse = "+")
formula_leads <- paste0("F", 1:8, ".D.ln_sales_tax", collapse = "+")
formula_RHS <- paste0("D.ln_sales_tax + ", formula_lags, "+", formula_leads)

## for linear hypothesis tests
lead.vars <- paste(paste0("F", 8:1, ".D.ln_sales_tax"), collapse = " + ")
lag.vars <- paste(paste0("L", 8:1, ".D.ln_sales_tax"), collapse = " + ")
lead.lp.restr <- paste(lead.vars, "= 0")
lag.lp.restr <- paste(lag.vars, "+ D.ln_sales_tax = 0")
total.lp.restr <- paste(lag.vars, "+", lead.vars, "+ D.ln_sales_tax = 0")


LRdiff_res <- data.table(NULL)
for (FE in FE_opts) {
  for (Y in outcomes) {
  
    ## Raw outcomes
    formula1 <- as.formula(paste0(
      Y, "~", formula_RHS, "|", FE, "| 0 | household_code"
    ))
    flog.info("Estimating with %s as outcome with %s FE.", Y, FE)
    res1 <- felm(formula = formula1, data = purchases.sample,
                 weights = purchases.sample$projection_factor)
    flog.info("Finished estimating with %s as outcome and  %s FE.", Y, FE)
    
    ## attach results
    flog.info("Writing results...")
    res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
    res1.dt[, outcome := Y]
    res1.dt[, spec := FE]
    res1.dt[, Rsq := summary(res1)$r.squared]
    res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
    res1.dt[, N.obs := nrow(purchases.sample[!is.na(get(Y))])]
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
      spec = FE,
      Rsq = summary(res1)$r.squared,
      adj.Rsq = summary(res1)$adj.r.squared)
      N.obs = nrow(purchases.sample[!is.na(get(Y))])
    LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)
    
    ##### Add the cumulative effect at each lead/lag (relative to -1)
    cumul.lead1.est <- 0
    cumul.lead1.se <- NA
    cumul.lead1.pval <- NA
    
    #cumul.lead2.est is just equal to minus the change between -2 and -1
    cumul.lead2.est <- - coef(summary(res1))[ "F1.D.ln_sales_tax", "Estimate"]
    cumul.lead2.se <- coef(summary(res1))[ "F1.D.ln_sales_tax", "Cluster s.e."]
    cumul.lead2.pval <- coef(summary(res1))[ "F1.D.ln_sales_tax", "Pr(>|t|)"]
    
    ##LEADS
    for(j in 3:9) {
      
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
    ## On Impact --> Effect = coefficient on D.ln_sales_tax
    cumul.lag0.est <- coef(summary(res1))[ "D.ln_sales_tax", "Estimate"]
    cumul.lag0.se <- coef(summary(res1))[ "D.ln_sales_tax", "Cluster s.e."]
    cumul.lag0.pval <- coef(summary(res1))[ "D.ln_sales_tax", "Pr(>|t|)"]
    
    for(j in 1:8) {
      
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
      rn = c("cumul.lead8.D.ln_sales_tax", "cumul.lead7.D.ln_sales_tax", "cumul.lead6.D.ln_sales_tax", "cumul.lead5.D.ln_sales_tax", "cumul.lead4.D.ln_sales_tax", "cumul.lead3.D.ln_sales_tax", "cumul.lead2.D.ln_sales_tax", "cumul.lead1.D.ln_sales_tax", 
             "cumul.lag0.D.ln_sales_tax", "cumul.lag1.D.ln_sales_tax", "cumul.lag2.D.ln_sales_tax", "cumul.lag3.D.ln_sales_tax", "cumul.lag4.D.ln_sales_tax", "cumul.lag5.D.ln_sales_tax", "cumul.lag6.D.ln_sales_tax", "cumul.lag7.D.ln_sales_tax", "cumul.lag8.D.ln_sales_tax"),
      Estimate = c(cumul.lead8.est, cumul.lead7.est, cumul.lead6.est, cumul.lead5.est, cumul.lead4.est, cumul.lead3.est, cumul.lead2.est, cumul.lead1.est, 
                   cumul.lag0.est, cumul.lag1.est, cumul.lag2.est, cumul.lag3.est, cumul.lag4.est, cumul.lag5.est, cumul.lag6.est, cumul.lag7.est, cumul.lag8.est),
      `Cluster s.e.` = c(cumul.lead8.se, cumul.lead7.se, cumul.lead6.se, cumul.lead5.se, cumul.lead4.se, cumul.lead3.se, cumul.lead2.se, cumul.lead1.se, 
                       cumul.lag0.se, cumul.lag1.se, cumul.lag2.se, cumul.lag3.se, cumul.lag4.se, cumul.lag5.se, cumul.lag6.se, cumul.lag7.se, cumul.lag8.se),
      `Pr(>|t|)` = c(cumul.lead8.pval, cumul.lead7.pval, cumul.lead6.pval, cumul.lead5.pval, cumul.lead4.pval, cumul.lead3.pval, cumul.lead2.pval, cumul.lead1.pval, 
                     cumul.lag0.pval, cumul.lag1.pval, cumul.lag2.pval, cumul.lag3.pval, cumul.lag4.pval, cumul.lag5.pval, cumul.lag6.pval, cumul.lag7.pval, cumul.lag8.pval),
      outcome = Y,
      spec = FE,
      Rsq = summary(res1)$r.squared,
      adj.Rsq = summary(res1)$adj.r.squared)
      N.obs = nrow(purchases.sample[!is.na(get(Y))])
    LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)
    
  }
}



                            ######### Type X Taxability #########

## Open Data
purchases.full <- fread("cleaning/consumer_panel_q_hh_mod_2006-2016.csv")

purchases.full$time <- factor(with(purchases.full, interaction(year, quarter)))


## Constraining Data set for estimations ------------ 
# Keep only "projection no-magnet" households: 
purchases.full[, sum(is.na(projection_factor))]
purchases.sample <- purchases.full[!is.na(projection_factor)]
rm(purchases.full)


## Create Necessary variables -----------------------

## Variables to restrict sample by taxability of product


# type X taxability
purchases.sample[, expenditures_taxable_same3 := ifelse(taxability == 1, expenditures_same3, NA)]
purchases.sample[, expenditures_non_taxable_same3 := ifelse(taxability == 0, expenditures_same3, NA)]
purchases.sample[, expenditures_unknown_same3 := ifelse(taxability == 2, expenditures_same3, NA)]

purchases.sample[, expenditures_taxable_diff3 := ifelse(taxability == 1, expenditures_diff3, NA)]
purchases.sample[, expenditures_non_taxable_diff3 := ifelse(taxability == 0, expenditures_diff3, NA)]
purchases.sample[, expenditures_unknown_diff3 := ifelse(taxability == 2, expenditures_diff3, NA)]

## shares

purchases.sample[, share_taxable_same3 := expenditures_taxable_same3/sum_total_exp_quarter]
purchases.sample[, share_taxable_diff3 := expenditures_taxable_diff3/sum_total_exp_quarter]
purchases.sample[, share_non_taxable_same3 := expenditures_non_taxable_same3/sum_total_exp_quarter]
purchases.sample[, share_non_taxable_diff3 := expenditures_non_taxable_diff3/sum_total_exp_quarter]
purchases.sample[, share_unknown_same3 := expenditures_unknown_same3/sum_total_exp_quarter]
purchases.sample[, share_unknown_diff3 := expenditures_unknown_diff3/sum_total_exp_quarter]

# Delete variables for efficiency
purchases.sample <- purchases.sample[, -c("taxability", "expenditures")]

## Logarithms of variables

# type x taxability
purchases.sample <- purchases.sample[, ln_expenditure_taxable_same3 := log(expenditures_taxable_same3)]
purchases.sample$ln_expenditure_taxable_same3[is.infinite(purchases.sample$ln_expenditure_taxable_same3)] <- NA
purchases.sample <- purchases.sample[, ln_expenditure_taxable_diff3 := log(expenditures_taxable_diff3 )]
purchases.sample$ln_expenditure_taxable_diff3[is.infinite(purchases.sample$ln_expenditure_taxable_diff3)] <- NA
purchases.sample <- purchases.sample[, ln_expenditure_non_taxable_same3 := log(expenditures_non_taxable_same3)]
purchases.sample$ln_expenditure_non_taxable_same3[is.infinite(purchases.sample$ln_expenditure_non_taxable_same3)] <- NA
purchases.sample <- purchases.sample[, ln_expenditure_non_taxable_diff3 := log(expenditures_non_taxable_diff3)]
purchases.sample$ln_expenditure_non_taxable_diff3[is.infinite(purchases.sample$ln_expenditure_non_taxable_diff3)] <- NA
purchases.sample <- purchases.sample[, ln_expenditure_unknown_same3 := log(expenditures_unknown_same3)]
purchases.sample$ln_expenditure_unknown_same3[is.infinite(purchases.sample$ln_expenditure_unknown_same3)] <- NA
purchases.sample <- purchases.sample[, ln_expenditure_unknown_diff3 := log(expenditures_unknown_diff3)]
purchases.sample$ln_expenditure_unknown_diff3[is.infinite(purchases.sample$ln_expenditure_unknown_diff3)] <- NA

# Delete variables for efficiency
purchases.sample <- purchases.sample[, -c("expenditures_taxable_same3", "expenditures_taxable_diff3", "expenditures_non_taxable_same3", 
                                          "expenditures_non_taxable_diff3", "expenditures_unknown_same3", "expenditures_unknown_diff3")]


# shares type x taxability
purchases.sample <- purchases.sample[, ln_share_taxable_same3 := log(share_taxable_same3)]
purchases.sample$ln_share_taxable_same3[is.infinite(purchases.sample$ln_share_taxable_same3)] <- NA
purchases.sample <- purchases.sample[, ln_share_taxable_diff3 := log(share_taxable_diff3)]
purchases.sample$ln_share_taxable_diff3[is.infinite(purchases.sample$ln_share_taxable_diff3)] <- NA
purchases.sample <- purchases.sample[, ln_share_non_taxable_same3 := log(share_non_taxable_same3)]
purchases.sample$ln_share_non_taxable_same3[is.infinite(purchases.sample$ln_share_non_taxable_same3)] <- NA
purchases.sample <- purchases.sample[, ln_share_non_taxable_diff3 := log(share_non_taxable_diff3)]
purchases.sample$ln_share_non_taxable_diff3[is.infinite(purchases.sample$ln_share_non_taxable_diff3)] <- NA
purchases.sample <- purchases.sample[, ln_share_unknown_same3 := log(share_unknown_same3)]
purchases.sample$ln_share_unknown_same3[is.infinite(purchases.sample$ln_share_unknown_same3)] <- NA
purchases.sample <- purchases.sample[, ln_share_unknown_diff3 := log(share_unknown_diff3)]
purchases.sample$ln_share_unknown_diff3[is.infinite(purchases.sample$ln_share_unknown_diff3)] <- NA

# Delete variables for efficiency
purchases.sample <- purchases.sample[, -c("share_taxable_same3", "share_taxable_diff3", "share_non_taxable_same3", 
                                          "share_non_taxable_diff3", "share_unknown_same3", "share_unknown_diff3")]


# Time
purchases.sample[, cal_time := 4 * year + quarter]

# Module X Time FE
purchases.sample[, module_by_time := .GRP, by = .(product_module_code, cal_time)]

# Module X Region X Time FE
purchases.sample[, module_by_region_time := .GRP, by = .(product_module_code, region_code, cal_time)]


## take first differences of outcomes
setkey(purchases.sample, household_code, product_module_code, year, quarter)
purchases.sample <- purchases.sample[order(household_code, product_module_code, cal_time),] ##Sort on hh by year-quarter (in ascending order)

# type x taxability logs
purchases.sample[, D.ln_expenditure_taxable_same3 := ln_expenditure_taxable_same3 - shift(ln_expenditure_taxable_same3, n=1, type="lag"),
                 by = .(product_module_code, household_code)]
purchases.sample[, D.ln_expenditure_taxable_diff3 := ln_expenditure_taxable_diff3 - shift(ln_expenditure_taxable_diff3, n=1, type="lag"),
                 by = .(product_module_code, household_code)]
purchases.sample[, D.ln_expenditure_non_taxable_same3 := ln_expenditure_non_taxable_same3 - shift(ln_expenditure_non_taxable_same3, n=1, type="lag"),
                 by = .(product_module_code, household_code)]
purchases.sample[, D.ln_expenditure_non_taxable_diff3 := ln_expenditure_non_taxable_diff3 - shift(ln_expenditure_non_taxable_diff3, n=1, type="lag"),
                 by = .(product_module_code, household_code)]
purchases.sample[, D.ln_expenditure_unknown_same3 := ln_expenditure_unknown_same3 - shift(ln_expenditure_unknown_same3, n=1, type="lag"),
                 by = .(product_module_code, household_code)]
purchases.sample[, D.ln_expenditure_unknown_diff3 := ln_expenditure_unknown_diff3 - shift(ln_expenditure_unknown_diff3, n=1, type="lag"),
                 by = .(product_module_code, household_code)]

purchases.sample <- purchases.sample[, -c("ln_expenditure_taxable_same3", "ln_expenditure_taxable_diff3", 
                                          "ln_expenditure_non_taxable_same3", "ln_expenditure_non_taxable_diff3", 
                                          "ln_expenditure_unknown_same3", "ln_expenditure_unknown_diff3")]

setkey(purchases.sample, household_code, product_module_code, year, quarter)
purchases.sample <- purchases.sample[order(household_code, product_module_code, cal_time),] ##Sort on hh by year-quarter (in ascending order)


# type x taxability shares logs
purchases.sample[, D.ln_share_taxable_same3 := ln_share_taxable_same3 - shift(ln_share_taxable_same3, n=1, type="lag"),
                 by = .(product_module_code, household_code)]
purchases.sample[, D.ln_share_taxable_diff3 := ln_share_taxable_diff3 - shift(ln_share_taxable_diff3, n=1, type="lag"),
                 by = .(product_module_code, household_code)]
purchases.sample[, D.ln_share_non_taxable_same3 := ln_share_non_taxable_same3 - shift(ln_share_non_taxable_same3, n=1, type="lag"),
                 by = .(product_module_code, household_code)]
purchases.sample[, D.ln_share_non_taxable_diff3 := ln_share_non_taxable_diff3 - shift(ln_share_non_taxable_diff3, n=1, type="lag"),
                 by = .(product_module_code, household_code)]
purchases.sample[, D.ln_share_unknown_same3 := ln_share_unknown_same3 - shift(ln_share_unknown_same3, n=1, type="lag"),
                 by = .(product_module_code, household_code)]
purchases.sample[, D.ln_share_unknown_diff3 := ln_share_unknown_diff3 - shift(ln_share_unknown_diff3, n=1, type="lag"),
                 by = .(product_module_code, household_code)]

purchases.sample <- purchases.sample[, -c("ln_share_taxable_same3", "ln_share_taxable_diff3", 
                                          "ln_share_non_taxable_same3", "ln_share_non_taxable_diff3", 
                                          "ln_share_unknown_same3", "ln_share_unknown_diff3")]

# Merge sales taxes and lags
purchases.sample <- merge(purchases.sample, household.quarter,
                          by = c("household_code", "year", "quarter"),
                          all.x = T
)

# Restrict data to interest window
purchases.sample <- purchases.sample[between(year, 2008, 2014)]
purchases.sample <- purchases.sample[ year >= 2009 | (year == 2008 & quarter >= 2)] ## First quarter of 2008, the difference was imputed not real data - so we drop it


## Estimations: Expenditure on type of module --------
output.results.file <- "../../../../../home/slacouture/HMS/HH_mod_quarter_leadslags_cumulative.csv"

outcomes_t <- c("D.ln_expenditure_taxable_same3", "D.ln_expenditure_taxable_diff3", 
                "D.ln_expenditure_non_taxable_same3", "D.ln_expenditure_non_taxable_diff3",
                "D.ln_expenditure_unknown_same3", "D.ln_expenditure_unknown_diff3", 
                "D.ln_share_taxable_same3", "D.ln_share_taxable_diff3", 
                "D.ln_share_non_taxable_same3", "D.ln_share_non_taxable_diff3",
                "D.ln_share_unknown_same3", "D.ln_share_unknown_diff3")

FE_opts <- c("module_by_region_time", "module_by_time")


formula_lags <- paste0("L", 1:8, ".D.ln_sales_tax", collapse = "+")
formula_leads <- paste0("F", 1:8, ".D.ln_sales_tax", collapse = "+")
formula_RHS <- paste0("D.ln_sales_tax + ", formula_lags, "+", formula_leads)

## for linear hypothesis tests
lead.vars <- paste(paste0("F", 8:1, ".D.ln_sales_tax"), collapse = " + ")
lag.vars <- paste(paste0("L", 8:1, ".D.ln_sales_tax"), collapse = " + ")
lead.lp.restr <- paste(lead.vars, "= 0")
lag.lp.restr <- paste(lag.vars, "+ D.ln_sales_tax = 0")
total.lp.restr <- paste(lag.vars, "+", lead.vars, "+ D.ln_sales_tax = 0")


for (FE in FE_opts) {
  for (Y in outcomes_t) {
    
    ## Raw outcomes
    formula1 <- as.formula(paste0(
      Y, "~", formula_RHS, "|", FE, "| 0 | household_code"
    ))
    flog.info("Estimating with %s as outcome with %s FE.", Y, FE)
    res1 <- felm(formula = formula1, data = purchases.sample,
                 weights = purchases.sample$projection_factor)
    flog.info("Finished estimating with %s as outcome and  %s FE.", Y, FE)
    
    ## attach results
    flog.info("Writing results...")
    res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
    res1.dt[, outcome := Y]
    res1.dt[, spec := FE]
    res1.dt[, Rsq := summary(res1)$r.squared]
    res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
    res1.dt[, N.obs := nrow(purchases.sample[!is.na(get(Y))])]
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
      spec = FE,
      Rsq = summary(res1)$r.squared,
      adj.Rsq = summary(res1)$adj.r.squared)
    N.obs = nrow(purchases.sample[!is.na(get(Y))])
    LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)
    
    ##### Add the cumulative effect at each lead/lag (relative to -1)
    cumul.lead1.est <- 0
    cumul.lead1.se <- NA
    cumul.lead1.pval <- NA
    
    #cumul.lead2.est is just equal to minus the change between -2 and -1
    cumul.lead2.est <- - coef(summary(res1))[ "F1.D.ln_sales_tax", "Estimate"]
    cumul.lead2.se <- coef(summary(res1))[ "F1.D.ln_sales_tax", "Cluster s.e."]
    cumul.lead2.pval <- coef(summary(res1))[ "F1.D.ln_sales_tax", "Pr(>|t|)"]
    
    ##LEADS
    for(j in 3:9) {
      
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
    ## On Impact --> Effect = coefficient on D.ln_sales_tax
    cumul.lag0.est <- coef(summary(res1))[ "D.ln_sales_tax", "Estimate"]
    cumul.lag0.se <- coef(summary(res1))[ "D.ln_sales_tax", "Cluster s.e."]
    cumul.lag0.pval <- coef(summary(res1))[ "D.ln_sales_tax", "Pr(>|t|)"]
    
    for(j in 1:8) {
      
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
      rn = c("cumul.lead8.D.ln_sales_tax", "cumul.lead7.D.ln_sales_tax", "cumul.lead6.D.ln_sales_tax", "cumul.lead5.D.ln_sales_tax", "cumul.lead4.D.ln_sales_tax", "cumul.lead3.D.ln_sales_tax", "cumul.lead2.D.ln_sales_tax", "cumul.lead1.D.ln_sales_tax", 
             "cumul.lag0.D.ln_sales_tax", "cumul.lag1.D.ln_sales_tax", "cumul.lag2.D.ln_sales_tax", "cumul.lag3.D.ln_sales_tax", "cumul.lag4.D.ln_sales_tax", "cumul.lag5.D.ln_sales_tax", "cumul.lag6.D.ln_sales_tax", "cumul.lag7.D.ln_sales_tax", "cumul.lag8.D.ln_sales_tax"),
      Estimate = c(cumul.lead8.est, cumul.lead7.est, cumul.lead6.est, cumul.lead5.est, cumul.lead4.est, cumul.lead3.est, cumul.lead2.est, cumul.lead1.est, 
                   cumul.lag0.est, cumul.lag1.est, cumul.lag2.est, cumul.lag3.est, cumul.lag4.est, cumul.lag5.est, cumul.lag6.est, cumul.lag7.est, cumul.lag8.est),
      `Cluster s.e.` = c(cumul.lead8.se, cumul.lead7.se, cumul.lead6.se, cumul.lead5.se, cumul.lead4.se, cumul.lead3.se, cumul.lead2.se, cumul.lead1.se, 
                       cumul.lag0.se, cumul.lag1.se, cumul.lag2.se, cumul.lag3.se, cumul.lag4.se, cumul.lag5.se, cumul.lag6.se, cumul.lag7.se, cumul.lag8.se),
      `Pr(>|t|)` = c(cumul.lead8.pval, cumul.lead7.pval, cumul.lead6.pval, cumul.lead5.pval, cumul.lead4.pval, cumul.lead3.pval, cumul.lead2.pval, cumul.lead1.pval, 
                     cumul.lag0.pval, cumul.lag1.pval, cumul.lag2.pval, cumul.lag3.pval, cumul.lag4.pval, cumul.lag5.pval, cumul.lag6.pval, cumul.lag7.pval, cumul.lag8.pval),
      outcome = Y,
      spec = FE,
      Rsq = summary(res1)$r.squared,
      adj.Rsq = summary(res1)$adj.r.squared)
    N.obs = nrow(purchases.sample[!is.na(get(Y))])
    LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)
    
  }
}

## summary values --------------------------------------------------------------
LRdiff_res$N_hholds <- length(unique(purchases.sample$household_code))
LRdiff_res$N_counties <- uniqueN(purchases.sample, by = c("fips_state_code", "fips_county_code"))
LRdiff_res$N_years <- uniqueN(purchases.sample, by = c("year"))

fwrite(LRdiff_res, output.results.file)

