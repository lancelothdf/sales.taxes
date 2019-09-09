## Sales taxes Project. Household Panel
# Running distributed lag regression in differences on the new panel (household x group x quarter) on all 
# purchases (for which taxability is identified). In this version we use a sales weight and
# run against both effective and statutory tax rates
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

## Open Data
purchases.sample <- fread("cleaning/consumer_panel_q_hh_group_2006-2016.csv")


# Time
purchases.sample[, cal_time := 4 * year + quarter]

# FE
purchases.sample[, region_by_group_by_time := .GRP, by = .(region_code, product_group_code, year, quarter)]
purchases.sample[, group_by_time := .GRP, by = .(product_group_code, year, quarter)]

# impute tax rates prior to 2008 and after 2014. Also for statutory
purchases.sample[, ln_sales_tax := ifelse(year < 2008, ln_sales_tax[year == 2008 & quarter == 1], ln_sales_tax),
                 by = .(household_code, product_group_code)]
purchases.sample[, ln_sales_tax := ifelse(year > 2014, ln_sales_tax[year == 2014 & quarter == 4], ln_sales_tax),
                 by = .(household_code, product_group_code)]

purchases.sample[, ln_staturtory_sales_tax := ifelse(year < 2008, ln_staturtory_sales_tax[year == 2008 & quarter == 1], ln_staturtory_sales_tax),
                 by = .(household_code, product_group_code)]
purchases.sample[, ln_staturtory_sales_tax := ifelse(year > 2014, ln_staturtory_sales_tax[year == 2014 & quarter == 4], ln_staturtory_sales_tax),
                 by = .(household_code, product_group_code)]

## take first differences of outcomes and treatment
purchases.sample <- purchases.sample[order(household_code, product_group_code, cal_time),] ##Sort on hh by year-quarter (in ascending order)

# tax
purchases.sample[, D.ln_sales_tax := ln_sales_tax - shift(ln_sales_tax, n=1, type="lag"),
                 by = .(household_code, product_group_code)]

purchases.sample[, D.ln_statutory_sales_tax := ln_staturtory_sales_tax - shift(ln_staturtory_sales_tax, n=1, type="lag"),
                 by = .(household_code, product_group_code)]


# expenditure
purchases.sample[, D.ln_expenditures := ln_expenditures - shift(ln_expenditures, n=1, type="lag"),
                 by = .(household_code, product_group_code)]
purchases.sample[, D.ln_expenditures_taxable := ln_expenditures_taxable - shift(ln_expenditures_taxable, n=1, type="lag"),
                 by = .(household_code, product_group_code)]
purchases.sample[, D.ln_expenditures_non_taxable := ln_expenditures_non_taxable - shift(ln_expenditures_non_taxable, n=1, type="lag"),
                 by = .(household_code, product_group_code)]

# share
purchases.sample[, D.ln_share := ln_share - shift(ln_share, n=1, type="lag"),
                 by = .(household_code, product_group_code)]
purchases.sample[, D.ln_share_taxable := ln_share_taxable - shift(ln_share_taxable, n=1, type="lag"),
                 by = .(household_code, product_group_code)]
purchases.sample[, D.ln_share_non_taxable := ln_share_non_taxable - shift(ln_share_non_taxable, n=1, type="lag"),
                 by = .(household_code, product_group_code)]



## generate lags and leads of ln_sales_tax and ln_statutory_sales_tax
for (lag.val in 1:8) {
  lag.X <- paste0("L", lag.val, ".D.ln_sales_tax")
  purchases.sample[, (lag.X) := shift(D.ln_sales_tax, n=lag.val, type="lag"),
                   by = .(household_code, product_group_code)]

  lead.X <- paste0("F", lag.val, ".D.ln_sales_tax")
  purchases.sample[, (lead.X) := shift(D.ln_sales_tax, n=lag.val, type="lead"),
                   by = .(household_code, product_group_code)]
}

for (lag.val in 1:8) {
  lag.X <- paste0("L", lag.val, ".D.ln_statutory_sales_tax")
  purchases.sample[, (lag.X) := shift(D.ln_statutory_sales_tax, n=lag.val, type="lag"),
                   by = .(household_code, product_group_code)]
  
  lead.X <- paste0("F", lag.val, ".D.ln_statutory_sales_tax")
  purchases.sample[, (lead.X) := shift(D.ln_statutory_sales_tax, n=lag.val, type="lead"),
                   by = .(household_code, product_group_code)]
}


# Restrict data to interest window
purchases.sample <- purchases.sample[between(year, 2008, 2014)]
purchases.sample <- purchases.sample[ year >= 2009 | (year == 2008 & quarter >= 2)] ## First quarter of 2008, the difference was imputed not real data - so we drop it

# Compute sales weight: by quarter, how large are purchases in each group in this sample?
base.weights <- purchases.sample[year == 2008 & quarter ==1, list(sales.weight = sum(expenditures, na.rm = T)), by = .(product_group_code)]
base.weights[, sales.weight := sales.weight / sum(sales.weight, na.rm = T)]

purchases.sample <- merge(purchases.sample, base.weights, by = "product_group_code")

# Build new weight as the prodcut of both household and group weights
purchases.sample[, projection_factor := projection_factor*base.weights]

# Drop observations without weights at the end
purchases.sample <- purchases.sample[!is.na(projection_factor)]


## Estimation Set up --------
output.results.file <- "../../../../../home/slacouture/HMS/HH_group_quarter_distributed_lags_weightedbase.csv"

outcomes <- c("D.ln_expenditures", "D.ln_share", "D.ln_expenditures_taxable", "D.ln_expenditures_non_taxable",
              "D.ln_share_taxable",  "D.ln_share_non_taxable")

FE_opts <- c("region_by_group_by_time", "group_by_time")

formula_lags <- paste0("L", 1:8, ".D.ln_sales_tax", collapse = "+")
formula_leads <- paste0("F", 1:8, ".D.ln_sales_tax", collapse = "+")
formula_RHS <- paste0("D.ln_sales_tax + ", formula_lags, "+", formula_leads)

## for linear hypothesis tests
lead.vars <- paste(paste0("F", 8:1, ".D.ln_sales_tax"), collapse = " + ")
lag.vars <- paste(paste0("L", 8:1, ".D.ln_sales_tax"), collapse = " + ")
lead.lp.restr <- paste(lead.vars, "= 0")
lag.lp.restr <- paste(lag.vars, "+ D.ln_sales_tax = 0")
total.lp.restr <- paste(lag.vars, "+", lead.vars, "+ D.ln_sales_tax = 0")

## Run Estimations ------

LRdiff_res <- data.table(NULL)
for (FE in FE_opts) {
  for (Y in outcomes) {

    ## Raw outcomes
    formula1 <- as.formula(paste0(
      Y, "~", formula_RHS, "|", FE, "| 0 | household_code"
    ))
    flog.info("Estimating with %s as outcome and %s FE.", Y, FE)
    res1 <- felm(formula = formula1, data = purchases.sample,
                 weights = purchases.sample$projection_factor)
    flog.info("Finished estimating with %s as outcome and %s FE.", Y, FE)


    ## attach results
    flog.info("Writing results...")
    res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
    res1.dt[, outcome := Y]
    res1.dt[, spec := FE]
    res1.dt[, Rsq := summary(res1)$r.squared]
    res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
    res1.dt[, N.obs := nrow(purchases.sample[!is.na(get(Y))])]
    res1.dt[, N_hholds := uniqueN(purchases.sample[!is.na(get(Y))], by = c("household_code"))]
    res1.dt[, N_groups := uniqueN(purchases.sample[!is.na(get(Y))], by = c("product_group_code"))]
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
      adj.Rsq = summary(res1)$adj.r.squared,
      N.obs = nrow(purchases.sample[!is.na(get(Y))]),
      N_hholds = uniqueN(purchases.sample[!is.na(get(Y))], by = c("household_code")),
      N_groups = uniqueN(purchases.sample[!is.na(get(Y))], by = c("product_group_code"))
    )
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
      adj.Rsq = summary(res1)$adj.r.squared,
      N.obs = nrow(purchases.sample[!is.na(get(Y))]),
      N_hholds = uniqueN(purchases.sample[!is.na(get(Y))], by = c("household_code")),
      N_groups = uniqueN(purchases.sample[!is.na(get(Y))], by = c("product_group_code"))
    )
    LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)
  }
}

LRdiff_res$N_years <- uniqueN(purchases.sample, by = c("year"))

fwrite(LRdiff_res, output.results.file)

## Estimation Set up on statutory --------
output.results.file <- "../../../../../home/slacouture/HMS/HH_group_quarter_distributed_lags_weightedbase_statutory.csv"

outcomes <- c("D.ln_expenditures", "D.ln_share", "D.ln_expenditures_taxable", "D.ln_expenditures_non_taxable",
              "D.ln_share_taxable",  "D.ln_share_non_taxable")

FE_opts <- c("region_by_group_by_time", "group_by_time")

formula_lags <- paste0("L", 1:8, ".D.ln_statutory_sales_tax", collapse = "+")
formula_leads <- paste0("F", 1:8, ".D.ln_statutory_sales_tax", collapse = "+")
formula_RHS <- paste0("D.ln_statutory_sales_tax + ", formula_lags, "+", formula_leads)

## for linear hypothesis tests
lead.vars <- paste(paste0("F", 8:1, ".D.ln_statutory_sales_tax"), collapse = " + ")
lag.vars <- paste(paste0("L", 8:1, ".D.ln_statutory_sales_tax"), collapse = " + ")
lead.lp.restr <- paste(lead.vars, "= 0")
lag.lp.restr <- paste(lag.vars, "+ D.ln_statutory_sales_tax = 0")
total.lp.restr <- paste(lag.vars, "+", lead.vars, "+ D.ln_statutory_sales_tax = 0")



## Run Estimations against 'statutory' tax rate ------

LRdiff_res <- data.table(NULL)
for (FE in FE_opts) {
  for (Y in outcomes) {
    
    ## Raw outcomes
    formula1 <- as.formula(paste0(
      Y, "~", formula_RHS, "|", FE, "| 0 | household_code"
    ))
    flog.info("Estimating with %s as outcome and %s FE.", Y, FE)
    res1 <- felm(formula = formula1, data = purchases.sample,
                 weights = purchases.sample$projection_factor)
    flog.info("Finished estimating with %s as outcome and %s FE.", Y, FE)
    
    
    ## attach results
    flog.info("Writing results...")
    res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
    res1.dt[, outcome := Y]
    res1.dt[, spec := FE]
    res1.dt[, Rsq := summary(res1)$r.squared]
    res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
    res1.dt[, N.obs := nrow(purchases.sample[!is.na(get(Y))])]
    res1.dt[, N_hholds := uniqueN(purchases.sample[!is.na(get(Y))], by = c("household_code"))]
    res1.dt[, N_groups := uniqueN(purchases.sample[!is.na(get(Y))], by = c("product_group_code"))]
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
      rn = c("Pre.D.ln_statutory_sales_tax", "Post.D.ln_statutory_sales_tax", "All.D.ln_statutory_sales_tax"),
      Estimate = c(lead.test.est, lag.test.est, total.test.est),
      `Cluster s.e.` = c(lead.test.se, lag.test.se, total.test.se),
      `Pr(>|t|)` = c(lead.test.pval, lag.test.pval, total.test.pval),
      outcome = Y,
      spec = FE,
      Rsq = summary(res1)$r.squared,
      adj.Rsq = summary(res1)$adj.r.squared,
      N.obs = nrow(purchases.sample[!is.na(get(Y))]),
      N_hholds = uniqueN(purchases.sample[!is.na(get(Y))], by = c("household_code")),
      N_groups = uniqueN(purchases.sample[!is.na(get(Y))], by = c("product_group_code"))
    )
    LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)
    
    
    ##### Add the cumulative effect at each lead/lag (relative to -1)
    cumul.lead1.est <- 0
    cumul.lead1.se <- NA
    cumul.lead1.pval <- NA
    
    #cumul.lead2.est is just equal to minus the change between -2 and -1
    cumul.lead2.est <- - coef(summary(res1))[ "F1.D.ln_statutory_sales_tax", "Estimate"]
    cumul.lead2.se <- coef(summary(res1))[ "F1.D.ln_statutory_sales_tax", "Cluster s.e."]
    cumul.lead2.pval <- coef(summary(res1))[ "F1.D.ln_statutory_sales_tax", "Pr(>|t|)"]
    
    ##LEADS
    for(j in 3:9) {
      
      ## Create a name for estimate, se and pval of each lead
      cumul.test.est.name <- paste("cumul.lead", j, ".est", sep = "")
      cumul.test.se.name <- paste("cumul.lead", j, ".se", sep = "")
      cumul.test.pval.name <- paste("cumul.lead", j, ".pval", sep = "")
      
      ## Create the formula to compute cumulative estimate at each lead/lag
      cumul.test.form <- paste0("-", paste(paste0("F", (j-1):1, ".D.ln_statutory_sales_tax"), collapse = " - "))
      cumul.test.form <- paste(cumul.test.form, " = 0")
      
      ## Compute estimate and store in variables names
      cumul.test <- glht(res1, linfct = cumul.test.form)
      
      assign(cumul.test.est.name, coef(summary(cumul.test))[[1]])
      assign(cumul.test.se.name, sqrt(vcov(summary(cumul.test)))[[1]])
      assign(cumul.test.pval.name, 2*(1 - pnorm(abs(coef(summary(cumul.test))[[1]]/sqrt(vcov(summary(cumul.test)))[[1]]))))
    }
    
    
    ##LAGS
    ## On Impact --> Effect = coefficient on D.ln_sales_tax
    cumul.lag0.est <- coef(summary(res1))[ "D.ln_statutory_sales_tax", "Estimate"]
    cumul.lag0.se <- coef(summary(res1))[ "D.ln_statutory_sales_tax", "Cluster s.e."]
    cumul.lag0.pval <- coef(summary(res1))[ "D.ln_statutory_sales_tax", "Pr(>|t|)"]
    
    for(j in 1:8) {
      
      ## Create a name for estimate, se and pval of each lead
      cumul.test.est.name <- paste("cumul.lag", j, ".est", sep = "")
      cumul.test.se.name <- paste("cumul.lag", j, ".se", sep = "")
      cumul.test.pval.name <- paste("cumul.lag", j, ".pval", sep = "")
      
      ## Create the formula to compute cumulative estimate at each lead/lag
      cumul.test.form <- paste("D.ln_statutory_sales_tax + ", paste(paste0("L", 1:j, ".D.ln_statutory_sales_tax"), collapse = " + "), sep = "")
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
      adj.Rsq = summary(res1)$adj.r.squared,
      N.obs = nrow(purchases.sample[!is.na(get(Y))]),
      N_hholds = uniqueN(purchases.sample[!is.na(get(Y))], by = c("household_code")),
      N_groups = uniqueN(purchases.sample[!is.na(get(Y))], by = c("product_group_code"))
    )
    LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)
  }
}

LRdiff_res$N_years <- uniqueN(purchases.sample, by = c("year"))

fwrite(LRdiff_res, output.results.file)