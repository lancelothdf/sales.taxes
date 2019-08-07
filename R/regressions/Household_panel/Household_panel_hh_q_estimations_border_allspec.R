## Sales taxes Project. Household Panel
# Running Basic Estimations on the new panel (household aggregate x month)
# Estimation on household living in border counties (are HH reacting by buying at a different state?)
# Author: Santiago Lacouture


library(data.table)
library(lfe)
library(futile.logger)
library(AER)
library(multcomp)
library(psych)
library(ggplot2)

setwd("/project2/igaarder/Data/Nielsen/Household_panel")

## Open Data
border.path <- "../../border_counties.csv"
purchases.full <- fread("cleaning/consumer_panel_q_hh_2006-2016.csv")

## Constraining Data set for estimations ------------ 
# Drop "magnet" households: 
purchases.full[, sum(is.na(projection_factor))]
# 2016154 obs
purchases.nomagnet <- purchases.full[!is.na(projection_factor)]

## Identifying Households living in counties in a state border ----------
# Minor change to merge
setnames(purchases.nomagnet, old = c("fips_state_code", "fips_county_code"), new = c("fips_state", "fips_county"))

list.counties <- data.frame(unique(purchases.nomagnet[,c('fips_state','fips_county')]))
border.counties <- fread(border.path)

border.counties[ , fips_state := floor(fips_county/1000)]
border.counties[, fips_county := fips_county - fips_state*1000]


setkey(border.counties, bordindx)
border.counties <- border.counties[, c("fips_state", "fips_county", "bordindx")]

# Identify county pairs from different states
border.counties <- border.counties[, list( different := fips_state - mean(fips_state)), by = bordindx]
border.counties <- border.counties[ different != 0 ]
border.counties <- border.counties[, .(fips_state, fips_county)]
border.counties <- unique(border.counties, by=c("fips_state", "fips_county"))

#Only keep counties that are in the Household panel
border.counties <- merge(list.counties, border.counties, by = c("fips_state", "fips_county"), all.x = T)

# Merge purchases.nomagnet with the identified counties
purchases.sample <- merge(purchases.nomagnet, purchases.borderhh, by = c("fips_state", "fips_county", "id"))

## Preparing data set for estimations ----------
# Generate some region by time and time FE
purchases.sample[, time := .GRP, by = .(year, quarter)]
purchases.sample[, region_by_time := .GRP, by = .(quarter, year, region_code)]

# Create variables
purchases.sample <- purchases.sample[, ln_expenditure_taxable := log(expenditure_taxable)]
purchases.sample$ln_expenditure_taxable[is.infinite(purchases.sample$ln_expenditure_taxable)] <- NA
purchases.sample <- purchases.sample[, ln_expenditure_non_taxable := log(expenditure_non_taxable)]
purchases.sample$ln_expenditure_non_taxable[is.infinite(purchases.sample$ln_expenditure_non_taxable)] <- NA
purchases.sample <- purchases.sample[, ln_expenditure_unknown := log(expenditure_unknown)]
purchases.sample$ln_expenditure_unknown[is.infinite(purchases.sample$ln_expenditure_unknown)] <- NA
purchases.sample <- purchases.sample[, ln_expenditure_same3 := log(expenditure_same3)]
purchases.sample$ln_expenditure_same3[is.infinite(purchases.sample$ln_expenditure_same3)] <- NA
purchases.sample <- purchases.sample[, ln_expenditure_diff3 := log(expenditure_diff3)]
purchases.sample$ln_expenditure_diff3[is.infinite(purchases.sample$ln_expenditure_diff3)] <- NA

## Shares
# type or taxability
purchases.sample[, share_taxable := expenditure_taxable/sum_total_exp_quarter]
purchases.sample[, share_non_taxable := expenditure_non_taxable/sum_total_exp_quarter]
purchases.sample[, share_unknown := expenditure_unknown/sum_total_exp_quarter]
purchases.sample[, share_same3 := expenditure_same3/sum_total_exp_quarter]
purchases.sample[, share_diff3 := expenditure_diff3/sum_total_exp_quarter]

# type x taxability
purchases.sample[, share_taxable_same3 := expenditures_same3_1/sum_total_exp_quarter]
purchases.sample[, share_taxable_diff3 := expenditures_diff3_1/sum_total_exp_quarter]
purchases.sample[, share_non_taxable_same3 := expenditures_same3_0/sum_total_exp_quarter]
purchases.sample[, share_non_taxable_diff3 := expenditures_diff3_0/sum_total_exp_quarter]
purchases.sample[, share_unknown_same3 := expenditures_same3_2/sum_total_exp_quarter]
purchases.sample[, share_unknown_diff3 := expenditures_diff3_2/sum_total_exp_quarter]

# type x taxability
purchases.sample <- purchases.sample[, ln_expenditure_taxable_same3 := log(expenditures_same3_1)]
purchases.sample$ln_expenditure_taxable_same3[is.infinite(purchases.sample$ln_expenditure_taxable_same3)] <- NA
purchases.sample <- purchases.sample[, ln_expenditure_taxable_diff3 := log(expenditures_diff3_1)]
purchases.sample$ln_expenditure_taxable_diff3[is.infinite(purchases.sample$ln_expenditure_taxable_diff3)] <- NA
purchases.sample <- purchases.sample[, ln_expenditure_non_taxable_same3 := log(expenditures_same3_0)]
purchases.sample$ln_expenditure_non_taxable_same3[is.infinite(purchases.sample$ln_expenditure_non_taxable_same3)] <- NA
purchases.sample <- purchases.sample[, ln_expenditure_non_taxable_diff3 := log(expenditures_diff3_0)]
purchases.sample$ln_expenditure_non_taxable_diff3[is.infinite(purchases.sample$ln_expenditure_non_taxable_diff3)] <- NA
purchases.sample <- purchases.sample[, ln_expenditure_unknown_same3 := log(expenditures_same3_2)]
purchases.sample$ln_expenditure_unknown_same3[is.infinite(purchases.sample$ln_expenditure_unknown_same3)] <- NA
purchases.sample <- purchases.sample[, ln_expenditure_unknown_diff3 := log(expenditures_diff3_2)]
purchases.sample$ln_expenditure_unknown_diff3[is.infinite(purchases.sample$ln_expenditure_unknown_diff3)] <- NA

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

# Calculated Time
purchases.sample[, cal_time := 4 * year + quarter]

# impute tax rates prior to 2008 and after 2014
purchases.sample[, ln_sales_tax := ifelse(year < 2008, ln_sales_tax[year == 2008 & quarter == 1], ln_sales_tax),
                 by = .(household_code)]
purchases.sample[, ln_sales_tax := ifelse(year > 2014, ln_sales_tax[year == 2014 & quarter == 4], ln_sales_tax),
                 by = .(household_code)]

## take first differences of outcomes and treatment
setkey(purchases.sample, household_code, year, quarter)
purchases.sample <- purchases.sample[order(household_code, cal_time),] ##Sort on hh by year-quarter (in ascending order)

# tax
purchases.sample[, D.ln_sales_tax := ln_sales_tax - shift(ln_sales_tax, n=1, type="lag"),
                 by = .(household_code)]

# type or taxability logs
purchases.sample[, D.ln_expenditure_taxable := ln_expenditure_taxable - shift(ln_expenditure_taxable, n=1, type="lag"),
                 by = .(household_code)]
purchases.sample[, D.ln_expenditure_non_taxable := ln_expenditure_non_taxable - shift(ln_expenditure_non_taxable, n=1, type="lag"),
                 by = .(household_code)]
purchases.sample[, D.ln_expenditure_unknown := ln_expenditure_unknown - shift(ln_expenditure_unknown, n=1, type="lag"),
                 by = .(household_code)]
purchases.sample[, D.ln_expenditure_diff3 := ln_expenditure_diff3 - shift(ln_expenditure_diff3, n=1, type="lag"),
                 by = .(household_code)]
purchases.sample[, D.ln_expenditure_same3 := ln_expenditure_same3 - shift(ln_expenditure_same3, n=1, type="lag"),
                 by = .(household_code)]

# type or taxability shares logs
purchases.sample[, D.ln_share_taxable := ln_share_taxable - shift(ln_share_taxable, n=1, type="lag"),
                 by = .(household_code)]
purchases.sample[, D.ln_share_non_taxable := ln_share_non_taxable - shift(ln_share_non_taxable, n=1, type="lag"),
                 by = .(household_code)]
purchases.sample[, D.ln_share_unknown := ln_share_unknown - shift(ln_share_unknown, n=1, type="lag"),
                 by = .(household_code)]
purchases.sample[, D.ln_share_diff3 := ln_share_diff3 - shift(ln_share_diff3, n=1, type="lag"),
                 by = .(household_code)]
purchases.sample[, D.ln_share_same3 := ln_share_same3 - shift(ln_share_same3, n=1, type="lag"),
                 by = .(household_code)]


# type x taxability logs
purchases.sample[, D.ln_expenditure_taxable_same3 := ln_expenditure_taxable_same3 - shift(ln_expenditure_taxable_same3, n=1, type="lag"),
                 by = .(household_code)]
purchases.sample[, D.ln_expenditure_taxable_diff3 := ln_expenditure_taxable_diff3 - shift(ln_expenditure_taxable_diff3, n=1, type="lag"),
                 by = .(household_code)]
purchases.sample[, D.ln_expenditure_non_taxable_same3 := ln_expenditure_non_taxable_same3 - shift(ln_expenditure_non_taxable_same3, n=1, type="lag"),
                 by = .(household_code)]
purchases.sample[, D.ln_expenditure_non_taxable_diff3 := ln_expenditure_non_taxable_diff3 - shift(ln_expenditure_non_taxable_diff3, n=1, type="lag"),
                 by = .(household_code)]
purchases.sample[, D.ln_expenditure_unknown_same3 := ln_expenditure_unknown_same3 - shift(ln_expenditure_unknown_same3, n=1, type="lag"),
                 by = .(household_code)]
purchases.sample[, D.ln_expenditure_unknown_diff3 := ln_expenditure_unknown_diff3 - shift(ln_expenditure_unknown_diff3, n=1, type="lag"),
                 by = .(household_code)]


# type x taxability shares logs
purchases.sample[, D.ln_share_taxable_same3 := ln_share_taxable_same3 - shift(ln_share_taxable_same3, n=1, type="lag"),
                 by = .(household_code)]
purchases.sample[, D.ln_share_taxable_diff3 := ln_share_taxable_diff3 - shift(ln_share_taxable_diff3, n=1, type="lag"),
                 by = .(household_code)]
purchases.sample[, D.ln_share_non_taxable_same3 := ln_share_non_taxable_same3 - shift(ln_share_non_taxable_same3, n=1, type="lag"),
                 by = .(household_code)]
purchases.sample[, D.ln_share_non_taxable_diff3 := ln_share_non_taxable_diff3 - shift(ln_share_non_taxable_diff3, n=1, type="lag"),
                 by = .(household_code)]
purchases.sample[, D.ln_share_unknown_same3 := ln_share_unknown_same3 - shift(ln_share_unknown_same3, n=1, type="lag"),
                 by = .(household_code)]
purchases.sample[, D.ln_share_unknown_diff3 := ln_share_unknown_diff3 - shift(ln_share_unknown_diff3, n=1, type="lag"),
                 by = .(household_code)]


## generate lags and leads of ln_sales_tax
for (lag.val in 1:8) {
  lag.X <- paste0("L", lag.val, ".D.ln_sales_tax")
  purchases.sample[, (lag.X) := shift(D.ln_sales_tax, n=lag.val, type="lag"),
                   by = .(household_code)]
  
  lead.X <- paste0("F", lag.val, ".D.ln_sales_tax")
  purchases.sample[, (lead.X) := shift(D.ln_sales_tax, n=lag.val, type="lead"),
                   by = .(household_code)]
}
# Restrict data to interest window
purchases.sample <- purchases.sample[between(year, 2008, 2014)]
purchases.sample <- purchases.sample[ year >= 2009 | (year == 2008 & quarter >= 2)] ## First quarter of 2008, the difference was imputed not real data - so we drop it

## Estimations: Twoway FE --------

output.results.file <- "../../../../../home/slacouture/HMS/HH_quarter_twowayFE_border.csv"

outcomes <- c("ln_expenditure_taxable", "ln_expenditure_non_taxable", "ln_expenditure_unknown",
              "ln_expenditure_diff3", "ln_expenditure_same3", "ln_share_taxable",
              "ln_share_non_taxable", "ln_share_unknown", "ln_share_same3", "ln_share_diff3")
outcomes_t <- c("ln_expenditure_taxable_same3", "ln_expenditure_taxable_diff3", 
                "ln_expenditure_non_taxable_same3", "ln_expenditure_non_taxable_diff3",
                "ln_expenditure_unknown_same3", "ln_expenditure_unknown_diff3", 
                "ln_share_taxable_same3", "ln_share_taxable_diff3", 
                "ln_share_non_taxable_same3", "ln_share_non_taxable_diff3",
                "ln_share_unknown_same3", "ln_share_unknown_diff3")

FE_opts <- c("region_by_time", "time")

LRdiff_res <- data.table(NULL)
for (Y in c(outcomes, outcomes_t)) {
  for (FE in FE_opts) {
    formula1 <- as.formula(paste0(
      Y, "~ ln_sales_tax | ", FE, "+ household_code | 0 | household_code"
    ))
    flog.info("Estimating with %s as outcome and %s FE", Y, FE)
    res1 <- felm(formula = formula1, data = purchases.sample,
                 weights = purchases.sample$projection_factor)
    flog.info("Finished estimating with %s as outcomeand %s FE.", Y, FE)
    
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
    
  }
}
# summary values
LRdiff_res$N_hholds <- length(unique(purchases.sample$household_code))
LRdiff_res$N_counties <- uniqueN(purchases.sample, by = c("fips_state", "fips_county"))
LRdiff_res$N_years <- uniqueN(purchases.sample, by = c("year"))
fwrite(LRdiff_res, output.results.file)




## Estimation Distributed lags --------
output.decriptives.file <- "../../../../../home/slacouture/HMS/HH_quarter_leadslags_describe_border.csv"
output.results.file <- "../../../../../home/slacouture/HMS/HH_quarter_distributed_lags_border.csv"

outcomes <- c("D.ln_expenditure_taxable", "D.ln_expenditure_non_taxable", "D.ln_expenditure_unknown",
              "D.ln_expenditure_diff3", "D.ln_expenditure_same3", "D.ln_share_taxable",
              "D.ln_share_non_taxable", "D.ln_share_unknown", "D.ln_share_same3", "D.ln_share_diff3")
outcomes_t <- c("D.ln_expenditure_taxable_same3", "D.ln_expenditure_taxable_diff3", 
                "D.ln_expenditure_non_taxable_same3", "D.ln_expenditure_non_taxable_diff3",
                "D.ln_expenditure_unknown_same3", "D.ln_expenditure_unknown_diff3", 
                "D.ln_share_taxable_same3", "D.ln_share_taxable_diff3", 
                "D.ln_share_non_taxable_same3", "D.ln_share_non_taxable_diff3",
                "D.ln_share_unknown_same3", "D.ln_share_unknown_diff3")

FE_opts <- c("region_by_time", "time")

formula_lags <- paste0("L", 1:8, ".D.ln_sales_tax", collapse = "+")
formula_leads <- paste0("F", 1:8, ".D.ln_sales_tax", collapse = "+")
formula_RHS <- paste0("D.ln_sales_tax + ", formula_lags, "+", formula_leads)

## for linear hypothesis tests
lead.vars <- paste(paste0("F", 8:1, ".D.ln_sales_tax"), collapse = " + ")
lag.vars <- paste(paste0("L", 8:1, ".D.ln_sales_tax"), collapse = " + ")
lead.lp.restr <- paste(lead.vars, "= 0")
lag.lp.restr <- paste(lag.vars, "+ D.ln_sales_tax = 0")
total.lp.restr <- paste(lag.vars, "+", lead.vars, "+ D.ln_sales_tax = 0")

## Run basic descriptives
descriptives <- describe(purchases.sample[, .(D.ln_expenditure_taxable, D.ln_expenditure_non_taxable,
                                              D.ln_expenditure_unknown, D.ln_expenditure_diff3, 
                                              D.ln_expenditure_same3, D.ln_share_taxable,
                                              D.ln_share_non_taxable, D.ln_share_unknown, 
                                              D.ln_share_same3, D.ln_share_diff3,
                                              D.ln_expenditure_taxable_same3, D.ln_expenditure_taxable_diff3,
                                              D.ln_expenditure_non_taxable_same3, D.ln_expenditure_non_taxable_diff3,
                                              D.ln_expenditure_unknown_same3, D.ln_expenditure_unknown_diff3, 
                                              D.ln_share_taxable_same3, D.ln_share_taxable_diff3, 
                                              D.ln_share_non_taxable_same3, D.ln_share_non_taxable_diff3,
                                              D.ln_share_unknown_same3, D.ln_share_unknown_diff3)])
des.est.out  <- data.table(descriptives, keep.rownames=T)
fwrite(des.est.out, output.decriptives.file)


## Run Estimations ------

LRdiff_res <- data.table(NULL)
for (FE in FE_opts) {
  for (Y in c(outcomes, outcomes_t)) {
    
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

# summary values
LRdiff_res$N_hholds <- length(unique(purchases.sample$household_code))
LRdiff_res$N_counties <- uniqueN(purchases.sample, by = c("fips_state", "fips_county"))
LRdiff_res$N_years <- uniqueN(purchases.sample, by = c("year"))

fwrite(LRdiff_res, output.results.file)