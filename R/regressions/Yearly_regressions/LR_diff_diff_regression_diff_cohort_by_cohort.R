### In this R-file we explore a specification meant to estimate long-run pass-through and sales/quantity response to tax changes

library(data.table)
library(lfe)
library(futile.logger)
library(AER)
library(multcomp)


setwd("/project2/igaarder")

## useful filepaths ------------------------------------------------------------
FE_pindex_path <- "Data/Nielsen/Pindex_FE_yearly_all_years.csv"
output_yearly <- "Data/Nielsen/yearly_nielsen_data.csv"
pre_trend_data_path <- "Data/Nielsen/pre_trend_data_yearly.csv"
#taxable_pi_path <- "Data/Nielsen/price_quantity_indices_taxableitems_2006-2016.csv"
#eventstudy_tr_path <- "Data/event_study_tr_groups_comprehensive_firstonly_no2012q4_2013q1q2.csv"
#tr_groups_path <- "Data/tr_groups_comprehensive_firstonly_no2012q4_2013q1q2.csv"

covariates.nhgis.path <- "Data/covariates/nhgis_county_clean.csv"
covariates.qcew.path <- "Data/covariates/qcew_clean.csv"
census.regions.path <- "Data/covariates/census_regions.csv"


# ### Prepare the data
## See LR_diff_diff_regression_sales_FE_specification.R -- the file output_yearly is prepared in that file
yearly_data <- fread(output_yearly)
## We keep all years -- Will help with leads and lags


################################
## Include some covariates
#List of unique counties in the sales data
list.counties <- data.frame(unique(yearly_data[,c('fips_state','fips_county')]))

covariates.nhgis <- fread(covariates.nhgis.path)
census.regions <- fread(census.regions.path)
census.regions <- merge(list.counties, census.regions, by = c("fips_state"),
                        all.x = T)
census.regions$Division <- census.regions$Region*10 + census.regions$Division


setkey(covariates.nhgis, year)
covariates.nhgis <- covariates.nhgis[list(2000),]

setnames(covariates.nhgis, c('statefp','countyfp'), c('fips_state', 'fips_county'))
covariates.nhgis <- covariates.nhgis[,c('fips_state', 'fips_county', 'pct_pop_urban')] #For now, let's just use "urban"
covariates.nhgis$urban <- as.integer(covariates.nhgis$pct_pop_urban >= 0.5)

covariates.nhgis <- merge(census.regions, covariates.nhgis, by = c("fips_state", "fips_county"),
                          all.x = T)


###
yearly_data <- merge(yearly_data, covariates.nhgis, by = c("fips_state", "fips_county"), all.x = T)

yearly_data[, region_by_time := .GRP, by = .(Region, year)]
yearly_data[, division_by_time := .GRP, by = .(Division, year)]
yearly_data[, urban_by_time := .GRP, by = .(urban, year)]
yearly_data[, regXurban_by_time := .GRP, by = .(Region, urban, year)]
yearly_data[, divXurban_by_time := .GRP, by = .(Division, urban, year)]
yearly_data[, reg_by_module_by_time := .GRP, by = .(Region, product_module_code, year)]
yearly_data[, div_by_module_by_time := .GRP, by = .(Division, product_module_code, year)]


### First, difference the relevant variables
yearly_data <- yearly_data[order(fips_state, fips_county, store_code_uc, product_module_code, -year),] ##Sort on store by year (year in descending order)


yearly_data[, dltax := -diff(ln_sales_tax)] ## Difference log of tax rate
yearly_data$dltax[yearly_data$year <= 2006] <- NA

yearly_data[, d_lcpricei := -diff(ln_cpricei)] ## Difference of log consumer price index (1)
yearly_data$d_lcpricei[yearly_data$year <= 2006] <- NA

yearly_data[, d_lcpricei2 := -diff(yearly_data$ln_cpricei2)] ## Difference of log consumer price index (2)
yearly_data$d_lcpricei2[yearly_data$year <= 2006] <- NA

yearly_data[, d_lquantity := -diff(yearly_data$ln_quantity)] ## Difference of log quantity (1)
yearly_data$d_lquantity[yearly_data$year <= 2006] <- NA

yearly_data[, d_lquantity2 := -diff(yearly_data$ln_quantity2)] ## Difference of log quantity (2)
yearly_data$d_lquantity2[yearly_data$year <= 2006] <- NA

#yearly_data$year <- factor(yearly_data$year) ##Convert the indicator for year to a factor variable (needed for interaction in the regression between ln_sales_tax and dummy for year)



#############################################
#Lag and lead of dltax
yearly_data <- yearly_data[order(fips_state, fips_county, store_code_uc, product_module_code, year), c("lag1", "lag2") := shift(.SD, 1:2, type = "lag"), .SDcols = "dltax", by = c("fips_state", "fips_county", "store_code_uc", "product_module_code")]
yearly_data <- yearly_data[order(fips_state, fips_county, store_code_uc, product_module_code, year), c("lead1", "lead2") := shift(.SD, 1:2, type = "lead"), .SDcols = "dltax", by = c("fips_state", "fips_county", "store_code_uc", "product_module_code")]

#For fully interacted regression, it will be useful to generate directly the interaction terms (because if we do it using the interaction command in ivreg, it will include terms like 2009Xlag1 and 2009Xlag2 wich are zero for all observations)
yearly_data[ , y2009.dltax := (year == 2009)*dltax]
yearly_data[ , y2009.lead1 := (year == 2009)*lead1]
yearly_data[ , y2009.lead2 := (year == 2009)*lead2]
yearly_data[ , y2010.lag1 := (year == 2010)*lag1]
yearly_data[ , y2010.dltax := (year == 2010)*dltax]
yearly_data[ , y2010.lead1 := (year == 2010)*lead1]
yearly_data[ , y2010.lead2 := (year == 2010)*lead2]
yearly_data[ , y2011.lag2 := (year == 2011)*lag2]
yearly_data[ , y2011.lag1 := (year == 2011)*lag1]
yearly_data[ , y2011.dltax := (year == 2011)*dltax]
yearly_data[ , y2011.lead1 := (year == 2011)*lead1]
yearly_data[ , y2011.lead2 := (year == 2011)*lead2]
yearly_data[ , y2012.lag2 := (year == 2012)*lag2]
yearly_data[ , y2012.lag1 := (year == 2012)*lag1]
yearly_data[ , y2012.dltax := (year == 2012)*dltax]
yearly_data[ , y2012.lead1 := (year == 2012)*lead1]
yearly_data[ , y2012.lead2 := (year == 2012)*lead2]
yearly_data[ , y2013.lag2 := (year == 2013)*lag2]
yearly_data[ , y2013.lag1 := (year == 2013)*lag1]
yearly_data[ , y2013.dltax := (year == 2013)*dltax]
yearly_data[ , y2013.lead1 := (year == 2013)*lead1]
yearly_data[ , y2014.lag2 := (year == 2014)*lag2]
yearly_data[ , y2014.lag1 := (year == 2014)*lag1]
yearly_data[ , y2014.dltax := (year == 2014)*dltax]

#############################################


### Second, run the diff-in-diff specifications (in first differences) and interact all leads, lags and current tax rate with year fixed effects
list.outcomes <- c("d_lcpricei", "d_lcpricei2", "d_lquantity", "d_lquantity2")

for(j in 1:length(list.outcomes)) {

  #
  outcome.j <- list.outcomes[j]

  
  ###### First - Only Year FE
  formula0 <- as.formula(paste0(
    outcome.j, " ~ y2009.dltax + y2009.lead1 + y2009.lead2 + y2010.lag1 + y2010.dltax + y2010.lead1 + y2010.lead2 + y2011.lag2 + y2011.lag1 + y2011.dltax + y2011.lead1 + y2011.lead2 + y2012.lag2 + y2012.lag1 + y2012.dltax + y2012.lead1 + y2012.lead2 + y2013.lag2 + y2013.lag1 + y2013.dltax + y2013.lead1 + y2014.lag2 + y2014.lag1 + y2014.dltax | year | 0 | state_by_module ", sep = ""))

  res0 <- felm(data = yearly_data,
               formula = formula0,
               weights = yearly_data$base.sales)

  if(j == 1) {
    
    LRdiff_res <-
      data.table(
        outcome = outcome.j,
        name = row.names(coef(summary(res0))),
        estimate = coef(summary(res0))[, "Estimate"],
        se = coef(summary(res0))[, "Cluster s.e."],
        pval = coef(summary(res0))[, "Pr(>|t|)"],
        Rsq = summary(res0)$r.squared,
        adj.Rsq = summary(res0)$adj.r.squared,
        time_controls = "year FE"
      )
  } else {
    
    LRdiff_res <- rbind(
      LRdiff_res,
      data.table(
        outcome = outcome.j,
        name = row.names(coef(summary(res0))),
        estimate = coef(summary(res0))[, "Estimate"],
        se = coef(summary(res0))[, "Cluster s.e."],
        pval = coef(summary(res0))[, "Pr(>|t|)"],
        Rsq = summary(res0)$r.squared,
        adj.Rsq = summary(res0)$adj.r.squared,
        time_controls = "year FE"
      )
    )
  }
  
  # Take averages across cohorts
  list.leads <- c("lead2", "lead1", "dltax", "lag1", "lag2")
  for(k in 1:length(list.leads)) {
  
  #Create list of year specific coefficients corresponding to lead/lag k
  list.coef <- row.names(coef(summary(res0))[grepl(list.leads[k], row.names(coef(summary(res0)))),])
  
  ## Equal weights across cohorts
  weights <- 1/length(list.coef)

  avg.coef <- paste(list.coef, collapse = paste("*", weights, " +", sep = ""))
  avg.coef <- paste(avg.coef, "*", weights, sep = "") ## Add one more at the end
  avg.coef.form <- paste0(avg.coef, " = 0", sep = "")
  avg.coef.test <- glht(res0, linfct = c(avg.coef.form))
  avg.pre.est <- coef(summary(avg.coef.test))[[1]]
  avg.pre.se <- sqrt(vcov(summary(avg.coef.test)))[[1]]
  avg.pre.pval <- 2*(1-pnorm(abs(avg.pre.est/avg.pre.se)))
  
  
  ## Save results
  LRdiff_res <- rbind(
    LRdiff_res,
    data.table(
      outcome = outcome.j,
      name = paste("avg_", list.leads[k], sep = ""),
      estimate = avg.pre.est,
      se = avg.pre.se,
      pval = avg.pre.pval,
      Rsq = summary(res0)$r.squared,
      adj.Rsq = summary(res0)$adj.r.squared,
      time_controls = "year FE"
    )
  )
  
  }
  
  
  
  ## Average across cohorts of total pre-period effect
  # First average each lead/lag over cohorts then sum different leads/lags
  ## Equal weights across cohorts
  list.coef.lead2 <- row.names(coef(summary(res0))[grepl("lead2", row.names(coef(summary(res0)))),])
  weights <- 1/length(list.coef.lead2)
  avg.coef.lead2 <- paste(list.coef.lead2, collapse = paste("*", weights, " +", sep = ""))
  avg.coef.lead2 <- paste(avg.coef.lead2, "*", weights, sep = "") ## Add one more at the end
  
  list.coef.lead1 <- row.names(coef(summary(res0))[grepl("lead1", row.names(coef(summary(res0)))),])
  weights <- 1/length(list.coef.lead1)
  avg.coef.lead1 <- paste(list.coef.lead1, collapse = paste("*", weights, " +", sep = ""))
  avg.coef.lead1 <- paste(avg.coef.lead1, "*", weights, sep = "") ## Add one more at the end
  
  avg.coef <- paste0(avg.coef.lead2, " + ", avg.coef.lead1, sep = "")
  avg.coef.form <- paste0(avg.coef, " = 0", sep = "")
  avg.coef.test <- glht(res0, linfct = c(avg.coef.form))
  avg.pre.est <- coef(summary(avg.coef.test))[[1]]
  avg.pre.se <- sqrt(vcov(summary(avg.coef.test)))[[1]]
  avg.pre.pval <- 2*(1-pnorm(abs(avg.pre.est/avg.pre.se)))
  
  
  ## Save results
  LRdiff_res <- rbind(
    LRdiff_res,
    data.table(
      outcome = outcome.j,
      name = "avg_pre_period",
      estimate = avg.pre.est,
      se = avg.pre.se,
      pval = avg.pre.pval,
      Rsq = summary(res0)$r.squared,
      adj.Rsq = summary(res0)$adj.r.squared,
      time_controls = "year FE"
    )
  )
  
  
  ## Average across cohorts of total post-period effect
  # First average each lead/lag over cohorts then sum different leads/lags
  ## Equal weights across cohorts
  list.coef.dltax <- row.names(coef(summary(res0))[grepl("dltax", row.names(coef(summary(res0)))),])
  weights <- 1/length(list.coef.dltax)
  avg.coef.dltax <- paste(list.coef.dltax, collapse = paste("*", weights, " +", sep = ""))
  avg.coef.dltax <- paste(avg.coef.dltax, "*", weights, sep = "") ## Add one more at the end
  
  list.coef.lag1 <- row.names(coef(summary(res0))[grepl("lag1", row.names(coef(summary(res0)))),])
  weights <- 1/length(list.coef.lag1)
  avg.coef.lag1 <- paste(list.coef.lag1, collapse = paste("*", weights, " +", sep = ""))
  avg.coef.lag1 <- paste(avg.coef.lag1, "*", weights, sep = "") ## Add one more at the end
  
  list.coef.lag2 <- row.names(coef(summary(res0))[grepl("lag2", row.names(coef(summary(res0)))),])
  weights <- 1/length(list.coef.lag2)
  avg.coef.lag2 <- paste(list.coef.lag2, collapse = paste("*", weights, " +", sep = ""))
  avg.coef.lag2 <- paste(avg.coef.lag2, "*", weights, sep = "") ## Add one more at the end
  
  avg.coef <- paste0(avg.coef.dltax, " + ", avg.coef.lag1, " + ", avg.coef.lag2, sep = "")
  avg.coef.form <- paste0(avg.coef, " = 0", sep = "")
  avg.coef.test <- glht(res0, linfct = c(avg.coef.form))
  avg.pre.est <- coef(summary(avg.coef.test))[[1]]
  avg.pre.se <- sqrt(vcov(summary(avg.coef.test)))[[1]]
  avg.pre.pval <- 2*(1-pnorm(abs(avg.pre.est/avg.pre.se)))
  
  
  ## Save results
  LRdiff_res <- rbind(
    LRdiff_res,
    data.table(
      outcome = outcome.j,
      name = "avg_post_period",
      estimate = avg.pre.est,
      se = avg.pre.se,
      pval = avg.pre.pval,
      Rsq = summary(res0)$r.squared,
      adj.Rsq = summary(res0)$adj.r.squared,
      time_controls = "year FE"
    )
  )
  

  ## Average across cohorts of total effect (pre- + post-)
  avg.coef <- paste0(avg.coef.lead2, " + ", avg.coef.lead1, " + ", avg.coef.dltax, " + ", avg.coef.lag1, " + ", avg.coef.lag2, sep = "")
  avg.coef.form <- paste0(avg.coef, " = 0", sep = "")
  avg.coef.test <- glht(res0, linfct = c(avg.coef.form))
  avg.pre.est <- coef(summary(avg.coef.test))[[1]]
  avg.pre.se <- sqrt(vcov(summary(avg.coef.test)))[[1]]
  avg.pre.pval <- 2*(1-pnorm(abs(avg.pre.est/avg.pre.se)))
  
  
  ## Save results
  LRdiff_res <- rbind(
    LRdiff_res,
    data.table(
      outcome = outcome.j,
      name = "avg_total_effect",
      estimate = avg.pre.est,
      se = avg.pre.se,
      pval = avg.pre.pval,
      Rsq = summary(res0)$r.squared,
      adj.Rsq = summary(res0)$adj.r.squared,
      time_controls = "year FE"
    )
  )

  
  ############# With Year-Module FE
  formula0 <- as.formula(paste0(
    outcome.j, " ~ y2009.dltax + y2009.lead1 + y2009.lead2 + y2010.lag1 + y2010.dltax + y2010.lead1 + y2010.lead2 + y2011.lag2 + y2011.lag1 + y2011.dltax + y2011.lead1 + y2011.lead2 + y2012.lag2 + y2012.lag1 + y2012.dltax + y2012.lead1 + y2012.lead2 + y2013.lag2 + y2013.lag1 + y2013.dltax + y2013.lead1 + y2014.lag2 + y2014.lag1 + y2014.dltax | module_by_time | 0 | state_by_module ", sep = ""))
  
  res0 <- felm(data = yearly_data,
               formula = formula0,
               weights = yearly_data$base.sales)
    
    LRdiff_res <- rbind(
      LRdiff_res,
      data.table(
        outcome = outcome.j,
        name = row.names(coef(summary(res0))),
        estimate = coef(summary(res0))[, "Estimate"],
        se = coef(summary(res0))[, "Cluster s.e."],
        pval = coef(summary(res0))[, "Pr(>|t|)"],
        Rsq = summary(res0)$r.squared,
        adj.Rsq = summary(res0)$adj.r.squared,
        time_controls = "Module-Year FE"
      )
    )
  
  # Take averages across cohorts
  list.leads <- c("lead2", "lead1", "dltax", "lag1", "lag2")
  for(k in 1:length(list.leads)) {
    
    #Create list of year specific coefficients corresponding to lead/lag k
    list.coef <- row.names(coef(summary(res0))[grepl(list.leads[k], row.names(coef(summary(res0)))),])
    
    ## Equal weights across cohorts
    weights <- 1/length(list.coef)
    
    avg.coef <- paste(list.coef, collapse = paste("*", weights, " +", sep = ""))
    avg.coef <- paste(avg.coef, "*", weights, sep = "") ## Add one more at the end
    avg.coef.form <- paste0(avg.coef, " = 0", sep = "")
    avg.coef.test <- glht(res0, linfct = c(avg.coef.form))
    avg.pre.est <- coef(summary(avg.coef.test))[[1]]
    avg.pre.se <- sqrt(vcov(summary(avg.coef.test)))[[1]]
    avg.pre.pval <- 2*(1-pnorm(abs(avg.pre.est/avg.pre.se)))
    
    
    ## Save results
    LRdiff_res <- rbind(
      LRdiff_res,
      data.table(
        outcome = outcome.j,
        name = paste("avg_", list.leads[k], sep = ""),
        estimate = avg.pre.est,
        se = avg.pre.se,
        pval = avg.pre.pval,
        Rsq = summary(res0)$r.squared,
        adj.Rsq = summary(res0)$adj.r.squared,
        time_controls = "Module-Year FE"
      )
    )
    
  }
  
  
  
  ## Average across cohorts of total pre-period effect
  # First average each lead/lag over cohorts then sum different leads/lags
  ## Equal weights across cohorts
  list.coef.lead2 <- row.names(coef(summary(res0))[grepl("lead2", row.names(coef(summary(res0)))),])
  weights <- 1/length(list.coef.lead2)
  avg.coef.lead2 <- paste(list.coef.lead2, collapse = paste("*", weights, " +", sep = ""))
  avg.coef.lead2 <- paste(avg.coef.lead2, "*", weights, sep = "") ## Add one more at the end
  
  list.coef.lead1 <- row.names(coef(summary(res0))[grepl("lead1", row.names(coef(summary(res0)))),])
  weights <- 1/length(list.coef.lead1)
  avg.coef.lead1 <- paste(list.coef.lead1, collapse = paste("*", weights, " +", sep = ""))
  avg.coef.lead1 <- paste(avg.coef.lead1, "*", weights, sep = "") ## Add one more at the end
  
  avg.coef <- paste0(avg.coef.lead2, " + ", avg.coef.lead1, sep = "")
  avg.coef.form <- paste0(avg.coef, " = 0", sep = "")
  avg.coef.test <- glht(res0, linfct = c(avg.coef.form))
  avg.pre.est <- coef(summary(avg.coef.test))[[1]]
  avg.pre.se <- sqrt(vcov(summary(avg.coef.test)))[[1]]
  avg.pre.pval <- 2*(1-pnorm(abs(avg.pre.est/avg.pre.se)))
  
  
  ## Save results
  LRdiff_res <- rbind(
    LRdiff_res,
    data.table(
      outcome = outcome.j,
      name = "avg_pre_period",
      estimate = avg.pre.est,
      se = avg.pre.se,
      pval = avg.pre.pval,
      Rsq = summary(res0)$r.squared,
      adj.Rsq = summary(res0)$adj.r.squared,
      time_controls = "Module-Year FE"
    )
  )
  
  
  ## Average across cohorts of total post-period effect
  # First average each lead/lag over cohorts then sum different leads/lags
  ## Equal weights across cohorts
  list.coef.dltax <- row.names(coef(summary(res0))[grepl("dltax", row.names(coef(summary(res0)))),])
  weights <- 1/length(list.coef.dltax)
  avg.coef.dltax <- paste(list.coef.dltax, collapse = paste("*", weights, " +", sep = ""))
  avg.coef.dltax <- paste(avg.coef.dltax, "*", weights, sep = "") ## Add one more at the end
  
  list.coef.lag1 <- row.names(coef(summary(res0))[grepl("lag1", row.names(coef(summary(res0)))),])
  weights <- 1/length(list.coef.lag1)
  avg.coef.lag1 <- paste(list.coef.lag1, collapse = paste("*", weights, " +", sep = ""))
  avg.coef.lag1 <- paste(avg.coef.lag1, "*", weights, sep = "") ## Add one more at the end
  
  list.coef.lag2 <- row.names(coef(summary(res0))[grepl("lag2", row.names(coef(summary(res0)))),])
  weights <- 1/length(list.coef.lag2)
  avg.coef.lag2 <- paste(list.coef.lag2, collapse = paste("*", weights, " +", sep = ""))
  avg.coef.lag2 <- paste(avg.coef.lag2, "*", weights, sep = "") ## Add one more at the end
  
  avg.coef <- paste0(avg.coef.dltax, " + ", avg.coef.lag1, " + ", avg.coef.lag2, sep = "")
  avg.coef.form <- paste0(avg.coef, " = 0", sep = "")
  avg.coef.test <- glht(res0, linfct = c(avg.coef.form))
  avg.pre.est <- coef(summary(avg.coef.test))[[1]]
  avg.pre.se <- sqrt(vcov(summary(avg.coef.test)))[[1]]
  avg.pre.pval <- 2*(1-pnorm(abs(avg.pre.est/avg.pre.se)))
  
  
  ## Save results
  LRdiff_res <- rbind(
    LRdiff_res,
    data.table(
      outcome = outcome.j,
      name = "avg_post_period",
      estimate = avg.pre.est,
      se = avg.pre.se,
      pval = avg.pre.pval,
      Rsq = summary(res0)$r.squared,
      adj.Rsq = summary(res0)$adj.r.squared,
      time_controls = "Module-Year FE"
    )
  )
  
  
  ## Average across cohorts of total effect (pre- + post-)
  avg.coef <- paste0(avg.coef.lead2, " + ", avg.coef.lead1, " + ", avg.coef.dltax, " + ", avg.coef.lag1, " + ", avg.coef.lag2, sep = "")
  avg.coef.form <- paste0(avg.coef, " = 0", sep = "")
  avg.coef.test <- glht(res0, linfct = c(avg.coef.form))
  avg.pre.est <- coef(summary(avg.coef.test))[[1]]
  avg.pre.se <- sqrt(vcov(summary(avg.coef.test)))[[1]]
  avg.pre.pval <- 2*(1-pnorm(abs(avg.pre.est/avg.pre.se)))
  
  
  ## Save results
  LRdiff_res <- rbind(
    LRdiff_res,
    data.table(
      outcome = outcome.j,
      name = "avg_total_effect",
      estimate = avg.pre.est,
      se = avg.pre.se,
      pval = avg.pre.pval,
      Rsq = summary(res0)$r.squared,
      adj.Rsq = summary(res0)$adj.r.squared,
      time_controls = "Module-Year FE"
    )
  )
  
  
  
  
  ########### With Region-by-Module-by-Year FE
  formula0 <- as.formula(paste0(
    outcome.j, " ~ y2009.dltax + y2009.lead1 + y2009.lead2 + y2010.lag1 + y2010.dltax + y2010.lead1 + y2010.lead2 + y2011.lag2 + y2011.lag1 + y2011.dltax + y2011.lead1 + y2011.lead2 + y2012.lag2 + y2012.lag1 + y2012.dltax + y2012.lead1 + y2012.lead2 + y2013.lag2 + y2013.lag1 + y2013.dltax + y2013.lead1 + y2014.lag2 + y2014.lag1 + y2014.dltax | reg_by_module_by_time | 0 | state_by_module ", sep = ""))
  
  res0 <- felm(data = yearly_data,
               formula = formula0,
               weights = yearly_data$base.sales)
  
  LRdiff_res <- rbind(
    LRdiff_res,
    data.table(
      outcome = outcome.j,
      name = row.names(coef(summary(res0))),
      estimate = coef(summary(res0))[, "Estimate"],
      se = coef(summary(res0))[, "Cluster s.e."],
      pval = coef(summary(res0))[, "Pr(>|t|)"],
      Rsq = summary(res0)$r.squared,
      adj.Rsq = summary(res0)$adj.r.squared,
      time_controls = "Module-Region-Year FE"
    )
  )
  
  # Take averages across cohorts
  list.leads <- c("lead2", "lead1", "dltax", "lag1", "lag2")
  for(k in 1:length(list.leads)) {
    
    #Create list of year specific coefficients corresponding to lead/lag k
    list.coef <- row.names(coef(summary(res0))[grepl(list.leads[k], row.names(coef(summary(res0)))),])
    
    ## Equal weights across cohorts
    weights <- 1/length(list.coef)
    
    avg.coef <- paste(list.coef, collapse = paste("*", weights, " +", sep = ""))
    avg.coef <- paste(avg.coef, "*", weights, sep = "") ## Add one more at the end
    avg.coef.form <- paste0(avg.coef, " = 0", sep = "")
    avg.coef.test <- glht(res0, linfct = c(avg.coef.form))
    avg.pre.est <- coef(summary(avg.coef.test))[[1]]
    avg.pre.se <- sqrt(vcov(summary(avg.coef.test)))[[1]]
    avg.pre.pval <- 2*(1-pnorm(abs(avg.pre.est/avg.pre.se)))
    
    
    ## Save results
    LRdiff_res <- rbind(
      LRdiff_res,
      data.table(
        outcome = outcome.j,
        name = paste("avg_", list.leads[k], sep = ""),
        estimate = avg.pre.est,
        se = avg.pre.se,
        pval = avg.pre.pval,
        Rsq = summary(res0)$r.squared,
        adj.Rsq = summary(res0)$adj.r.squared,
        time_controls = "Module-Region-Year FE"
      )
    )
    
  }
  
  
  
  ## Average across cohorts of total pre-period effect
  # First average each lead/lag over cohorts then sum different leads/lags
  ## Equal weights across cohorts
  list.coef.lead2 <- row.names(coef(summary(res0))[grepl("lead2", row.names(coef(summary(res0)))),])
  weights <- 1/length(list.coef.lead2)
  avg.coef.lead2 <- paste(list.coef.lead2, collapse = paste("*", weights, " +", sep = ""))
  avg.coef.lead2 <- paste(avg.coef.lead2, "*", weights, sep = "") ## Add one more at the end
  
  list.coef.lead1 <- row.names(coef(summary(res0))[grepl("lead1", row.names(coef(summary(res0)))),])
  weights <- 1/length(list.coef.lead1)
  avg.coef.lead1 <- paste(list.coef.lead1, collapse = paste("*", weights, " +", sep = ""))
  avg.coef.lead1 <- paste(avg.coef.lead1, "*", weights, sep = "") ## Add one more at the end
  
  avg.coef <- paste0(avg.coef.lead2, " + ", avg.coef.lead1, sep = "")
  avg.coef.form <- paste0(avg.coef, " = 0", sep = "")
  avg.coef.test <- glht(res0, linfct = c(avg.coef.form))
  avg.pre.est <- coef(summary(avg.coef.test))[[1]]
  avg.pre.se <- sqrt(vcov(summary(avg.coef.test)))[[1]]
  avg.pre.pval <- 2*(1-pnorm(abs(avg.pre.est/avg.pre.se)))
  
  
  ## Save results
  LRdiff_res <- rbind(
    LRdiff_res,
    data.table(
      outcome = outcome.j,
      name = "avg_pre_period",
      estimate = avg.pre.est,
      se = avg.pre.se,
      pval = avg.pre.pval,
      Rsq = summary(res0)$r.squared,
      adj.Rsq = summary(res0)$adj.r.squared,
      time_controls = "Module-Region-Year FE"
    )
  )
  
  
  ## Average across cohorts of total post-period effect
  # First average each lead/lag over cohorts then sum different leads/lags
  ## Equal weights across cohorts
  list.coef.dltax <- row.names(coef(summary(res0))[grepl("dltax", row.names(coef(summary(res0)))),])
  weights <- 1/length(list.coef.dltax)
  avg.coef.dltax <- paste(list.coef.dltax, collapse = paste("*", weights, " +", sep = ""))
  avg.coef.dltax <- paste(avg.coef.dltax, "*", weights, sep = "") ## Add one more at the end
  
  list.coef.lag1 <- row.names(coef(summary(res0))[grepl("lag1", row.names(coef(summary(res0)))),])
  weights <- 1/length(list.coef.lag1)
  avg.coef.lag1 <- paste(list.coef.lag1, collapse = paste("*", weights, " +", sep = ""))
  avg.coef.lag1 <- paste(avg.coef.lag1, "*", weights, sep = "") ## Add one more at the end
  
  list.coef.lag2 <- row.names(coef(summary(res0))[grepl("lag2", row.names(coef(summary(res0)))),])
  weights <- 1/length(list.coef.lag2)
  avg.coef.lag2 <- paste(list.coef.lag2, collapse = paste("*", weights, " +", sep = ""))
  avg.coef.lag2 <- paste(avg.coef.lag2, "*", weights, sep = "") ## Add one more at the end
  
  avg.coef <- paste0(avg.coef.dltax, " + ", avg.coef.lag1, " + ", avg.coef.lag2, sep = "")
  avg.coef.form <- paste0(avg.coef, " = 0", sep = "")
  avg.coef.test <- glht(res0, linfct = c(avg.coef.form))
  avg.pre.est <- coef(summary(avg.coef.test))[[1]]
  avg.pre.se <- sqrt(vcov(summary(avg.coef.test)))[[1]]
  avg.pre.pval <- 2*(1-pnorm(abs(avg.pre.est/avg.pre.se)))
  
  
  ## Save results
  LRdiff_res <- rbind(
    LRdiff_res,
    data.table(
      outcome = outcome.j,
      name = "avg_post_period",
      estimate = avg.pre.est,
      se = avg.pre.se,
      pval = avg.pre.pval,
      Rsq = summary(res0)$r.squared,
      adj.Rsq = summary(res0)$adj.r.squared,
      time_controls = "Module-Region-Year FE"
    )
  )
  
  
  ## Average across cohorts of total effect (pre- + post-)
  avg.coef <- paste0(avg.coef.lead2, " + ", avg.coef.lead1, " + ", avg.coef.dltax, " + ", avg.coef.lag1, " + ", avg.coef.lag2, sep = "")
  avg.coef.form <- paste0(avg.coef, " = 0", sep = "")
  avg.coef.test <- glht(res0, linfct = c(avg.coef.form))
  avg.pre.est <- coef(summary(avg.coef.test))[[1]]
  avg.pre.se <- sqrt(vcov(summary(avg.coef.test)))[[1]]
  avg.pre.pval <- 2*(1-pnorm(abs(avg.pre.est/avg.pre.se)))
  
  
  ## Save results
  LRdiff_res <- rbind(
    LRdiff_res,
    data.table(
      outcome = outcome.j,
      name = "avg_total_effect",
      estimate = avg.pre.est,
      se = avg.pre.se,
      pval = avg.pre.pval,
      Rsq = summary(res0)$r.squared,
      adj.Rsq = summary(res0)$adj.r.squared,
      time_controls = "Module-Region-Year FE"
    )
  )
  
}


## summary values --------------------------------------------------------------
LRdiff_res$N_obs <- nrow(yearly_data)
LRdiff_res$N_modules <- length(unique(yearly_data$product_module_code))
LRdiff_res$N_stores <- length(unique(yearly_data$store_code_uc))
LRdiff_res$N_counties <- uniqueN(yearly_data, by = c("fips_state", "fips_county"))
LRdiff_res$N_years <- uniqueN(yearly_data, by = c("year")) # should be 6 (we lose one because we difference)
LRdiff_res$N_county_modules <- uniqueN(yearly_data, by = c("fips_state", "fips_county",
                                                           "product_module_code"))

fwrite(LRdiff_res, "Data/LRdiff_results_diff_cohortbycohort.csv")

