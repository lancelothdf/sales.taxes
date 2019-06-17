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




#############################################
#Lag and lead of dltax
yearly_data <- yearly_data[order(fips_state, fips_county, store_code_uc, product_module_code, year), c("lag1", "lag2", "lag3") := shift(.SD, 1:3, type = "lag"), .SDcols = "ln_sales_tax", by = c("fips_state", "fips_county", "store_code_uc", "product_module_code")]
yearly_data <- yearly_data[order(fips_state, fips_county, store_code_uc, product_module_code, year), c("lead1", "lead2", "lead3") := shift(.SD, 1:3, type = "lead"), .SDcols = "ln_sales_tax", by = c("fips_state", "fips_county", "store_code_uc", "product_module_code")]


#For fully interacted regression, it will be useful to generate directly the interaction terms (because if we do it using the interaction command in ivreg, it will include terms like 2009Xlag1 and 2009Xlag2 wich are zero for all observations)
yearly_data[ , y2008.ln_sales_tax := (year == 2008)*ln_sales_tax]
yearly_data[ , y2008.lead1 := (year == 2008)*lead1]
yearly_data[ , y2008.lead2 := (year == 2008)*lead2]
yearly_data[ , y2008.lead3 := (year == 2008)*lead3]
yearly_data[ , y2009.lag1 := (year == 2009)*lag1]
yearly_data[ , y2009.ln_sales_tax := (year == 2009)*ln_sales_tax]
yearly_data[ , y2009.lead1 := (year == 2009)*lead1]
yearly_data[ , y2009.lead2 := (year == 2009)*lead2]
yearly_data[ , y2009.lead3 := (year == 2009)*lead3]
yearly_data[ , y2010.lag2 := (year == 2010)*lag2]
yearly_data[ , y2010.lag1 := (year == 2010)*lag1]
yearly_data[ , y2010.ln_sales_tax := (year == 2010)*ln_sales_tax]
yearly_data[ , y2010.lead1 := (year == 2010)*lead1]
yearly_data[ , y2010.lead2 := (year == 2010)*lead2]
yearly_data[ , y2010.lead3 := (year == 2010)*lead3]
yearly_data[ , y2011.lag3 := (year == 2011)*lag3]
yearly_data[ , y2011.lag2 := (year == 2011)*lag2]
yearly_data[ , y2011.lag1 := (year == 2011)*lag1]
yearly_data[ , y2011.ln_sales_tax := (year == 2011)*ln_sales_tax]
yearly_data[ , y2011.lead1 := (year == 2011)*lead1]
yearly_data[ , y2011.lead2 := (year == 2011)*lead2]
yearly_data[ , y2011.lead3 := (year == 2011)*lead3]
yearly_data[ , y2012.lag3 := (year == 2012)*lag3]
yearly_data[ , y2012.lag2 := (year == 2012)*lag2]
yearly_data[ , y2012.lag1 := (year == 2012)*lag1]
yearly_data[ , y2012.ln_sales_tax := (year == 2012)*ln_sales_tax]
yearly_data[ , y2012.lead1 := (year == 2012)*lead1]
yearly_data[ , y2012.lead2 := (year == 2012)*lead2]
yearly_data[ , y2013.lag3 := (year == 2013)*lag3]
yearly_data[ , y2013.lag2 := (year == 2013)*lag2]
yearly_data[ , y2013.lag1 := (year == 2013)*lag1]
yearly_data[ , y2013.ln_sales_tax := (year == 2013)*ln_sales_tax]
yearly_data[ , y2013.lead1 := (year == 2013)*lead1]
yearly_data[ , y2014.lag3 := (year == 2014)*lag3]
yearly_data[ , y2014.lag2 := (year == 2014)*lag2]
yearly_data[ , y2014.lag1 := (year == 2014)*lag1]
yearly_data[ , y2014.ln_sales_tax := (year == 2014)*ln_sales_tax]

#############################################
yearly_data <- yearly_data[year >= 2008 & year <= 2014,]

### Second, run the diff-in-diff specifications (in first differences) and interact all leads, lags and current tax rate with year fixed effects
list.outcomes <- c("ln_cpricei", "ln_cpricei2", "ln_quantity", "ln_quantity2")


for(j in 1:length(list.outcomes)) {

  #
  outcome.j <- list.outcomes[j]


  ###### First - Only Year FE - normalize at t = - 1 and in 2008
  formula0 <- as.formula(paste0(
    outcome.j, " ~ y2009.lag1 + y2009.ln_sales_tax + y2009.lead2 + y2009.lead3 + y2010.lag2 + y2010.lag1 + y2010.ln_sales_tax + y2010.lead2 + y2010.lead3 + y2011.lag3 + y2011.lag2 + y2011.lag1 + y2011.ln_sales_tax + y2011.lead2 + y2011.lead3 + y2012.lag3 + y2012.lag2 + y2012.lag1 + y2012.ln_sales_tax + y2012.lead2 + y2013.lag3 + y2013.lag2 + y2013.lag1 + y2013.ln_sales_tax | store_module + year | 0 | state_by_module ", sep = ""))

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
  list.leads <- c("lead3", "lead2", "ln_sales_tax", "lag1", "lag2", "lag3")
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
  # First average each lead/lag over cohorts then sum over different leads/lags
  ## Equal weights across cohorts
  list.coef.lead3 <- row.names(coef(summary(res0))[grepl("lead3", row.names(coef(summary(res0)))),])
  weights <- 1/length(list.coef.lead3)
  avg.coef.lead3 <- paste(list.coef.lead3, collapse = paste("*", weights, " +", sep = ""))
  avg.coef.lead3 <- paste(avg.coef.lead3, "*", weights, sep = "") ## Add one more at the end

  list.coef.lead2 <- row.names(coef(summary(res0))[grepl("lead2", row.names(coef(summary(res0)))),])
  weights <- 1/length(list.coef.lead2)
  avg.coef.lead2 <- paste(list.coef.lead2, collapse = paste("*", weights, " +", sep = ""))
  avg.coef.lead2 <- paste(avg.coef.lead2, "*", weights, sep = "") ## Add one more at the end

  avg.coef <- paste0(avg.coef.lead3, " + ", avg.coef.lead2, sep = "")
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
  # First average each lead/lag over cohorts then sum over different leads/lags
  ## Equal weights across cohorts
  list.coef.ln_sales_tax <- row.names(coef(summary(res0))[grepl("ln_sales_tax", row.names(coef(summary(res0)))),])
  weights <- 1/length(list.coef.ln_sales_tax)
  avg.coef.ln_sales_tax <- paste(list.coef.ln_sales_tax, collapse = paste("*", weights, " +", sep = ""))
  avg.coef.ln_sales_tax <- paste(avg.coef.ln_sales_tax, "*", weights, sep = "") ## Add one more at the end

  list.coef.lag1 <- row.names(coef(summary(res0))[grepl("lag1", row.names(coef(summary(res0)))),])
  weights <- 1/length(list.coef.lag1)
  avg.coef.lag1 <- paste(list.coef.lag1, collapse = paste("*", weights, " +", sep = ""))
  avg.coef.lag1 <- paste(avg.coef.lag1, "*", weights, sep = "") ## Add one more at the end

  list.coef.lag2 <- row.names(coef(summary(res0))[grepl("lag2", row.names(coef(summary(res0)))),])
  weights <- 1/length(list.coef.lag2)
  avg.coef.lag2 <- paste(list.coef.lag2, collapse = paste("*", weights, " +", sep = ""))
  avg.coef.lag2 <- paste(avg.coef.lag2, "*", weights, sep = "") ## Add one more at the end

  list.coef.lag3 <- row.names(coef(summary(res0))[grepl("lag3", row.names(coef(summary(res0)))),])
  weights <- 1/length(list.coef.lag3)
  avg.coef.lag3 <- paste(list.coef.lag3, collapse = paste("*", weights, " +", sep = ""))
  avg.coef.lag3 <- paste(avg.coef.lag3, "*", weights, sep = "") ## Add one more at the end

  avg.coef <- paste0(avg.coef.ln_sales_tax, " + ", avg.coef.lag1, " + ", avg.coef.lag2, " + ", avg.coef.lag3, sep = "")
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




  ############# With Year-Module FE
  formula0 <- as.formula(paste0(
    outcome.j, " ~ y2009.lag1 + y2009.ln_sales_tax + y2009.lead2 + y2009.lead3 + y2010.lag2 + y2010.lag1 + y2010.ln_sales_tax + y2010.lead2 + y2010.lead3 + y2011.lag3 + y2011.lag2 + y2011.lag1 + y2011.ln_sales_tax + y2011.lead2 + y2011.lead3 + y2012.lag3 + y2012.lag2 + y2012.lag1 + y2012.ln_sales_tax + y2012.lead2 + y2013.lag3 + y2013.lag2 + y2013.lag1 + y2013.ln_sales_tax | store_module + module_by_time | 0 | state_by_module ", sep = ""))

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
  list.leads <- c("lead3", "lead2", "ln_sales_tax", "lag1", "lag2", "lag3")
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



  ## Average across cohorts of avg pre-period effect
  # First average each lead/lag over cohorts then sum over different leads/lags
  ## Equal weights across cohorts
  list.coef.lead3 <- row.names(coef(summary(res0))[grepl("lead3", row.names(coef(summary(res0)))),])
  weights <- 1/length(list.coef.lead3)
  avg.coef.lead3 <- paste(list.coef.lead3, collapse = paste("*", weights, " +", sep = ""))
  avg.coef.lead3 <- paste(avg.coef.lead3, "*", weights, sep = "") ## Add one more at the end

  list.coef.lead2 <- row.names(coef(summary(res0))[grepl("lead2", row.names(coef(summary(res0)))),])
  weights <- 1/length(list.coef.lead2)
  avg.coef.lead2 <- paste(list.coef.lead2, collapse = paste("*", weights, " +", sep = ""))
  avg.coef.lead2 <- paste(avg.coef.lead2, "*", weights, sep = "") ## Add one more at the end

  avg.coef <- paste0(avg.coef.lead3, " + ", avg.coef.lead2, sep = "")
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
  # First average each lead/lag over cohorts then sum over different leads/lags
  ## Equal weights across cohorts
  list.coef.ln_sales_tax <- row.names(coef(summary(res0))[grepl("ln_sales_tax", row.names(coef(summary(res0)))),])
  weights <- 1/length(list.coef.ln_sales_tax)
  avg.coef.ln_sales_tax <- paste(list.coef.ln_sales_tax, collapse = paste("*", weights, " +", sep = ""))
  avg.coef.ln_sales_tax <- paste(avg.coef.ln_sales_tax, "*", weights, sep = "") ## Add one more at the end

  list.coef.lag1 <- row.names(coef(summary(res0))[grepl("lag1", row.names(coef(summary(res0)))),])
  weights <- 1/length(list.coef.lag1)
  avg.coef.lag1 <- paste(list.coef.lag1, collapse = paste("*", weights, " +", sep = ""))
  avg.coef.lag1 <- paste(avg.coef.lag1, "*", weights, sep = "") ## Add one more at the end

  list.coef.lag2 <- row.names(coef(summary(res0))[grepl("lag2", row.names(coef(summary(res0)))),])
  weights <- 1/length(list.coef.lag2)
  avg.coef.lag2 <- paste(list.coef.lag2, collapse = paste("*", weights, " +", sep = ""))
  avg.coef.lag2 <- paste(avg.coef.lag2, "*", weights, sep = "") ## Add one more at the end

  list.coef.lag3 <- row.names(coef(summary(res0))[grepl("lag3", row.names(coef(summary(res0)))),])
  weights <- 1/length(list.coef.lag3)
  avg.coef.lag3 <- paste(list.coef.lag3, collapse = paste("*", weights, " +", sep = ""))
  avg.coef.lag3 <- paste(avg.coef.lag3, "*", weights, sep = "") ## Add one more at the end

  avg.coef <- paste0(avg.coef.ln_sales_tax, " + ", avg.coef.lag1, " + ", avg.coef.lag2, " + ", avg.coef.lag3, sep = "")
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




  ########### With Region-by-Module-by-Year FE
  formula0 <- as.formula(paste0(
    outcome.j, " ~ y2009.lag1 + y2009.ln_sales_tax + y2009.lead2 + y2009.lead3 + y2010.lag2 + y2010.lag1 + y2010.ln_sales_tax + y2010.lead2 + y2010.lead3 + y2011.lag3 + y2011.lag2 + y2011.lag1 + y2011.ln_sales_tax + y2011.lead2 + y2011.lead3 + y2012.lag3 + y2012.lag2 + y2012.lag1 + y2012.ln_sales_tax + y2012.lead2 + y2013.lag3 + y2013.lag2 + y2013.lag1 + y2013.ln_sales_tax | store_module + reg_by_module_by_time | 0 | state_by_module ", sep = ""))

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
  list.leads <- c("lead3", "lead2", "ln_sales_tax", "lag1", "lag2", "lag3")
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
  # First average each lead/lag over cohorts then sum over different leads/lags
  ## Equal weights across cohorts
  list.coef.lead3 <- row.names(coef(summary(res0))[grepl("lead3", row.names(coef(summary(res0)))),])
  weights <- 1/length(list.coef.lead3)
  avg.coef.lead3 <- paste(list.coef.lead3, collapse = paste("*", weights, " +", sep = ""))
  avg.coef.lead3 <- paste(avg.coef.lead3, "*", weights, sep = "") ## Add one more at the end

  list.coef.lead2 <- row.names(coef(summary(res0))[grepl("lead2", row.names(coef(summary(res0)))),])
  weights <- 1/length(list.coef.lead2)
  avg.coef.lead2 <- paste(list.coef.lead2, collapse = paste("*", weights, " +", sep = ""))
  avg.coef.lead2 <- paste(avg.coef.lead2, "*", weights, sep = "") ## Add one more at the end

  avg.coef <- paste0(avg.coef.lead3, " + ", avg.coef.lead2, sep = "")
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
  # First average each lead/lag over cohorts then sum over different leads/lags
  ## Equal weights across cohorts
  list.coef.ln_sales_tax <- row.names(coef(summary(res0))[grepl("ln_sales_tax", row.names(coef(summary(res0)))),])
  weights <- 1/length(list.coef.ln_sales_tax)
  avg.coef.ln_sales_tax <- paste(list.coef.ln_sales_tax, collapse = paste("*", weights, " +", sep = ""))
  avg.coef.ln_sales_tax <- paste(avg.coef.ln_sales_tax, "*", weights, sep = "") ## Add one more at the end

  list.coef.lag1 <- row.names(coef(summary(res0))[grepl("lag1", row.names(coef(summary(res0)))),])
  weights <- 1/length(list.coef.lag1)
  avg.coef.lag1 <- paste(list.coef.lag1, collapse = paste("*", weights, " +", sep = ""))
  avg.coef.lag1 <- paste(avg.coef.lag1, "*", weights, sep = "") ## Add one more at the end

  list.coef.lag2 <- row.names(coef(summary(res0))[grepl("lag2", row.names(coef(summary(res0)))),])
  weights <- 1/length(list.coef.lag2)
  avg.coef.lag2 <- paste(list.coef.lag2, collapse = paste("*", weights, " +", sep = ""))
  avg.coef.lag2 <- paste(avg.coef.lag2, "*", weights, sep = "") ## Add one more at the end

  list.coef.lag3 <- row.names(coef(summary(res0))[grepl("lag3", row.names(coef(summary(res0)))),])
  weights <- 1/length(list.coef.lag3)
  avg.coef.lag3 <- paste(list.coef.lag3, collapse = paste("*", weights, " +", sep = ""))
  avg.coef.lag3 <- paste(avg.coef.lag3, "*", weights, sep = "") ## Add one more at the end

  avg.coef <- paste0(avg.coef.ln_sales_tax, " + ", avg.coef.lag1, " + ", avg.coef.lag2, " + ", avg.coef.lag3, sep = "")
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


}


## summary values --------------------------------------------------------------
LRdiff_res$N_obs <- nrow(yearly_data)
LRdiff_res$N_modules <- length(unique(yearly_data$product_module_code))
LRdiff_res$N_stores <- length(unique(yearly_data$store_code_uc))
LRdiff_res$N_counties <- uniqueN(yearly_data, by = c("fips_state", "fips_county"))
LRdiff_res$N_years <- uniqueN(yearly_data, by = c("year")) # should be 6 (we lose one because we difference)
LRdiff_res$N_county_modules <- uniqueN(yearly_data, by = c("fips_state", "fips_county",
                                                           "product_module_code"))

fwrite(LRdiff_res, "Data/LRdiff_results_FE_cohortbycohort.csv")

