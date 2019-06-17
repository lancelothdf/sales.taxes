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

yearly_data$year <- factor(yearly_data$year) ##Convert the indicator for year to a factor variable (needed for interaction in the regression between ln_sales_tax and dummy for year)



#############################################
#Lag and lead of dltax
yearly_data <- yearly_data[order(fips_state, fips_county, store_code_uc, product_module_code, year), c("lag1", "lag2") := shift(.SD, 1:2, type = "lag"), .SDcols = "dltax", by = c("fips_state", "fips_county", "store_code_uc", "product_module_code")]
yearly_data <- yearly_data[order(fips_state, fips_county, store_code_uc, product_module_code, year), c("lead1", "lead2") := shift(.SD, 1:2, type = "lead"), .SDcols = "dltax", by = c("fips_state", "fips_county", "store_code_uc", "product_module_code")]

#For fully interacted regression, it will be useful to generate some dummy variables for each year
yearly_data$year2009 <- factor(yearly_data$year == 2009)
yearly_data$year2010 <- factor(yearly_data$year == 2010)
yearly_data$year2011 <- factor(yearly_data$year == 2011)
yearly_data$year2012 <- factor(yearly_data$year == 2012)
yearly_data$year2013 <- factor(yearly_data$year == 2013)
yearly_data$year2014 <- factor(yearly_data$year == 2014)
#############################################


### Second, run the diff-in-diff specifications (in first differences) and interact all leads, lags and current tax rate with year fixed effects
list.outcomes <- c("d_lcpricei", "d_lcpricei2", "d_lquantity", "d_lquantity2")

for(j in 1:length(list.outcomes)) {

  #
  outcome.j <- list.outcomes[j]

### There might be a better way to code this - this is a mess
  ## Only year FE -##NOTE: we will need to drop some lags in early years and leads in later years
  # This is because there is no variation in ln_tax in 2007, 2008, 2015 and 2016 since it is imputed in those years
  #form.lead2 <- "year2009:lead2 + year2010:lead2 + year2011:lead2 + year2012:lead2"
  #form.lead1 <- "year2009:lead1 + year2010:lead1 + year2011:lead1 + year2012:lead1 + year2013:lead1"
  #form.dltax <- "year2009:dltax" + "year2010:dltax" + "year2011:dltax" + "year2012:dltax" + "year2013:dltax" + "year2014:dltax"
  #form.lag1 <- "year2010:lag1 + year2011:lag1 + year2012:lag1 + year2013:lag1 + year2014:lag1"
  #form.lag2 <- "year2011:lag2 + year2012:lag2 + year2013:lag2 + year2014:lag2"
  form.lead2 <- "year2010:lead2 + year2011:lead2 + year2012:lead2"
  form.lead1 <- "year2010:lead1 + year2011:lead1 + year2012:lead1 + year2013:lead1"
  form.dltax <- "year2010:dltax + year2011:dltax + year2012:dltax + year2013:dltax + year2014:dltax"
  form.lag1 <- "year2010:lag1 + year2011:lag1 + year2012:lag1 + year2013:lag1"
  form.lag2 <- "year2011:lag2 + year2012:lag2 + year2013:lag2"
  ## Also exclude 2009
  
  formula0 <- as.formula(paste0(
    outcome.j, " ~ ", form.lead2, " + ", form.lead1, " + year:dltax + ", form.lag1, " + ", form.lag2, " | year | 0 | state_by_module ", sep = ""))

  res0 <- felm(data = yearly_data,
               formula = formula0,
               weights = yearly_data[]$base.sales)

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
  
  #list.coef <- row.names(is.nan(coef(summary(res0))[, "Estimate"]) == TRUE)
  list.coef <- row.names(coef(summary(res0))[grepl(list.leads[k], row.names(coef(summary(res0)))),])
  #temp <- as.data.table(coef(summary(res0))[list.coef,])
  #temp$names <- list.coef
  #temp <- temp[is.nan(Estimate) == FALSE]
  #list.coef <- temp$names  ## List of variable names for each lead/lag that is non "NaN"
  
  ## Equal weights across cohorts
  weights <- 1/length(list.coef)

  avg.coef <- paste(list.coef, collapse = paste("*", weights, " +", sep = ""))
  avg.coef <- paste(avg.coef, "*", weights, sep = "") ## Add one more at the end
  avg.coef.form <- paste0(avg.coef, " = 0", sep = "")
  avg.coef.test <- glht(res0, linfct = c(avg.coef.form))
  avg.pre.est <- coef(summary(avg.coef.test))[[1]]
  avg.pre.se <- sqrt(vcov(summary(avg.coef.test)))[[1]]
  avg.pre.pval <- 2*(1-pnorm(abs(avg.pre.est/avg.pre.se)))
    
  }
  
  # Take sums of pre-, post- and total effect
  # Take linear combinations of coefficients
  #Total pre-period
  lc.pre <- "lead2 + lead1"
  lc.pre.form <- paste0(lc.pre, " = 0", sep = "")
  lc.pre.test <- glht(res0, linfct = c(lc.pre.form))
  lc.pre.est <- coef(summary(lc.pre.test))[[1]]
  lc.pre.se <- sqrt(vcov(summary(lc.pre.test)))[[1]]
  lc.pre.pval <- 2*(1 - pnorm(abs(lc.pre.est/lc.pre.se)))
  
  ## Save results
  LRdiff_res <- rbind(
    LRdiff_res,
    data.table(
      outcome = outcome.j,
      estimate = lc.pre.est,
      se = lc.pre.se,
      pval = lc.pre.pval,
      Rsq = summary(res0)$r.squared,
      adj.Rsq = summary(res0)$adj.r.squared,
      time_controls = "year FE",
      lead_lag = "avg. pre"
    )
  )
  
  
  #Total post-period
  lc.post <- "dltax + lag1 + lag2"
  lc.post.form <- paste0(lc.post, " = 0", sep = "")
  lc.post.test <- glht(res0, linfct = c(lc.post.form))
  lc.post.est <- coef(summary(lc.post.test))[[1]]
  lc.post.se <- sqrt(vcov(summary(lc.post.test)))[[1]]
  lc.post.pval <- 2*(1 - pnorm(abs(lc.post.est/lc.post.se)))
  
  ## Save results
  LRdiff_res <- rbind(
    LRdiff_res,
    data.table(
      outcome = outcome.j,
      estimate = lc.post.est,
      se = lc.post.se,
      pval = lc.post.pval,
      Rsq = summary(res0)$r.squared,
      adj.Rsq = summary(res0)$adj.r.squared,
      time_controls = "year FE",
      lead_lag = "avg. post"
    )
  )
  
  
  #Total pre- and post-period
  lc.tot <- "lead2 + lead1 + dltax + lag1 + lag2"
  lc.tot.form <- paste0(lc.tot, " = 0", sep = "")
  lc.tot.test <- glht(res0, linfct = c(lc.tot.form))
  lc.tot.est <- coef(summary(lc.tot.test))[[1]]
  lc.tot.se <- sqrt(vcov(summary(lc.tot.test)))[[1]]
  lc.tot.pval <- 2*(1 - pnorm(abs(lc.tot.est/lc.tot.se)))
  
  ## Save results
  LRdiff_res <- rbind(
    LRdiff_res,
    data.table(
      outcome = outcome.j,
      estimate = lc.tot.est,
      se = lc.tot.se,
      pval = lc.tot.pval,
      Rsq = summary(res0)$r.squared,
      adj.Rsq = summary(res0)$adj.r.squared,
      time_controls = "year FE",
      lead_lag = "avg. total effect"
    )
  )
  


}