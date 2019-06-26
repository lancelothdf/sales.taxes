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

zillow_path <- "Data/covariates/zillow_long_by_county_clean.csv"
zillow_state_path <- "Data/covariates/zillow_long_by_state_clean.csv"
unemp.path <- "Data/covariates/county_monthly_unemp_clean.csv"


###OUTPUT
output.results.file <- "Data/LRdiff_results_diff_cohortbycohort.csv"


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


###
yearly_data <- merge(yearly_data, census.regions, by = c("fips_state", "fips_county"), all.x = T)

yearly_data[, region_by_time := .GRP, by = .(Region, year)]
yearly_data[, division_by_time := .GRP, by = .(Division, year)]
yearly_data[, region_by_module_by_time := .GRP, by = .(Region, product_module_code, year)]
yearly_data[, division_by_module_by_time := .GRP, by = .(Division, product_module_code, year)]
yearly_data[, cal_time := year]  ## Useless but impose it by symmetry with other R files


######## Import and prep house price and unemployment data

### Start with house prices
# First build a frame to make sure we can assign every county a home price
all_counties <- unique(yearly_data[, .(fips_state, fips_county)])
county_skeleton <- data.table(NULL)
for (X in 2006:2016) {
  for (Y in 1:12) {
    all_counties[, year := X]
    all_counties[, month := Y]
    county_skeleton <- rbind(county_skeleton, all_counties)
  }
}

## prep house price data
zillow_dt <- fread(zillow_path)
zillow_dt <- zillow_dt[between(year, 2006, 2016)]
zillow_dt <- zillow_dt[, .(fips_state, fips_county, median_home_price, year, month)]
zillow_dt <- merge(county_skeleton, zillow_dt, all.x = T,
                   by = c("fips_state", "fips_county", "year", "month"))

## prep state-level house prices (for when county-level is missing)
zillow_state_dt <- fread(zillow_state_path)
zillow_state_dt <- zillow_state_dt[between(year, 2006, 2016)]
zillow_state_dt <- zillow_state_dt[, .(fips_state, median_home_price, year, month)]
setnames(zillow_state_dt, "median_home_price", "state_median_home_price")
zillow_state_dt$month <- as.integer(round(zillow_state_dt$month))

zillow_dt <- merge(zillow_dt, zillow_state_dt, all.x = T,
                   by = c("fips_state", "year", "month"))
zillow_dt[is.na(median_home_price), median_home_price := state_median_home_price]
zillow_dt[, state_median_home_price := NULL]


## collapse to years
zillow_dt <- zillow_dt[, list(ln_home_price = log(mean(median_home_price))),
                       by = .(year, fips_state, fips_county)]

##
yearly_data <- merge(yearly_data, zillow_dt, by = c("fips_state", "fips_county", "year"), all.x = T)


### Unemployment data
unemp.data <- fread(unemp.path)
unemp.data <- unemp.data[, c("fips_state", "fips_county", "year", "month", "rate")]
unemp.data <- unemp.data[, list(unemp = mean(rate)), by = .(year, fips_state, fips_county)]
unemp.data <- unemp.data[year >= 2006 & year <= 2016,]
unemp.data <- unemp.data[, ln_unemp := log(unemp)]


##
yearly_data <- merge(yearly_data, unemp.data, by = c("fips_state", "fips_county", "year"), all.x = T)



### First, difference the relevant variables
yearly_data <- yearly_data[order(store_code_uc, product_module_code, year),] ##Sort on store by year (year in ascending order)


## Difference the outcomes, treatment and econ covariates
yearly_data[, D.ln_cpricei := ln_cpricei - shift(ln_cpricei, n=1, type="lag"),
            by = .(store_code_uc, product_module_code)]

yearly_data[, D.ln_cpricei2 := ln_cpricei2 - shift(ln_cpricei2, n=1, type="lag"),
            by = .(store_code_uc, product_module_code)]

yearly_data[, D.ln_quantity := ln_quantity - shift(ln_quantity, n=1, type="lag"),
            by = .(store_code_uc, product_module_code)]

yearly_data[, D.ln_quantity2 := ln_quantity2 - shift(ln_quantity2, n=1, type="lag"),
            by = .(store_code_uc, product_module_code)]

yearly_data[, D.ln_sales_tax := ln_sales_tax - shift(ln_sales_tax, n=1, type="lag"),
            by = .(store_code_uc, product_module_code)]

yearly_data[, D.ln_unemp := ln_unemp - shift(ln_unemp, n=1, type = "lag"),
            by = .(store_code_uc, product_module_code)]

yearly_data[, D.ln_home_price := ln_home_price - shift(ln_home_price, n=1, type = "lag"),
            by = .(store_code_uc, product_module_code)]



## Create 2 - year differences
yearly_data[, D2.ln_unemp := ln_unemp - shift(ln_unemp, n=2, type = "lag"),
            by = .(store_code_uc, product_module_code)]

yearly_data[, D2.ln_home_price := ln_home_price - shift(ln_home_price, n=2, type = "lag"),
            by = .(store_code_uc, product_module_code)]


#############################################
#Lag and lead of D.ln_sales_tax, D.ln_unemp and D.ln_home_price
yearly_data <- yearly_data[order(fips_state, fips_county, store_code_uc, product_module_code, year), c("L1.D.ln_sales_tax", "L2.D.ln_sales_tax") := shift(.SD, 1:2, type = "lag"), .SDcols = "D.ln_sales_tax", by = c("fips_state", "fips_county", "store_code_uc", "product_module_code")]
yearly_data <- yearly_data[order(fips_state, fips_county, store_code_uc, product_module_code, year), c("F1.D.ln_sales_tax", "F2.D.ln_sales_tax") := shift(.SD, 1:2, type = "lead"), .SDcols = "D.ln_sales_tax", by = c("fips_state", "fips_county", "store_code_uc", "product_module_code")]

yearly_data <- yearly_data[order(fips_state, fips_county, store_code_uc, product_module_code, year), c("L1.D.ln_unemp", "L2.D.ln_unemp") := shift(.SD, 1:2, type = "lag"), .SDcols = "D.ln_unemp", by = c("fips_state", "fips_county", "store_code_uc", "product_module_code")]
yearly_data <- yearly_data[order(fips_state, fips_county, store_code_uc, product_module_code, year), c("F1.D.ln_unemp", "F2.D.ln_unemp") := shift(.SD, 1:2, type = "lead"), .SDcols = "D.ln_unemp", by = c("fips_state", "fips_county", "store_code_uc", "product_module_code")]
yearly_data <- yearly_data[order(fips_state, fips_county, store_code_uc, product_module_code, year), c("L1.D.ln_home_price", "L2.D.ln_home_price") := shift(.SD, 1:2, type = "lag"), .SDcols = "D.ln_home_price", by = c("fips_state", "fips_county", "store_code_uc", "product_module_code")]
yearly_data <- yearly_data[order(fips_state, fips_county, store_code_uc, product_module_code, year), c("F1.D.ln_home_price", "F2.D.ln_home_price") := shift(.SD, 1:2, type = "lead"), .SDcols = "D.ln_home_price", by = c("fips_state", "fips_county", "store_code_uc", "product_module_code")]


##Create a lead and lag of the 2-year difference in unemployment and home price
yearly_data <- yearly_data[order(fips_state, fips_county, store_code_uc, product_module_code, year), c("F2.D2.ln_unemp") := shift(.SD, 2, type = "lead"), .SDcols = "D2.ln_unemp", by = c("fips_state", "fips_county", "store_code_uc", "product_module_code")]
yearly_data <- yearly_data[order(fips_state, fips_county, store_code_uc, product_module_code, year), c("L1.D2.ln_unemp") := shift(.SD, 1, type = "lag"), .SDcols = "D2.ln_unemp", by = c("fips_state", "fips_county", "store_code_uc", "product_module_code")]
yearly_data <- yearly_data[order(fips_state, fips_county, store_code_uc, product_module_code, year), c("F2.D2.ln_home_price") := shift(.SD, 2, type = "lead"), .SDcols = "D2.ln_home_price", by = c("fips_state", "fips_county", "store_code_uc", "product_module_code")]
yearly_data <- yearly_data[order(fips_state, fips_county, store_code_uc, product_module_code, year), c("L1.D2.ln_home_price") := shift(.SD, 1, type = "lag"), .SDcols = "D2.ln_home_price", by = c("fips_state", "fips_county", "store_code_uc", "product_module_code")]


#############################################

## Save some Memory and keep only variables we are going to need below
yearly_data <- yearly_data[, c("fips_state", "fips_county", "year", "store_code_uc", "product_module_code", "base.sales", "state_by_module", "module_by_time", "region_by_module_by_time", "division_by_module_by_time", "cal_time", "D.ln_cpricei", "D.ln_cpricei2", "D.ln_quantity", "D.ln_quantity2", "D.ln_sales_tax", "D.ln_unemp", "D.ln_home_price", "L1.D.ln_sales_tax", "L2.D.ln_sales_tax", "F1.D.ln_sales_tax", "F2.D.ln_sales_tax", "L1.D.ln_unemp", "L2.D.ln_unemp", "F1.D.ln_unemp", "F2.D.ln_unemp", "L1.D.ln_home_price", "L2.D.ln_home_price", "F1.D.ln_home_price", "F2.D.ln_home_price", "D2.ln_unemp", "D2.ln_home_price", "F2.D2.ln_unemp", "L1.D2.ln_unemp", "F2.D2.ln_home_price", "L1.D2.ln_home_price")]


## !!! Only include years for which we actually have the outcome and tax rate
## ---------------------------------------------------
yearly_data <- yearly_data[year >= 2009 & year <= 2014,] ## We lose 2008 because we are looking at differences

#For fully interacted regression, it will be useful to generate directly the interaction terms (because if we do it using the interaction command in ivreg, it will include terms like 2009Xlag1 and 2009Xlag2 wich are zero for all observations)
yearly_data[ , y2009.D.ln_sales_tax := (year == 2009)*D.ln_sales_tax]
yearly_data[ , y2009.F1.D.ln_sales_tax := (year == 2009)*F1.D.ln_sales_tax]
yearly_data[ , y2009.F2.D.ln_sales_tax := (year == 2009)*F2.D.ln_sales_tax]
yearly_data[ , y2010.L1.D.ln_sales_tax := (year == 2010)*L1.D.ln_sales_tax]
yearly_data[ , y2010.D.ln_sales_tax := (year == 2010)*D.ln_sales_tax]
yearly_data[ , y2010.F1.D.ln_sales_tax := (year == 2010)*F1.D.ln_sales_tax]
yearly_data[ , y2010.F2.D.ln_sales_tax := (year == 2010)*F2.D.ln_sales_tax]
yearly_data[ , y2011.L2.D.ln_sales_tax := (year == 2011)*L2.D.ln_sales_tax]
yearly_data[ , y2011.L1.D.ln_sales_tax := (year == 2011)*L1.D.ln_sales_tax]
yearly_data[ , y2011.D.ln_sales_tax := (year == 2011)*D.ln_sales_tax]
yearly_data[ , y2011.F1.D.ln_sales_tax := (year == 2011)*F1.D.ln_sales_tax]
yearly_data[ , y2011.F2.D.ln_sales_tax := (year == 2011)*F2.D.ln_sales_tax]
yearly_data[ , y2012.L2.D.ln_sales_tax := (year == 2012)*L2.D.ln_sales_tax]
yearly_data[ , y2012.L1.D.ln_sales_tax := (year == 2012)*L1.D.ln_sales_tax]
yearly_data[ , y2012.D.ln_sales_tax := (year == 2012)*D.ln_sales_tax]
yearly_data[ , y2012.F1.D.ln_sales_tax := (year == 2012)*F1.D.ln_sales_tax]
yearly_data[ , y2012.F2.D.ln_sales_tax := (year == 2012)*F2.D.ln_sales_tax]
yearly_data[ , y2013.L2.D.ln_sales_tax := (year == 2013)*L2.D.ln_sales_tax]
yearly_data[ , y2013.L1.D.ln_sales_tax := (year == 2013)*L1.D.ln_sales_tax]
yearly_data[ , y2013.D.ln_sales_tax := (year == 2013)*D.ln_sales_tax]
yearly_data[ , y2013.F1.D.ln_sales_tax := (year == 2013)*F1.D.ln_sales_tax]
yearly_data[ , y2014.L2.D.ln_sales_tax := (year == 2014)*L2.D.ln_sales_tax]
yearly_data[ , y2014.L1.D.ln_sales_tax := (year == 2014)*L1.D.ln_sales_tax]
yearly_data[ , y2014.D.ln_sales_tax := (year == 2014)*D.ln_sales_tax]

#############################################
## Make sure that year is treated as factor variable
yearly_data$year <- factor(yearly_data$year)


### Second, run the diff-in-diff specifications (in first differences) and interact all leads, lags and current tax rate with year fixed effects
### Second, run the diff-in-diff specifications (in first differences)
list.outcomes <- c("D.ln_cpricei", "D.ln_cpricei2", "D.ln_quantity", "D.ln_quantity2")
FE_opts <- c("cal_time", "module_by_time", "region_by_module_by_time", "division_by_module_by_time")
Econ_opts <- c("D.ln_unemp:year", "D.ln_home_price:year", "D.ln_unemp:year + D.ln_home_price:year")
Econ_opts2 <- c("D.ln_unemp:year + F2.D2.ln_unemp:year + L1.D2.ln_unemp:year", "D.ln_unemp:year + D.ln_home_price:year + F2.D2.ln_unemp:year + F2.D2.ln_home_price:year + L1.D2.ln_unemp:year + L1.D2.ln_home_price:year")


## Create a matrix with controls for econ conditions that include leads and lags - also store indicators that will be used in final matrix with results
Econ_w_lags <- c("D.ln_unemp", "D.ln_unemp", "D.ln_unemp + D.ln_home_price", "D.ln_unemp + D.ln_home_price")
Econ_w_lags <- rbind(Econ_w_lags, c("Yes", "Yes", "Yes", "Yes"))
Econ_w_lags <- rbind(Econ_w_lags, c("No", "Yes", "No", "Yes"))

Econ_w_lags <- rbind(Econ_w_lags, c("L2.D.ln_unemp:year + L1.D.ln_unemp:year + D.ln_unemp:year", "L2.D.ln_unemp:year + L1.D.ln_unemp:year + D.ln_unemp:year + F1.D.ln_unemp:year + F2.D.ln_unemp:year", "L2.D.ln_unemp:year + L1.D.ln_unemp:year + L2.D.ln_home_price:year + L1.D.ln_home_price:year + D.ln_unemp:year + D.ln_home_price:year", "L2.D.ln_unemp:year + L1.D.ln_unemp:year + L2.D.ln_home_price:year + L1.D.ln_home_price:year + D.ln_unemp:year + D.ln_home_price:year + F1.D.ln_unemp:year + F2.D.ln_unemp:year + F1.D.ln_home_price:year + F2.D.ln_home_price:year"))


##Create basic formula
basic.form <- "y2009.D.ln_sales_tax + y2009.F1.D.ln_sales_tax + y2009.F2.D.ln_sales_tax + y2010.L1.D.ln_sales_tax + y2010.D.ln_sales_tax + y2010.F1.D.ln_sales_tax + y2010.F2.D.ln_sales_tax + y2011.L2.D.ln_sales_tax + y2011.L1.D.ln_sales_tax + y2011.D.ln_sales_tax + y2011.F1.D.ln_sales_tax + y2011.F2.D.ln_sales_tax + y2012.L2.D.ln_sales_tax + y2012.L1.D.ln_sales_tax + y2012.D.ln_sales_tax + y2012.F1.D.ln_sales_tax + y2012.F2.D.ln_sales_tax + y2013.L2.D.ln_sales_tax + y2013.L1.D.ln_sales_tax + y2013.D.ln_sales_tax + y2013.F1.D.ln_sales_tax + y2014.L2.D.ln_sales_tax + y2014.L1.D.ln_sales_tax + y2014.D.ln_sales_tax"

LRdiff_res <- data.table(NULL)
for(j in 1:length(list.outcomes)) {
  for(FE in FE_opts) {

  #
  outcome.j <- list.outcomes[j]


  ##
  formula1 <- as.formula(paste0(
    outcome.j, " ~ ", basic.form, " | ", FE, " | 0 | state_by_module "
  ))

  res1 <- felm(data = yearly_data,
               formula = formula1,
               weights = yearly_data$base.sales)


  ## attach results
  flog.info("Writing results...")
  res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
  res1.dt[, outcome := outcome.j]
  res1.dt[, controls := FE]
  res1.dt[, econ := "none"]
  res1.dt[, lag.econ := NA]
  res1.dt[, lead.econ := NA]
  res1.dt[, Rsq := summary(res1)$r.squared]
  res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
  LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
  fwrite(LRdiff_res, output.results.file)


  ##############
  ##Average each lead/lag over cohorts
  # Take averages across cohorts
  list.leads <- c(".F2.D.ln_sales_tax", ".F1.D.ln_sales_tax", "20[0-9][0-9].D.ln_sales_tax", ".L1.D.ln_sales_tax", ".L2.D.ln_sales_tax")
  for(k in 1:length(list.leads)) {

    #Create list of year specific coefficients corresponding to lead/lag k
    list.coef <- row.names(coef(summary(res1))[grepl(list.leads[k], row.names(coef(summary(res1)))),])

    ## Equal weights across cohorts
    weights <- 1/length(list.coef)

    avg.coef <- paste(list.coef, collapse = paste("*", weights, " +", sep = ""))
    avg.coef <- paste(avg.coef, "*", weights, sep = "") ## Add one more at the end
    avg.coef.form <- paste0(avg.coef, " = 0", sep = "")
    avg.coef.test <- glht(res1, linfct = c(avg.coef.form))
    avg.pre.est <- coef(summary(avg.coef.test))[[1]]
    avg.pre.se <- sqrt(vcov(summary(avg.coef.test)))[[1]]
    avg.pre.pval <- 2*(1-pnorm(abs(avg.pre.est/avg.pre.se)))


    ## Append results
    lp.dt <- data.table(
      rn = paste("avg.", list.leads[k]),
      Estimate = avg.pre.est,
      `Cluster s.e.` = avg.pre.se,
      `Pr(>|t|)` = avg.pre.pval,
      outcome = outcome.j,
      controls = FE,
      econ = "none",
      lag.econ = NA,
      lead.econ = NA,
      Rsq = summary(res1)$r.squared,
      adj.Rsq = summary(res1)$adj.r.squared)
    LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T)

  }
  fwrite(LRdiff_res, output.results.file)


  ## sum leads (+ average across cohorts)
  flog.info("Summing leads...")
  list.coef.lead2 <- row.names(coef(summary(res1))[grepl(".F2.D.ln_sales_tax", row.names(coef(summary(res1)))),])
  weights <- 1/length(list.coef.lead2)
  avg.coef.lead2 <- paste(list.coef.lead2, collapse = paste("*", weights, " +", sep = ""))
  avg.coef.lead2 <- paste(avg.coef.lead2, "*", weights, sep = "") ## Add one more at the end

  list.coef.lead1 <- row.names(coef(summary(res1))[grepl(".F1.D.ln_sales_tax", row.names(coef(summary(res1)))),])
  weights <- 1/length(list.coef.lead1)
  avg.coef.lead1 <- paste(list.coef.lead1, collapse = paste("*", weights, " +", sep = ""))
  avg.coef.lead1 <- paste(avg.coef.lead1, "*", weights, sep = "") ## Add one more at the end

  avg.coef <- paste0(avg.coef.lead2, " + ", avg.coef.lead1, sep = "")
  avg.coef.form <- paste0(avg.coef, " = 0", sep = "")
  avg.coef.test <- glht(res1, linfct = c(avg.coef.form))
  avg.pre.est <- coef(summary(avg.coef.test))[[1]]
  avg.pre.se <- sqrt(vcov(summary(avg.coef.test)))[[1]]
  avg.pre.pval <- 2*(1-pnorm(abs(avg.pre.est/avg.pre.se)))


  ## Append results
  lp.dt <- data.table(
    rn = "avg.pre.D.ln_sales_tax",
    Estimate = avg.pre.est,
    `Cluster s.e.` = avg.pre.se,
    `Pr(>|t|)` = avg.pre.pval,
    outcome = outcome.j,
    controls = FE,
    econ = "none",
    lag.econ = NA,
    lead.econ = NA,
    Rsq = summary(res1)$r.squared,
    adj.Rsq = summary(res1)$adj.r.squared)
  LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T)

  fwrite(LRdiff_res, output.results.file)



  ## Sum across lags
  flog.info("Summing lags...")
  list.coef.dltax <- row.names(coef(summary(res1))[grepl("20[0-9][0-9].D.ln_sales_tax", row.names(coef(summary(res1)))),])
  weights <- 1/length(list.coef.dltax)
  avg.coef.dltax <- paste(list.coef.dltax, collapse = paste("*", weights, " +", sep = ""))
  avg.coef.dltax <- paste(avg.coef.dltax, "*", weights, sep = "") ## Add one more at the end

  list.coef.lag1 <- row.names(coef(summary(res1))[grepl(".L1.", row.names(coef(summary(res1)))),])
  weights <- 1/length(list.coef.lag1)
  avg.coef.lag1 <- paste(list.coef.lag1, collapse = paste("*", weights, " +", sep = ""))
  avg.coef.lag1 <- paste(avg.coef.lag1, "*", weights, sep = "") ## Add one more at the end

  list.coef.lag2 <- row.names(coef(summary(res1))[grepl(".L2.", row.names(coef(summary(res1)))),])
  weights <- 1/length(list.coef.lag2)
  avg.coef.lag2 <- paste(list.coef.lag2, collapse = paste("*", weights, " +", sep = ""))
  avg.coef.lag2 <- paste(avg.coef.lag2, "*", weights, sep = "") ## Add one more at the end


  avg.coef <- paste0(avg.coef.dltax, " + ", avg.coef.lag1, " + ", avg.coef.lag2, sep = "")
  avg.coef.form <- paste0(avg.coef, " = 0", sep = "")
  avg.coef.test <- glht(res1, linfct = c(avg.coef.form))
  avg.pre.est <- coef(summary(avg.coef.test))[[1]]
  avg.pre.se <- sqrt(vcov(summary(avg.coef.test)))[[1]]
  avg.pre.pval <- 2*(1-pnorm(abs(avg.pre.est/avg.pre.se)))


  ## Append results
  lp.dt <- data.table(
    rn = "avg.post.D.ln_sales_tax",
    Estimate = avg.pre.est,
    `Cluster s.e.` = avg.pre.se,
    `Pr(>|t|)` = avg.pre.pval,
    outcome = outcome.j,
    controls = FE,
    econ = "none",
    lag.econ = NA,
    lead.econ = NA,
    Rsq = summary(res1)$r.squared,
    adj.Rsq = summary(res1)$adj.r.squared)
  LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T)

  fwrite(LRdiff_res, output.results.file)


  ## sum all
  flog.info("Summing all...")
  avg.coef <- paste0(avg.coef.lead2, " + ", avg.coef.lead1, " + ", avg.coef.dltax, " + ", avg.coef.lag1, " + ", avg.coef.lag2, sep = "")
  avg.coef.form <- paste0(avg.coef, " = 0", sep = "")
  avg.coef.test <- glht(res1, linfct = c(avg.coef.form))
  avg.pre.est <- coef(summary(avg.coef.test))[[1]]
  avg.pre.se <- sqrt(vcov(summary(avg.coef.test)))[[1]]
  avg.pre.pval <- 2*(1-pnorm(abs(avg.pre.est/avg.pre.se)))

  ## Append results
  lp.dt <- data.table(
    rn = "avg.total.D.ln_sales_tax",
    Estimate = avg.pre.est,
    `Cluster s.e.` = avg.pre.se,
    `Pr(>|t|)` = avg.pre.pval,
    outcome = outcome.j,
    controls = FE,
    econ = "none",
    lag.econ = NA,
    lead.econ = NA,
    Rsq = summary(res1)$r.squared,
    adj.Rsq = summary(res1)$adj.r.squared)
  LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T)

  fwrite(LRdiff_res, output.results.file)



  ########
  ##
  for(EC in c(Econ_opts, Econ_opts2)) {

    #
    outcome.j <- list.outcomes[j]


    ##
    formula1 <- as.formula(paste0(
      outcome.j, " ~ ", basic.form, " + ", EC, " | ", FE, " | 0 | state_by_module "
    ))

    res1 <- felm(data = yearly_data,
                 formula = formula1,
                 weights = yearly_data$base.sales)


    ## attach results
    flog.info("Writing results...")
    res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
    res1.dt[, outcome := outcome.j]
    res1.dt[, controls := FE]
    res1.dt[, econ := EC]
    res1.dt[, lag.econ := "No"]
    res1.dt[, lead.econ := "No"]
    res1.dt[, Rsq := summary(res1)$r.squared]
    res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)


    ##############
    ##Average each lead/lag over cohorts
    # Take averages across cohorts
    list.leads <- c(".F2.D.ln_sales_tax", ".F1.D.ln_sales_tax", ".L1.D.ln_sales_tax", ".L2.D.ln_sales_tax")
    for(k in 1:length(list.leads)) {

      #Create list of year specific coefficients corresponding to lead/lag k
      list.coef <- row.names(coef(summary(res1))[grepl(list.leads[k], row.names(coef(summary(res1)))),])

      ## Equal weights across cohorts
      weights <- 1/length(list.coef)

      avg.coef <- paste(list.coef, collapse = paste("*", weights, " +", sep = ""))
      avg.coef <- paste(avg.coef, "*", weights, sep = "") ## Add one more at the end
      avg.coef.form <- paste0(avg.coef, " = 0", sep = "")
      avg.coef.test <- glht(res1, linfct = c(avg.coef.form))
      avg.pre.est <- coef(summary(avg.coef.test))[[1]]
      avg.pre.se <- sqrt(vcov(summary(avg.coef.test)))[[1]]
      avg.pre.pval <- 2*(1-pnorm(abs(avg.pre.est/avg.pre.se)))


      ## Append results
      lp.dt <- data.table(
        rn = paste("avg.", list.leads[k]),
        Estimate = avg.pre.est,
        `Cluster s.e.` = avg.pre.se,
        `Pr(>|t|)` = avg.pre.pval,
        outcome = outcome.j,
        controls = FE,
        econ = EC,
        lag.econ = "No",
        lead.econ = "No",
        Rsq = summary(res1)$r.squared,
        adj.Rsq = summary(res1)$adj.r.squared)
      LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T)

    }
    fwrite(LRdiff_res, output.results.file)


    ## sum leads (+ average across cohorts)
    flog.info("Summing leads...")
    list.coef.lead2 <- row.names(coef(summary(res1))[grepl(".F2.D.ln_sales_tax", row.names(coef(summary(res1)))),])
    weights <- 1/length(list.coef.lead2)
    avg.coef.lead2 <- paste(list.coef.lead2, collapse = paste("*", weights, " +", sep = ""))
    avg.coef.lead2 <- paste(avg.coef.lead2, "*", weights, sep = "") ## Add one more at the end

    list.coef.lead1 <- row.names(coef(summary(res1))[grepl(".F1.D.ln_sales_tax", row.names(coef(summary(res1)))),])
    weights <- 1/length(list.coef.lead1)
    avg.coef.lead1 <- paste(list.coef.lead1, collapse = paste("*", weights, " +", sep = ""))
    avg.coef.lead1 <- paste(avg.coef.lead1, "*", weights, sep = "") ## Add one more at the end

    avg.coef <- paste0(avg.coef.lead2, " + ", avg.coef.lead1, sep = "")
    avg.coef.form <- paste0(avg.coef, " = 0", sep = "")
    avg.coef.test <- glht(res1, linfct = c(avg.coef.form))
    avg.pre.est <- coef(summary(avg.coef.test))[[1]]
    avg.pre.se <- sqrt(vcov(summary(avg.coef.test)))[[1]]
    avg.pre.pval <- 2*(1-pnorm(abs(avg.pre.est/avg.pre.se)))


    ## Append results
    lp.dt <- data.table(
      rn = "avg.pre.D.ln_sales_tax",
      Estimate = avg.pre.est,
      `Cluster s.e.` = avg.pre.se,
      `Pr(>|t|)` = avg.pre.pval,
      outcome = outcome.j,
      controls = FE,
      econ = EC,
      lag.econ = "No",
      lead.econ = "No",
      Rsq = summary(res1)$r.squared,
      adj.Rsq = summary(res1)$adj.r.squared)
    LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T)

    fwrite(LRdiff_res, output.results.file)



    ## Sum across lags
    flog.info("Summing lags...")
    list.coef.dltax <- row.names(coef(summary(res1))[grepl("20[0-9][0-9].D.ln_sales_tax", row.names(coef(summary(res1)))),])
    weights <- 1/length(list.coef.dltax)
    avg.coef.dltax <- paste(list.coef.dltax, collapse = paste("*", weights, " +", sep = ""))
    avg.coef.dltax <- paste(avg.coef.dltax, "*", weights, sep = "") ## Add one more at the end

    list.coef.lag1 <- row.names(coef(summary(res1))[grepl(".L1.", row.names(coef(summary(res1)))),])
    weights <- 1/length(list.coef.lag1)
    avg.coef.lag1 <- paste(list.coef.lag1, collapse = paste("*", weights, " +", sep = ""))
    avg.coef.lag1 <- paste(avg.coef.lag1, "*", weights, sep = "") ## Add one more at the end

    list.coef.lag2 <- row.names(coef(summary(res1))[grepl(".L2.", row.names(coef(summary(res1)))),])
    weights <- 1/length(list.coef.lag2)
    avg.coef.lag2 <- paste(list.coef.lag2, collapse = paste("*", weights, " +", sep = ""))
    avg.coef.lag2 <- paste(avg.coef.lag2, "*", weights, sep = "") ## Add one more at the end


    avg.coef <- paste0(avg.coef.dltax, " + ", avg.coef.lag1, " + ", avg.coef.lag2, sep = "")
    avg.coef.form <- paste0(avg.coef, " = 0", sep = "")
    avg.coef.test <- glht(res1, linfct = c(avg.coef.form))
    avg.pre.est <- coef(summary(avg.coef.test))[[1]]
    avg.pre.se <- sqrt(vcov(summary(avg.coef.test)))[[1]]
    avg.pre.pval <- 2*(1-pnorm(abs(avg.pre.est/avg.pre.se)))


    ## Append results
    lp.dt <- data.table(
      rn = "avg.post.D.ln_sales_tax",
      Estimate = avg.pre.est,
      `Cluster s.e.` = avg.pre.se,
      `Pr(>|t|)` = avg.pre.pval,
      outcome = outcome.j,
      controls = FE,
      econ = EC,
      lag.econ = "No",
      lead.econ = "No",
      Rsq = summary(res1)$r.squared,
      adj.Rsq = summary(res1)$adj.r.squared)
    LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T)

    fwrite(LRdiff_res, output.results.file)


    ## sum all
    flog.info("Summing all...")
    avg.coef <- paste0(avg.coef.lead2, " + ", avg.coef.lead1, " + ", avg.coef.dltax, " + ", avg.coef.lag1, " + ", avg.coef.lag2, sep = "")
    avg.coef.form <- paste0(avg.coef, " = 0", sep = "")
    avg.coef.test <- glht(res1, linfct = c(avg.coef.form))
    avg.pre.est <- coef(summary(avg.coef.test))[[1]]
    avg.pre.se <- sqrt(vcov(summary(avg.coef.test)))[[1]]
    avg.pre.pval <- 2*(1-pnorm(abs(avg.pre.est/avg.pre.se)))

    ## Append results
    lp.dt <- data.table(
      rn = "avg.total.D.ln_sales_tax",
      Estimate = avg.pre.est,
      `Cluster s.e.` = avg.pre.se,
      `Pr(>|t|)` = avg.pre.pval,
      outcome = outcome.j,
      controls = FE,
      econ = EC,
      lag.econ = "No",
      lead.econ = "No",
      Rsq = summary(res1)$r.squared,
      adj.Rsq = summary(res1)$adj.r.squared)
    LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T)

    fwrite(LRdiff_res, output.results.file)

    }


  ########
  ##
  for(i in 1:dim(Econ_w_lags)[2]) {

    EC <- Econ_w_lags[4, i]

    #
    outcome.j <- list.outcomes[j]


    ##
    formula1 <- as.formula(paste0(
      outcome.j, " ~ ", basic.form, " + ", EC, " | ", FE, " | 0 | state_by_module "
    ))

    res1 <- felm(data = yearly_data,
                 formula = formula1,
                 weights = yearly_data$base.sales)


    ## attach results
    flog.info("Writing results...")
    res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
    res1.dt[, outcome := outcome.j]
    res1.dt[, controls := FE]
    res1.dt[, econ := Econ_w_lags[1,i]]
    res1.dt[, lag.econ := Econ_w_lags[2,i]]
    res1.dt[, lead.econ := Econ_w_lags[3,i]]
    res1.dt[, Rsq := summary(res1)$r.squared]
    res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)


    ##############
    ##Average each lead/lag over cohorts
    # Take averages across cohorts
    list.leads <- c(".F2.D.ln_sales_tax", ".F1.D.ln_sales_tax", ".L1.D.ln_sales_tax", ".L2.D.ln_sales_tax")
    for(k in 1:length(list.leads)) {

      #Create list of year specific coefficients corresponding to lead/lag k
      list.coef <- row.names(coef(summary(res1))[grepl(list.leads[k], row.names(coef(summary(res1)))),])

      ## Equal weights across cohorts
      weights <- 1/length(list.coef)

      avg.coef <- paste(list.coef, collapse = paste("*", weights, " +", sep = ""))
      avg.coef <- paste(avg.coef, "*", weights, sep = "") ## Add one more at the end
      avg.coef.form <- paste0(avg.coef, " = 0", sep = "")
      avg.coef.test <- glht(res1, linfct = c(avg.coef.form))
      avg.pre.est <- coef(summary(avg.coef.test))[[1]]
      avg.pre.se <- sqrt(vcov(summary(avg.coef.test)))[[1]]
      avg.pre.pval <- 2*(1-pnorm(abs(avg.pre.est/avg.pre.se)))


      ## Append results
      lp.dt <- data.table(
        rn = paste("avg.", list.leads[k]),
        Estimate = avg.pre.est,
        `Cluster s.e.` = avg.pre.se,
        `Pr(>|t|)` = avg.pre.pval,
        outcome = outcome.j,
        controls = FE,
        econ = Econ_w_lags[1,i],
        lag.econ = Econ_w_lags[2,i],
        lead.econ = Econ_w_lags[3,i],
        Rsq = summary(res1)$r.squared,
        adj.Rsq = summary(res1)$adj.r.squared)
      LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T)

    }
    fwrite(LRdiff_res, output.results.file)


    ## sum leads (+ average across cohorts)
    flog.info("Summing leads...")
    list.coef.lead2 <- row.names(coef(summary(res1))[grepl(".F2.D.ln_sales_tax", row.names(coef(summary(res1)))),])
    weights <- 1/length(list.coef.lead2)
    avg.coef.lead2 <- paste(list.coef.lead2, collapse = paste("*", weights, " +", sep = ""))
    avg.coef.lead2 <- paste(avg.coef.lead2, "*", weights, sep = "") ## Add one more at the end

    list.coef.lead1 <- row.names(coef(summary(res1))[grepl(".F1.D.ln_sales_tax", row.names(coef(summary(res1)))),])
    weights <- 1/length(list.coef.lead1)
    avg.coef.lead1 <- paste(list.coef.lead1, collapse = paste("*", weights, " +", sep = ""))
    avg.coef.lead1 <- paste(avg.coef.lead1, "*", weights, sep = "") ## Add one more at the end

    avg.coef <- paste0(avg.coef.lead2, " + ", avg.coef.lead1, sep = "")
    avg.coef.form <- paste0(avg.coef, " = 0", sep = "")
    avg.coef.test <- glht(res1, linfct = c(avg.coef.form))
    avg.pre.est <- coef(summary(avg.coef.test))[[1]]
    avg.pre.se <- sqrt(vcov(summary(avg.coef.test)))[[1]]
    avg.pre.pval <- 2*(1-pnorm(abs(avg.pre.est/avg.pre.se)))


    ## Append results
    lp.dt <- data.table(
      rn = "avg.pre.D.ln_sales_tax",
      Estimate = avg.pre.est,
      `Cluster s.e.` = avg.pre.se,
      `Pr(>|t|)` = avg.pre.pval,
      outcome = outcome.j,
      controls = FE,
      econ = Econ_w_lags[1,i],
      lag.econ = Econ_w_lags[2,i],
      lead.econ = Econ_w_lags[3,i],
      Rsq = summary(res1)$r.squared,
      adj.Rsq = summary(res1)$adj.r.squared)
    LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T)

    fwrite(LRdiff_res, output.results.file)



    ## Sum across lags
    flog.info("Summing lags...")
    list.coef.dltax <- row.names(coef(summary(res1))[grepl("20[0-9][0-9].D.ln_sales_tax", row.names(coef(summary(res1)))),])
    weights <- 1/length(list.coef.dltax)
    avg.coef.dltax <- paste(list.coef.dltax, collapse = paste("*", weights, " +", sep = ""))
    avg.coef.dltax <- paste(avg.coef.dltax, "*", weights, sep = "") ## Add one more at the end

    list.coef.lag1 <- row.names(coef(summary(res1))[grepl(".L1.", row.names(coef(summary(res1)))),])
    weights <- 1/length(list.coef.lag1)
    avg.coef.lag1 <- paste(list.coef.lag1, collapse = paste("*", weights, " +", sep = ""))
    avg.coef.lag1 <- paste(avg.coef.lag1, "*", weights, sep = "") ## Add one more at the end

    list.coef.lag2 <- row.names(coef(summary(res1))[grepl(".L2.", row.names(coef(summary(res1)))),])
    weights <- 1/length(list.coef.lag2)
    avg.coef.lag2 <- paste(list.coef.lag2, collapse = paste("*", weights, " +", sep = ""))
    avg.coef.lag2 <- paste(avg.coef.lag2, "*", weights, sep = "") ## Add one more at the end


    avg.coef <- paste0(avg.coef.dltax, " + ", avg.coef.lag1, " + ", avg.coef.lag2, sep = "")
    avg.coef.form <- paste0(avg.coef, " = 0", sep = "")
    avg.coef.test <- glht(res1, linfct = c(avg.coef.form))
    avg.pre.est <- coef(summary(avg.coef.test))[[1]]
    avg.pre.se <- sqrt(vcov(summary(avg.coef.test)))[[1]]
    avg.pre.pval <- 2*(1-pnorm(abs(avg.pre.est/avg.pre.se)))


    ## Append results
    lp.dt <- data.table(
      rn = "avg.post.D.ln_sales_tax",
      Estimate = avg.pre.est,
      `Cluster s.e.` = avg.pre.se,
      `Pr(>|t|)` = avg.pre.pval,
      outcome = outcome.j,
      controls = FE,
      econ = Econ_w_lags[1,i],
      lag.econ = Econ_w_lags[2,i],
      lead.econ = Econ_w_lags[3,i],
      Rsq = summary(res1)$r.squared,
      adj.Rsq = summary(res1)$adj.r.squared)
    LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T)

    fwrite(LRdiff_res, output.results.file)


    ## sum all
    flog.info("Summing all...")
    avg.coef <- paste0(avg.coef.lead2, " + ", avg.coef.lead1, " + ", avg.coef.dltax, " + ", avg.coef.lag1, " + ", avg.coef.lag2, sep = "")
    avg.coef.form <- paste0(avg.coef, " = 0", sep = "")
    avg.coef.test <- glht(res1, linfct = c(avg.coef.form))
    avg.pre.est <- coef(summary(avg.coef.test))[[1]]
    avg.pre.se <- sqrt(vcov(summary(avg.coef.test)))[[1]]
    avg.pre.pval <- 2*(1-pnorm(abs(avg.pre.est/avg.pre.se)))

    ## Append results
    lp.dt <- data.table(
      rn = "avg.total.D.ln_sales_tax",
      Estimate = avg.pre.est,
      `Cluster s.e.` = avg.pre.se,
      `Pr(>|t|)` = avg.pre.pval,
      outcome = outcome.j,
      controls = FE,
      econ = Econ_w_lags[1,i],
      lag.econ = Econ_w_lags[2,i],
      lead.econ = Econ_w_lags[3,i],
      Rsq = summary(res1)$r.squared,
      adj.Rsq = summary(res1)$adj.r.squared)
    LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T)

    fwrite(LRdiff_res, output.results.file)

  }



  }
}


## summary values --------------------------------------------------------------
LRdiff_res$N_obs <- nrow(yearly_data)
LRdiff_res$N_modules <- length(unique(yearly_data$product_module_code))
LRdiff_res$N_stores <- length(unique(yearly_data$store_code_uc))
LRdiff_res$N_counties <- uniqueN(yearly_data, by = c("fips_state", "fips_county"))
LRdiff_res$N_years <- uniqueN(yearly_data, by = c("year")) # should be 6 (we lose one because we difference)
LRdiff_res$N_county_modules <- uniqueN(yearly_data, by = c("fips_state", "fips_county",
                                                           "product_module_code"))

fwrite(LRdiff_res, output.results.file)

