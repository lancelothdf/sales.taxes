###
### In this do-file we explore the "very long-run specification": Diff-in-diff over 1) product and 2) stores

library(data.table)
library(lfe)
library(futile.logger)
library(AER)
library(multcomp)
library(splitstackshape)



setwd("/project2/igaarder")

## useful filepaths ------------------------------------------------------------
all_goods_pi_path <- "Data/Nielsen/price_quantity_indices_allitems_2006-2016_notaxinfo.csv"
FE_pindex_path <- "Data/Nielsen/Pindex_FE_yearly_all_years.csv"
output_yearly <- "Data/Nielsen/yearly_nielsen_data.csv"
pre_trend_data_path <- "Data/Nielsen/pre_trend_data_yearly.csv"
#taxable_pi_path <- "Data/Nielsen/price_quantity_indices_taxableitems_2006-2016.csv"
#eventstudy_tr_path <- "Data/event_study_tr_groups_comprehensive_firstonly_no2012q4_2013q1q2.csv"
#tr_groups_path <- "Data/tr_groups_comprehensive_firstonly_no2012q4_2013q1q2.csv"


###
covariates.nhgis.path <- "Data/covariates/nhgis_county_clean.csv"
covariates.qcew.path <- "Data/covariates/qcew_clean.csv"
census.regions.path <- "Data/covariates/census_regions.csv"

tax.path <- "Data/county_monthly_tax_rates.csv"

zillow_path <- "Data/covariates/zillow_long_by_county_clean.csv"
zillow_state_path <- "Data/covariates/zillow_long_by_state_clean.csv"
unemp.path <- "Data/covariates/county_monthly_unemp_clean.csv"
border.path <- "Data/border_counties.csv"



## Where to save results
output.results.file <- "Data/LRdiff_results_LRspec_borderstate_counties.csv"


# ### Prepare the data
## See LR_diff_diff_regression_sales_FE_specification.R -- the file output_yearly is prepared in that file
yearly_data <- fread(output_yearly)
yearly_data <- yearly_data[, -c("n")]  ## Lose the variable n because we are going to create a new one


### Drop observations for which the sales tax rate is imputed
yearly_data <- yearly_data[year >= 2008 & year <= 2014]
yearly_data$year <- factor(yearly_data$year) ##Convert the indicator for year to a factor variable (needed for interaction in the regression between ln_sales_tax and dummy for year)




###### Merge some covariates

## Time invariant covariates
list.counties <- data.frame(unique(yearly_data[,c('fips_state','fips_county')]))

#nhgis 2010
nhgis2010 <- fread(covariates.nhgis.path)
nhgis2010 <- nhgis2010[year == 2010,] ## Keep the 2000 values
nhgis2010 <- nhgis2010[, c("statefp", "countyfp", "pct_pop_over_65", "pct_pop_under_25", "pct_pop_black", "pct_pop_urban", "housing_ownership_share")]
names(nhgis2010) <- c("fips_state", "fips_county", "pct_pop_over_65", "pct_pop_under_25", "pct_pop_black", "pct_pop_urban", "housing_ownership_share")
covariates <- merge(list.counties, nhgis2010, by = c("fips_state", "fips_county"), all.x = T)


#nhgis 2000 (because education and median income variables are missing in 2010)
nhgis2000 <- fread(covariates.nhgis.path)
nhgis2000 <- nhgis2000[year == 2000,] ## Keep the 2000 values
nhgis2000 <- nhgis2000[, c("statefp", "countyfp", "pct_pop_no_college", "pct_pop_bachelors", "median_income")]
names(nhgis2000) <- c("fips_state", "fips_county", "pct_pop_no_college", "pct_pop_bachelors", "median_income")

nhgis2000[, median_income := log(median_income)]
covariates <- merge(covariates, nhgis2000, by = c("fips_state", "fips_county"), all.x = T)

#census regions/divisions
census.regions <- fread(census.regions.path)
census.regions[, Division := Region*10 + Division]
covariates <- merge(covariates, census.regions, by = c("fips_state"), all.x = T)

## Time variant covariates
list.obs <- data.frame(unique(yearly_data[,c('fips_state','fips_county', 'year')]))
covariates <- merge(list.obs, covariates, by = c("fips_county", "fips_state"), all.x = T)

#qcew
qcew <- fread(covariates.qcew.path)
qcew <- qcew[year >= 2008 & year <= 2014,]
qcew <- qcew[, fips_state := as.numeric(substr(area_fips, 1, 2))]
qcew <- qcew[, fips_county := as.numeric(substr(area_fips, 3, 5))]
qcew <- qcew[, ln_mean_wage := log(total_mean_wage)]
qcew <- qcew[, c("fips_state", "fips_county", "year", "ln_mean_wage")]
covariates <- merge(covariates, qcew, by = c("year", "fips_county", "fips_state"), all.x = T)


#Zillow
all_counties <- unique(yearly_data[, .(fips_state, fips_county)])
county_skeleton <- data.table(NULL)
for (X in 2008:2014) {
  for (Y in 1:12) {
    all_counties[, year := X]
    all_counties[, month := Y]
    county_skeleton <- rbind(county_skeleton, all_counties)
  }
}


zillow_dt <- fread(zillow_path)
zillow_dt <- zillow_dt[between(year, 2008, 2014)]
zillow_dt <- zillow_dt[, .(fips_state, fips_county, median_home_price, year, month)]
zillow_dt <- merge(county_skeleton, zillow_dt, all.x = T,
                   by = c("fips_state", "fips_county", "year", "month"))

## prep state-level house prices (for when county-level is missing)
zillow_state_dt <- fread(zillow_state_path)
zillow_state_dt <- zillow_state_dt[between(year, 2008, 2014)]
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
covariates <- merge(covariates, zillow_dt, by = c("year", "fips_county", "fips_state"), all.x = T)


### Unemployment data
unemp.data <- fread(unemp.path)
unemp.data <- unemp.data[, c("fips_state", "fips_county", "year", "month", "rate")]
unemp.data <- unemp.data[, list(unemp = mean(rate)), by = .(year, fips_state, fips_county)]
unemp.data <- unemp.data[year >= 2006 & year <= 2016,]
unemp.data <- unemp.data[, ln_unemp := log(unemp)]
unemp.data <- unemp.data[, c("year", "fips_state", "fips_county", "ln_unemp")]

covariates <- merge(covariates, unemp.data, by = c("year", "fips_county", "fips_state"), all.x = T)

#### Finally merge covariates to yearly_data
yearly_data <- merge(yearly_data, covariates, by = c("fips_state", "fips_county", "year"), all.x = T)
rm(nhgis2000, nhgis2010, census.regions, qcew, zillow_dt, zillow_state_dt, unemp.data, covariates)




#########################################################
## Make county border pairs

########## Import data with border counties
list.counties <- data.frame(unique(yearly_data[,c('fips_state','fips_county')]))
border.counties <- fread(border.path)

border.counties[ , fips_state := floor(fips_county/1000)]
border.counties[, fips_county := fips_county - fips_state*1000]

#Only keep counties that are in the Nielsen dataset
border.counties <- merge(list.counties, border.counties,by = c("fips_state", "fips_county"), all.x = T)

#Only keep pairs for which both counties are in the data
setDT(border.counties)
keep_counties <- border.counties[, list(n = .N),
                                 by = .(bordindx)]
keep_counties <- keep_counties[n == 2]

setkey(border.counties, bordindx)
setkey(keep_counties, bordindx)

border.counties <- border.counties[keep_counties]
border.counties <- border.counties[, c("fips_state", "fips_county", "bordindx")]


## Keep only pairs of counties that are on either side of a state border
border.counties[, min_state := min(fips_state), by = .(bordindx)]
border.counties[, max_state := max(fips_state), by = .(bordindx)]
border.counties <- border.counties[ min_state != max_state,]
border.counties <- border.counties[, c("fips_state", "fips_county", "bordindx")]


#Count the number of times each county appears in the data set (number of pairs that it is a part of)
n_counties <- border.counties[, list(n = .N),
                              by = .(fips_state, fips_county)]
border.counties[, id := seq_len(.N), by = .(fips_state, fips_county)] ##Create an ID within each county (that will map into a border index bordindx).  Later we will duplicate each observation in yearly_data as many times as this county appears in border pairs and will create the same id --> so that we can then assign a bordindx to each

## Duplicate each observation in yearly_data as many times as this county appears in the border dataset
yearly_data <- merge(yearly_data, n_counties, by = c("fips_state", "fips_county"), all.x = TRUE) #Merge number of times each county appears in list of border pairs
yearly_data <- yearly_data[!is.na(n)] ## Will lose a lot of observations - those that are not in "border counties"
yearly_data <- expandRows(yearly_data, "n", drop = FALSE) #Duplicate observations as many times as county appears in border county pairs
yearly_data <- yearly_data[ , id := seq_len(.N), by = .(store_code_uc, product_module_code, year)] #Create the same ID as in border.counties.


## Merge yearly_data with the bordindx IDs
yearly_data <- merge(yearly_data, border.counties, by = c("fips_state", "fips_county", "id"), all.x = T)

## Generate some module-time and pair-module-time FE
yearly_data[, module_by_time := .GRP, by = .(product_module_code, year)]
yearly_data[, pair_by_module_by_time := .GRP, by = .(bordindx, year, product_module_code)]


## Generate some regressions weights (want to divide usual weights by number of times the county appears in the dataset)
yearly_data[, weight := base.sales/n]



####
#################################################################################
outcomes <- c("ln_cpricei2", "ln_quantity2", "pct_pop_urban", "housing_ownership_share", "median_income", "pct_pop_no_college", "pct_pop_bachelors", "pct_pop_over_65", "pct_pop_under_25", "pct_pop_black", "ln_unemp", "ln_home_price")


cohort.weights <- rep(1, 7) ##Construct weights to average across cohorts/years.  Start with equal weights
cohort.weights <- cohort.weights/sum(cohort.weights)


LRdiff_res <- data.table(NULL)
for(Y in outcomes) {

  ### No pair fixed effects
  formula0 <- as.formula(paste0(
    Y, " ~ ln_sales_tax:year | module_by_time | 0 | state_by_module ", sep = ""
  ))

  res0 <- felm(data = yearly_data,
               formula = formula0,
               weights = yearly_data$weight)



  ## attach results
  flog.info("Writing results...")
  res1.dt <- data.table(coef(summary(res0)), keep.rownames=T)
  res1.dt[, outcome := Y]
  res1.dt[, Rsq := summary(res0)$r.squared]
  res1.dt[, adj.Rsq := summary(res0)$adj.r.squared]
  res1.dt[, specification := "LR"]
  res1.dt[, FE := "module_by_time"]
  res1.dt[, covariates := "No"]
  res1.dt[, year := "All"]
  LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
  fwrite(LRdiff_res, output.results.file)  ## Write results to a csv file


  ### Take linear combinations of coefficients and attach results (this is the coefficient of interest)
  lc.lr0 <- paste0(cohort.weights[1], "*ln_sales_tax:year2008 + ", cohort.weights[2], "*ln_sales_tax:year2009 + ", cohort.weights[3], "*ln_sales_tax:year2010 + ", cohort.weights[4], "*ln_sales_tax:year2011 + ", cohort.weights[5], "*ln_sales_tax:year2012 + ", cohort.weights[6], "*ln_sales_tax:year2013 + ", cohort.weights[7], "*ln_sales_tax:year2014", sep = "")
  lc.formula0 <- paste0(lc.lr0, " = 0", sep = "")
  lc.test0 <- glht(res0, linfct = c(lc.formula0))

  # Calculate the p-value
  pval <- 2*(1 - pnorm(abs(coef(summary(lc.test0))[[1]]/sqrt(vcov(summary(lc.test0)))[[1]])))


  lp.dt <- data.table(
    rn = "avg.ln_sales_tax",
    Estimate = coef(summary(lc.test0))[[1]],
    `Cluster s.e.` = sqrt(vcov(summary(lc.test0)))[[1]],
    `Pr(>|t|)` = pval,
    outcome = Y,
    Rsq = summary(res0)$r.squared,
    adj.Rsq = summary(res0)$adj.r.squared,
    specification = "LR",
    FE = "module_by_time",
    covariates = "No",
    year = "All")
  LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T) ## Merge results to LRdiff_res
  fwrite(LRdiff_res, output.results.file) ## Write resulting file to a csv file


  ### pair fixed effects
  formula0 <- as.formula(paste0(
    Y, " ~ ln_sales_tax:year | pair_by_module_by_time | 0 | state_by_module ", sep = ""
  ))

  res0 <- felm(data = yearly_data,
               formula = formula0,
               weights = yearly_data$weight)



  ## attach results
  flog.info("Writing results...")
  res1.dt <- data.table(coef(summary(res0)), keep.rownames=T)
  res1.dt[, outcome := Y]
  res1.dt[, Rsq := summary(res0)$r.squared]
  res1.dt[, adj.Rsq := summary(res0)$adj.r.squared]
  res1.dt[, specification := "LR"]
  res1.dt[, FE := "pair_by_module_by_time"]
  res1.dt[, covariates := "No"]
  res1.dt[, year := "All"]
  LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
  fwrite(LRdiff_res, output.results.file)  ## Write results to a csv file



  ### Take linear combinations of coefficients and attach results (this is the coefficient of interest)
  lc.lr0 <- paste0(cohort.weights[1], "*ln_sales_tax:year2008 + ", cohort.weights[2], "*ln_sales_tax:year2009 + ", cohort.weights[3], "*ln_sales_tax:year2010 + ", cohort.weights[4], "*ln_sales_tax:year2011 + ", cohort.weights[5], "*ln_sales_tax:year2012 + ", cohort.weights[6], "*ln_sales_tax:year2013 + ", cohort.weights[7], "*ln_sales_tax:year2014", sep = "")
  lc.formula0 <- paste0(lc.lr0, " = 0", sep = "")
  lc.test0 <- glht(res0, linfct = c(lc.formula0))

  # Calculate the p-value
  pval <- 2*(1 - pnorm(abs(coef(summary(lc.test0))[[1]]/sqrt(vcov(summary(lc.test0)))[[1]])))


  lp.dt <- data.table(
    rn = "avg.ln_sales_tax",
    Estimate = coef(summary(lc.test0))[[1]],
    `Cluster s.e.` = sqrt(vcov(summary(lc.test0)))[[1]],
    `Pr(>|t|)` = pval,
    outcome = Y,
    Rsq = summary(res0)$r.squared,
    adj.Rsq = summary(res0)$adj.r.squared,
    specification = "LR",
    FE = "pair_by_module_by_time",
    covariates = "No",
    year = "All")
  LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T) ## Merge results to LRdiff_res
  fwrite(LRdiff_res, output.results.file) ## Write resulting file to a csv file

}


### Now regress price and quantity on tax rate including the covariates - cannot interact the covariates with year fixed effect because regression does not run
outcomes <- c("ln_cpricei2", "ln_quantity2")
controls <- c("pct_pop_urban", "housing_ownership_share", "median_income", "pct_pop_no_college", "pct_pop_bachelors", "pct_pop_over_65", "pct_pop_under_25", "pct_pop_black", "ln_unemp", "ln_home_price")


for(Y in outcomes) {

  ### module_by_time FE
  formula0 <- as.formula(paste0(
    Y, " ~ ln_sales_tax:year + pct_pop_urban + housing_ownership_share + median_income + pct_pop_no_college + pct_pop_bachelors + pct_pop_over_65 + pct_pop_under_25 + pct_pop_black + ln_unemp + ln_home_price | module_by_time | 0 | state_by_module ", sep = ""
  ))

  res0 <- felm(data = yearly_data,
               formula = formula0,
               weights = yearly_data$weight)



  ## attach results
  flog.info("Writing results...")
  res1.dt <- data.table(coef(summary(res0)), keep.rownames=T)
  res1.dt[, outcome := Y]
  res1.dt[, Rsq := summary(res0)$r.squared]
  res1.dt[, adj.Rsq := summary(res0)$adj.r.squared]
  res1.dt[, specification := "LR"]
  res1.dt[, FE := "module_by_time"]
  res1.dt[, covariates := "Yes - pooled"]
  res1.dt[, year := "All"]
  LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
  fwrite(LRdiff_res, output.results.file)  ## Write results to a csv file



  ### Take linear combinations of coefficients and attach results (this is the coefficient of interest)
  lc.lr0 <- paste0(cohort.weights[1], "*ln_sales_tax:year2008 + ", cohort.weights[2], "*ln_sales_tax:year2009 + ", cohort.weights[3], "*ln_sales_tax:year2010 + ", cohort.weights[4], "*ln_sales_tax:year2011 + ", cohort.weights[5], "*ln_sales_tax:year2012 + ", cohort.weights[6], "*ln_sales_tax:year2013 + ", cohort.weights[7], "*ln_sales_tax:year2014", sep = "")
  lc.formula0 <- paste0(lc.lr0, " = 0", sep = "")
  lc.test0 <- glht(res0, linfct = c(lc.formula0))

  # Calculate the p-value
  pval <- 2*(1 - pnorm(abs(coef(summary(lc.test0))[[1]]/sqrt(vcov(summary(lc.test0)))[[1]])))


  lp.dt <- data.table(
    rn = "avg.ln_sales_tax",
    Estimate = coef(summary(lc.test0))[[1]],
    `Cluster s.e.` = sqrt(vcov(summary(lc.test0)))[[1]],
    `Pr(>|t|)` = pval,
    outcome = Y,
    Rsq = summary(res0)$r.squared,
    adj.Rsq = summary(res0)$adj.r.squared,
    specification = "LR",
    FE = "module_by_time",
    covariates = "Yes - pooled",
    year = "All")
  LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T) ## Merge results to LRdiff_res
  fwrite(LRdiff_res, output.results.file) ## Write resulting file to a csv file



  ### pair_by_module_by_time FE
  formula0 <- as.formula(paste0(
    Y, " ~ ln_sales_tax:year + pct_pop_urban + housing_ownership_share + median_income + pct_pop_no_college + pct_pop_bachelors + pct_pop_over_65 + pct_pop_under_25 + pct_pop_black + ln_unemp + ln_home_price | pair_by_module_by_time | 0 | state_by_module ", sep = ""
  ))

  res0 <- felm(data = yearly_data,
               formula = formula0,
               weights = yearly_data$weight)



  ## attach results
  flog.info("Writing results...")
  res1.dt <- data.table(coef(summary(res0)), keep.rownames=T)
  res1.dt[, outcome := Y]
  res1.dt[, Rsq := summary(res0)$r.squared]
  res1.dt[, adj.Rsq := summary(res0)$adj.r.squared]
  res1.dt[, specification := "LR"]
  res1.dt[, FE := "pair_by_module_by_time"]
  res1.dt[, covariates := "Yes - pooled"]
  res1.dt[, year := "All"]
  LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
  fwrite(LRdiff_res, output.results.file)  ## Write results to a csv file



  ### Take linear combinations of coefficients and attach results (this is the coefficient of interest)
  lc.lr0 <- paste0(cohort.weights[1], "*ln_sales_tax:year2008 + ", cohort.weights[2], "*ln_sales_tax:year2009 + ", cohort.weights[3], "*ln_sales_tax:year2010 + ", cohort.weights[4], "*ln_sales_tax:year2011 + ", cohort.weights[5], "*ln_sales_tax:year2012 + ", cohort.weights[6], "*ln_sales_tax:year2013 + ", cohort.weights[7], "*ln_sales_tax:year2014", sep = "")
  lc.formula0 <- paste0(lc.lr0, " = 0", sep = "")
  lc.test0 <- glht(res0, linfct = c(lc.formula0))

  # Calculate the p-value
  pval <- 2*(1 - pnorm(abs(coef(summary(lc.test0))[[1]]/sqrt(vcov(summary(lc.test0)))[[1]])))


  lp.dt <- data.table(
    rn = "avg.ln_sales_tax",
    Estimate = coef(summary(lc.test0))[[1]],
    `Cluster s.e.` = sqrt(vcov(summary(lc.test0)))[[1]],
    `Pr(>|t|)` = pval,
    outcome = Y,
    Rsq = summary(res0)$r.squared,
    adj.Rsq = summary(res0)$adj.r.squared,
    specification = "LR",
    FE = "pair_by_module_by_time",
    covariates = "Yes - pooled",
    year = "All")
  LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T) ## Merge results to LRdiff_res
  fwrite(LRdiff_res, output.results.file) ## Write resulting file to a csv file


}



#### Now run the regression year by year and take an average (no standard errors for now)
for(Y in outcomes) {

  results <- matrix(0, nrow = 7, ncol = 1)
  i <- 1

  for(yr in 2008:2014) {

   ### Module FE
    formula0 <- as.formula(paste0(
      Y, " ~ ln_sales_tax + pct_pop_urban + housing_ownership_share + median_income + pct_pop_no_college + pct_pop_bachelors + pct_pop_over_65 + pct_pop_under_25 + pct_pop_black + ln_unemp + ln_home_price | module_by_time | 0 | state_by_module ", sep = ""
    ))

    res0 <- felm(data = yearly_data[year == yr],
                 formula = formula0,
                 weights = yearly_data[year == yr]$weight)



    ## attach results
    flog.info("Writing results...")
    res1.dt <- data.table(coef(summary(res0)), keep.rownames=T)
    res1.dt[, outcome := Y]
    res1.dt[, Rsq := summary(res0)$r.squared]
    res1.dt[, adj.Rsq := summary(res0)$adj.r.squared]
    res1.dt[, specification := "LR"]
    res1.dt[, FE := "module_by_time"]
    res1.dt[, covariates := "Yes"]
    res1.dt[, year := yr]
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)  ## Write results to a csv file


    ##Store estimate for ln_sales_tax (for each year) in the results matrix
    results[i] <- coef(summary(res0))["ln_sales_tax", "Estimate"]
    i <- i+1

  }

  ### Average estimates across years to get a weighted average estimate
  lp.dt <- data.table(
    rn = "avg.ln_sales_tax",
    Estimate = cohort.weights[1]*results[1] + cohort.weights[2]*results[2] + cohort.weights[3]*results[3] + cohort.weights[4]*results[4] + cohort.weights[5]*results[5] + cohort.weights[6]*results[6] + cohort.weights[7]*results[7],
    `Cluster s.e.` = NA,
    `Pr(>|t|)` = NA,
    outcome = Y,
    Rsq = NA,
    adj.Rsq = NA,
    specification = "LR",
    FE = "module_by_time",
    covariates = "Yes",
    year = "All")
  LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T) ## Merge results to LRdiff_res
  fwrite(LRdiff_res, output.results.file) ## Write resulting file to a csv file



   ### Pair by Module FE
    formula0 <- as.formula(paste0(
      Y, " ~ ln_sales_tax + pct_pop_urban + housing_ownership_share + median_income + pct_pop_no_college + pct_pop_bachelors + pct_pop_over_65 + pct_pop_under_25 + pct_pop_black + ln_unemp + ln_home_price | pair_by_module_by_time | 0 | state_by_module ", sep = ""
    ))

    res0 <- felm(data = yearly_data[year == yr],
                 formula = formula0,
                 weights = yearly_data[year == yr]$weight)



    ## attach results
    flog.info("Writing results...")
    res1.dt <- data.table(coef(summary(res0)), keep.rownames=T)
    res1.dt[, outcome := Y]
    res1.dt[, Rsq := summary(res0)$r.squared]
    res1.dt[, adj.Rsq := summary(res0)$adj.r.squared]
    res1.dt[, specification := "LR"]
    res1.dt[, FE := "pair_by_module_by_time"]
    res1.dt[, covariates := "Yes"]
    res1.dt[, year := yr]
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)  ## Write results to a csv file


    ##Store estimate for ln_sales_tax (for each year) in the results matrix
    results[i] <- coef(summary(res0))["ln_sales_tax", "Estimate"]
    i <- i+1

  }

  ### Average estimates across years to get a weighted average estimate
  lp.dt <- data.table(
    rn = "avg.ln_sales_tax",
    Estimate = cohort.weights[1]*results[1] + cohort.weights[2]*results[2] + cohort.weights[3]*results[3] + cohort.weights[4]*results[4] + cohort.weights[5]*results[5] + cohort.weights[6]*results[6] + cohort.weights[7]*results[7],
    `Cluster s.e.` = NA,
    `Pr(>|t|)` = NA,
    outcome = Y,
    Rsq = NA,
    adj.Rsq = NA,
    specification = "LR",
    FE = "pair_by_module_by_time",
    covariates = "Yes",
    year = "All")
  LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T) ## Merge results to LRdiff_res
  fwrite(LRdiff_res, output.results.file) ## Write resulting file to a csv file

}



## summary values --------------------------------------------------------------
LRdiff_res$N_obs <- nrow(yearly_data)
LRdiff_res$N_modules <- length(unique(yearly_data$product_module_code))
LRdiff_res$N_stores <- length(unique(yearly_data$store_code_uc))
LRdiff_res$N_counties <- uniqueN(yearly_data, by = c("fips_state", "fips_county"))
LRdiff_res$N_years <- uniqueN(yearly_data, by = c("year"))
LRdiff_res$N_county_modules <- uniqueN(yearly_data, by = c("fips_state", "fips_county",
                                                           "product_module_code"))
LRdiff_res$N_store_modules <- uniqueN(yearly_data, by = c("store_code_uc", "product_module_code"))
LRdiff_res$N_state_modules <- uniqueN(yearly_data, by = c("fips_state", "product_module_code"))

fwrite(LRdiff_res, output.results.file)

