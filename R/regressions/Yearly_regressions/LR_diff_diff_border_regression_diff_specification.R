### In this R-file we explore a specification meant to estimate long-run pass-through and sales/quantity response to tax changes

library(data.table)
library(lfe)
library(futile.logger)
library(AER)
library(splitstackshape)


setwd("/project2/igaarder")

## useful filepaths ------------------------------------------------------------
FE_pindex_path <- "Data/Nielsen/Pindex_FE_yearly_all_years.csv"
output_yearly <- "Data/Nielsen/yearly_nielsen_data.csv"
#taxable_pi_path <- "Data/Nielsen/price_quantity_indices_taxableitems_2006-2016.csv"
#eventstudy_tr_path <- "Data/event_study_tr_groups_comprehensive_firstonly_no2012q4_2013q1q2.csv"
#tr_groups_path <- "Data/tr_groups_comprehensive_firstonly_no2012q4_2013q1q2.csv"

covariates.nhgis.path <- "Data/covariates/nhgis_county_clean.csv"
covariates.qcew.path <- "Data/covariates/qcew_clean.csv"
census.regions.path <- "Data/covariates/census_regions.csv"

tax.path <- "Data/county_monthly_tax_rates.csv"

zillow_path <- "Data/covariates/zillow_long_by_county_clean.csv"
zillow_state_path <- "Data/covariates/zillow_long_by_state_clean.csv"
unemp.path <- "Data/covariates/county_monthly_unemp_clean.csv"
border.path <- "Data/border_counties.csv"



######### Regression analysis
## run the analysis on price ---------------------------------------------------
yearly_data <- fread(output_yearly)
## In this case we did not restrict the data to 2008-2014 yet because we need to first compute the leads and lags



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
#yearly_data <- merge(yearly_data, covariates.nhgis, by = c("fips_state", "fips_county"), all.x = T)
yearly_data <- merge(yearly_data, census.regions, by = c("fips_state", "fips_county"), all.x = T)

yearly_data[, region_by_time := .GRP, by = .(Region, year)]
yearly_data[, division_by_time := .GRP, by = .(Division, year)]
yearly_data[, reg_by_module_by_time := .GRP, by = .(Region, product_module_code, year)]
yearly_data[, div_by_module_by_time := .GRP, by = .(Division, product_module_code, year)]


########## Select a subset of variables to reduce size of dataset
yearly_data <- yearly_data[, c("fips_state", "fips_county", "store_code_uc", "year", "product_module_code", "ln_cpricei", "ln_cpricei2", "ln_quantity", "ln_quantity2", "ln_sales_tax", "base.sales", "store_module", "yr", "state_by_module", "module_by_time", "reg_by_module_by_time", "div_by_module_by_time")]


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



########## Import data with border counties
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

#Count the number of times each county appears in the data set (number of pairs that it is a part of)
n_counties <- border.counties[, list(n = .N),
                                  by = .(fips_state, fips_county)]
border.counties[, id := seq_len(.N), by = .(fips_state, fips_county)] ##Create an ID within each county (that will map into a border index bordindx).  Later we will duplicate each observation in yearly_data as many times as this county appears in border pairs and will create the same id --> so that we can then assign a bordindx to each

## Duplicate each observation in yearly_data as many times as this county appears in the border dataset
yearly_data <- merge(yearly_data, n_counties, by = c("fips_state", "fips_county"), all.x = TRUE) #Merge number of times each county appears in list of border pairs
yearly_data <- yearly_data[!is.na(n)]
yearly_data <- expandRows(yearly_data, "n", drop = FALSE) #Duplicate observations as many times as county appears in border county pairs
yearly_data <- yearly_data[ , id := seq_len(.N), by = .(store_code_uc, product_module_code, year)] #Create the same ID as in border.counties.


## Merge yearly_data with the bordindx IDs
yearly_data <- merge(yearly_data, border.counties, by = c("fips_state", "fips_county", "id"), all.x = T)

## Generate some pair-specific FE
yearly_data[, pair_by_time := .GRP, by = .(bordindx, year)]
yearly_data[, pair_by_module_by_time := .GRP, by = .(bordindx, year, product_module_code)]

## Generate some regressions weights (want to divide usual weights by number of times the county appears in the dataset)
yearly_data[, weight := base.sales/n]


#####
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

yearly_data[, d_lunemp := -diff(yearly_data$ln_unemp)] ## Difference of log quantity (1)
yearly_data$d_lunemp[yearly_data$year <= 2006] <- NA

yearly_data[, d_lhprice := -diff(yearly_data$ln_home_price)] ## Difference of log quantity (2)
yearly_data$d_lhprice[yearly_data$year <= 2006] <- NA

########
# Delete some variables to save some memory - Mostly lose the variables in level since we are going to run these regressions in differences
yearly_data <- yearly_data[, c("fips_state", "fips_county", "id", "year", "store_code_uc", "product_module_code", "store_module", "yr", "state_by_module", "module_by_time", "reg_by_module_by_time", "div_by_module_by_time", "n", "bordindx", "pair_by_time", "pair_by_module_by_time", "weight", "dltax", "d_lcpricei", "d_lcpricei2", "d_lquantity", "d_lquantity2", "d_lunemp", "d_lhprice")]


#############################################
#Lag and lead of dltax
yearly_data <- yearly_data[order(fips_state, fips_county, store_code_uc, product_module_code, year), c("lag1", "lag2") := shift(.SD, 1:2, type = "lag"), .SDcols = "dltax", by = c("fips_state", "fips_county", "store_code_uc", "product_module_code")]
yearly_data <- yearly_data[order(fips_state, fips_county, store_code_uc, product_module_code, year), c("lead1", "lead2") := shift(.SD, 1:2, type = "lead"), .SDcols = "dltax", by = c("fips_state", "fips_county", "store_code_uc", "product_module_code")]
yearly_data <- yearly_data[year >= 2009 & year <= 2014,] ##Now restrict the sample to observations for which we actually have the tax rate
#############################################


####
#################################################################################
#################################################################################
#############################################
## Demean the data


## module-store mean
yearly_data[, mean_ltax_ms := weighted.mean(dltax, w = weight), by = .(store_code_uc, product_module_code)]
print("Calculated store-module mean dltax")

yearly_data[, mean_lcpricei_ms := weighted.mean(d_lcpricei, w = weight), by = .(store_code_uc, product_module_code)]
print("Calculated store-module mean d_lcpricei")

yearly_data[, mean_lcpricei2_ms := weighted.mean(d_lcpricei2, w = weight), by = .(store_code_uc, product_module_code)]
print("Calculated store-module mean d_lcpricei2")

yearly_data[, mean_lquantity_ms := weighted.mean(d_lquantity, w = weight), by = .(store_code_uc, product_module_code)]
print("Calculated store-module mean d_lquantity")

yearly_data[, mean_lquantity2_ms := weighted.mean(d_lquantity2, w = weight), by = .(store_code_uc, product_module_code)]
print("Calculated store-module mean d_lquantity2")

yearly_data[, mean_lunemp_ms := weighted.mean(d_lunemp, w = weight), by = .(store_code_uc, product_module_code)]
print("Calculated store-module mean d_lunemp")

yearly_data[, mean_lhprice_ms := weighted.mean(d_lhprice, w = weight), by = .(store_code_uc, product_module_code)]
print("Calculated store-module mean d_lhprice")

yearly_data[, mean_lead1_ms := weighted.mean(lead1, w = weight), by = .(store_code_uc, product_module_code)]
print("Calculated store-module mean lead1")

yearly_data[, mean_lead2_ms := weighted.mean(lead2, w = weight), by = .(store_code_uc, product_module_code)]
print("Calculated store-module mean lead2")

yearly_data[, mean_lag1_ms := weighted.mean(lag1, w = weight), by = .(store_code_uc, product_module_code)]
print("Calculated store-module mean lag1")

yearly_data[, mean_lag2_ms := weighted.mean(lag2, w = weight), by = .(store_code_uc, product_module_code)]
print("Calculated store-module mean lag2")


##
#fwrite(yearly_data, "Data/Nielsen/demeaned_yearly_data_leadlags.csv")
#print("Saved the mean by module-border-year")
##
#yearly_data <- fread("Data/Nielsen/demeaned_yearly_data_leadlags.csv")


### To obtain residuals after two-way fixed effect: substract mean on each dimension then add sample mean (works because the sample is balanced).
## Create demeaned variables
yearly_data[, dltax_ms := dltax - mean_ltax_ms]
yearly_data[, d_lcpricei_ms := d_lcpricei - mean_lcpricei_ms]
yearly_data[, d_lcpricei2_ms := d_lcpricei2 - mean_lcpricei2_ms]
yearly_data[, d_lquantity_ms := d_lquantity - mean_lquantity_ms]
yearly_data[, d_lquantity2_ms := d_lquantity2 - mean_lquantity2_ms]
yearly_data[, d_lunemp_ms := d_lunemp - mean_lunemp_ms]
yearly_data[, d_lhprice_ms := d_lhprice - mean_lhprice_ms]
yearly_data[, lead1_ms := lead1 - mean_lead1_ms]
yearly_data[, lead2_ms := lead2 - mean_lead2_ms]
yearly_data[, lag1_ms := lag1 - mean_lag1_ms]
yearly_data[, lag2_ms := lag2 - mean_lag2_ms]


#### Run regressions with pair-specific FEs
#################################################################################
#################################################################################
#############################################
### First: regress price and quantity on tax rate after controling for econ. conditions
list.outcomes <- c("d_lcpricei", "d_lcpricei2", "d_lquantity", "d_lquantity2", "d_lhprice", "d_lunemp")

for(j in 1:length(list.outcomes)) {

  #
  outcome.j <- list.outcomes[j]
  outcome.j.ms <- paste(outcome.j, "_ms", sep = "")

  ## no FE (only store_module and time)
  price_formula <- as.formula(paste0(
    outcome.j.ms, " ~ lead2_ms + lead1_ms + dltax_ms + lag1_ms + lag2_ms | pair_by_time + module_by_time | 0 | state_by_module "
  ))



  res0 <- felm(data = yearly_data,
                     formula = price_formula,
                     weights = yearly_data$weight)

  if(j == 1) {
    LRdiff_res <-
      data.table(
        outcome = outcome.j,
        estimate = coef(summary(res0))["lead2_ms", "Estimate"],
        se = coef(summary(res0))["lead2_ms", "Cluster s.e."],
        pval = coef(summary(res0))["lead2_ms", "Pr(>|t|)"],
        Rsq = summary(res0)$r.squared,
        adj.Rsq = summary(res0)$adj.r.squared,
        time_controls = "Module by time + pair by time FE",
        lead_lag = -2
      )

  } else {

    LRdiff_res <-
      rbind(LRdiff_res,
            data.table(
              outcome = outcome.j,
              estimate = coef(summary(res0))["lead2_ms", "Estimate"],
              se = coef(summary(res0))["lead2_ms", "Cluster s.e."],
              pval = coef(summary(res0))["lead2_ms", "Pr(>|t|)"],
              Rsq = summary(res0)$r.squared,
              adj.Rsq = summary(res0)$adj.r.squared,
              time_controls = "Module by time + pair by time FE",
              lead_lag = -2
            )
      )
  }


  ### Append other results
  LRdiff_res <- rbind(
    LRdiff_res,
    data.table(
      outcome = outcome.j,
      estimate = coef(summary(res0))["lead1_ms", "Estimate"],
      se = coef(summary(res0))["lead1_ms", "Cluster s.e."],
      pval = coef(summary(res0))["lead1_ms", "Pr(>|t|)"],
      Rsq = summary(res0)$r.squared,
      adj.Rsq = summary(res0)$adj.r.squared,
      time_controls = "Module by time + pair by time FE",
      lead_lag = -1
    ),
    data.table(
      outcome = outcome.j,
      estimate = coef(summary(res0))["dltax_ms", "Estimate"],
      se = coef(summary(res0))["dltax_ms", "Cluster s.e."],
      pval = coef(summary(res0))["dltax_ms", "Pr(>|t|)"],
      Rsq = summary(res0)$r.squared,
      adj.Rsq = summary(res0)$adj.r.squared,
      time_controls = "Module by time + pair by time FE",
      lead_lag = 0
    ),
    data.table(
      outcome = outcome.j,
      estimate = coef(summary(res0))["lag1_ms", "Estimate"],
      se = coef(summary(res0))["lag1_ms", "Cluster s.e."],
      pval = coef(summary(res0))["lag1_ms", "Pr(>|t|)"],
      Rsq = summary(res0)$r.squared,
      adj.Rsq = summary(res0)$adj.r.squared,
      time_controls = "Module by time + pair by time FE",
      lead_lag = 1
    ),
    data.table(
      outcome = outcome.j,
      estimate = coef(summary(res0))["lag2_ms", "Estimate"],
      se = coef(summary(res0))["lag2_ms", "Cluster s.e."],
      pval = coef(summary(res0))["lag2_ms", "Pr(>|t|)"],
      Rsq = summary(res0)$r.squared,
      adj.Rsq = summary(res0)$adj.r.squared,
      time_controls = "Module by time + pair by time FE",
      lead_lag = 2
    )
  )

  # Take sums of pre-, post- and total effect
  # Take linear combinations of coefficients
  #Total pre-period
  lc.pre <- "lead2_ms + lead1_ms"
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
      time_controls = "Module by time + pair by time FE",
      lead_lag = "avg. pre"
    )
  )


  #Total post-period
  lc.post <- "dltax_ms + lag1_ms + lag2_ms"
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
      time_controls = "Module by time + pair by time FE",
      lead_lag = "avg. post"
    )
  )


  #Total pre- and post-period
  lc.tot <- "lead2_ms + lead1_ms + dltax_ms + lag1_ms + lag2_ms"
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
      time_controls = "Module by time + pair by time FE",
      lead_lag = "avg. total effect"
    )
  )


  print(paste("Done - ", outcome.j, "pair X year + mod X year FE"))
  fwrite(LRdiff_res, "Data/LRdiff_results_county_pairs_leadlags.csv")


  ## with module-time FE
  price_formula <- as.formula(paste0(
    outcome.j.ms, " ~ lead2_ms + lead1_ms + dltax_ms + lag1_ms + lag2_ms | pair_by_module_by_time | 0 | state_by_module "
  ))

  res0 <- felm(data = yearly_data,
                    formula = price_formula,
                    weights = yearly_data$weight)



    LRdiff_res <-
      rbind(LRdiff_res,
            data.table(
              outcome = outcome.j,
              estimate = coef(summary(res0))["lead2_ms", "Estimate"],
              se = coef(summary(res0))["lead2_ms", "Cluster s.e."],
              pval = coef(summary(res0))["lead2_ms", "Pr(>|t|)"],
              Rsq = summary(res0)$r.squared,
              adj.Rsq = summary(res0)$adj.r.squared,
              time_controls = "pair by module by time FE",
              lead_lag = -2
            )
      )


  ### Append other results
  LRdiff_res <- rbind(
    LRdiff_res,
    data.table(
      outcome = outcome.j,
      estimate = coef(summary(res0))["lead1_ms", "Estimate"],
      se = coef(summary(res0))["lead1_ms", "Cluster s.e."],
      pval = coef(summary(res0))["lead1_ms", "Pr(>|t|)"],
      Rsq = summary(res0)$r.squared,
      adj.Rsq = summary(res0)$adj.r.squared,
      time_controls = "pair by module by time FE",
      lead_lag = -1
    ),
    data.table(
      outcome = outcome.j,
      estimate = coef(summary(res0))["dltax_ms", "Estimate"],
      se = coef(summary(res0))["dltax_ms", "Cluster s.e."],
      pval = coef(summary(res0))["dltax_ms", "Pr(>|t|)"],
      Rsq = summary(res0)$r.squared,
      adj.Rsq = summary(res0)$adj.r.squared,
      time_controls = "pair by module by time FE",
      lead_lag = 0
    ),
    data.table(
      outcome = outcome.j,
      estimate = coef(summary(res0))["lag1_ms", "Estimate"],
      se = coef(summary(res0))["lag1_ms", "Cluster s.e."],
      pval = coef(summary(res0))["lag1_ms", "Pr(>|t|)"],
      Rsq = summary(res0)$r.squared,
      adj.Rsq = summary(res0)$adj.r.squared,
      time_controls = "pair by module by time FE",
      lead_lag = 1
    ),
    data.table(
      outcome = outcome.j,
      estimate = coef(summary(res0))["lag2_ms", "Estimate"],
      se = coef(summary(res0))["lag2_ms", "Cluster s.e."],
      pval = coef(summary(res0))["lag2_ms", "Pr(>|t|)"],
      Rsq = summary(res0)$r.squared,
      adj.Rsq = summary(res0)$adj.r.squared,
      time_controls = "pair by module by time FE",
      lead_lag = 2
    )
  )

  # Take sums of pre-, post- and total effect
  # Take linear combinations of coefficients
  #Total pre-period
  lc.pre <- "lead2_ms + lead1_ms"
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
      time_controls = "pair by module by time FE",
      lead_lag = "avg. pre"
    )
  )


  #Total post-period
  lc.post <- "dltax_ms + lag1_ms + lag2_ms"
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
      time_controls = "pair by module by time FE",
      lead_lag = "avg. post"
    )
  )


  #Total pre- and post-period
  lc.tot <- "lead2_ms + lead1_ms + dltax_ms + lag1_ms + lag2_ms"
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
      time_controls = "pair by module by time FE",
      lead_lag = "avg. total effect"
    )
  )

  print(paste("Done - ", outcome.j, "mod X pair X year FE"))
  fwrite(LRdiff_res, "Data/LRdiff_results_county_pairs_leadlags.csv")
}


## summary values --------------------------------------------------------------
## Some of these values are needed to adjust standard errors to reflect the correct number of FEs
LRdiff_res$N_obs <- nrow(yearly_data)
LRdiff_res$N_modules <- length(unique(yearly_data$product_module_code))
LRdiff_res$N_stores <- length(unique(yearly_data$store_code_uc))
LRdiff_res$N_counties <- uniqueN(yearly_data, by = c("fips_state", "fips_county"))
LRdiff_res$N_years <- uniqueN(yearly_data, by = c("year")) # should be 7
LRdiff_res$N_county_modules <- uniqueN(yearly_data, by = c("fips_state", "fips_county",
                                                           "product_module_code"))
LRdiff_res$N_county_pairs <- length(unique(yearly_data$bordindx))
LRdiff_res$N_store_modules <- uniqueN(yearly_data, by = c("store_code_uc", "product_module_code"))
LRdiff_res$N_module_years <- uniqueN(yearly_data, by = c("product_module_code", "year"))
LRdiff_res$N_pair_years <- uniqueN(yearly_data, by = c("bordindx", "year"))
LRdiff_res$N_pair_year_modules <- uniqueN(yearly_data, by = c("bordindx", "year", "product_module_code"))

fwrite(LRdiff_res, "Data/LRdiff_results_county_pairs_leadlags.csv")

