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
output.results.file <- "Data/LRdiff_results_diff_leadlags_1reg.csv"


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


#############################################
#Lag and lead of D.ln_sales_tax, D.ln_unemp and D.ln_home_price
yearly_data <- yearly_data[order(fips_state, fips_county, store_code_uc, product_module_code, year), c("L1.D.ln_sales_tax", "L2.D.ln_sales_tax") := shift(.SD, 1:2, type = "lag"), .SDcols = "D.ln_sales_tax", by = c("fips_state", "fips_county", "store_code_uc", "product_module_code")]
yearly_data <- yearly_data[order(fips_state, fips_county, store_code_uc, product_module_code, year), c("F1.D.ln_sales_tax", "F2.D.ln_sales_tax") := shift(.SD, 1:2, type = "lead"), .SDcols = "D.ln_sales_tax", by = c("fips_state", "fips_county", "store_code_uc", "product_module_code")]

yearly_data <- yearly_data[order(fips_state, fips_county, store_code_uc, product_module_code, year), c("L1.D.ln_unemp", "L2.D.ln_unemp") := shift(.SD, 1:2, type = "lag"), .SDcols = "D.ln_unemp", by = c("fips_state", "fips_county", "store_code_uc", "product_module_code")]
yearly_data <- yearly_data[order(fips_state, fips_county, store_code_uc, product_module_code, year), c("F1.D.ln_unemp", "F2.D.ln_unemp") := shift(.SD, 1:2, type = "lead"), .SDcols = "D.ln_unemp", by = c("fips_state", "fips_county", "store_code_uc", "product_module_code")]
yearly_data <- yearly_data[order(fips_state, fips_county, store_code_uc, product_module_code, year), c("L1.D.ln_home_price", "L2.D.ln_home_price") := shift(.SD, 1:2, type = "lag"), .SDcols = "D.ln_home_price", by = c("fips_state", "fips_county", "store_code_uc", "product_module_code")]
yearly_data <- yearly_data[order(fips_state, fips_county, store_code_uc, product_module_code, year), c("F1.D.ln_home_price", "F2.D.ln_home_price") := shift(.SD, 1:2, type = "lead"), .SDcols = "D.ln_home_price", by = c("fips_state", "fips_county", "store_code_uc", "product_module_code")]
#############################################

## Save some Memory and keep only variables we are going to need below
yearly_data <- yearly_data[, c("fips_state", "fips_county", "year", "store_code_uc", "product_module_code", "base.sales", "state_by_module", "module_by_time", "region_by_module_by_time", "division_by_module_by_time", "cal_time", "D.ln_cpricei", "D.ln_cpricei2", "D.ln_quantity", "D.ln_quantity2", "D.ln_sales_tax", "D.ln_unemp", "D.ln_home_price", "L1.D.ln_sales_tax", "L2.D.ln_sales_tax", "F1.D.ln_sales_tax", "F2.D.ln_sales_tax", "L1.D.ln_unemp", "L2.D.ln_unemp", "F1.D.ln_unemp", "F2.D.ln_unemp", "L1.D.ln_home_price", "L2.D.ln_home_price", "F1.D.ln_home_price", "F2.D.ln_home_price")]

## !!! Only include years for which we actually have the outcome and tax rate
## ---------------------------------------------------
yearly_data <- yearly_data[year >= 2009 & year <= 2014,] ## We lose 2008 because we are looking at differences



### Second, run the diff-in-diff specifications (in first differences)
list.outcomes <- c("D.ln_cpricei", "D.ln_cpricei2", "D.ln_quantity", "D.ln_quantity2")
FE_opts <- c("cal_time", "module_by_time", "region_by_module_by_time", "division_by_module_by_time")
Econ_opts <- c("D.ln_unemp", "D.ln_home_price", "D.ln_unemp + D.ln_home_price")


## Create a matrix with controls for econ conditions that include leads and lags - also store indicators that will be used in final matrix with results
Econ_w_lags <- c("D.ln_unemp", "D.ln_unemp", "D.ln_unemp + D.ln_home_price", "D.ln_unemp + D.ln_home_price")
Econ_w_lags <- rbind(Econ_w_lags, c("Yes", "Yes", "Yes", "Yes"))
Econ_w_lags <- rbind(Econ_w_lags, c("No", "Yes", "No", "Yes"))

Econ_w_lags <- rbind(Econ_w_lags, c("L2.D.ln_unemp + L1.D.ln_unemp + D.ln_unemp", "L2.D.ln_unemp + L1.D.ln_unemp + D.ln_unemp + F1.D.ln_unemp + F2.D.ln_unemp", "L2.D.ln_unemp + L1.D.ln_unemp + L2.D.ln_home_price + L1.D.ln_home_price + D.ln_unemp + D.ln_home_price", "L2.D.ln_unemp + L1.D.ln_unemp + L2.D.ln_home_price + L1.D.ln_home_price + D.ln_unemp + D.ln_home_price + F1.D.ln_unemp + F2.D.ln_unemp + F1.D.ln_home_price + F2.D.ln_home_price"))


LRdiff_res <- data.table(NULL)
for(j in 1:length(list.outcomes)) {
  for(FE in FE_opts) {


  #
  outcome.j <- list.outcomes[j]

  ##
  formula1 <- as.formula(paste0(
    outcome.j, " ~ F2.D.ln_sales_tax + F1.D.ln_sales_tax + D.ln_sales_tax + L1.D.ln_sales_tax + L2.D.ln_sales_tax |", FE, " | 0 | state_by_module "
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


  # Take sums of pre-, post- and total effect
  # Take linear combinations of coefficients
  #Total pre-period
  lc.lead <- "F2.D.ln_sales_tax + F1.D.ln_sales_tax"
  lc.lead.form <- paste0(lc.lead, " = 0", sep = "")
  lc.lead.test <- glht(res1, linfct = c(lc.lead.form))
  lc.lead.est <- coef(summary(lc.lead.test))[[1]]
  lc.lead.se <- sqrt(vcov(summary(lc.lead.test)))[[1]]
  lc.lead.pval <- 2*(1 - pnorm(abs(lc.lead.est/lc.lead.se)))

  #Total post-period
  lc.lag <- "D.ln_sales_tax + L1.D.ln_sales_tax + L2.D.ln_sales_tax"
  lc.lag.form <- paste0(lc.lag, " = 0", sep = "")
  lc.lag.test <- glht(res1, linfct = c(lc.lag.form))
  lc.lag.est <- coef(summary(lc.lag.test))[[1]]
  lc.lag.se <- sqrt(vcov(summary(lc.lag.test)))[[1]]
  lc.lag.pval <- 2*(1 - pnorm(abs(lc.lag.est/lc.lag.se)))


  #Total pre- and post-period
  lc.tot <- "F2.D.ln_sales_tax + F1.D.ln_sales_tax + D.ln_sales_tax + L1.D.ln_sales_tax + L2.D.ln_sales_tax"
  lc.tot.form <- paste0(lc.tot, " = 0", sep = "")
  lc.tot.test <- glht(res1, linfct = c(lc.tot.form))
  lc.tot.est <- coef(summary(lc.tot.test))[[1]]
  lc.tot.se <- sqrt(vcov(summary(lc.tot.test)))[[1]]
  lc.tot.pval <- 2*(1 - pnorm(abs(lc.tot.est/lc.tot.se)))


  ## linear hypothesis results
  lp.dt <- data.table(
    rn = c("Pre.D.ln_sales_tax", "Post.D.ln_sales_tax", "All.D.ln_sales_tax"),
    Estimate = c(lc.lead.est, lc.lag.est, lc.tot.est),
    `Cluster s.e.` = c(lc.lead.se, lc.lag.se, lc.tot.se),
    `Pr(>|t|)` = c(lc.lead.pval, lc.lag.pval, lc.tot.pval),
    outcome = outcome.j,
    controls = FE,
    econ = "none",
    lag.econ = NA,
    lead.econ = NA,
    Rsq = summary(res1)$r.squared,
    adj.Rsq = summary(res1)$adj.r.squared)
    LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)


    ##
    for(EC in Econ_opts) {

      ##
      formula1 <- as.formula(paste0(
        outcome.j, " ~ F2.D.ln_sales_tax + F1.D.ln_sales_tax + D.ln_sales_tax + L1.D.ln_sales_tax + L2.D.ln_sales_tax + ", EC, " |", FE, " | 0 | state_by_module "
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


      # Take sums of pre-, post- and total effect
      # Take linear combinations of coefficients
      #Total pre-period
      lc.lead <- "F2.D.ln_sales_tax + F1.D.ln_sales_tax"
      lc.lead.form <- paste0(lc.lead, " = 0", sep = "")
      lc.lead.test <- glht(res1, linfct = c(lc.lead.form))
      lc.lead.est <- coef(summary(lc.lead.test))[[1]]
      lc.lead.se <- sqrt(vcov(summary(lc.lead.test)))[[1]]
      lc.lead.pval <- 2*(1 - pnorm(abs(lc.lead.est/lc.lead.se)))

      #Total post-period
      lc.lag <- "D.ln_sales_tax + L1.D.ln_sales_tax + L2.D.ln_sales_tax"
      lc.lag.form <- paste0(lc.lag, " = 0", sep = "")
      lc.lag.test <- glht(res1, linfct = c(lc.lag.form))
      lc.lag.est <- coef(summary(lc.lag.test))[[1]]
      lc.lag.se <- sqrt(vcov(summary(lc.lag.test)))[[1]]
      lc.lag.pval <- 2*(1 - pnorm(abs(lc.lag.est/lc.lag.se)))


      #Total pre- and post-period
      lc.tot <- "F2.D.ln_sales_tax + F1.D.ln_sales_tax + D.ln_sales_tax + L1.D.ln_sales_tax + L2.D.ln_sales_tax"
      lc.tot.form <- paste0(lc.tot, " = 0", sep = "")
      lc.tot.test <- glht(res1, linfct = c(lc.tot.form))
      lc.tot.est <- coef(summary(lc.tot.test))[[1]]
      lc.tot.se <- sqrt(vcov(summary(lc.tot.test)))[[1]]
      lc.tot.pval <- 2*(1 - pnorm(abs(lc.tot.est/lc.tot.se)))


      ## linear hypothesis results
      lp.dt <- data.table(
        rn = c("Pre.D.ln_sales_tax", "Post.D.ln_sales_tax", "All.D.ln_sales_tax"),
        Estimate = c(lc.lead.est, lc.lag.est, lc.tot.est),
        `Cluster s.e.` = c(lc.lead.se, lc.lag.se, lc.tot.se),
        `Pr(>|t|)` = c(lc.lead.pval, lc.lag.pval, lc.tot.pval),
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

    ##
    for(i in 1:dim(Econ_w_lags)[2]) {

      EC <- Econ_w_lags[4, i]

      ## Formula
      formula1 <- as.formula(paste0(
        outcome.j, " ~ F2.D.ln_sales_tax + F1.D.ln_sales_tax + D.ln_sales_tax + L1.D.ln_sales_tax + L2.D.ln_sales_tax + ", EC, " | ", FE, " | 0 | state_by_module "
      ))


      ## Run regression
      res1 <- felm(data = yearly_data,
                   formula = formula1,
                   weights = yearly_data$base.sales)

      ## attach results
      flog.info("Writing results...")
      res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
      res1.dt[, outcome := outcome.j]
      res1.dt[, controls := FE]
      res1.dt[, econ := Econ_w_lags[1,i]]
      res1.dt[, econ.lag := Econ_w_lags[2,i]]
      res1.dt[, econ.lead := Econ_w_lags[3,i]]
      res1.dt[, Rsq := summary(res1)$r.squared]
      res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
      LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
      fwrite(LRdiff_res, output.results.file)


      # Take sums of pre-, post- and total effect
      # Take linear combinations of coefficients
      #Total pre-period
      lc.lead <- "F2.D.ln_sales_tax + F1.D.ln_sales_tax"
      lc.lead.form <- paste0(lc.lead, " = 0", sep = "")
      lc.lead.test <- glht(res1, linfct = c(lc.lead.form))
      lc.lead.est <- coef(summary(lc.lead.test))[[1]]
      lc.lead.se <- sqrt(vcov(summary(lc.lead.test)))[[1]]
      lc.lead.pval <- 2*(1 - pnorm(abs(lc.lead.est/lc.lead.se)))

      #Total post-period
      lc.lag <- "D.ln_sales_tax + L1.D.ln_sales_tax + L2.D.ln_sales_tax"
      lc.lag.form <- paste0(lc.lag, " = 0", sep = "")
      lc.lag.test <- glht(res1, linfct = c(lc.lag.form))
      lc.lag.est <- coef(summary(lc.lag.test))[[1]]
      lc.lag.se <- sqrt(vcov(summary(lc.lag.test)))[[1]]
      lc.lag.pval <- 2*(1 - pnorm(abs(lc.lag.est/lc.lag.se)))


      #Total pre- and post-period
      lc.tot <- "F2.D.ln_sales_tax + F1.D.ln_sales_tax + D.ln_sales_tax + L1.D.ln_sales_tax + L2.D.ln_sales_tax"
      lc.tot.form <- paste0(lc.tot, " = 0", sep = "")
      lc.tot.test <- glht(res1, linfct = c(lc.tot.form))
      lc.tot.est <- coef(summary(lc.tot.test))[[1]]
      lc.tot.se <- sqrt(vcov(summary(lc.tot.test)))[[1]]
      lc.tot.pval <- 2*(1 - pnorm(abs(lc.tot.est/lc.tot.se)))


      ## linear hypothesis results
      lp.dt <- data.table(
        rn = c("Pre.D.ln_sales_tax", "Post.D.ln_sales_tax", "All.D.ln_sales_tax"),
        Estimate = c(lc.lead.est, lc.lag.est, lc.tot.est),
        `Cluster s.e.` = c(lc.lead.se, lc.lag.se, lc.tot.se),
        `Pr(>|t|)` = c(lc.lead.pval, lc.lag.pval, lc.tot.pval),
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


