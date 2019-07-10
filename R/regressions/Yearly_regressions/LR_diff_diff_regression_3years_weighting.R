### In this R-file we explore a specification meant to estimate long-run pass-through and sales/quantity response to tax changes
# We exploit 2-year variations in sales tax rates and quantity/price

library(data.table)
library(lfe)
library(futile.logger)
library(AER)


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
border.path <- "Data/Border_states.csv"


###OUTPUT
output.results.file <- "Data/LRdiff_3years_FE_weighting.csv"
output.pretrend.file <- "Data/LRdiff_3years_pretrends_weighting.csv"


######### Regression analysis
yearly_data <- fread(output_yearly)



################################
## Include some covariates
#List of unique counties in the sales data
list.counties <- data.frame(unique(yearly_data[,c('fips_state','fips_county')]))

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
yearly_data[, cal_time := (year-2008)/6] ## Of course normalization here does not matter to estimation - normalization chosen here so that with two year intervals, cal_time will take values 0, 1, 2 and 3



###### Delete some variables to save some memory
yearly_data <- yearly_data[, c("fips_state", "fips_county", "store_code_uc", "year", "product_module_code", "sales", "ln_cpricei", "ln_sales_tax", "ln_quantity", "ln_sales", "base.sales", "ln_cpricei2", "ln_quantity2", "store_module", "state_by_module", "module_by_time", "store_by_time", "region_by_module_by_time", "division_by_module_by_time", "cal_time")]



######## Import and prep house price, unemployment and population data

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


#### Population data
covariates.nhgis <- fread(covariates.nhgis.path)

setkey(covariates.nhgis, year)
covariates.nhgis <- covariates.nhgis[list(2000),]

setnames(covariates.nhgis, c('statefp','countyfp'), c('fips_state', 'fips_county'))
covariates.nhgis <- covariates.nhgis[,c('fips_state', 'fips_county', 'pop')]

##
yearly_data <- merge(yearly_data, covariates.nhgis, by = c("fips_state", "fips_county"))


######## Limit data to relevant time periods only and create lag and lead econ conditions
##NOTE: Make sure that we only include 2008-2014 in the regressions!!
yearly_data <- yearly_data[year %in% c(2006, 2008, 2011, 2014, 2016),]
yearly_data <- yearly_data[order(store_code_uc, product_module_code, year),]


##
yearly_data[, lag_ln_unemp := shift(ln_unemp, n=1, type="lag"),
         by = .(store_code_uc, product_module_code)]

yearly_data[, lag_ln_home_price := shift(ln_home_price, n=1, type="lag"),
            by = .(store_code_uc, product_module_code)]

yearly_data[, lead_ln_unemp := shift(ln_unemp, n=1, type = "lead"),
            by = .(store_code_uc, product_module_code)]

yearly_data[, lead_ln_home_price := shift(ln_home_price, n=1, type = "lead"),
            by = .(store_code_uc, product_module_code)]


## Create lead of tax rate and outcomes to test for pre-trends
yearly_data[, lead_ln_cpricei := shift(ln_cpricei, n=1, type = "lead"),
            by = .(store_code_uc, product_module_code)]

yearly_data[, lead_ln_cpricei2 := shift(ln_cpricei2, n=1, type = "lead"),
            by = .(store_code_uc, product_module_code)]

yearly_data[, lead_ln_quantity := shift(ln_quantity, n=1, type = "lead"),
            by = .(store_code_uc, product_module_code)]

yearly_data[, lead_ln_quantity2 := shift(ln_quantity2, n=1, type = "lead"),
            by = .(store_code_uc, product_module_code)]

yearly_data[, lead_ln_sales_tax := shift(ln_sales_tax, n=1, type = "lead"),
            by = .(store_code_uc, product_module_code)]



## !!! Only include 2008 - 2010 - 2012 - 2014 to create 2-year differences
## ---------------------------------------------------
yearly_data <- yearly_data[year >= 2008 & year <= 2014,]
#yearly_data <- yearly_data[year %in% c(2008, 2010, 2012, 2014),]




#############################################
### First: regress price and quantity on tax rate after controling for econ. conditions (+ without econ controls)
list.outcomes <- c("ln_cpricei", "ln_cpricei2", "ln_quantity", "ln_quantity2")
econ.outcomes <- c("ln_unemp", "ln_home_price")

#FE_opts <- c("module_by_time", "region_by_module_by_time", "division_by_module_by_time")
FE_opts <- c("region_by_module_by_time")
Econ_opts <- c("ln_unemp + lag_ln_unemp + ln_home_price + lag_ln_home_price")


### Because negative weights are not admitted and ln_sales can be negative - impute zero weight to negative values (probably should not put too much value on these results)
yearly_data[ln_sales < 0,]$ln_sales <-0


LRdiff_res <- data.table(NULL)
for(Y in c(list.outcomes, econ.outcomes)) {
  for(FE in FE_opts) {



    formula1 <- as.formula(paste0(
      Y, " ~ ln_sales_tax | ", "store_module + ", FE, " | 0 | state_by_module "
    ))


    ##### Run regression with different weights

    ## Run regression with base.sales
    res1 <- felm(data = yearly_data,
                       formula = formula1,
                       weights = yearly_data$base.sales)


    ## attach results
    flog.info("Writing results...")
    res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
    res1.dt[, outcome := Y]
    res1.dt[, controls := FE]
    res1.dt[, econ := "none"]
    res1.dt[, lag.econ := NA]
    res1.dt[, weights := "base.sales"]
    res1.dt[, Rsq := summary(res1)$r.squared]
    res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)


    ## Run regression with current sales
    res1 <- felm(data = yearly_data,
                 formula = formula1,
                 weights = yearly_data$sales)


    ## attach results
    flog.info("Writing results...")
    res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
    res1.dt[, outcome := Y]
    res1.dt[, controls := FE]
    res1.dt[, econ := "none"]
    res1.dt[, lag.econ := NA]
    res1.dt[, weights := "sales"]
    res1.dt[, Rsq := summary(res1)$r.squared]
    res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)


    ## Run regression with current log sales
    res1 <- felm(data = yearly_data,
                 formula = formula1,
                 weights = yearly_data$ln_sales)


    ## attach results
    flog.info("Writing results...")
    res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
    res1.dt[, outcome := Y]
    res1.dt[, controls := FE]
    res1.dt[, econ := "none"]
    res1.dt[, lag.econ := NA]
    res1.dt[, weights := "ln_sales"]
    res1.dt[, Rsq := summary(res1)$r.squared]
    res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)



    ## Run regression with population weights
    res1 <- felm(data = yearly_data,
                 formula = formula1,
                 weights = yearly_data$pop)


    ## attach results
    flog.info("Writing results...")
    res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
    res1.dt[, outcome := Y]
    res1.dt[, controls := FE]
    res1.dt[, econ := "none"]
    res1.dt[, lag.econ := NA]
    res1.dt[, weights := "population"]
    res1.dt[, Rsq := summary(res1)$r.squared]
    res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)



    ## Run regression without weights but on counties with at least 150K population
    res1 <- felm(data = yearly_data[pop >= 150000,],
                 formula = formula1)


    ## attach results
    flog.info("Writing results...")
    res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
    res1.dt[, outcome := Y]
    res1.dt[, controls := FE]
    res1.dt[, econ := "none"]
    res1.dt[, lag.econ := NA]
    res1.dt[, weights := "Pop over 150K only"]
    res1.dt[, Rsq := summary(res1)$r.squared]
    res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)


    ## Run regression without weights
    res1 <- felm(data = yearly_data,
                 formula = formula1)


    ## attach results
    flog.info("Writing results...")
    res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
    res1.dt[, outcome := Y]
    res1.dt[, controls := FE]
    res1.dt[, econ := "none"]
    res1.dt[, lag.econ := NA]
    res1.dt[, weights := "None"]
    res1.dt[, Rsq := summary(res1)$r.squared]
    res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)

  }
}



######
## Same regressions with Econ controls

for(Y in c(list.outcomes)) {
  for(FE in FE_opts) {
    for(EC in Econ_opts) {

      ## Formula
      formula1 <- as.formula(paste0(
        Y, " ~ ln_sales_tax", " + ", EC, " | ", "store_module + ", FE, " | 0 | state_by_module "
      ))


      ## Run regression with base.sales
      res1 <- felm(data = yearly_data,
                   formula = formula1,
                   weights = yearly_data$base.sales)


      ## attach results
      flog.info("Writing results...")
      res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
      res1.dt[, outcome := Y]
      res1.dt[, controls := FE]
      res1.dt[, econ := "ln_unemp + ln_home_price"]
      res1.dt[, lag.econ := "Yes"]
      res1.dt[, weights := "base.sales"]
      res1.dt[, Rsq := summary(res1)$r.squared]
      res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
      LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
      fwrite(LRdiff_res, output.results.file)


      ## Run regression with current sales
      res1 <- felm(data = yearly_data,
                   formula = formula1,
                   weights = yearly_data$sales)


      ## attach results
      flog.info("Writing results...")
      res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
      res1.dt[, outcome := Y]
      res1.dt[, controls := FE]
      res1.dt[, econ := "ln_unemp + ln_home_price"]
      res1.dt[, lag.econ := "Yes"]
      res1.dt[, weights := "sales"]
      res1.dt[, Rsq := summary(res1)$r.squared]
      res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
      LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
      fwrite(LRdiff_res, output.results.file)


      ## Run regression with current log sales
      res1 <- felm(data = yearly_data,
                   formula = formula1,
                   weights = yearly_data$ln_sales)


      ## attach results
      flog.info("Writing results...")
      res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
      res1.dt[, outcome := Y]
      res1.dt[, controls := FE]
      res1.dt[, econ := "ln_unemp + ln_home_price"]
      res1.dt[, lag.econ := "Yes"]
      res1.dt[, weights := "ln_sales"]
      res1.dt[, Rsq := summary(res1)$r.squared]
      res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
      LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
      fwrite(LRdiff_res, output.results.file)


      ## Run regression with population weights
      res1 <- felm(data = yearly_data,
                   formula = formula1,
                   weights = yearly_data$pop)


      ## attach results
      flog.info("Writing results...")
      res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
      res1.dt[, outcome := Y]
      res1.dt[, controls := FE]
      res1.dt[, econ := "ln_unemp + ln_home_price"]
      res1.dt[, lag.econ := "Yes"]
      res1.dt[, weights := "population"]
      res1.dt[, Rsq := summary(res1)$r.squared]
      res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
      LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
      fwrite(LRdiff_res, output.results.file)



      ## Run regression without weights but on counties with at least 150K population
      res1 <- felm(data = yearly_data[pop >= 150000,],
                   formula = formula1)


      ## attach results
      flog.info("Writing results...")
      res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
      res1.dt[, outcome := Y]
      res1.dt[, controls := FE]
      res1.dt[, econ := "ln_unemp + ln_home_price"]
      res1.dt[, lag.econ := "Yes"]
      res1.dt[, weights := "Pop over 150K only"]
      res1.dt[, Rsq := summary(res1)$r.squared]
      res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
      LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
      fwrite(LRdiff_res, output.results.file)



      ## Run regression without weights
      res1 <- felm(data = yearly_data,
                   formula = formula1)


      ## attach results
      flog.info("Writing results...")
      res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
      res1.dt[, outcome := Y]
      res1.dt[, controls := FE]
      res1.dt[, econ := "ln_unemp + ln_home_price"]
      res1.dt[, lag.econ := "Yes"]
      res1.dt[, weights := "None"]
      res1.dt[, Rsq := summary(res1)$r.squared]
      res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
      LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
      fwrite(LRdiff_res, output.results.file)

    }
  }
}


## summary values --------------------------------------------------------------
LRdiff_res$N_obs <- nrow(yearly_data)
LRdiff_res$N_modules <- length(unique(yearly_data$product_module_code))
LRdiff_res$N_stores <- length(unique(yearly_data$store_code_uc))
LRdiff_res$N_counties <- uniqueN(yearly_data, by = c("fips_state", "fips_county"))
LRdiff_res$N_years <- uniqueN(yearly_data, by = c("year")) # should be 7
LRdiff_res$N_county_modules <- uniqueN(yearly_data, by = c("fips_state", "fips_county",
                                                           "product_module_code"))
LRdiff_res$N_store_modules <- uniqueN(yearly_data, by = c("store_code_uc",
                                                           "product_module_code"))
LRdiff_res$N_state_modules <- uniqueN(yearly_data, by = c("fips_state",
                                                           "product_module_code"))


fwrite(LRdiff_res, output.results.file)



########################
########################
### PRE-TREND test

#############################################
### First: regress price and quantity on tax rate after controling for econ. conditions (+ without econ controls)
list.outcomes <- c("lead_ln_cpricei", "lead_ln_cpricei2", "lead_ln_quantity", "lead_ln_quantity2", "lead_ln_unemp", "lead_ln_home_price")
econ.outcomes <- c("lead_ln_unemp", "lead_ln_home_price")

FE_opts <- c("module_by_time", "region_by_module_by_time", "division_by_module_by_time")


#### First: pre-trends without econ controls
LRdiff_res <- data.table(NULL)
for(Y in c(list.outcomes, econ.outcomes)) {
  for(FE in FE_opts) {



    ## Formula
    formula1 <- as.formula(paste0(
      Y, " ~ ln_sales_tax + lead_ln_sales_tax | ", "store_module + ", FE, " | 0 | state_by_module "
    ))


    ## Run regression with base.sales
    res1 <- felm(data = yearly_data,
                 formula = formula1,
                 weights = yearly_data$base.sales)


    ## attach results
    flog.info("Writing results...")
    res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
    res1.dt[, outcome := Y]
    res1.dt[, controls := FE]
    res1.dt[, econ := "none"]
    res1.dt[, weights := "base.sales"]
    res1.dt[, Rsq := summary(res1)$r.squared]
    res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, output.pretrend.file)


    ## Run regression with current sales
    res1 <- felm(data = yearly_data,
                 formula = formula1,
                 weights = yearly_data$sales)


    ## attach results
    flog.info("Writing results...")
    res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
    res1.dt[, outcome := Y]
    res1.dt[, controls := FE]
    res1.dt[, econ := "none"]
    res1.dt[, weights := "sales"]
    res1.dt[, Rsq := summary(res1)$r.squared]
    res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, output.pretrend.file)


    ## Run regression with current log sales
    res1 <- felm(data = yearly_data,
                 formula = formula1,
                 weights = yearly_data$ln_sales)


    ## attach results
    flog.info("Writing results...")
    res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
    res1.dt[, outcome := Y]
    res1.dt[, controls := FE]
    res1.dt[, econ := "none"]
    res1.dt[, weights := "ln_sales"]
    res1.dt[, Rsq := summary(res1)$r.squared]
    res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, output.pretrend.file)


    ## Run regression with population weights
    res1 <- felm(data = yearly_data,
                 formula = formula1,
                 weights = yearly_data$pop)


    ## attach results
    flog.info("Writing results...")
    res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
    res1.dt[, outcome := Y]
    res1.dt[, controls := FE]
    res1.dt[, econ := "none"]
    res1.dt[, weights := "population"]
    res1.dt[, Rsq := summary(res1)$r.squared]
    res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, output.pretrend.file)



    ## Run regression without weights but on counties with at least 150K population
    res1 <- felm(data = yearly_data[pop >= 150000,],
                 formula = formula1)


    ## attach results
    flog.info("Writing results...")
    res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
    res1.dt[, outcome := Y]
    res1.dt[, controls := FE]
    res1.dt[, econ := "none"]
    res1.dt[, weights := "Pop over 150K only"]
    res1.dt[, Rsq := summary(res1)$r.squared]
    res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, output.pretrend.file)



    ## Run regression without weights
    res1 <- felm(data = yearly_data,
                 formula = formula1)


    ## attach results
    flog.info("Writing results...")
    res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
    res1.dt[, outcome := Y]
    res1.dt[, controls := FE]
    res1.dt[, econ := "none"]
    res1.dt[, weights := "None"]
    res1.dt[, Rsq := summary(res1)$r.squared]
    res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, output.pretrend.file)

  }
}


### Second: pre-trends with econ controls
for(Y in c(list.outcomes)) {
  for(FE in FE_opts) {



    ## Formula
    formula1 <- as.formula(paste0(
      Y, " ~ ln_sales_tax + lead_ln_sales_tax + lead_ln_unemp + lead_ln_home_price | ", "store_module + ", FE, " | 0 | state_by_module "
    ))


    ## Run regression with base.sales
    res1 <- felm(data = yearly_data,
                 formula = formula1,
                 weights = yearly_data$base.sales)


    ## attach results
    flog.info("Writing results...")
    res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
    res1.dt[, outcome := Y]
    res1.dt[, controls := FE]
    res1.dt[, econ := "ln_unemp + ln_home_price"]
    res1.dt[, weights := "base.sales"]
    res1.dt[, Rsq := summary(res1)$r.squared]
    res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, output.pretrend.file)


    ## Run regression with current sales
    res1 <- felm(data = yearly_data,
                 formula = formula1,
                 weights = yearly_data$sales)


    ## attach results
    flog.info("Writing results...")
    res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
    res1.dt[, outcome := Y]
    res1.dt[, controls := FE]
    res1.dt[, econ := "ln_unemp + ln_home_price"]
    res1.dt[, weights := "sales"]
    res1.dt[, Rsq := summary(res1)$r.squared]
    res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, output.pretrend.file)


    ## Run regression with current log sales
    res1 <- felm(data = yearly_data,
                 formula = formula1,
                 weights = yearly_data$ln_sales)


    ## attach results
    flog.info("Writing results...")
    res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
    res1.dt[, outcome := Y]
    res1.dt[, controls := FE]
    res1.dt[, econ := "ln_unemp + ln_home_price"]
    res1.dt[, weights := "ln_sales"]
    res1.dt[, Rsq := summary(res1)$r.squared]
    res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, output.pretrend.file)


    ## Run regression with population weights
    res1 <- felm(data = yearly_data,
                 formula = formula1,
                 weights = yearly_data$pop)


    ## attach results
    flog.info("Writing results...")
    res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
    res1.dt[, outcome := Y]
    res1.dt[, controls := FE]
    res1.dt[, econ := "ln_unemp + ln_home_price"]
    res1.dt[, weights := "population"]
    res1.dt[, Rsq := summary(res1)$r.squared]
    res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, output.pretrend.file)



    ## Run regression without weights but on counties with at least 150K population
    res1 <- felm(data = yearly_data[pop >= 150000,],
                 formula = formula1)


    ## attach results
    flog.info("Writing results...")
    res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
    res1.dt[, outcome := Y]
    res1.dt[, controls := FE]
    res1.dt[, econ := "ln_unemp + ln_home_price"]
    res1.dt[, weights := "Pop over 150K only"]
    res1.dt[, Rsq := summary(res1)$r.squared]
    res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, output.pretrend.file)


    ## Run regression without weights
    res1 <- felm(data = yearly_data,
                 formula = formula1)


    ## attach results
    flog.info("Writing results...")
    res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
    res1.dt[, outcome := Y]
    res1.dt[, controls := FE]
    res1.dt[, econ := "ln_unemp + ln_home_price"]
    res1.dt[, weights := "None"]
    res1.dt[, Rsq := summary(res1)$r.squared]
    res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, output.pretrend.file)

  }
}


## summary values --------------------------------------------------------------
LRdiff_res$N_obs <- nrow(yearly_data)
LRdiff_res$N_modules <- length(unique(yearly_data$product_module_code))
LRdiff_res$N_stores <- length(unique(yearly_data$store_code_uc))
LRdiff_res$N_counties <- uniqueN(yearly_data, by = c("fips_state", "fips_county"))
LRdiff_res$N_years <- uniqueN(yearly_data, by = c("year")) # should be 7
LRdiff_res$N_county_modules <- uniqueN(yearly_data, by = c("fips_state", "fips_county",
                                                           "product_module_code"))
LRdiff_res$N_store_modules <- uniqueN(yearly_data, by = c("store_code_uc",
                                                          "product_module_code"))
LRdiff_res$N_state_modules <- uniqueN(yearly_data, by = c("fips_state",
                                                          "product_module_code"))


fwrite(LRdiff_res, output.pretrend.file)
