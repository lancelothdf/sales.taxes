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


###OUTPUT
output.results.file <- "Data/LRdiff_2years_Diff.csv"

######### Regression analysis
yearly_data <- fread(output_yearly)



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
yearly_data[, cal_time := (year-2008)/2] ## Of course normalization here does not matter to estimation - normalization chosen here so that with two year intervals, cal_time will take values 0, 1, 2 and 3


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



######## Limit data to relevant time periods only and create lag and lead econ conditions
##NOTE: Make sure that we only include 2008-2014 in the regressions!!
yearly_data <- yearly_data[year %in% c(2006, 2008, 2010, 2012, 2014, 2016),]
yearly_data <- yearly_data[order(store_code_uc, product_module_code, year),]


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


##
yearly_data[, lag_D.ln_unemp := shift(ln_unemp, n=1, type="lag"),
         by = .(store_code_uc, product_module_code)]

yearly_data[, lead_D.ln_unemp := shift(ln_unemp, n=1, type = "lead"),
              by = .(store_code_uc, product_module_code)]

yearly_data[, lag_D.ln_home_price := shift(ln_home_price, n=1, type="lag"),
            by = .(store_code_uc, product_module_code)]

yearly_data[, lead_D.ln_home_price := shift(ln_home_price, n=1, type = "lead"),
            by = .(store_code_uc, product_module_code)]



## !!! Only include 2010 - 2012 - 2014 to create 2-year differences
## ---------------------------------------------------
yearly_data <- yearly_data[year >= 2010 & year <= 2014,]
#yearly_data <- yearly_data[year %in% c(2008, 2010, 2012, 2014),]



#############################################
### First: regress price and quantity on tax rate after controling for econ. conditions (+ without econ controls)
list.outcomes <- c("D.ln_cpricei", "D.ln_cpricei2", "D.ln_quantity", "D.ln_quantity2")
FE_opts <- c("cal_time", "module_by_time", "region_by_module_by_time", "division_by_module_by_time")
Econ_opts <- c("D.ln_unemp", "D.ln_home_price", "D.ln_unemp + D.ln_home_price")


## Create a matrix with controls for econ conditions that include leads and lags - also store indicators that will be used in final matrix with results
Econ_w_lags <- c("D.ln_unemp", "D.ln_unemp", "D.ln_unemp + D.ln_home_price", "D.ln_unemp + D.ln_home_price")
Econ_w_lags <- rbind(Econ_w_lags, c("Yes", "Yes", "Yes", "Yes"))
Econ_w_lags <- rbind(Econ_w_lags, c("No", "Yes", "No", "Yes"))

Econ_w_lags <- rbind(Econ_w_lags, c("lag_D.ln_unemp + D.ln_unemp", "lag_D.ln_unemp + D.ln_unemp + lead_D.ln_unemp", "lag_D.ln_unemp + lag_D.ln_home_price + D.ln_unemp + D.ln_home_price", "lag_D.ln_unemp + lag_D.ln_home_price + D.ln_unemp + D.ln_home_price + lead_D.ln_unemp + lead_D.ln_home_price"))


LRdiff_res <- data.table(NULL)
for(j in 1:length(list.outcomes)) {
  for(FE in FE_opts) {

    #
    outcome.j <- list.outcomes[j]


    ## Formula
    formula1 <- as.formula(paste0(
      outcome.j, " ~ D.ln_sales_tax | ", "store_module + ", FE, " | 0 | state_by_module "
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
    res1.dt[, econ := "none"]
    res1.dt[, lag := NA]
    res1.dt[, lead := NA]
    res1.dt[, Rsq := summary(res1)$r.squared]
    res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)

    ##
    for(EC in Econ_opts) {

      ## Formula
      formula1 <- as.formula(paste0(
        outcome.j, " ~ D.ln_sales_tax", " + ", EC, " | ", "store_module + ", FE, " | 0 | state_by_module "
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
      res1.dt[, econ := EC]
      res1.dt[, lag := "No"]
      res1.dt[, lead := "No"]
      res1.dt[, Rsq := summary(res1)$r.squared]
      res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
      LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
      fwrite(LRdiff_res, output.results.file)
    }

    ##
    for(i in 1:dim(Econ_w_lags)[2]) {

      EC <- Econ_w_lags[4, i]

      ## Formula
      formula1 <- as.formula(paste0(
        outcome.j, " ~ D.ln_sales_tax", " + ", EC, " | ", "store_module + ", FE, " | 0 | state_by_module "
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
      res1.dt[, lag := Econ_w_lags[2,i]]
      res1.dt[, lead := Econ_w_lags[3,i]]
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


####
fwrite(LRdiff_res, output.results.file)
