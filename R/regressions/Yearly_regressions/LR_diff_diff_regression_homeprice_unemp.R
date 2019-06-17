### In this R-file we explore a specification meant to estimate long-run pass-through and sales/quantity response to tax changes

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


######### Regression analysis ##NOTE: Make sure that we only include 2008-2014 in the regressions!!
## run the analysis on price ---------------------------------------------------
yearly_data <- fread(output_yearly)
yearly_data <- yearly_data[year >= 2008 & year <= 2014,]


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


#############################################
### First: regress price and quantity on tax rate after controling for econ. conditions
list.outcomes <- c("ln_cpricei", "ln_cpricei2", "ln_quantity", "ln_quantity2")

for(j in 1:length(list.outcomes)) {

  #
  outcome.j <- list.outcomes[j]


  #### First control for house prices
  ## no FE (only store_module and time)
   price_formula1 <- as.formula(paste0(
     outcome.j, " ~ ln_sales_tax + ln_home_price | store_module + yr | 0 | state_by_module "
   ))



   price_res1 <- felm(data = yearly_data,
                     formula = price_formula1,
                     weights = yearly_data$base.sales)

   if(j == 1) {
   LRdiff_res <-
     data.table(
       outcome = outcome.j,
       estimate = coef(summary(price_res1))["ln_sales_tax", "Estimate"],
       se = coef(summary(price_res1))["ln_sales_tax", "Cluster s.e."],
       pval = coef(summary(price_res1))["ln_sales_tax", "Pr(>|t|)"],
       est_house = coef(summary(price_res1))["ln_home_price", "Estimate"],
       se_house = coef(summary(price_res1))["ln_home_price", "Cluster s.e."],
       pval_house = coef(summary(price_res1))["ln_home_price", "Pr(>|t|)"],
       est_unemp = NA,
       se_unemp = NA,
       pval_unemp = NA,
       Rsq = summary(price_res1)$r.squared,
       adj.Rsq = summary(price_res1)$adj.r.squared,
       time_controls = "time FE",
       econ_controls = "House prices"
     )

   } else {

     LRdiff_res <-
       rbind(LRdiff_res,
       data.table(
         outcome = outcome.j,
         estimate = coef(summary(price_res1))["ln_sales_tax", "Estimate"],
         se = coef(summary(price_res1))["ln_sales_tax", "Cluster s.e."],
         pval = coef(summary(price_res1))["ln_sales_tax", "Pr(>|t|)"],
         est_house = coef(summary(price_res1))["ln_home_price", "Estimate"],
         se_house = coef(summary(price_res1))["ln_home_price", "Cluster s.e."],
         pval_house = coef(summary(price_res1))["ln_home_price", "Pr(>|t|)"],
         est_unemp = NA,
         se_unemp = NA,
         pval_unemp = NA,
         Rsq = summary(price_res1)$r.squared,
         adj.Rsq = summary(price_res1)$adj.r.squared,
         time_controls = "time FE",
         econ_controls = "House prices"
       )
       )
   }


  ## with module-time FE
  price_formula <- as.formula(paste0(
    outcome.j, " ~ ln_sales_tax + ln_home_price | store_module + module_by_time | 0 | state_by_module "
  ))

  price_res <- felm(data = yearly_data,
                     formula = price_formula,
                     weights = yearly_data$base.sales)

  LRdiff_res <-rbind(
    LRdiff_res,
    data.table(
      outcome = outcome.j,
      estimate = coef(summary(price_res))["ln_sales_tax", "Estimate"],
      se = coef(summary(price_res))["ln_sales_tax", "Cluster s.e."],
      pval = coef(summary(price_res))["ln_sales_tax", "Pr(>|t|)"],
      est_house = coef(summary(price_res))["ln_home_price", "Estimate"],
      se_house = coef(summary(price_res))["ln_home_price", "Cluster s.e."],
      pval_house = coef(summary(price_res))["ln_home_price", "Pr(>|t|)"],
      est_unemp = NA,
      se_unemp = NA,
      pval_unemp = NA,
      Rsq = summary(price_res)$r.squared,
      adj.Rsq = summary(price_res)$adj.r.squared,
      time_controls = "module-time FE",
      econ_controls = "House prices"
    )
  )



  ## with Module_by_Region_by_time FE
  price_formula8 <- as.formula(paste0(
    outcome.j, " ~ ln_sales_tax + ln_home_price | store_module + module_by_time + reg_by_module_by_time | 0 | state_by_module "
  ))

  price_res4 <- felm(data = yearly_data,
                     formula = price_formula8,
                     weights = yearly_data$base.sales)

  LRdiff_res <-rbind(
    LRdiff_res,
    data.table(
      outcome = outcome.j,
      estimate = coef(summary(price_res4))["ln_sales_tax", "Estimate"],
      se = coef(summary(price_res4))["ln_sales_tax", "Cluster s.e."],
      pval = coef(summary(price_res4))["ln_sales_tax", "Pr(>|t|)"],
      est_house = coef(summary(price_res4))["ln_home_price", "Estimate"],
      se_house = coef(summary(price_res4))["ln_home_price", "Cluster s.e."],
      pval_house = coef(summary(price_res4))["ln_home_price", "Pr(>|t|)"],
      est_unemp = NA,
      se_unemp = NA,
      pval_unemp = NA,
      Rsq = summary(price_res4)$r.squared,
      adj.Rsq = summary(price_res4)$adj.r.squared,
      time_controls = "module-region-time FE",
      econ_controls = "House prices"
    )
  )


  ## with Module_by_Division_by_time FE
  price_formula8 <- as.formula(paste0(
    outcome.j, " ~ ln_sales_tax + ln_home_price | store_module + module_by_time + div_by_module_by_time | 0 | state_by_module "
  ))

  price_res4 <- felm(data = yearly_data,
                     formula = price_formula8,
                     weights = yearly_data$base.sales)

  LRdiff_res <-rbind(
    LRdiff_res,
    data.table(
      outcome = outcome.j,
      estimate = coef(summary(price_res4))["ln_sales_tax", "Estimate"],
      se = coef(summary(price_res4))["ln_sales_tax", "Cluster s.e."],
      pval = coef(summary(price_res4))["ln_sales_tax", "Pr(>|t|)"],
      est_house = coef(summary(price_res4))["ln_home_price", "Estimate"],
      se_house = coef(summary(price_res4))["ln_home_price", "Cluster s.e."],
      pval_house = coef(summary(price_res4))["ln_home_price", "Pr(>|t|)"],
      est_unemp = NA,
      se_unemp = NA,
      pval_unemp = NA,
      Rsq = summary(price_res4)$r.squared,
      adj.Rsq = summary(price_res4)$adj.r.squared,
      time_controls = "module-division-time FE",
      econ_controls = "House prices"
    )
  )


  #### Second, control for the unemployment rate (at the county-level)
  ## no FE (only store_module and time)
  price_formula1 <- as.formula(paste0(
    outcome.j, " ~ ln_sales_tax + ln_unemp | store_module + yr | 0 | state_by_module "
  ))



  price_res1 <- felm(data = yearly_data,
                     formula = price_formula1,
                     weights = yearly_data$base.sales)


  LRdiff_res <-
    rbind(LRdiff_res,
          data.table(
            outcome = outcome.j,
            estimate = coef(summary(price_res1))["ln_sales_tax", "Estimate"],
            se = coef(summary(price_res1))["ln_sales_tax", "Cluster s.e."],
            pval = coef(summary(price_res1))["ln_sales_tax", "Pr(>|t|)"],
            est_house = NA,
            se_house = NA,
            pval_house = NA,
            est_unemp = coef(summary(price_res1))["ln_unemp", "Estimate"],
            se_unemp = coef(summary(price_res1))["ln_unemp", "Cluster s.e."],
            pval_unemp = coef(summary(price_res1))["ln_unemp", "Pr(>|t|)"],
            Rsq = summary(price_res1)$r.squared,
            adj.Rsq = summary(price_res1)$adj.r.squared,
            time_controls = "time FE",
            econ_controls = "Unemployment rate"
          )
    )


  ## with module-time FE
  price_formula <- as.formula(paste0(
    outcome.j, " ~ ln_sales_tax + ln_unemp | store_module + module_by_time | 0 | state_by_module "
  ))

  price_res <- felm(data = yearly_data,
                    formula = price_formula,
                    weights = yearly_data$base.sales)

  LRdiff_res <-rbind(
    LRdiff_res,
    data.table(
      outcome = outcome.j,
      estimate = coef(summary(price_res))["ln_sales_tax", "Estimate"],
      se = coef(summary(price_res))["ln_sales_tax", "Cluster s.e."],
      pval = coef(summary(price_res))["ln_sales_tax", "Pr(>|t|)"],
      est_house = NA,
      se_house = NA,
      pval_house = NA,
      est_unemp = coef(summary(price_res))["ln_unemp", "Estimate"],
      se_unemp = coef(summary(price_res))["ln_unemp", "Cluster s.e."],
      pval_unemp = coef(summary(price_res))["ln_unemp", "Pr(>|t|)"],
      Rsq = summary(price_res)$r.squared,
      adj.Rsq = summary(price_res)$adj.r.squared,
      time_controls = "module-time FE",
      econ_controls = "Unemployment rate"
    )
  )



  ## with Module_by_Region_by_time FE
  price_formula8 <- as.formula(paste0(
    outcome.j, " ~ ln_sales_tax + ln_unemp | store_module + module_by_time + reg_by_module_by_time | 0 | state_by_module "
  ))

  price_res4 <- felm(data = yearly_data,
                     formula = price_formula8,
                     weights = yearly_data$base.sales)

  LRdiff_res <-rbind(
    LRdiff_res,
    data.table(
      outcome = outcome.j,
      estimate = coef(summary(price_res4))["ln_sales_tax", "Estimate"],
      se = coef(summary(price_res4))["ln_sales_tax", "Cluster s.e."],
      pval = coef(summary(price_res4))["ln_sales_tax", "Pr(>|t|)"],
      est_house = NA,
      se_house = NA,
      pval_house = NA,
      est_unemp = coef(summary(price_res4))["ln_unemp", "Estimate"],
      se_unemp = coef(summary(price_res4))["ln_unemp", "Cluster s.e."],
      pval_unemp = coef(summary(price_res4))["ln_unemp", "Pr(>|t|)"],
      Rsq = summary(price_res4)$r.squared,
      adj.Rsq = summary(price_res4)$adj.r.squared,
      time_controls = "module-region-time FE",
      econ_controls = "Unemployment rate"
    )
  )


  ## with Module_by_Division_by_time FE
  price_formula8 <- as.formula(paste0(
    outcome.j, " ~ ln_sales_tax + ln_unemp | store_module + module_by_time + div_by_module_by_time | 0 | state_by_module "
  ))

  price_res4 <- felm(data = yearly_data,
                     formula = price_formula8,
                     weights = yearly_data$base.sales)

  LRdiff_res <-rbind(
    LRdiff_res,
    data.table(
      outcome = outcome.j,
      estimate = coef(summary(price_res4))["ln_sales_tax", "Estimate"],
      se = coef(summary(price_res4))["ln_sales_tax", "Cluster s.e."],
      pval = coef(summary(price_res4))["ln_sales_tax", "Pr(>|t|)"],
      est_house = NA,
      se_house = NA,
      pval_house = NA,
      est_unemp = coef(summary(price_res4))["ln_unemp", "Estimate"],
      se_unemp = coef(summary(price_res4))["ln_unemp", "Cluster s.e."],
      pval_unemp = coef(summary(price_res4))["ln_unemp", "Pr(>|t|)"],
      Rsq = summary(price_res4)$r.squared,
      adj.Rsq = summary(price_res4)$adj.r.squared,
      time_controls = "module-division-time FE",
      econ_controls = "Unemployment rate"
    )
  )



  #### Third, control for both house prices and unemployment
  ## no FE (only store_module and time)
  price_formula1 <- as.formula(paste0(
    outcome.j, " ~ ln_sales_tax + ln_home_price + ln_unemp | store_module + yr | 0 | state_by_module "
  ))



  price_res1 <- felm(data = yearly_data,
                     formula = price_formula1,
                     weights = yearly_data$base.sales)


  LRdiff_res <-
    rbind(LRdiff_res,
          data.table(
            outcome = outcome.j,
            estimate = coef(summary(price_res1))["ln_sales_tax", "Estimate"],
            se = coef(summary(price_res1))["ln_sales_tax", "Cluster s.e."],
            pval = coef(summary(price_res1))["ln_sales_tax", "Pr(>|t|)"],
            est_house = coef(summary(price_res1))["ln_home_price", "Estimate"],
            se_house = coef(summary(price_res1))["ln_home_price", "Cluster s.e."],
            pval_house = coef(summary(price_res1))["ln_home_price", "Pr(>|t|)"],
            est_unemp = coef(summary(price_res1))["ln_unemp", "Estimate"],
            se_unemp = coef(summary(price_res1))["ln_unemp", "Cluster s.e."],
            pval_unemp = coef(summary(price_res1))["ln_unemp", "Pr(>|t|)"],
            Rsq = summary(price_res1)$r.squared,
            adj.Rsq = summary(price_res1)$adj.r.squared,
            time_controls = "time FE",
            econ_controls = "House prices + Unemployment rate"
          )
    )


  ## with module-time FE
  price_formula <- as.formula(paste0(
    outcome.j, " ~ ln_sales_tax + ln_home_price + ln_unemp | store_module + module_by_time | 0 | state_by_module "
  ))

  price_res <- felm(data = yearly_data,
                    formula = price_formula,
                    weights = yearly_data$base.sales)

  LRdiff_res <-rbind(
    LRdiff_res,
    data.table(
      outcome = outcome.j,
      estimate = coef(summary(price_res))["ln_sales_tax", "Estimate"],
      se = coef(summary(price_res))["ln_sales_tax", "Cluster s.e."],
      pval = coef(summary(price_res))["ln_sales_tax", "Pr(>|t|)"],
      est_house = coef(summary(price_res))["ln_home_price", "Estimate"],
      se_house = coef(summary(price_res))["ln_home_price", "Cluster s.e."],
      pval_house = coef(summary(price_res))["ln_home_price", "Pr(>|t|)"],
      est_unemp = coef(summary(price_res))["ln_unemp", "Estimate"],
      se_unemp = coef(summary(price_res))["ln_unemp", "Cluster s.e."],
      pval_unemp = coef(summary(price_res))["ln_unemp", "Pr(>|t|)"],
      Rsq = summary(price_res)$r.squared,
      adj.Rsq = summary(price_res)$adj.r.squared,
      time_controls = "module-time FE",
      econ_controls = "House prices + Unemployment rate"
    )
  )



  ## with Module_by_Region_by_time FE
  price_formula8 <- as.formula(paste0(
    outcome.j, " ~ ln_sales_tax + ln_home_price +ln_unemp | store_module + module_by_time + reg_by_module_by_time | 0 | state_by_module "
  ))

  price_res4 <- felm(data = yearly_data,
                     formula = price_formula8,
                     weights = yearly_data$base.sales)

  LRdiff_res <-rbind(
    LRdiff_res,
    data.table(
      outcome = outcome.j,
      estimate = coef(summary(price_res4))["ln_sales_tax", "Estimate"],
      se = coef(summary(price_res4))["ln_sales_tax", "Cluster s.e."],
      pval = coef(summary(price_res4))["ln_sales_tax", "Pr(>|t|)"],
      est_house = coef(summary(price_res4))["ln_home_price", "Estimate"],
      se_house = coef(summary(price_res4))["ln_home_price", "Cluster s.e."],
      pval_house = coef(summary(price_res4))["ln_home_price", "Pr(>|t|)"],
      est_unemp = coef(summary(price_res4))["ln_unemp", "Estimate"],
      se_unemp = coef(summary(price_res4))["ln_unemp", "Cluster s.e."],
      pval_unemp = coef(summary(price_res4))["ln_unemp", "Pr(>|t|)"],
      Rsq = summary(price_res4)$r.squared,
      adj.Rsq = summary(price_res4)$adj.r.squared,
      time_controls = "module-region-time FE",
      econ_controls = "House prices + Unemployment rate"
    )
  )


  ## with Module_by_Division_by_time FE
  price_formula8 <- as.formula(paste0(
    outcome.j, " ~ ln_sales_tax + ln_home_price + ln_unemp | store_module + module_by_time + div_by_module_by_time | 0 | state_by_module "
  ))

  price_res4 <- felm(data = yearly_data,
                     formula = price_formula8,
                     weights = yearly_data$base.sales)

  LRdiff_res <-rbind(
    LRdiff_res,
    data.table(
      outcome = outcome.j,
      estimate = coef(summary(price_res4))["ln_sales_tax", "Estimate"],
      se = coef(summary(price_res4))["ln_sales_tax", "Cluster s.e."],
      pval = coef(summary(price_res4))["ln_sales_tax", "Pr(>|t|)"],
      est_house = coef(summary(price_res4))["ln_home_price", "Estimate"],
      se_house = coef(summary(price_res4))["ln_home_price", "Cluster s.e."],
      pval_house = coef(summary(price_res4))["ln_home_price", "Pr(>|t|)"],
      est_unemp = coef(summary(price_res4))["ln_unemp", "Estimate"],
      se_unemp = coef(summary(price_res4))["ln_unemp", "Cluster s.e."],
      pval_unemp = coef(summary(price_res4))["ln_unemp", "Pr(>|t|)"],
      Rsq = summary(price_res4)$r.squared,
      adj.Rsq = summary(price_res4)$adj.r.squared,
      time_controls = "module-division-time FE",
      econ_controls = "House prices + Unemployment rate"
    )
  )

}


## summary values --------------------------------------------------------------
LRdiff_res$N_obs <- nrow(yearly_data)
LRdiff_res$N_modules <- length(unique(yearly_data$product_module_code))
LRdiff_res$N_stores <- length(unique(yearly_data$store_code_uc))
LRdiff_res$N_counties <- uniqueN(yearly_data, by = c("fips_state", "fips_county"))
LRdiff_res$N_years <- uniqueN(yearly_data, by = c("year")) # should be 7
LRdiff_res$N_county_modules <- uniqueN(yearly_data, by = c("fips_state", "fips_county",
                                                    "product_module_code"))

fwrite(LRdiff_res, "Data/LRdiff_results_econ_controls.csv")





#################################################################################
#################################################################################
#############################################
### Second: regress econ. conditions on ln_sales_tax (like a placebo test)
list.outcomes <- c("ln_home_price", "ln_unemp", "unemp")

for(j in 1:length(list.outcomes)) {

  #
  outcome.j <- list.outcomes[j]


  #### First control for house prices
  ## no FE (only store_module and time)
  price_formula1 <- as.formula(paste0(
    outcome.j, " ~ ln_sales_tax | store_module + yr | 0 | state_by_module "
  ))



  price_res1 <- felm(data = yearly_data,
                     formula = price_formula1,
                     weights = yearly_data$base.sales)

  if(j == 1) {
    LRdiff_res <-
      data.table(
        outcome = outcome.j,
        estimate = coef(summary(price_res1))["ln_sales_tax", "Estimate"],
        se = coef(summary(price_res1))["ln_sales_tax", "Cluster s.e."],
        pval = coef(summary(price_res1))["ln_sales_tax", "Pr(>|t|)"],
        Rsq = summary(price_res1)$r.squared,
        adj.Rsq = summary(price_res1)$adj.r.squared,
        time_controls = "time FE"
      )

  } else {

    LRdiff_res <-
      rbind(LRdiff_res,
            data.table(
              outcome = outcome.j,
              estimate = coef(summary(price_res1))["ln_sales_tax", "Estimate"],
              se = coef(summary(price_res1))["ln_sales_tax", "Cluster s.e."],
              pval = coef(summary(price_res1))["ln_sales_tax", "Pr(>|t|)"],
              Rsq = summary(price_res1)$r.squared,
              adj.Rsq = summary(price_res1)$adj.r.squared,
              time_controls = "time FE"
            )
      )
  }


  ## with module-time FE
  price_formula <- as.formula(paste0(
    outcome.j, " ~ ln_sales_tax | store_module + module_by_time | 0 | state_by_module "
  ))

  price_res <- felm(data = yearly_data,
                    formula = price_formula,
                    weights = yearly_data$base.sales)

  LRdiff_res <-rbind(
    LRdiff_res,
    data.table(
      outcome = outcome.j,
      estimate = coef(summary(price_res))["ln_sales_tax", "Estimate"],
      se = coef(summary(price_res))["ln_sales_tax", "Cluster s.e."],
      pval = coef(summary(price_res))["ln_sales_tax", "Pr(>|t|)"],
      Rsq = summary(price_res)$r.squared,
      adj.Rsq = summary(price_res)$adj.r.squared,
      time_controls = "module-time FE"
    )
  )



  ## with Module_by_Region_by_time FE
  price_formula8 <- as.formula(paste0(
    outcome.j, " ~ ln_sales_tax | store_module + module_by_time + reg_by_module_by_time | 0 | state_by_module "
  ))

  price_res4 <- felm(data = yearly_data,
                     formula = price_formula8,
                     weights = yearly_data$base.sales)

  LRdiff_res <-rbind(
    LRdiff_res,
    data.table(
      outcome = outcome.j,
      estimate = coef(summary(price_res4))["ln_sales_tax", "Estimate"],
      se = coef(summary(price_res4))["ln_sales_tax", "Cluster s.e."],
      pval = coef(summary(price_res4))["ln_sales_tax", "Pr(>|t|)"],
      Rsq = summary(price_res4)$r.squared,
      adj.Rsq = summary(price_res4)$adj.r.squared,
      time_controls = "module-region-time FE"
    )
  )


  ## with Module_by_Division_by_time FE
  price_formula8 <- as.formula(paste0(
    outcome.j, " ~ ln_sales_tax | store_module + module_by_time + div_by_module_by_time | 0 | state_by_module "
  ))

  price_res4 <- felm(data = yearly_data,
                     formula = price_formula8,
                     weights = yearly_data$base.sales)

  LRdiff_res <-rbind(
    LRdiff_res,
    data.table(
      outcome = outcome.j,
      estimate = coef(summary(price_res4))["ln_sales_tax", "Estimate"],
      se = coef(summary(price_res4))["ln_sales_tax", "Cluster s.e."],
      pval = coef(summary(price_res4))["ln_sales_tax", "Pr(>|t|)"],
      Rsq = summary(price_res4)$r.squared,
      adj.Rsq = summary(price_res4)$adj.r.squared,
      time_controls = "module-division-time FE"
    )
  )

}


## summary values --------------------------------------------------------------
LRdiff_res$N_obs <- nrow(yearly_data)
LRdiff_res$N_modules <- length(unique(yearly_data$product_module_code))
LRdiff_res$N_stores <- length(unique(yearly_data$store_code_uc))
LRdiff_res$N_counties <- uniqueN(yearly_data, by = c("fips_state", "fips_county"))
LRdiff_res$N_years <- uniqueN(yearly_data, by = c("year")) # should be 7
LRdiff_res$N_county_modules <- uniqueN(yearly_data, by = c("fips_state", "fips_county",
                                                           "product_module_code"))

fwrite(LRdiff_res, "Data/LRdiff_results_placebos_econ_conditions.csv")


