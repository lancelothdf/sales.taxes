### In this R-file we explore a specification meant to estimate long-run pass-through and sales/quantity response to tax changes

library(data.table)
library(lfe)
library(futile.logger)
library(AER)


setwd("/project2/igaarder")

## useful filepaths ------------------------------------------------------------
all_goods_pi_path <- "Data/Nielsen/price_quantity_indices_allitems_2006-2016_notaxinfo.csv"
FE_pindex_path <- "Data/Nielsen/Pindex_FE_yearly_all_years.csv"
output_semester <- "Data/Nielsen/semester_nielsen_data.csv"
pre_trend_data_path <- "Data/Nielsen/pre_trend_data_semester.csv"
Q1_data_path <- "Data/all_nielsen_data_2006_2016_Q1only.csv"
#taxable_pi_path <- "Data/Nielsen/price_quantity_indices_taxableitems_2006-2016.csv"
#eventstudy_tr_path <- "Data/event_study_tr_groups_comprehensive_firstonly_no2012q4_2013q1q2.csv"
#tr_groups_path <- "Data/tr_groups_comprehensive_firstonly_no2012q4_2013q1q2.csv"


covariates.nhgis.path <- "Data/covariates/nhgis_county_clean.csv"
covariates.qcew.path <- "Data/covariates/qcew_clean.csv"
census.regions.path <- "Data/covariates/census_regions.csv"

zillow_path <- "Data/covariates/zillow_long_by_county_clean.csv"
zillow_state_path <- "Data/covariates/zillow_long_by_state_clean.csv"
unemp.path <- "Data/covariates/county_monthly_unemp_clean.csv"

# ### Prepare the data
## See LR_diff_diff_regression_sales_FE_specification.R -- the file output_yearly is prepared in that file
yearly_data <- fread(output_semester)


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

yearly_data[, region_by_time := .GRP, by = .(Region, year, semester)]
yearly_data[, division_by_time := .GRP, by = .(Division, year, semester)]
yearly_data[, urban_by_time := .GRP, by = .(urban, year, semester)]
yearly_data[, regXurban_by_time := .GRP, by = .(Region, urban, year, semester)]
yearly_data[, divXurban_by_time := .GRP, by = .(Division, urban, year, semester)]
yearly_data[, reg_by_module_by_time := .GRP, by = .(Region, product_module_code, year, semester)]
yearly_data[, div_by_module_by_time := .GRP, by = .(Division, product_module_code, year, semester)]
################################



##################################

### Create measure of change in log sales tax rate
yearly_data <- yearly_data[order(fips_state, fips_county, store_code_uc, product_module_code, -year, -semester),] ##Sort on store by year (year in descending order)

yearly_data$dltax <- -diff(yearly_data$ln_sales_tax) ## Difference log of tax rate
yearly_data$dltax[yearly_data$year <= 2008] <- NA



### First produce graphs to explore pre-trends
list.outcomes <- c("ln_cpricei", "ln_quantity", "ln_sales_tax")
for(j in 1:length(list.outcomes)) {

  #
  outcome.j <- list.outcomes[j]

  ## Start with log price index (1)
  diff_data <- yearly_data[, c("year", "fips_state", "fips_county", "store_code_uc", "product_module_code", "dltax", "base.sales")]
  temp <- yearly_data[[outcome.j]]
  diff_data$outcome <- temp

  # Compute residualized price index for different designs
  reg_formula <- as.formula(paste0(outcome.j,
    " ~ 0 | yr | 0 | state_by_module "
  ))

  reg_res <- felm(data = yearly_data,
                              formula = reg_formula,
                              weights = yearly_data$base.sales)

  diff_data$outcome_res0 <- reg_res$residuals

  reg_formula <- as.formula(paste0(outcome.j,
    " ~ 0 | module_by_time | 0 | state_by_module "
  ))

  reg_res <- felm(data = yearly_data,
                  formula = reg_formula,
                  weights = yearly_data$base.sales)

  diff_data$outcome_res1 <- reg_res$residuals

  reg_formula <- as.formula(paste0(outcome.j,
    " ~ 0 | store_by_time | 0 | state_by_module "
  ))

  reg_res <- felm(data = yearly_data,
                  formula = reg_formula,
                  weights = yearly_data$base.sales)

  diff_data$outcome_res2 <- reg_res$residuals


  ##
  reg_formula <- as.formula(paste0(outcome.j,
    " ~ 0 | module_by_time + store_by_time | 0 | state_by_module "
  ))

  reg_res <- felm(data = yearly_data,
                  formula = reg_formula,
                  weights = yearly_data$base.sales)

  diff_data$outcome_res3 <- reg_res$residuals


  ##
  reg_formula <- as.formula(paste0(outcome.j,
                                   " ~ 0 | module_by_time + region_by_time | 0 | state_by_module "
  ))

  reg_res <- felm(data = yearly_data,
                  formula = reg_formula,
                  weights = yearly_data$base.sales)

  diff_data$outcome_res4 <- reg_res$residuals


  ##
  reg_formula <- as.formula(paste0(outcome.j,
                                   " ~ 0 | module_by_time + division_by_time | 0 | state_by_module "
  ))

  reg_res <- felm(data = yearly_data,
                  formula = reg_formula,
                  weights = yearly_data$base.sales)

  diff_data$outcome_res5 <- reg_res$residuals


  ##
  reg_formula <- as.formula(paste0(outcome.j,
                                   " ~ 0 | reg_by_module_by_time | 0 | state_by_module "
  ))

  reg_res <- felm(data = yearly_data,
                  formula = reg_formula,
                  weights = yearly_data$base.sales)

  diff_data$outcome_res6 <- reg_res$residuals





  # Create means by "treatment" vs "control"
  for (k in 2009:2014) {

    data.year.k <- diff_data[year == k, ]

    # Identify observations that had a tax change in year k
    increase_store_modules <- data.year.k[dltax >= 0.001, c("store_code_uc", "product_module_code")]
    nochange_store_modules <- data.year.k[dltax < 0.001 & dltax > - 0.001, c("store_code_uc", "product_module_code")]
    decrease_store_modules <- data.year.k[dltax <= -0.001, c("store_code_uc", "product_module_code")]

    setkey(increase_store_modules, store_code_uc, product_module_code)
    setkey(nochange_store_modules, store_code_uc, product_module_code)
    setkey(decrease_store_modules, store_code_uc, product_module_code)


        #Keep only data in year k-3 to k+3 (or 2014)
        data.year.j <- diff_data[year >= k-3 & year <= min(k+3, 2016), ]
        setkey(data.year.j, store_code_uc, product_module_code)

        #Divide data in increase/decrease/no change in year k
        increase.data.j <- data.year.j[increase_store_modules]
        nochange.data.j <- data.year.j[nochange_store_modules]
        decrease.data.j <- data.year.j[decrease_store_modules]

        #Take the weighted means
        increase.data.j <- increase.data.j[, list(dltax = weighted.mean(dltax, w = base.sales), outcome = weighted.mean(outcome, w = base.sales), outcome_res0 = weighted.mean(outcome_res0, w = base.sales), outcome_res1 = weighted.mean(outcome_res1, w = base.sales), outcome_res2 = weighted.mean(outcome_res2, w = base.sales), outcome_res3 = weighted.mean(outcome_res3, w=base.sales), outcome_res4 = weighted.mean(outcome_res4, w=base.sales), outcome_res5 = weighted.mean(outcome_res5, w=base.sales), outcome_res6 = weighted.mean(outcome_res6, w=base.sales), totsales = sum(base.sales), n = .N), by = .(year)]
        decrease.data.j <- decrease.data.j[, list(dltax = weighted.mean(dltax, w = base.sales), outcome = weighted.mean(outcome, w = base.sales), outcome_res0 = weighted.mean(outcome_res0, w = base.sales), outcome_res1 = weighted.mean(outcome_res1, w = base.sales), outcome_res2 = weighted.mean(outcome_res2, w = base.sales), outcome_res3 = weighted.mean(outcome_res3, w=base.sales), outcome_res4 = weighted.mean(outcome_res4, w=base.sales), outcome_res5 = weighted.mean(outcome_res5, w=base.sales), outcome_res6 = weighted.mean(outcome_res6, w=base.sales), totsales = sum(base.sales), n = .N), by = .(year)]
        nochange.data.j <- nochange.data.j[, list(dltax = weighted.mean(dltax, w = base.sales), outcome = weighted.mean(outcome, w = base.sales), outcome_res0 = weighted.mean(outcome_res0, w = base.sales), outcome_res1 = weighted.mean(outcome_res1, w = base.sales), outcome_res2 = weighted.mean(outcome_res2, w = base.sales), outcome_res3 = weighted.mean(outcome_res3, w=base.sales), outcome_res4 = weighted.mean(outcome_res4, w=base.sales), outcome_res5 = weighted.mean(outcome_res5, w=base.sales), outcome_res6 = weighted.mean(outcome_res6, w=base.sales), totsales = sum(base.sales), n = .N), by = .(year)]

        #Compute the average change in log(1+tax) in year k for each group
        increase.data.j[, dltax_k_inc := dltax[year == k]]
        decrease.data.j$dltax_k_inc <- increase.data.j$dltax_k_inc
        nochange.data.j$dltax_k_inc <- increase.data.j$dltax_k_inc

        decrease.data.j[, dltax_k_dec := dltax[year == k]]
        increase.data.j$dltax_k_dec <- decrease.data.j$dltax_k_dec
        nochange.data.j$dltax_k_dec <- decrease.data.j$dltax_k_dec

        nochange.data.j[, dltax_k_noc := dltax[year == k]]
        increase.data.j$dltax_k_noc <- nochange.data.j$dltax_k_noc
        decrease.data.j$dltax_k_noc <- nochange.data.j$dltax_k_noc

        #Create cohort, treatment and variable variables
        increase.data.j$cohort <- k
        decrease.data.j$cohort <- k
        nochange.data.j$cohort <- k
        increase.data.j$treatment <- "increase"
        decrease.data.j$treatment <- "decrease"
        nochange.data.j$treatment <- "no change"
        increase.data.j$variable <- outcome.j
        decrease.data.j$variable <- outcome.j
        nochange.data.j$variable <- outcome.j


        if(j == 1 & k == 2009) {
          pre.trend.data <- rbind(increase.data.j, decrease.data.j, nochange.data.j)
        }
        else {
          pre.trend.data <- rbind(pre.trend.data, increase.data.j, decrease.data.j, nochange.data.j)
        }
  }

}

pre.trend.data[, tt_event := year - cohort]
fwrite(pre.trend.data, pre_trend_data_path)


#### Plot the averages across cohorts
#pre.trend.data <- fread(pre_trend_data_path)
