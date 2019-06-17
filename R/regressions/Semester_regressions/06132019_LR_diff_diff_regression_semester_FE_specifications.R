### In this R-file we explore a specification meant to estimate long-run pass-through and sales/quantity response to tax changes

library(data.table)
library(lfe)
library(futile.logger)
library(AER)


setwd("/project2/igaarder")

## useful filepaths ------------------------------------------------------------
FE_pindex_path <- "Data/Nielsen/Pindex_FE_yearly_all_years.csv"
output_semester <- "Data/Nielsen/semester_nielsen_data.csv"
#Q1_data_path <- "Data/all_nielsen_data_2006_2016_Q1only.csv"
#taxable_pi_path <- "Data/Nielsen/price_quantity_indices_taxableitems_2006-2016.csv"
#eventstudy_tr_path <- "Data/event_study_tr_groups_comprehensive_firstonly_no2012q4_2013q1q2.csv"
#tr_groups_path <- "Data/tr_groups_comprehensive_firstonly_no2012q4_2013q1q2.csv"

covariates.nhgis.path <- "Data/covariates/nhgis_county_clean.csv"
covariates.qcew.path <- "Data/covariates/qcew_clean.csv"
census.regions.path <- "Data/covariates/census_regions.csv"



######### Regression analysis ##NOTE: Make sure that we only include 2008-2014 in the regressions!!
## run the analysis on price ---------------------------------------------------
yearly_data <- fread(output_semester)
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

yearly_data[, region_by_time := .GRP, by = .(Region, year, semester)]
yearly_data[, division_by_time := .GRP, by = .(Division, year, semester)]
yearly_data[, urban_by_time := .GRP, by = .(urban, year, semester)]
yearly_data[, regXurban_by_time := .GRP, by = .(Region, urban, year, semester)]
yearly_data[, divXurban_by_time := .GRP, by = .(Division, urban, year, semester)]
yearly_data[, reg_by_module_by_time := .GRP, by = .(Region, product_module_code, year, semester)]
yearly_data[, div_by_module_by_time := .GRP, by = .(Division, product_module_code, year, semester)]


##Make sure that some of these variables are treated as factor variables
#yearly_data$product_module_code <- factor(yearly_data$product_module_code)


#############################################
list.outcomes <- c("ln_cpricei", "ln_quantity")

for(j in 1:length(list.outcomes)) {

    #
    outcome.j <- list.outcomes[j]

    ## no FE (only store_module and time)
   formula0 <- as.formula(paste0(outcome.j,
     " ~ ln_sales_tax | store_module + sem | 0 | state_by_module "
   ))

   res0 <- felm(data = yearly_data,
                   formula = formula0,
                   weights = yearly_data$base.sales)
  if(j == 1) {

    LRdiff_res <-
       data.table(
         outcome = outcome.j,
         estimate = coef(summary(res0))["ln_sales_tax", "Estimate"],
         se = coef(summary(res0))["ln_sales_tax", "Cluster s.e."],
         pval = coef(summary(res0))["ln_sales_tax", "Pr(>|t|)"],
         Rsq = summary(res0)$r.squared,
         adj.Rsq = summary(res0)$adj.r.squared,
         time_controls = "time FE"
       )
  } else {

    LRdiff_res <- rbind(
      LRdiff_res,
      data.table(
        outcome = outcome.j,
        estimate = coef(summary(res0))["ln_sales_tax", "Estimate"],
        se = coef(summary(res0))["ln_sales_tax", "Cluster s.e."],
        pval = coef(summary(res0))["ln_sales_tax", "Pr(>|t|)"],
        Rsq = summary(res0)$r.squared,
        adj.Rsq = summary(res0)$adj.r.squared,
        time_controls = "time FE"
      )
    )

  }

  ## with module-time FE
  formula0 <- as.formula(paste0(outcome.j,
    " ~ ln_sales_tax | store_module + module_by_time | 0 | state_by_module "
  ))

  res0 <- felm(data = yearly_data,
                     formula = formula0,
                     weights = yearly_data$base.sales)

  LRdiff_res <-rbind(
    LRdiff_res,
    data.table(
      outcome = outcome.j,
      estimate = coef(summary(res0))["ln_sales_tax", "Estimate"],
      se = coef(summary(res0))["ln_sales_tax", "Cluster s.e."],
      pval = coef(summary(res0))["ln_sales_tax", "Pr(>|t|)"],
      Rsq = summary(res0)$r.squared,
      adj.Rsq = summary(res0)$adj.r.squared,
      time_controls = "module-time FE"
    )
  )


  ## with store-time FE + module-time FE
  formula0 <- as.formula(paste0(outcome.j,
    " ~ ln_sales_tax | store_module + store_by_time + module_by_time | 0 | state_by_module "
  ))

  res0 <- felm(data = yearly_data,
                     formula = formula0,
                     weights = yearly_data$base.sales)


  LRdiff_res <-rbind(
    LRdiff_res,
    data.table(
      outcome = outcome.j,
      estimate = coef(summary(res0))["ln_sales_tax", "Estimate"],
      se = coef(summary(res0))["ln_sales_tax", "Cluster s.e."],
      pval = coef(summary(res0))["ln_sales_tax", "Pr(>|t|)"],
      Rsq = summary(res0)$r.squared,
      adj.Rsq = summary(res0)$adj.r.squared,
      time_controls = "store-time FE + module-time FE"
    )
  )



  ## with Module_by_Region_by_time FE
  formula0 <- as.formula(paste0(outcome.j,
    " ~ ln_sales_tax | store_module + reg_by_module_by_time | 0 | state_by_module "
  ))

  res0 <- felm(data = yearly_data,
                     formula = formula0,
                     weights = yearly_data$base.sales)

  LRdiff_res <-rbind(
    LRdiff_res,
    data.table(
      outcome = outcome.j,
      estimate = coef(summary(res0))["ln_sales_tax", "Estimate"],
      se = coef(summary(res0))["ln_sales_tax", "Cluster s.e."],
      pval = coef(summary(res0))["ln_sales_tax", "Pr(>|t|)"],
      Rsq = summary(res0)$r.squared,
      adj.Rsq = summary(res0)$adj.r.squared,
      time_controls = "module-region-time FE"
    )
  )


  ## with Module_by_Division_by_time FE
  formula0 <- as.formula(paste0(outcome.j,
    " ~ ln_sales_tax | store_module + div_by_module_by_time | 0 | state_by_module "
  ))

  res0 <- felm(data = yearly_data,
                     formula = formula0,
                     weights = yearly_data$base.sales)

  LRdiff_res <-rbind(
    LRdiff_res,
    data.table(
      outcome = outcome.j,
      estimate = coef(summary(res0))["ln_sales_tax", "Estimate"],
      se = coef(summary(res0))["ln_sales_tax", "Cluster s.e."],
      pval = coef(summary(res0))["ln_sales_tax", "Pr(>|t|)"],
      Rsq = summary(res0)$r.squared,
      adj.Rsq = summary(res0)$adj.r.squared,
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

fwrite(LRdiff_res, "Data/LRdiff_results_semester.csv")


