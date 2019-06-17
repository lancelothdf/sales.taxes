### In this R-file we explore a specification meant to estimate long-run pass-through and sales/quantity response to tax changes

library(data.table)
library(lfe)
library(futile.logger)
library(AER)


setwd("/project2/igaarder")

## useful filepaths ------------------------------------------------------------
FE_pindex_path <- "Data/Nielsen/Pindex_FE_yearly_all_years.csv"
output_yearly <- "Data/Nielsen/yearly_nielsen_data.csv"
Q1_data_path <- "Data/all_nielsen_data_2006_2016_Q1only.csv"
#taxable_pi_path <- "Data/Nielsen/price_quantity_indices_taxableitems_2006-2016.csv"
#eventstudy_tr_path <- "Data/event_study_tr_groups_comprehensive_firstonly_no2012q4_2013q1q2.csv"
#tr_groups_path <- "Data/tr_groups_comprehensive_firstonly_no2012q4_2013q1q2.csv"

covariates.nhgis.path <- "Data/covariates/nhgis_county_clean.csv"
covariates.qcew.path <- "Data/covariates/qcew_clean.csv"
census.regions.path <- "Data/covariates/census_regions.csv"



######### Regression analysis ##NOTE: Make sure that we only include 2008-2014 in the regressions!!
## run the analysis on price ---------------------------------------------------
yearly_data <- fread(output_yearly)
yearly_data <- yearly_data[year >= 2008 & year <= 2014,]


###############################
Q1_data <- fread(Q1_data_path)
yearly_data <- merge(yearly_data, Q1_data, by = c("fips_state", "fips_county", "store_code_uc", "product_module_code", "year"), all.x = T)
rm(Q1_data)

## Use Q1 tax rate as tax rate for that year - Idem with price measures
## For quantity - we keep yearly average because of seasonality (but will do robustness check with Q1 quantity)
yearly_data[, ln_sales_tax := ln_sales_tax_Q1]
#yearly_data[, ln_cpricei := ln_cpricei_Q1]
#yearly_data[, ln_cpricei2 := ln_cpricei2_Q1]
#yearly_data <- yearly_data[, -c("ln_cpricei_Q1", "ln_cpricei2_Q1", "ln_sales_tax_Q1")]
yearly_data <- yearly_data[, -c("ln_sales_tax_Q1")]

yearly_data <- yearly_data[!is.na(base.sales) & !is.na(ln_cpricei) &
                             !is.na(ln_sales_tax) & !is.na(ln_cpricei2) & !is.na(ln_quantity) &
                             !is.na(ln_quantity2) & !is.na(ln_quantity_Q1) & !is.na(ln_quantity2_Q1) &
                             !is.na(ln_cpricei_Q1) & !is.na(ln_cpricei2_Q1)]


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


##Make sure that some of these variables are treated as factor variables
#yearly_data$product_module_code <- factor(yearly_data$product_module_code)


#############################################
list.outcomes <- c("ln_cpricei", "ln_cpricei2", "ln_cpricei_Q1", "ln_cpricei2_Q1", "ln_quantity", "ln_quantity2", "ln_quantity_Q1", "ln_quantity2_Q1")

for(j in 1:length(list.outcomes)) {

    #
    outcome.j <- list.outcomes[j]

    ## no FE (only store_module and time)
   formula0 <- as.formula(paste0(outcome.j,
     " ~ ln_sales_tax | store_module + yr | 0 | state_by_module "
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

fwrite(LRdiff_res, "Data/LRdiff_results_taxQ1.csv")


