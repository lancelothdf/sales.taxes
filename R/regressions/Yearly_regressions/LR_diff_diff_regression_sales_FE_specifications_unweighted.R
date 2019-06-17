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


##Make sure that some of these variables are treated as factor variables
#yearly_data$product_module_code <- factor(yearly_data$product_module_code)


list.outcomes <- c("ln_cpricei", "ln_cpricei2", "ln_quantity", "ln_quantity2")
for(j in 1:length(list.outcomes)) {
  
  #
  outcome.j <- list.outcomes[j]
  
  
  ## Only year FE
  formula0 <- as.formula(paste0(
    outcome.j, " ~ ln_sales_tax | store_module + yr | 0 | state_by_module "
  ))
  
  res0 <- felm(data = yearly_data,
               formula = formula0)
  
  if(j == 1) {
    
    LRdiff_res <-
      data.table(
        outcome = outcome.j,
        estimate = coef(summary(res0))["ln_sales_tax", "Estimate"],
        se = coef(summary(res0))["ln_sales_tax", "Cluster s.e."],
        pval = coef(summary(res0))["ln_sales_tax", "Pr(>|t|)"],
        Rsq = summary(res0)$r.squared,
        adj.Rsq = summary(res0)$adj.r.squared,
        time_controls = "year FE"
      )
  }
  else {
    
    LRdiff_res <- rbind(
      LRdiff_res,
      data.table(
        outcome = outcome.j,
        estimate = coef(summary(res0))["ln_sales_tax", "Estimate"],
        se = coef(summary(res0))["ln_sales_tax", "Cluster s.e."],
        pval = coef(summary(res0))["ln_sales_tax", "Pr(>|t|)"],
        Rsq = summary(res0)$r.squared,
        adj.Rsq = summary(res0)$adj.r.squared,
        time_controls = "year FE"
      )
    )
  }
  
  

  ## Module by year FE
  outcome.j <- list.outcomes[j]
  
  formula3 <- as.formula(paste0(
    outcome.j, " ~ ln_sales_tax | store_module + module_by_time | 0 | state_by_module "
  ))
  
  res3 <- felm(data = yearly_data,
               formula = formula3)
  
  LRdiff_res <- rbind(
    LRdiff_res,
    data.table(
      outcome = outcome.j,
      estimate = coef(summary(res3))["ln_sales_tax", "Estimate"],
      se = coef(summary(res3))["ln_sales_tax", "Cluster s.e."],
      pval = coef(summary(res3))["ln_sales_tax", "Pr(>|t|)"],
      Rsq = summary(res3)$r.squared,
      adj.Rsq = summary(res3)$adj.r.squared,
      time_controls = "Module by time FE"
    )
  )
  
  

  ## Module by year FE + Region by module by year FE
  outcome.j <- list.outcomes[j]
  
  formula9 <- as.formula(paste0(
    outcome.j, " ~ ln_sales_tax | store_module + reg_by_module_by_time | 0 | state_by_module "
  ))
  
  res9 <- felm(data = yearly_data,
               formula = formula9)
  
  
  LRdiff_res <- rbind(
    LRdiff_res,
    data.table(
      outcome = outcome.j,
      estimate = coef(summary(res9))["ln_sales_tax", "Estimate"],
      se = coef(summary(res9))["ln_sales_tax", "Cluster s.e."],
      pval = coef(summary(res9))["ln_sales_tax", "Pr(>|t|)"],
      Rsq = summary(res9)$r.squared,
      adj.Rsq = summary(res9)$adj.r.squared,
      time_controls = "Module by Region by time FE"
    )
  )
  
  
  ## Module by year FE + Division by module by year FE
  outcome.j <- list.outcomes[j]
  
  formula10 <- as.formula(paste0(
    outcome.j, " ~ ln_sales_tax | store_module + div_by_module_by_time | 0 | state_by_module "
  ))
  
  res10 <- felm(data = yearly_data,
                formula = formula10)
  
  
  LRdiff_res <- rbind(
    LRdiff_res,
    data.table(
      outcome = outcome.j,
      estimate = coef(summary(res10))["ln_sales_tax", "Estimate"],
      se = coef(summary(res10))["ln_sales_tax", "Cluster s.e."],
      pval = coef(summary(res10))["ln_sales_tax", "Pr(>|t|)"],
      Rsq = summary(res10)$r.squared,
      adj.Rsq = summary(res10)$adj.r.squared,
      time_controls = "Module by Division by time FE"
    )
  )
  
}


## summary values --------------------------------------------------------------
LRdiff_res$N_obs <- nrow(yearly_data)
LRdiff_res$N_modules <- length(unique(yearly_data$product_module_code))
LRdiff_res$N_stores <- length(unique(yearly_data$store_code_uc))
LRdiff_res$N_counties <- uniqueN(yearly_data, by = c("fips_state", "fips_county"))
LRdiff_res$N_years <- uniqueN(yearly_data, by = c("year")) ##Should be 7
LRdiff_res$N_county_modules <- uniqueN(yearly_data, by = c("fips_state", "fips_county",
                                                           "product_module_code"))

fwrite(LRdiff_res, "Data/LRdiff_results_FE_unweighted.csv")

