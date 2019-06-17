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


######## Import state and county tax rates
tax_data <- fread(tax.path)
#tax_data <- tax_data[,list(state_tax = mean(stat_st_tax), county_tax = mean(stat_cty_tax)), by = .(fips_state, fips_county, year)] ##stat_st_tax and stat_cty_tax are the median state and county tax rates within a county - meant to get at statutory tax rate.  We take an equally weighted average across months.
tax_data <- tax_data[,list(state_tax = mean(state_tax), county_tax = mean(county_tax)), by = .(fips_state, fips_county, year)] ##stat_st_tax and stat_cty_tax are the median state and county tax rates within a county - meant to get at statutory tax rate.  We take an equally weighted average across months.


## Impute county and state tax for 2006-2007 and 2015-2016 (+ define cpricei for these years)
tax_data[, base.st.tax := state_tax[year == 2008],
            by = .(fips_state, fips_county)]
tax_data[, base.cty.tax := county_tax[year == 2008],
         by = .(fips_state, fips_county)]
tax_data[, end.st.tax := state_tax[year == 2014],
         by = .(fips_state, fips_county)]
tax_data[, end.cty.tax := county_tax[year == 2014],
         by = .(fips_state, fips_county)]

#merge data
yearly_data <- merge(yearly_data, tax_data, by = c("fips_state", "fips_county", "year"), all.x = T)

#Now actually impute tax rate for 2006-2007 and 2015-2016 in the yearly data
yearly_data[year <= 2007,]$state_tax <- yearly_data[year <= 2007,]$base.st.tax
yearly_data[year <= 2007,]$county_tax <- yearly_data[year <= 2007,]$base.cty.tax
yearly_data[year >= 2015,]$state_tax <- yearly_data[year >= 2015,]$base.st.tax
yearly_data[year >= 2015,]$county_tax <- yearly_data[year >= 2015,]$base.cty.tax
yearly_data[, statutory_tax := state_tax + county_tax]

## Replace statutory tax rate = 0 for non-taxable goods (then also take logs)
yearly_data[, statutory_tax := (ln_sales_tax != 0)*statutory_tax]
yearly_data[, statutory_tax := log(1 + statutory_tax)]## Take logs



#############################################
list.outcomes <- c("ln_cpricei", "ln_cpricei2", "ln_quantity", "ln_quantity2")

for(j in 1:length(list.outcomes)) {

  #
  outcome.j <- list.outcomes[j]

  ## no FE (only store_module and time)
   price_formula1 <- as.formula(paste0(
     outcome.j, " ~ 0 | store_module + yr | (ln_sales_tax ~ statutory_tax) | state_by_module "
   ))

   price_res1 <- felm(data = yearly_data,
                     formula = price_formula1,
                     weights = yearly_data$base.sales)

   if(j == 1) {
   LRdiff_res <-
     data.table(
       outcome = outcome.j,
       estimate = coef(summary(price_res1))["`ln_sales_tax(fit)`", "Estimate"],
       se = coef(summary(price_res1))["`ln_sales_tax(fit)`", "Cluster s.e."],
       pval = coef(summary(price_res1))["`ln_sales_tax(fit)`", "Pr(>|t|)"],
       Rsq = summary(price_res1)$r.squared,
       adj.Rsq = summary(price_res1)$adj.r.squared,
       frst_coef = coef(summary(price_res1$stage1))["statutory_tax", "Estimate"],
       frst_se = coef(summary(price_res1$stage1))["statutory_tax", "Cluster s.e."],
       frst_pval = coef(summary(price_res1$stage1))["statutory_tax", "Pr(>|t|)"],
       frst_F = price_res1$stage1$iv1fstat$ln_sales_tax[[5]],
       time_controls = "time FE"
     )

   } else {

     LRdiff_res <-
       rbind(LRdiff_res,
       data.table(
         outcome = outcome.j,
         estimate = coef(summary(price_res1))["`ln_sales_tax(fit)`", "Estimate"],
         se = coef(summary(price_res1))["`ln_sales_tax(fit)`", "Cluster s.e."],
         pval = coef(summary(price_res1))["`ln_sales_tax(fit)`", "Pr(>|t|)"],
         Rsq = summary(price_res1)$r.squared,
         adj.Rsq = summary(price_res1)$adj.r.squared,
         frst_coef = coef(summary(price_res1$stage1))["statutory_tax", "Estimate"],
         frst_se = coef(summary(price_res1$stage1))["statutory_tax", "Cluster s.e."],
         frst_pval = coef(summary(price_res1$stage1))["statutory_tax", "Pr(>|t|)"],
         frst_F = price_res1$stage1$iv1fstat$ln_sales_tax[[5]],
         time_controls = "time FE"
       )
       )
   }

  ## with module-time FE
  price_formula <- as.formula(paste0(
    outcome.j, " ~ 0 | store_module + module_by_time | (ln_sales_tax ~ statutory_tax) | state_by_module "
  ))

  price_res <- felm(data = yearly_data,
                     formula = price_formula,
                     weights = yearly_data$base.sales)

  LRdiff_res <-rbind(
    LRdiff_res,
    data.table(
      outcome = outcome.j,
      estimate = coef(summary(price_res))["`ln_sales_tax(fit)`", "Estimate"],
      se = coef(summary(price_res))["`ln_sales_tax(fit)`", "Cluster s.e."],
      pval = coef(summary(price_res))["`ln_sales_tax(fit)`", "Pr(>|t|)"],
      Rsq = summary(price_res)$r.squared,
      adj.Rsq = summary(price_res)$adj.r.squared,
      frst_coef = coef(summary(price_res$stage1))["statutory_tax", "Estimate"],
      frst_se = coef(summary(price_res$stage1))["statutory_tax", "Cluster s.e."],
      frst_pval = coef(summary(price_res$stage1))["statutory_tax", "Pr(>|t|)"],
      frst_F = price_res$stage1$iv1fstat$ln_sales_tax[[5]],
      time_controls = "module-time FE"
    )
  )



  ## with Module_by_Region_by_time FE
  price_formula8 <- as.formula(paste0(
    outcome.j, " ~ 0 | store_module + module_by_time + reg_by_module_by_time | (ln_sales_tax ~ statutory_tax) | state_by_module "
  ))

  price_res4 <- felm(data = yearly_data,
                     formula = price_formula8,
                     weights = yearly_data$base.sales)

  LRdiff_res <-rbind(
    LRdiff_res,
    data.table(
      outcome = outcome.j,
      estimate = coef(summary(price_res4))["`ln_sales_tax(fit)`", "Estimate"],
      se = coef(summary(price_res4))["`ln_sales_tax(fit)`", "Cluster s.e."],
      pval = coef(summary(price_res4))["`ln_sales_tax(fit)`", "Pr(>|t|)"],
      Rsq = summary(price_res4)$r.squared,
      adj.Rsq = summary(price_res4)$adj.r.squared,
      frst_coef = coef(summary(price_res4$stage1))["statutory_tax", "Estimate"],
      frst_se = coef(summary(price_res4$stage1))["statutory_tax", "Cluster s.e."],
      frst_pval = coef(summary(price_res4$stage1))["statutory_tax", "Pr(>|t|)"],
      frst_F = price_res4$stage1$iv1fstat$ln_sales_tax[[5]],
      time_controls = "module-region-time FE"
    )
  )


  ## with Module_by_Division_by_time FE
  price_formula8 <- as.formula(paste0(
    outcome.j, " ~ 0 | store_module + module_by_time + div_by_module_by_time | (ln_sales_tax ~ statutory_tax) | state_by_module "
  ))

  price_res4 <- felm(data = yearly_data,
                     formula = price_formula8,
                     weights = yearly_data$base.sales)

  LRdiff_res <-rbind(
    LRdiff_res,
    data.table(
      outcome = outcome.j,
      estimate = coef(summary(price_res4))["`ln_sales_tax(fit)`", "Estimate"],
      se = coef(summary(price_res4))["`ln_sales_tax(fit)`", "Cluster s.e."],
      pval = coef(summary(price_res4))["`ln_sales_tax(fit)`", "Pr(>|t|)"],
      Rsq = summary(price_res4)$r.squared,
      adj.Rsq = summary(price_res4)$adj.r.squared,
      frst_coef = coef(summary(price_res4$stage1))["statutory_tax", "Estimate"],
      frst_se = coef(summary(price_res4$stage1))["statutory_tax", "Cluster s.e."],
      frst_pval = coef(summary(price_res4$stage1))["statutory_tax", "Pr(>|t|)"],
      frst_F = price_res4$stage1$iv1fstat$ln_sales_tax[[5]],
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

fwrite(LRdiff_res, "Data/LRdiff_results_IV_statutory_tax.csv")


