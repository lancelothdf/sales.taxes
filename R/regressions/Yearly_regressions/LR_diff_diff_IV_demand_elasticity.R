### In this R-file we explore a specification meant to estimate long-run pass-through and sales/quantity response to tax changes

library(data.table)
library(lfe)
library(futile.logger)
library(AER)


setwd("/project2/igaarder")

## useful filepaths ------------------------------------------------------------
all_goods_pi_path <- "Data/Nielsen/price_quantity_indices_allitems_2006-2016_notaxinfo.csv"
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



## no FE (only store_module and time)
# Price index 1 (Laspeyres)
iv_formula1 <- as.formula(paste0(
  "ln_quantity ~ 0 | store_module + yr | (ln_cpricei ~ ln_sales_tax) | state_by_module "
))

iv_res1 <- felm(data = yearly_data,
                   formula = iv_formula1,
                   weights = yearly_data$base.sales)

LRdiff_res <-
  data.table(
    outcome = "Demand elasticity (1)",
    estimate = coef(summary(iv_res1))["`ln_cpricei(fit)`", "Estimate"],
    se = coef(summary(iv_res1))["`ln_cpricei(fit)`", "Cluster s.e."],
    pval = coef(summary(iv_res1))["`ln_cpricei(fit)`", "Pr(>|t|)"],
    Rsq = summary(iv_res1)$r.squared,
    adj.Rsq = summary(iv_res1)$adj.r.squared,
    frst_coef = coef(summary(iv_res1$stage1))["ln_sales_tax", "Estimate"],
    frst_se = coef(summary(iv_res1$stage1))["ln_sales_tax", "Cluster s.e."],
    frst_pval = coef(summary(iv_res1$stage1))["ln_sales_tax", "Pr(>|t|)"],
    frst_F = iv_res1$stage1$iv1fstat$ln_cpricei[[5]],
    time_controls = "time FE"
  )



## with module-time FE - First measure of prices
## First price index
iv_formula2 <- as.formula(paste0(
  "ln_quantity ~ 0 | store_module + module_by_time | (ln_cpricei ~ ln_sales_tax) | state_by_module "
))

iv_res2 <- felm(data = yearly_data,
                  formula = iv_formula2,
                  weights = yearly_data$base.sales)

LRdiff_res <-rbind(
  LRdiff_res,
  data.table(
    outcome = "Demand elasticity (1)",
    estimate = coef(summary(iv_res2))["`ln_cpricei(fit)`", "Estimate"],
    se = coef(summary(iv_res2))["`ln_cpricei(fit)`", "Cluster s.e."],
    pval = coef(summary(iv_res2))["`ln_cpricei(fit)`", "Pr(>|t|)"],
    Rsq = summary(iv_res2)$r.squared,
    adj.Rsq = summary(iv_res2)$adj.r.squared,
    frst_coef = coef(summary(iv_res2$stage1))["ln_sales_tax", "Estimate"],
    frst_se = coef(summary(iv_res2$stage1))["ln_sales_tax", "Cluster s.e."],
    frst_pval = coef(summary(iv_res2$stage1))["ln_sales_tax", "Pr(>|t|)"],
    frst_F = iv_res2$stage1$iv1fstat$ln_cpricei[[5]],
    time_controls = "module-time FE"
  )
)


## with store-time FE (instead of module-time) - First measure of prices
iv_formula3 <- as.formula(paste0(
  "ln_quantity ~ 0 | store_module + store_by_time | (ln_cpricei ~ ln_sales_tax) | state_by_module "
))

iv_res3 <- felm(data = yearly_data,
                   formula = iv_formula3,
                   weights = yearly_data$base.sales)


LRdiff_res <-rbind(
  LRdiff_res,
  data.table(
    outcome = "Demand elasticity (1)",
    estimate = coef(summary(iv_res3))["`ln_cpricei(fit)`", "Estimate"],
    se = coef(summary(iv_res3))["`ln_cpricei(fit)`", "Cluster s.e."],
    pval = coef(summary(iv_res3))["`ln_cpricei(fit)`", "Pr(>|t|)"],
    Rsq = summary(iv_res3)$r.squared,
    adj.Rsq = summary(iv_res3)$adj.r.squared,
    frst_coef = coef(summary(iv_res3$stage1))["ln_sales_tax", "Estimate"],
    frst_se = coef(summary(iv_res3$stage1))["ln_sales_tax", "Cluster s.e."],
    frst_pval = coef(summary(iv_res3$stage1))["ln_sales_tax", "Pr(>|t|)"],
    frst_F = iv_res3$stage1$iv1fstat$ln_cpricei[[5]],
    time_controls = "store-time FE"
  )
)



## with store-time FE + module-time FE - First measure of prices
iv_formula4 <- as.formula(paste0(
  "ln_quantity ~ 0 | store_module + store_by_time + module_by_time | (ln_cpricei ~ ln_sales_tax) | state_by_module "
))

iv_res4 <- felm(data = yearly_data,
                   formula = iv_formula4,
                   weights = yearly_data$base.sales)


LRdiff_res <-rbind(
  LRdiff_res,
  data.table(
    outcome = "Demand elasticity (1)",
    estimate = coef(summary(iv_res4))["`ln_cpricei(fit)`", "Estimate"],
    se = coef(summary(iv_res4))["`ln_cpricei(fit)`", "Cluster s.e."],
    pval = coef(summary(iv_res4))["`ln_cpricei(fit)`", "Pr(>|t|)"],
    Rsq = summary(iv_res4)$r.squared,
    adj.Rsq = summary(iv_res4)$adj.r.squared,
    frst_coef = coef(summary(iv_res4$stage1))["ln_sales_tax", "Estimate"],
    frst_se = coef(summary(iv_res4$stage1))["ln_sales_tax", "Cluster s.e."],
    frst_pval = coef(summary(iv_res4$stage1))["ln_sales_tax", "Pr(>|t|)"],
    frst_F = iv_res4$stage1$iv1fstat$ln_cpricei[[5]],
    time_controls = "module-time + store-time FE"
  )
)


## with module-region-time FE - First measure of prices
## First price index
iv_formula9 <- as.formula(paste0(
  "ln_quantity ~ 0 | store_module + reg_by_module_by_time | (ln_cpricei ~ ln_sales_tax) | state_by_module "
))

iv_res9 <- felm(data = yearly_data,
                formula = iv_formula9,
                weights = yearly_data$base.sales)

LRdiff_res <-rbind(
  LRdiff_res,
  data.table(
    outcome = "Demand elasticity (1)",
    estimate = coef(summary(iv_res9))["`ln_cpricei(fit)`", "Estimate"],
    se = coef(summary(iv_res9))["`ln_cpricei(fit)`", "Cluster s.e."],
    pval = coef(summary(iv_res9))["`ln_cpricei(fit)`", "Pr(>|t|)"],
    Rsq = summary(iv_res9)$r.squared,
    adj.Rsq = summary(iv_res9)$adj.r.squared,
    frst_coef = coef(summary(iv_res9$stage1))["ln_sales_tax", "Estimate"],
    frst_se = coef(summary(iv_res9$stage1))["ln_sales_tax", "Cluster s.e."],
    frst_pval = coef(summary(iv_res9$stage1))["ln_sales_tax", "Pr(>|t|)"],
    frst_F = iv_res9$stage1$iv1fstat$ln_cpricei[[5]],
    time_controls = "region-module-time FE"
  )
)



## no FE (only store_module and time) - Second measure of prices
iv_formula5 <- as.formula(paste0(
  "ln_quantity2 ~ 0 | store_module + yr | (ln_cpricei2 ~ ln_sales_tax) | state_by_module "
))

iv_res5 <- felm(data = yearly_data,
                    formula = iv_formula5,
                    weights = yearly_data$base.sales)

LRdiff_res <-rbind(
  LRdiff_res,
  data.table(
    outcome = "Demand elasticity (2)",
    estimate = coef(summary(iv_res5))["`ln_cpricei2(fit)`", "Estimate"],
    se = coef(summary(iv_res5))["`ln_cpricei2(fit)`", "Cluster s.e."],
    pval = coef(summary(iv_res5))["`ln_cpricei2(fit)`", "Pr(>|t|)"],
    Rsq = summary(iv_res5)$r.squared,
    adj.Rsq = summary(iv_res5)$adj.r.squared,
    frst_coef = coef(summary(iv_res5$stage1))["ln_sales_tax", "Estimate"],
    frst_se = coef(summary(iv_res5$stage1))["ln_sales_tax", "Cluster s.e."],
    frst_pval = coef(summary(iv_res5$stage1))["ln_sales_tax", "Pr(>|t|)"],
    frst_F = iv_res5$stage1$iv1fstat$ln_cpricei2[[5]],
    time_controls = "time FE"
  )
)




## with module-time FE - Second measure of prices
iv_formula6 <- as.formula(paste0(
  "ln_quantity2 ~ 0 | store_module + module_by_time | (ln_cpricei2 ~ ln_sales_tax) | state_by_module "
))

iv_res6 <- felm(data = yearly_data,
                   formula = iv_formula6,
                   weights = yearly_data$base.sales)

LRdiff_res <-rbind(
  LRdiff_res,
  data.table(
    outcome = "Demand elasticity (2)",
    estimate = coef(summary(iv_res6))["`ln_cpricei2(fit)`", "Estimate"],
    se = coef(summary(iv_res6))["`ln_cpricei2(fit)`", "Cluster s.e."],
    pval = coef(summary(iv_res6))["`ln_cpricei2(fit)`", "Pr(>|t|)"],
    Rsq = summary(iv_res6)$r.squared,
    adj.Rsq = summary(iv_res6)$adj.r.squared,
    frst_coef = coef(summary(iv_res6$stage1))["ln_sales_tax", "Estimate"],
    frst_se = coef(summary(iv_res6$stage1))["ln_sales_tax", "Cluster s.e."],
    frst_pval = coef(summary(iv_res6$stage1))["ln_sales_tax", "Pr(>|t|)"],
    frst_F = iv_res6$stage1$iv1fstat$ln_cpricei2[[5]],
    time_controls = "module-time FE"
  )
)


## with store-time FE (instead of module-time) - Second measure of prices
iv_formula7 <- as.formula(paste0(
  "ln_quantity2 ~ 0 | store_module + store_by_time | (ln_cpricei2 ~ ln_sales_tax) | state_by_module "
))

iv_res7 <- felm(data = yearly_data,
                    formula = iv_formula7,
                    weights = yearly_data$base.sales)


LRdiff_res <-rbind(
  LRdiff_res,
  data.table(
    outcome = "Demand elasticity (2)",
    estimate = coef(summary(iv_res7))["`ln_cpricei2(fit)`", "Estimate"],
    se = coef(summary(iv_res7))["`ln_cpricei2(fit)`", "Cluster s.e."],
    pval = coef(summary(iv_res7))["`ln_cpricei2(fit)`", "Pr(>|t|)"],
    Rsq = summary(iv_res7)$r.squared,
    adj.Rsq = summary(iv_res7)$adj.r.squared,
    frst_coef = coef(summary(iv_res7$stage1))["ln_sales_tax", "Estimate"],
    frst_se = coef(summary(iv_res7$stage1))["ln_sales_tax", "Cluster s.e."],
    frst_pval = coef(summary(iv_res7$stage1))["ln_sales_tax", "Pr(>|t|)"],
    frst_F = iv_res7$stage1$iv1fstat$ln_cpricei2[[5]],
    time_controls = "store-time FE"
  )
)



## with store-time FE + module-time FE - Second measure of prices
iv_formula8 <- as.formula(paste0(
  "ln_quantity2 ~ 0 | store_module + store_by_time + module_by_time | (ln_cpricei2 ~ ln_sales_tax) | state_by_module "
))

iv_res8 <- felm(data = yearly_data,
                    formula = iv_formula8,
                    weights = yearly_data$base.sales)


LRdiff_res <-rbind(
  LRdiff_res,
  data.table(
    outcome = "Demand elasticity (2)",
    estimate = coef(summary(iv_res8))["`ln_cpricei2(fit)`", "Estimate"],
    se = coef(summary(iv_res8))["`ln_cpricei2(fit)`", "Cluster s.e."],
    pval = coef(summary(iv_res8))["`ln_cpricei2(fit)`", "Pr(>|t|)"],
    Rsq = summary(iv_res8)$r.squared,
    adj.Rsq = summary(iv_res8)$adj.r.squared,
    frst_coef = coef(summary(iv_res8$stage1))["ln_sales_tax", "Estimate"],
    frst_se = coef(summary(iv_res8$stage1))["ln_sales_tax", "Cluster s.e."],
    frst_pval = coef(summary(iv_res8$stage1))["ln_sales_tax", "Pr(>|t|)"],
    frst_F = iv_res8$stage1$iv1fstat$ln_cpricei2[[5]],
    time_controls = "module-time + store-time FE"
  )
)


## with module-region-time FE - Second measure of prices
## Second price index
iv_formula10 <- as.formula(paste0(
  "ln_quantity2 ~ 0 | store_module + reg_by_module_by_time | (ln_cpricei2 ~ ln_sales_tax) | state_by_module "
))

iv_res10 <- felm(data = yearly_data,
                formula = iv_formula10,
                weights = yearly_data$base.sales)

LRdiff_res <-rbind(
  LRdiff_res,
  data.table(
    outcome = "Demand elasticity (2)",
    estimate = coef(summary(iv_res10))["`ln_cpricei2(fit)`", "Estimate"],
    se = coef(summary(iv_res10))["`ln_cpricei2(fit)`", "Cluster s.e."],
    pval = coef(summary(iv_res10))["`ln_cpricei2(fit)`", "Pr(>|t|)"],
    Rsq = summary(iv_res10)$r.squared,
    adj.Rsq = summary(iv_res10)$adj.r.squared,
    frst_coef = coef(summary(iv_res10$stage1))["ln_sales_tax", "Estimate"],
    frst_se = coef(summary(iv_res10$stage1))["ln_sales_tax", "Cluster s.e."],
    frst_pval = coef(summary(iv_res10$stage1))["ln_sales_tax", "Pr(>|t|)"],
    frst_F = iv_res10$stage1$iv1fstat$ln_cpricei2[[5]],
    time_controls = "region-module-time FE"
  )
)



## summary values --------------------------------------------------------------
LRdiff_res$N_obs <- nrow(yearly_data)
LRdiff_res$N_modules <- length(unique(yearly_data$product_module_code))
LRdiff_res$N_stores <- length(unique(yearly_data$store_code_uc))
LRdiff_res$N_counties <- uniqueN(yearly_data, by = c("fips_state", "fips_county"))
LRdiff_res$N_years <- uniqueN(yearly_data, by = c("year")) # should be 7
LRdiff_res$N_county_modules <- uniqueN(yearly_data, by = c("fips_state", "fips_county",
                                                           "product_module_code"))

fwrite(LRdiff_res, "Data/LRdiff_iv_results.csv")

