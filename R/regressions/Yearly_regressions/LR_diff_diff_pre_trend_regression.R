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
yearly_data <- yearly_data[year >= 2006 & year <= 2016,]


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


#######################
#######################
## Lead and Lagged regressions

setDT(yearly_data)
yearly_data <- yearly_data[order(fips_state, fips_county, store_code_uc, product_module_code, year),] ##Sort on store by year (year in descending order)

outcomes <- c("ln_cpricei", "ln_cpricei2", "ln_quantity", "ln_quantity2")
nlags <- 4

for(i in 1:length(outcomes)) {



  outcome.i <- outcomes[i]


  #############################################
  #### Run the specification with no lags or leads
  ## no FE (only store_module and time)
  price_formula <- as.formula(paste0(outcome.i,
    " ~ ln_sales_tax | store_module + yr | 0 | state_by_module ", sep = ""
  ))

  price_res <- felm(data = yearly_data[year >= 2008 & year <= 2014,],
                    formula = price_formula,
                    weights = yearly_data[year >= 2008 & year <= 2014,]$base.sales)  ## We make sure that we do not use post-2014 tax rate data (because tax rates are imputed after 2014)

  if(i == 1) {

    LRdiff_res <-
      data.table(
        outcome = outcome.i,
        estimate = coef(summary(price_res))["ln_sales_tax", "Estimate"],
        se = coef(summary(price_res))["ln_sales_tax", "Cluster s.e."],
        pval = coef(summary(price_res))["ln_sales_tax", "Pr(>|t|)"],
        Rsq = summary(price_res)$r.squared,
        adj.Rsq = summary(price_res)$adj.r.squared,
        time_controls = "time FE",
        lead_lag = paste0("lead_", 0, sep = "") ####NOTE: Kind of confusing but "lagged outcome" corresponds to lead regression (pre-trend because e.g. 2006 outcome is regressed on 2007 tax rate)
      )
  }
  else {

    LRdiff_res <- rbind(
      LRdiff_res,
      data.table(
        outcome = outcome.i,
        estimate = coef(summary(price_res))["ln_sales_tax", "Estimate"],
        se = coef(summary(price_res))["ln_sales_tax", "Cluster s.e."],
        pval = coef(summary(price_res))["ln_sales_tax", "Pr(>|t|)"],
        Rsq = summary(price_res)$r.squared,
        adj.Rsq = summary(price_res)$adj.r.squared,
        time_controls = "time FE",
        lead_lag = paste0("lead_", 0, sep = "")
      )
    )
  }


  ## with module-time FE
  price_formula <- as.formula(paste0(outcome.i,
    " ~ ln_sales_tax | store_module + module_by_time | 0 | state_by_module ", sep = ""
  ))

  price_res <- felm(data = yearly_data[year >= 2008 & year <= 2014,],
                    formula = price_formula,
                    weights = yearly_data[year >= 2008 & year <= 2014,]$base.sales)

  LRdiff_res <-rbind(
    LRdiff_res,
    data.table(
      outcome = outcome.i,
      estimate = coef(summary(price_res))["ln_sales_tax", "Estimate"],
      se = coef(summary(price_res))["ln_sales_tax", "Cluster s.e."],
      pval = coef(summary(price_res))["ln_sales_tax", "Pr(>|t|)"],
      Rsq = summary(price_res)$r.squared,
      adj.Rsq = summary(price_res)$adj.r.squared,
      time_controls = "module-time FE",
      lead_lag = paste0("lead_", 0, sep = "")
    )
  )


  ## with store-time FE (instead of module-time)
  price_formula <- as.formula(paste0(outcome.i,
    " ~ ln_sales_tax | store_module + store_by_time | 0 | state_by_module ", sep = ""
  ))

  price_res <- felm(data = yearly_data[year >= 2008 & year <= 2014,],
                    formula = price_formula,
                    weights = yearly_data[year >= 2008 & year <= 2014,]$base.sales)


  LRdiff_res <-rbind(
    LRdiff_res,
    data.table(
      outcome = outcome.i,
      estimate = coef(summary(price_res))["ln_sales_tax", "Estimate"],
      se = coef(summary(price_res))["ln_sales_tax", "Cluster s.e."],
      pval = coef(summary(price_res))["ln_sales_tax", "Pr(>|t|)"],
      Rsq = summary(price_res)$r.squared,
      adj.Rsq = summary(price_res)$adj.r.squared,
      time_controls = "store-time FE",
      lead_lag = paste0("lead_", 0, sep = "")
    )
  )


  ## with store-time FE + module-time FE
  price_formula <- as.formula(paste0(outcome.i,
    " ~ ln_sales_tax | store_module + store_by_time + module_by_time | 0 | state_by_module ", sep = ""
  ))

  price_res <- felm(data = yearly_data[year >= 2008 & year <= 2014,],
                    formula = price_formula,
                    weights = yearly_data[year >= 2008 & year <= 2014,]$base.sales)


  LRdiff_res <-rbind(
    LRdiff_res,
    data.table(
      outcome = outcome.i,
      estimate = coef(summary(price_res))["ln_sales_tax", "Estimate"],
      se = coef(summary(price_res))["ln_sales_tax", "Cluster s.e."],
      pval = coef(summary(price_res))["ln_sales_tax", "Pr(>|t|)"],
      Rsq = summary(price_res)$r.squared,
      adj.Rsq = summary(price_res)$adj.r.squared,
      time_controls = "store-time FE + module-time FE",
      lead_lag = paste0("lead_", 0, sep = "")
    )
  )




  for(j in 1:nlags) {

    #############################################
    #Lags and leads
    yearly_data <- yearly_data[order(fips_state, fips_county, store_code_uc, product_module_code, year), "lag" := shift(.SD, j, type = "lag"), .SDcols = outcome.i, by = c("fips_state", "fips_county", "store_code_uc", "product_module_code")]
    yearly_data <- yearly_data[order(fips_state, fips_county, store_code_uc, product_module_code, year), "lead" := shift(.SD, j, type = "lead"), .SDcols = outcome.i, by = c("fips_state", "fips_county", "store_code_uc", "product_module_code")]


    #############################################
    #### LAGS
     ## no FE (only store_module and time)
     price_formula <- as.formula(paste0(
       "lag ~ ln_sales_tax | store_module + yr | 0 | state_by_module "
     ))

     price_res <- felm(data = yearly_data[year <= 2014,],
                       formula = price_formula,
                       weights = yearly_data[year <= 2014,]$base.sales)  ## We make sure that we do not use post-2014 tax rate data (because tax rates are imputed after 2014)


       #LRdiff_res <- rbind(
        # LRdiff_res,
        # data.table(
        #   outcome = outcome.i,
        #   estimate = coef(summary(price_res))["ln_sales_tax", "Estimate"],
        #   se = coef(summary(price_res))["ln_sales_tax", "Cluster s.e."],
        #   pval = coef(summary(price_res))["ln_sales_tax", "Pr(>|t|)"],
        #   Rsq = summary(price_res)$r.squared,
        #   adj.Rsq = summary(price_res)$adj.r.squared,
        #   time_controls = "time FE",
        #   lead_lag = paste0("lead_", j, sep = "") ####NOTE: Kind of confusing but "lagged outcome" corresponds to lead regression (pre-trend because e.g. 2006 outcome is regressed on 2007 tax rate)
        # )
       #)


      LRdiff_res <- rbind(
        LRdiff_res,
        data.table(
          outcome = outcome.i,
          estimate = coef(summary(price_res))["ln_sales_tax", "Estimate"],
          se = coef(summary(price_res))["ln_sales_tax", "Cluster s.e."],
          pval = coef(summary(price_res))["ln_sales_tax", "Pr(>|t|)"],
          Rsq = summary(price_res)$r.squared,
          adj.Rsq = summary(price_res)$adj.r.squared,
          time_controls = "time FE",
          lead_lag = paste0("lead_", j, sep = "")
        )
      )


    ## with module-time FE
    price_formula <- as.formula(paste0(
      "lag ~ ln_sales_tax | store_module + module_by_time | 0 | state_by_module "
    ))

    price_res <- felm(data = yearly_data[year <= 2014,],
                       formula = price_formula,
                       weights = yearly_data[year <= 2014,]$base.sales)

    LRdiff_res <-rbind(
      LRdiff_res,
      data.table(
        outcome = outcome.i,
        estimate = coef(summary(price_res))["ln_sales_tax", "Estimate"],
        se = coef(summary(price_res))["ln_sales_tax", "Cluster s.e."],
        pval = coef(summary(price_res))["ln_sales_tax", "Pr(>|t|)"],
        Rsq = summary(price_res)$r.squared,
        adj.Rsq = summary(price_res)$adj.r.squared,
        time_controls = "module-time FE",
        lead_lag = paste0("lead_", j, sep = "")
      )
    )


    ## with store-time FE (instead of module-time)
    price_formula <- as.formula(paste0(
      "lag ~ ln_sales_tax | store_module + store_by_time | 0 | state_by_module "
    ))

    price_res <- felm(data = yearly_data[year <= 2014,],
                      formula = price_formula,
                      weights = yearly_data[year <= 2014,]$base.sales)


    LRdiff_res <-rbind(
      LRdiff_res,
      data.table(
        outcome = outcome.i,
        estimate = coef(summary(price_res))["ln_sales_tax", "Estimate"],
        se = coef(summary(price_res))["ln_sales_tax", "Cluster s.e."],
        pval = coef(summary(price_res))["ln_sales_tax", "Pr(>|t|)"],
        Rsq = summary(price_res)$r.squared,
        adj.Rsq = summary(price_res)$adj.r.squared,
        time_controls = "store-time FE",
        lead_lag = paste0("lead_", j, sep = "")
      )
    )


    ## with store-time FE + module-time FE
    price_formula <- as.formula(paste0(
      "lag ~ ln_sales_tax | store_module + store_by_time + module_by_time | 0 | state_by_module "
    ))

    price_res <- felm(data = yearly_data[year <= 2014,],
                       formula = price_formula,
                       weights = yearly_data[year <= 2014,]$base.sales)


    LRdiff_res <-rbind(
      LRdiff_res,
      data.table(
        outcome = outcome.i,
        estimate = coef(summary(price_res))["ln_sales_tax", "Estimate"],
        se = coef(summary(price_res))["ln_sales_tax", "Cluster s.e."],
        pval = coef(summary(price_res))["ln_sales_tax", "Pr(>|t|)"],
        Rsq = summary(price_res)$r.squared,
        adj.Rsq = summary(price_res)$adj.r.squared,
        time_controls = "store-time FE + module-time FE",
        lead_lag = paste0("lead_", j, sep = "")
      )
    )



    #### LEADS
    ## no FE (only store_module and time)
    price_formula <- as.formula(paste0(
      "lead ~ ln_sales_tax | store_module + yr | 0 | state_by_module "
    ))

    price_res <- felm(data = yearly_data[year >= 2008,],
                      formula = price_formula,
                      weights = yearly_data[year >= 2008,]$base.sales) ## We make sure that we do not use pre-2008 tax rates data (because it is imputed before 2008)


      LRdiff_res <- rbind(
        LRdiff_res,
        data.table(
          outcome = outcome.i,
          estimate = coef(summary(price_res))["ln_sales_tax", "Estimate"],
          se = coef(summary(price_res))["ln_sales_tax", "Cluster s.e."],
          pval = coef(summary(price_res))["ln_sales_tax", "Pr(>|t|)"],
          Rsq = summary(price_res)$r.squared,
          adj.Rsq = summary(price_res)$adj.r.squared,
          time_controls = "time FE",
          lead_lag = paste0("lag_", j, sep = "")  ####NOTE: Kind of confusing but "lagged outcome" corresponds to lead regression (pre-trend because e.g. 2006 outcome is regressed on 2007 tax rate)
        )
      )


    ## with module-time FE
    price_formula <- as.formula(paste0(
      "lead ~ ln_sales_tax | store_module + module_by_time | 0 | state_by_module "
    ))

    price_res <- felm(data = yearly_data[year >= 2008,],
                      formula = price_formula,
                      weights = yearly_data[year >= 2008,]$base.sales)

    LRdiff_res <-rbind(
      LRdiff_res,
      data.table(
        outcome = outcome.i,
        estimate = coef(summary(price_res))["ln_sales_tax", "Estimate"],
        se = coef(summary(price_res))["ln_sales_tax", "Cluster s.e."],
        pval = coef(summary(price_res))["ln_sales_tax", "Pr(>|t|)"],
        Rsq = summary(price_res)$r.squared,
        adj.Rsq = summary(price_res)$adj.r.squared,
        time_controls = "module-time FE",
        lead_lag = paste0("lag_", j, sep = "")
      )
    )


    ## with store-time FE (instead of module-time)
    price_formula <- as.formula(paste0(
      "lead ~ ln_sales_tax | store_module + store_by_time | 0 | state_by_module "
    ))

    price_res <- felm(data = yearly_data[year >= 2008,],
                      formula = price_formula,
                      weights = yearly_data[year >= 2008,]$base.sales)


    LRdiff_res <-rbind(
      LRdiff_res,
      data.table(
        outcome = outcome.i,
        estimate = coef(summary(price_res))["ln_sales_tax", "Estimate"],
        se = coef(summary(price_res))["ln_sales_tax", "Cluster s.e."],
        pval = coef(summary(price_res))["ln_sales_tax", "Pr(>|t|)"],
        Rsq = summary(price_res)$r.squared,
        adj.Rsq = summary(price_res)$adj.r.squared,
        time_controls = "store-time FE",
        lead_lag = paste0("lag_", j, sep = "")
      )
    )


    ## with store-time FE + module-time FE
    price_formula <- as.formula(paste0(
      "lead ~ ln_sales_tax | store_module + store_by_time + module_by_time | 0 | state_by_module "
    ))

    price_res <- felm(data = yearly_data[year >= 2008,],
                      formula = price_formula,
                      weights = yearly_data[year >= 2008,]$base.sales)


    LRdiff_res <-rbind(
      LRdiff_res,
      data.table(
        outcome = outcome.i,
        estimate = coef(summary(price_res))["ln_sales_tax", "Estimate"],
        se = coef(summary(price_res))["ln_sales_tax", "Cluster s.e."],
        pval = coef(summary(price_res))["ln_sales_tax", "Pr(>|t|)"],
        Rsq = summary(price_res)$r.squared,
        adj.Rsq = summary(price_res)$adj.r.squared,
        time_controls = "store-time FE + module-time FE",
        lead_lag = paste0("lag_", j, sep = "")
      )
    )


  }
}





fwrite(LRdiff_res, "Data/LRdiff_pretrend_results.csv")


