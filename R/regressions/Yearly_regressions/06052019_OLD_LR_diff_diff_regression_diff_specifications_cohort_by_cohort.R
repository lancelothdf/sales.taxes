### In this R-file we explore a specification meant to estimate long-run pass-through and sales/quantity response to tax changes

library(data.table)
library(lfe)
library(futile.logger)
library(AER)
library(multcomp)


setwd("/project2/igaarder")

## useful filepaths ------------------------------------------------------------
FE_pindex_path <- "Data/Nielsen/Pindex_FE_yearly_all_years.csv"
output_yearly <- "Data/Nielsen/yearly_nielsen_data.csv"
pre_trend_data_path <- "Data/Nielsen/pre_trend_data_yearly.csv"
#taxable_pi_path <- "Data/Nielsen/price_quantity_indices_taxableitems_2006-2016.csv"
#eventstudy_tr_path <- "Data/event_study_tr_groups_comprehensive_firstonly_no2012q4_2013q1q2.csv"
#tr_groups_path <- "Data/tr_groups_comprehensive_firstonly_no2012q4_2013q1q2.csv"

covariates.nhgis.path <- "Data/covariates/nhgis_county_clean.csv"
covariates.qcew.path <- "Data/covariates/qcew_clean.csv"
census.regions.path <- "Data/covariates/census_regions.csv"


# ### Prepare the data
## See LR_diff_diff_regression_sales_FE_specification.R -- the file output_yearly is prepared in that file
yearly_data <- fread(output_yearly)
## We keep all years -- Will help with leads and lags


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


### First, difference the relevant variables
yearly_data <- yearly_data[order(fips_state, fips_county, store_code_uc, product_module_code, -year),] ##Sort on store by year (year in descending order)


yearly_data[, dltax := -diff(ln_sales_tax)] ## Difference log of tax rate
yearly_data$dltax[yearly_data$year <= 2006] <- NA

yearly_data[, d_lcpricei := -diff(ln_cpricei)] ## Difference of log consumer price index (1)
yearly_data$d_lcpricei[yearly_data$year <= 2006] <- NA

yearly_data[, d_lcpricei2 := -diff(yearly_data$ln_cpricei2)] ## Difference of log consumer price index (2)
yearly_data$d_lcpricei2[yearly_data$year <= 2006] <- NA

yearly_data[, d_lquantity := -diff(yearly_data$ln_quantity)] ## Difference of log quantity (1)
yearly_data$d_lquantity[yearly_data$year <= 2006] <- NA

yearly_data[, d_lquantity2 := -diff(yearly_data$ln_quantity2)] ## Difference of log quantity (2)
yearly_data$d_lquantity2[yearly_data$year <= 2006] <- NA


#############################################
#Lag and lead of dltax
yearly_data <- yearly_data[order(fips_state, fips_county, store_code_uc, product_module_code, year), c("lag1", "lag2") := shift(.SD, 1:2, type = "lag"), .SDcols = "dltax", by = c("fips_state", "fips_county", "store_code_uc", "product_module_code")]
yearly_data <- yearly_data[order(fips_state, fips_county, store_code_uc, product_module_code, year), c("lead1", "lead2") := shift(.SD, 1:2, type = "lead"), .SDcols = "dltax", by = c("fips_state", "fips_county", "store_code_uc", "product_module_code")]

#############################################

### Second, run the diff-in-diff specifications (in first differences)
list.outcomes <- c("d_lcpricei", "d_lcpricei2", "d_lquantity", "d_lquantity2")
for(j in 1:length(list.outcomes)) {
  for(k in 2009:2014) {

    #
    outcome.j <- list.outcomes[j]


    ## Only year FE
    formula0 <- as.formula(paste0(
      outcome.j, " ~ 1 + lead2 + lead1 + dltax + lag1 + lag2 | 1 | 0 | state_by_module "
    ))

    res0 <- felm(data = yearly_data[year == k,],
                       formula = formula0,
                       weights = yearly_data[year == k,]$base.sales)

    if(j == 1 & k == 2009) {

      LRdiff_res <-
        data.table(
          outcome = outcome.j,
          estimate = coef(summary(res0))["lead2", "Estimate"],
          se = coef(summary(res0))["lead2", "Cluster s.e."],
          pval = coef(summary(res0))["lead2", "Pr(>|t|)"],
          Rsq = summary(res0)$r.squared,
          adj.Rsq = summary(res0)$adj.r.squared,
          time_controls = "year FE",
          lead_lag = -2,
          cohort = k
        )
    } else {

      LRdiff_res <- rbind(
        LRdiff_res,
        data.table(
          outcome = outcome.j,
          estimate = coef(summary(res0))["lead2", "Estimate"],
          se = coef(summary(res0))["lead2", "Cluster s.e."],
          pval = coef(summary(res0))["lead2", "Pr(>|t|)"],
          Rsq = summary(res0)$r.squared,
          adj.Rsq = summary(res0)$adj.r.squared,
          time_controls = "year FE",
          lead_lag = -2,
          cohort = k
        )
      )
    }

    ### Append other results
    LRdiff_res <- rbind(
      LRdiff_res,
      data.table(
        outcome = outcome.j,
        estimate = coef(summary(res0))["lead1", "Estimate"],
        se = coef(summary(res0))["lead1", "Cluster s.e."],
        pval = coef(summary(res0))["lead1", "Pr(>|t|)"],
        Rsq = summary(res0)$r.squared,
        adj.Rsq = summary(res0)$adj.r.squared,
        time_controls = "year FE",
        lead_lag = -1,
        cohort = k
      ),
      data.table(
        outcome = outcome.j,
        estimate = coef(summary(res0))["dltax", "Estimate"],
        se = coef(summary(res0))["dltax", "Cluster s.e."],
        pval = coef(summary(res0))["dltax", "Pr(>|t|)"],
        Rsq = summary(res0)$r.squared,
        adj.Rsq = summary(res0)$adj.r.squared,
        time_controls = "year FE",
        lead_lag = 0,
        cohort = k
      ),
      data.table(
        outcome = outcome.j,
        estimate = coef(summary(res0))["lag1", "Estimate"],
        se = coef(summary(res0))["lag1", "Cluster s.e."],
        pval = coef(summary(res0))["lag1", "Pr(>|t|)"],
        Rsq = summary(res0)$r.squared,
        adj.Rsq = summary(res0)$adj.r.squared,
        time_controls = "year FE",
        lead_lag = 1,
        cohort = k
      ),
      data.table(
        outcome = outcome.j,
        estimate = coef(summary(res0))["lag2", "Estimate"],
        se = coef(summary(res0))["lag2", "Cluster s.e."],
        pval = coef(summary(res0))["lag2", "Pr(>|t|)"],
        Rsq = summary(res0)$r.squared,
        adj.Rsq = summary(res0)$adj.r.squared,
        time_controls = "year FE",
        lead_lag = 2,
        cohort = k
      )
    )

    # Take sums of pre-, post- and total effect
    # Take linear combinations of coefficients
    #Total pre-period
    lc.pre <- "lead2 + lead1"
    lc.pre.form <- paste0(lc.pre, " = 0", sep = "")
    lc.pre.test <- glht(res0, linfct = c(lc.pre.form))
    lc.pre.est <- coef(summary(lc.pre.test))[[1]]
    lc.pre.se <- sqrt(vcov(summary(lc.pre.test)))[[1]]
    lc.pre.pval <- 2*(1 - pnorm(abs(lc.pre.est/lc.pre.se)))

    ## Save results
    LRdiff_res <- rbind(
      LRdiff_res,
      data.table(
      outcome = outcome.j,
      estimate = lc.pre.est,
      se = lc.pre.se,
      pval = lc.pre.pval,
      Rsq = summary(res0)$r.squared,
      adj.Rsq = summary(res0)$adj.r.squared,
      time_controls = "year FE",
      lead_lag = "avg. pre",
      cohort = k
    )
    )


    #Total post-period
    lc.post <- "dltax + lag1 + lag2"
    lc.post.form <- paste0(lc.post, " = 0", sep = "")
    lc.post.test <- glht(res0, linfct = c(lc.post.form))
    lc.post.est <- coef(summary(lc.post.test))[[1]]
    lc.post.se <- sqrt(vcov(summary(lc.post.test)))[[1]]
    lc.post.pval <- 2*(1 - pnorm(abs(lc.post.est/lc.post.se)))

    ## Save results
    LRdiff_res <- rbind(
      LRdiff_res,
      data.table(
        outcome = outcome.j,
        estimate = lc.post.est,
        se = lc.post.se,
        pval = lc.post.pval,
        Rsq = summary(res0)$r.squared,
        adj.Rsq = summary(res0)$adj.r.squared,
        time_controls = "year FE",
        lead_lag = "avg. post",
        cohort = k
      )
    )


    #Total pre- and post-period
    lc.tot <- "lead2 + lead1 + dltax + lag1 + lag2"
    lc.tot.form <- paste0(lc.tot, " = 0", sep = "")
    lc.tot.test <- glht(res0, linfct = c(lc.tot.form))
    lc.tot.est <- coef(summary(lc.tot.test))[[1]]
    lc.tot.se <- sqrt(vcov(summary(lc.tot.test)))[[1]]
    lc.tot.pval <- 2*(1 - pnorm(abs(lc.tot.est/lc.tot.se)))

    ## Save results
    LRdiff_res <- rbind(
      LRdiff_res,
      data.table(
        outcome = outcome.j,
        estimate = lc.tot.est,
        se = lc.tot.se,
        pval = lc.tot.pval,
        Rsq = summary(res0)$r.squared,
        adj.Rsq = summary(res0)$adj.r.squared,
        time_controls = "year FE",
        lead_lag = "avg. total effect",
        cohort = k
      )
    )


    ## Module by year FE
    outcome.j <- list.outcomes[j]

    formula1 <- as.formula(paste0(
      outcome.j, " ~ lead2 + lead1 + dltax + lag1 + lag2 | module_by_time | 0 | state_by_module "
    ))

    res1 <- felm(data = yearly_data[year == k,],
                 formula = formula1,
                 weights = yearly_data[year == k,]$base.sales)


    ### Append results
    LRdiff_res <- rbind(
      LRdiff_res,
      data.table(
        outcome = outcome.j,
        estimate = coef(summary(res1))["lead2", "Estimate"],
        se = coef(summary(res1))["lead2", "Cluster s.e."],
        pval = coef(summary(res1))["lead2", "Pr(>|t|)"],
        Rsq = summary(res1)$r.squared,
        adj.Rsq = summary(res1)$adj.r.squared,
        time_controls = "Module by year FE",
        lead_lag = -2,
        cohort = k
      ),
      data.table(
        outcome = outcome.j,
        estimate = coef(summary(res1))["lead1", "Estimate"],
        se = coef(summary(res1))["lead1", "Cluster s.e."],
        pval = coef(summary(res1))["lead1", "Pr(>|t|)"],
        Rsq = summary(res1)$r.squared,
        adj.Rsq = summary(res1)$adj.r.squared,
        time_controls = "Module by year FE",
        lead_lag = -1,
        cohort = k
      ),
      data.table(
        outcome = outcome.j,
        estimate = coef(summary(res1))["dltax", "Estimate"],
        se = coef(summary(res1))["dltax", "Cluster s.e."],
        pval = coef(summary(res1))["dltax", "Pr(>|t|)"],
        Rsq = summary(res1)$r.squared,
        adj.Rsq = summary(res1)$adj.r.squared,
        time_controls = "Module by year FE",
        lead_lag = 0,
        cohort = k
      ),
      data.table(
        outcome = outcome.j,
        estimate = coef(summary(res1))["lag1", "Estimate"],
        se = coef(summary(res1))["lag1", "Cluster s.e."],
        pval = coef(summary(res1))["lag1", "Pr(>|t|)"],
        Rsq = summary(res1)$r.squared,
        adj.Rsq = summary(res1)$adj.r.squared,
        time_controls = "Module by year FE",
        lead_lag = 1,
        cohort = k
      ),
      data.table(
        outcome = outcome.j,
        estimate = coef(summary(res1))["lag2", "Estimate"],
        se = coef(summary(res1))["lag2", "Cluster s.e."],
        pval = coef(summary(res1))["lag2", "Pr(>|t|)"],
        Rsq = summary(res1)$r.squared,
        adj.Rsq = summary(res1)$adj.r.squared,
        time_controls = "Module by year FE",
        lead_lag = 2,
        cohort = k
      )
    )

    # Take sums of pre-, post- and total effect
    # Take linear combinations of coefficients
    #Total pre-period
    lc.pre <- "lead2 + lead1"
    lc.pre.form <- paste0(lc.pre, " = 0", sep = "")
    lc.pre.test <- glht(res1, linfct = c(lc.pre.form))
    lc.pre.est <- coef(summary(lc.pre.test))[[1]]
    lc.pre.se <- sqrt(vcov(summary(lc.pre.test)))[[1]]
    lc.pre.pval <- 2*(1 - pnorm(abs(lc.pre.est/lc.pre.se)))

    ## Save results
    LRdiff_res <- rbind(
      LRdiff_res,
      data.table(
        outcome = outcome.j,
        estimate = lc.pre.est,
        se = lc.pre.se,
        pval = lc.pre.pval,
        Rsq = summary(res1)$r.squared,
        adj.Rsq = summary(res1)$adj.r.squared,
        time_controls = "Module by year FE",
        lead_lag = "avg. pre",
        cohort = k
      )
    )


    #Total post-period
    lc.post <- "dltax + lag1 + lag2"
    lc.post.form <- paste0(lc.post, " = 0", sep = "")
    lc.post.test <- glht(res1, linfct = c(lc.post.form))
    lc.post.est <- coef(summary(lc.post.test))[[1]]
    lc.post.se <- sqrt(vcov(summary(lc.post.test)))[[1]]
    lc.post.pval <- 2*(1 - pnorm(abs(lc.post.est/lc.post.se)))

    ## Save results
    LRdiff_res <- rbind(
      LRdiff_res,
      data.table(
        outcome = outcome.j,
        estimate = lc.post.est,
        se = lc.post.se,
        pval = lc.post.pval,
        Rsq = summary(res1)$r.squared,
        adj.Rsq = summary(res1)$adj.r.squared,
        time_controls = "Module by year FE",
        lead_lag = "avg. post",
        cohort = k
      )
    )


    #Total pre- and post-period
    lc.tot <- "lead2 + lead1 + dltax + lag1 + lag2"
    lc.tot.form <- paste0(lc.tot, " = 0", sep = "")
    lc.tot.test <- glht(res1, linfct = c(lc.tot.form))
    lc.tot.est <- coef(summary(lc.tot.test))[[1]]
    lc.tot.se <- sqrt(vcov(summary(lc.tot.test)))[[1]]
    lc.tot.pval <- 2*(1 - pnorm(abs(lc.tot.est/lc.tot.se)))

    ## Save results
    LRdiff_res <- rbind(
      LRdiff_res,
      data.table(
        outcome = outcome.j,
        estimate = lc.tot.est,
        se = lc.tot.se,
        pval = lc.tot.pval,
        Rsq = summary(res1)$r.squared,
        adj.Rsq = summary(res1)$adj.r.squared,
        time_controls = "Module by year FE",
        lead_lag = "avg. total effect",
        cohort = k
      )
    )


    ## Module by Region by year FE
    outcome.j <- list.outcomes[j]

    formula2 <- as.formula(paste0(
      outcome.j, " ~ lead2 + lead1 + dltax + lag1 + lag2 | reg_by_module_by_time | 0 | state_by_module "
    ))

    res2 <- felm(data = yearly_data[year == k,],
                 formula = formula2,
                 weights = yearly_data[year == k,]$base.sales)


    ### Append results
    LRdiff_res <- rbind(
      LRdiff_res,
      data.table(
        outcome = outcome.j,
        estimate = coef(summary(res2))["lead2", "Estimate"],
        se = coef(summary(res2))["lead2", "Cluster s.e."],
        pval = coef(summary(res2))["lead2", "Pr(>|t|)"],
        Rsq = summary(res2)$r.squared,
        adj.Rsq = summary(res2)$adj.r.squared,
        time_controls = "Module by region by year FE",
        lead_lag = -2,
        cohort = k
      ),
      data.table(
        outcome = outcome.j,
        estimate = coef(summary(res2))["lead1", "Estimate"],
        se = coef(summary(res2))["lead1", "Cluster s.e."],
        pval = coef(summary(res2))["lead1", "Pr(>|t|)"],
        Rsq = summary(res2)$r.squared,
        adj.Rsq = summary(res2)$adj.r.squared,
        time_controls = "Module by region by year FE",
        lead_lag = -1,
        cohort = k
      ),
      data.table(
        outcome = outcome.j,
        estimate = coef(summary(res2))["dltax", "Estimate"],
        se = coef(summary(res2))["dltax", "Cluster s.e."],
        pval = coef(summary(res2))["dltax", "Pr(>|t|)"],
        Rsq = summary(res2)$r.squared,
        adj.Rsq = summary(res2)$adj.r.squared,
        time_controls = "Module by region by year FE",
        lead_lag = 0,
        cohort = k
      ),
      data.table(
        outcome = outcome.j,
        estimate = coef(summary(res2))["lag1", "Estimate"],
        se = coef(summary(res2))["lag1", "Cluster s.e."],
        pval = coef(summary(res2))["lag1", "Pr(>|t|)"],
        Rsq = summary(res2)$r.squared,
        adj.Rsq = summary(res2)$adj.r.squared,
        time_controls = "Module by region by year FE",
        lead_lag = 1,
        cohort = k
      ),
      data.table(
        outcome = outcome.j,
        estimate = coef(summary(res2))["lag2", "Estimate"],
        se = coef(summary(res2))["lag2", "Cluster s.e."],
        pval = coef(summary(res2))["lag2", "Pr(>|t|)"],
        Rsq = summary(res2)$r.squared,
        adj.Rsq = summary(res2)$adj.r.squared,
        time_controls = "Module by region by year FE",
        lead_lag = 2,
        cohort = k
      )
    )


    # Take sums of pre-, post- and total effect
    # Take linear combinations of coefficients
    #Total pre-period
    lc.pre <- "lead2 + lead1"
    lc.pre.form <- paste0(lc.pre, " = 0", sep = "")
    lc.pre.test <- glht(res2, linfct = c(lc.pre.form))
    lc.pre.est <- coef(summary(lc.pre.test))[[1]]
    lc.pre.se <- sqrt(vcov(summary(lc.pre.test)))[[1]]
    lc.pre.pval <- 2*(1 - pnorm(abs(lc.pre.est/lc.pre.se)))

    ## Save results
    LRdiff_res <- rbind(
      LRdiff_res,
      data.table(
        outcome = outcome.j,
        estimate = lc.pre.est,
        se = lc.pre.se,
        pval = lc.pre.pval,
        Rsq = summary(res2)$r.squared,
        adj.Rsq = summary(res2)$adj.r.squared,
        time_controls = "Module by region by year FE",
        lead_lag = "avg. pre",
        cohort = k
      )
    )


    #Total post-period
    lc.post <- "dltax + lag1 + lag2"
    lc.post.form <- paste0(lc.post, " = 0", sep = "")
    lc.post.test <- glht(res2, linfct = c(lc.post.form))
    lc.post.est <- coef(summary(lc.post.test))[[1]]
    lc.post.se <- sqrt(vcov(summary(lc.post.test)))[[1]]
    lc.post.pval <- 2*(1 - pnorm(abs(lc.post.est/lc.post.se)))

    ## Save results
    LRdiff_res <- rbind(
      LRdiff_res,
      data.table(
        outcome = outcome.j,
        estimate = lc.post.est,
        se = lc.post.se,
        pval = lc.post.pval,
        Rsq = summary(res2)$r.squared,
        adj.Rsq = summary(res2)$adj.r.squared,
        time_controls = "Module by region by year FE",
        lead_lag = "avg. post",
        cohort = k
      )
    )


    #Total pre- and post-period
    lc.tot <- "lead2 + lead1 + dltax + lag1 + lag2"
    lc.tot.form <- paste0(lc.tot, " = 0", sep = "")
    lc.tot.test <- glht(res2, linfct = c(lc.tot.form))
    lc.tot.est <- coef(summary(lc.tot.test))[[1]]
    lc.tot.se <- sqrt(vcov(summary(lc.tot.test)))[[1]]
    lc.tot.pval <- 2*(1 - pnorm(abs(lc.tot.est/lc.tot.se)))

    ## Save results
    LRdiff_res <- rbind(
      LRdiff_res,
      data.table(
        outcome = outcome.j,
        estimate = lc.tot.est,
        se = lc.tot.se,
        pval = lc.tot.pval,
        Rsq = summary(res2)$r.squared,
        adj.Rsq = summary(res2)$adj.r.squared,
        time_controls = "Module by region by year FE",
        lead_lag = "avg. total effect",
        cohort = k
      )
    )

  }
}


## summary values --------------------------------------------------------------
LRdiff_res$N_obs <- nrow(yearly_data)
LRdiff_res$N_modules <- length(unique(yearly_data$product_module_code))
LRdiff_res$N_stores <- length(unique(yearly_data$store_code_uc))
LRdiff_res$N_counties <- uniqueN(yearly_data, by = c("fips_state", "fips_county"))
LRdiff_res$N_years <- uniqueN(yearly_data, by = c("year")) # should be 6 (we lose one because we difference)
LRdiff_res$N_county_modules <- uniqueN(yearly_data, by = c("fips_state", "fips_county",
                                                           "product_module_code"))

fwrite(LRdiff_res, "Data/LRdiff_results_diff_leadlags_cohortbycohort.csv")


