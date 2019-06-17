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
pre_trend_data_path <- "Data/Nielsen/pre_trend_data_yearly.csv"
#taxable_pi_path <- "Data/Nielsen/price_quantity_indices_taxableitems_2006-2016.csv"
#eventstudy_tr_path <- "Data/event_study_tr_groups_comprehensive_firstonly_no2012q4_2013q1q2.csv"
#tr_groups_path <- "Data/tr_groups_comprehensive_firstonly_no2012q4_2013q1q2.csv"


# ### Prepare the data
## See LR_diff_diff_regression_sales_FE_specification.R -- the file output_yearly is prepared in that file
yearly_data <- fread(output_yearly)


### First, difference the relevant variables
yearly_data <- yearly_data[year >= 2008 & year <= 2014]
yearly_data <- yearly_data[order(fips_state, fips_county, store_code_uc, product_module_code, -year),] ##Sort on store by year (year in descending order)

yearly_data$dltax <- -diff(yearly_data$ln_sales_tax) ## Difference log of tax rate
yearly_data$dltax[yearly_data$year <= 2008] <- NA

yearly_data$d_lcpricei <- - diff(yearly_data$ln_cpricei) ## Difference of log consumer price index (1)
yearly_data$d_lcpricei[yearly_data$year <= 2008] <- NA

yearly_data$d_lcpricei2 <- - diff(yearly_data$ln_cpricei2) ## Difference of log consumer price index (2)
yearly_data$d_lcpricei2[yearly_data$year <= 2008] <- NA

yearly_data$d_lquantity <- - diff(yearly_data$ln_quantity) ## Difference of log quantity (1)
yearly_data$d_lquantity[yearly_data$year <= 2008] <- NA

yearly_data$d_lquantity2 <- - diff(yearly_data$ln_quantity2) ## Difference of log quantity (2)
yearly_data$d_lquantity2[yearly_data$year <= 2008] <- NA

yearly_data$d_expend_share <- - diff(yearly_data$expend_share) ## Difference of expenditure share
yearly_data$d_expend_share[yearly_data$year <= 2008] <- NA

yearly_data$d_lexpend_share <- - diff(yearly_data$ln_expend_share) ## Difference of log expenditure share
yearly_data$d_lexpend_share[yearly_data$year <= 2008] <- NA

## Drop 2008
yearly_data <- yearly_data[year > 2008]

### Second, run the diff-in-diff specifications (in first differences)
list.outcomes <- c("d_lcpricei", "d_lcpricei2", "d_lquantity", "d_lquantity2", "d_expend_share", "d_lexpend_share")
for(j in 1:length(list.outcomes)) {

  #
  outcome.j <- list.outcomes[j]


  ## Only year FE
  formula0 <- as.formula(paste0(
    outcome.j, " ~ dltax | yr | 0 | state_by_module "
  ))

  res0 <- felm(data = yearly_data,
                     formula = formula0,
                     weights = yearly_data$base.sales)

  if(j == 1) {

    LRdiff_res <-
      data.table(
        outcome = outcome.j,
        estimate = coef(summary(res0))["dltax", "Estimate"],
        se = coef(summary(res0))["dltax", "Cluster s.e."],
        pval = coef(summary(res0))["dltax", "Pr(>|t|)"],
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
        estimate = coef(summary(res0))["dltax", "Estimate"],
        se = coef(summary(res0))["dltax", "Cluster s.e."],
        pval = coef(summary(res0))["dltax", "Pr(>|t|)"],
        Rsq = summary(res0)$r.squared,
        adj.Rsq = summary(res0)$adj.r.squared,
        time_controls = "year FE"
      )
    )
  }


  ## Linear time trend (module specific)
  outcome.j <- list.outcomes[j]

  formula1 <- as.formula(paste0(
    outcome.j, " ~ dltax | yr + product_module_code | 0 | state_by_module "
  ))

  res1 <- felm(data = yearly_data,
               formula = formula1,
               weights = yearly_data$base.sales)

  LRdiff_res <- rbind(
    LRdiff_res,
    data.table(
      outcome = outcome.j,
      estimate = coef(summary(res1))["dltax", "Estimate"],
      se = coef(summary(res1))["dltax", "Cluster s.e."],
      pval = coef(summary(res1))["dltax", "Pr(>|t|)"],
      Rsq = summary(res1)$r.squared,
      adj.Rsq = summary(res1)$adj.r.squared,
      time_controls = "Module linear trend + year FE"
    )
  )


  ## Linear time trend (module by store specific)
  outcome.j <- list.outcomes[j]

  formula2 <- as.formula(paste0(
    outcome.j, " ~ dltax | yr + store_module | 0 | state_by_module "
  ))

  res2 <- felm(data = yearly_data,
               formula = formula2,
               weights = yearly_data$base.sales)

  LRdiff_res <- rbind(
    LRdiff_res,
    data.table(
      outcome = outcome.j,
      estimate = coef(summary(res2))["dltax", "Estimate"],
      se = coef(summary(res2))["dltax", "Cluster s.e."],
      pval = coef(summary(res2))["dltax", "Pr(>|t|)"],
      Rsq = summary(res2)$r.squared,
      adj.Rsq = summary(res2)$adj.r.squared,
      time_controls = "Store by Module linear trend + year FE"
    )
  )


  ## Module by year FE
  outcome.j <- list.outcomes[j]

  formula3 <- as.formula(paste0(
    outcome.j, " ~ dltax | module_by_time | 0 | state_by_module "
  ))

  res3 <- felm(data = yearly_data,
               formula = formula3,
               weights = yearly_data$base.sales)

  LRdiff_res <- rbind(
    LRdiff_res,
    data.table(
      outcome = outcome.j,
      estimate = coef(summary(res3))["dltax", "Estimate"],
      se = coef(summary(res3))["dltax", "Cluster s.e."],
      pval = coef(summary(res3))["dltax", "Pr(>|t|)"],
      Rsq = summary(res3)$r.squared,
      adj.Rsq = summary(res3)$adj.r.squared,
      time_controls = "Module by time FE"
    )
  )


  ## Store by year FE
  outcome.j <- list.outcomes[j]

  formula4 <- as.formula(paste0(
    outcome.j, " ~ dltax | store_by_time | 0 | state_by_module "
  ))

  res4 <- felm(data = yearly_data,
               formula = formula4,
               weights = yearly_data$base.sales)

  LRdiff_res <- rbind(
    LRdiff_res,
    data.table(
      outcome = outcome.j,
      estimate = coef(summary(res4))["dltax", "Estimate"],
      se = coef(summary(res4))["dltax", "Cluster s.e."],
      pval = coef(summary(res4))["dltax", "Pr(>|t|)"],
      Rsq = summary(res4)$r.squared,
      adj.Rsq = summary(res4)$adj.r.squared,
      time_controls = "Store by time FE"
    )
  )

  ## Store by year + Module by year FE
  outcome.j <- list.outcomes[j]

  formula5 <- as.formula(paste0(
    outcome.j, " ~ dltax | module_by_time + store_by_time | 0 | state_by_module "
  ))

  res5 <- felm(data = yearly_data,
               formula = formula5,
               weights = yearly_data$base.sales)


  LRdiff_res <- rbind(
    LRdiff_res,
    data.table(
      outcome = outcome.j,
      estimate = coef(summary(res5))["dltax", "Estimate"],
      se = coef(summary(res5))["dltax", "Cluster s.e."],
      pval = coef(summary(res5))["dltax", "Pr(>|t|)"],
      Rsq = summary(res5)$r.squared,
      adj.Rsq = summary(res5)$adj.r.squared,
      time_controls = "Store by time + Module by time FE"
    )
  )


  ## Module by year FE + store-specific linear trend
  outcome.j <- list.outcomes[j]

  formula6 <- as.formula(paste0(
    outcome.j, " ~ dltax | module_by_time + store_code_uc | 0 | state_by_module "
  ))

  res6 <- felm(data = yearly_data,
               formula = formula6,
               weights = yearly_data$base.sales)


  LRdiff_res <- rbind(
    LRdiff_res,
    data.table(
      outcome = outcome.j,
      estimate = coef(summary(res6))["dltax", "Estimate"],
      se = coef(summary(res6))["dltax", "Cluster s.e."],
      pval = coef(summary(res6))["dltax", "Pr(>|t|)"],
      Rsq = summary(res6)$r.squared,
      adj.Rsq = summary(res6)$adj.r.squared,
      time_controls = "Module by time FE + Store linear trend"
    )
  )

}


## summary values --------------------------------------------------------------
LRdiff_res$N_obs <- nrow(yearly_data)
LRdiff_res$N_modules <- length(unique(yearly_data$product_module_code))
LRdiff_res$N_stores <- length(unique(yearly_data$store_code_uc))
LRdiff_res$N_counties <- uniqueN(yearly_data, by = c("fips_state", "fips_county"))
LRdiff_res$N_years <- uniqueN(yearly_data, by = c("year")) # should be 6 (we lose one because we difference)
LRdiff_res$N_county_modules <- uniqueN(yearly_data, by = c("fips_state", "fips_county",
                                                           "product_module_code"))

fwrite(LRdiff_res, "Data/LRdiff_results_diffspec.csv")


