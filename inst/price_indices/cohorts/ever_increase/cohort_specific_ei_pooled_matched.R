#' Author: John Bonney
#'
#' This file creates event-study means plots based on the tax-inclusive price indices.
#' It is an adaptation of explore_pi_pre_trends.R that removes reforms in
#' 2013 Q1 and creates means for four groups: treated, future treated, future
#' treated in over one year, or never treated.
#'

library(data.table)
library(readstata13)
library(sales.taxes)
library(zoo)
library(ggplot2)
library(futile.logger)
library(MatchIt)

setwd("/project2/igaarder")
change_of_interest <- "Ever increase"
get_p_score <- F
output.filepath <- "Data/pi_all_cohorts_ei_pooled_taxable.csv"

# check function
g <- function(dt) {
  print(head(dt))
}

## useful filepaths ------------------------------------------------------------
original.eventstudy_tr_path <- "Data/event_study_tr_groups_comprehensive_w2014.csv"
original.tr_groups_path <- "Data/tr_groups_comprehensive_w2014.csv"
eventstudy_tr_path <- "Data/event_study_tr_groups_comprehensive_firstonly_no2012q4_2013q1q2.csv"
tr_groups_path <- "Data/tr_groups_comprehensive_firstonly_no2012q4_2013q1q2.csv"
sales_data_path <- "Data/sales_quarterly_2006-2016.csv"
tax_rates_path <- "Data/county_monthly_tax_rates.csv"
module_exemptions_path <- "Data/modules_exemptions_long.csv"
all_goods_pi_path <- "Data/Nielsen/price_quantity_indices_allitems_2006-2016_notaxinfo.csv"
taxable_pi_path <- "Data/Nielsen/price_quantity_indices_taxableitems_2006-2016.csv"

## covariate filepaths (for matching)
zillow_path <- "Data/covariates/zillow_long_by_state_clean.csv"
unemp_path <- "Data/covariates/county_monthly_unemp_clean.csv"
nhgis_path <- "Data/covariates/nhgis_county_clean.csv"
qcew_path <- "Data/covariates/qcew_clean.csv"
fips_path <- "Data/covariates/state_fips_master.csv"

## propensity score output path
prop_output_path <- "Data/matched_cohort_samples.csv"

## read in covariates and merge together
zillow_dt <- fread(zillow_path)
zillow_dt <- zillow_dt[, .(fips_state, median_home_price, year, month)]
zillow_dt <- zillow_dt[year >= 2007]
zillow_dt[, median_home_price := log(median_home_price)]
setkey(zillow_dt, fips_state, year, month)
zillow_dt[, median_home_price.change := shift(median_home_price, n = 6) - shift(median_home_price, n = 24),
          by = fips_state]
zillow_dt <- zillow_dt[between(year, 2009, 2013)]
zillow_dt[, month := as.integer(round(month))]

unemp_dt <- fread(unemp_path)
unemp_dt <- unemp_dt[, .(fips_state, fips_county, rate, year, month)]
unemp_dt <- unemp_dt[year >= 2007]
setkey(unemp_dt, fips_state, fips_county, year, month)
unemp_dt[, rate.change := shift(rate, n = 6) - shift(rate, n = 24),
         by = .(fips_state, fips_county)]
unemp_dt <- unemp_dt[between(year, 2009, 2013)]

covariates <- merge(unemp_dt, zillow_dt, by = c("fips_state", "year", "month"))

fips_dt <- fread(fips_path)
fips_dt <- fips_dt[, .(fips_state, region_name, division_name)]
covariates <- merge(covariates, fips_dt, by = "fips_state")
covariates[, region := factor(region_name)]
covariates[, division := factor(division_name)]
covariates[, c("region_name", "division_name") := NULL]

fixed_covariates <- fread(nhgis_path)
setnames(fixed_covariates, skip_absent = T,
         old = c("statefp", "countyfp"),
         new = c("fips_state", "fips_county"))
fixed_covariates <- fixed_covariates[year == 2000]
fixed_covariates <- fixed_covariates[, .(
  fips_state, fips_county, pct_pop_bachelors, pct_pop_urban
  )]
covariates <- merge(covariates, fixed_covariates,
                    by = c("fips_state", "fips_county"))
# since cohorts are defined by quarters:
covariates <- covariates[month %in% c(2, 5, 8, 11)]
covariates[, quarter := ceiling(month / 3)]
covariates[, month := NULL]
# now I have a yearXquarter dataset that can be subset for cohorts

## prepare treatment events for treated and control groups ---------------------
tr_events <- fread(eventstudy_tr_path)
tr_events <- tr_events[tr_group == change_of_interest]

control_counties <- fread(original.eventstudy_tr_path)
control_counties <- control_counties[tr_group == change_of_interest]

## limit to first event only
control_counties[, treatment_month := 12 * ref_year + ref_month]
control_counties[, min.event := (treatment_month == min(treatment_month)),
                 by = .(fips_state, fips_county, tr_group)]
control_counties <- control_counties[min.event == T]
control_counties[, min.event := NULL]
control_counties[, ref_quarter := ceiling(ref_month / 3)]

## no change control group
control_counties.nt <- fread(tr_groups_path)
control_counties.nt <- control_counties.nt[tr_group == "No change"]
control_counties.nt <- unique(control_counties.nt[, .(fips_county, fips_state)])
control_counties.nt[, treated := 0]

## construct matched control group
if (get_p_score) {
  control.matched <- data.table(NULL)
  tr_events.matching <- copy(tr_events)
  tr_events.matching[, ref_quarter := ceiling(ref_month / 3)]
  for (ref.yr in 2009:2013) {
    for (ref.qtr in 1:4) {
      flog.info("Matching for %sQ%s", ref.yr, ref.qtr)
      cohort_treated <- tr_events.matching[ref_year == ref.yr & ref_quarter == ref.qtr]
      cohort_treated <- cohort_treated[, .(fips_state, fips_county)]
      cohort_treated[, treated := 1]
      if (nrow(cohort_treated) == 0) next
      cohort_counties <- rbind(cohort_treated,
                               control_counties.nt)
      pre.N <- nrow(unique(cohort_counties[treated == 1], by = c("fips_state", "fips_county")))
      cohort_covariates <- merge(cohort_counties,
                                 covariates[year == ref.yr & quarter == ref.qtr],
                                 by = c("fips_state", "fips_county"))
      post.N <- nrow(unique(cohort_covariates[treated == 1], by = c("fips_state", "fips_county")))
      if (pre.N != post.N) {
        warning(sprintf("%s treated counties dropped when merging covariates", pre.N - post.N))
      }

      prop_formula <- as.formula(paste(
        "treated ~ rate + rate.change + median_home_price + median_home_price.change +",
        "pct_pop_bachelors + pct_pop_urban + division"
      ))
      # prop_res <- glm(formula = prop_formula,
      #                  data = cohort_covariates,
      #                  family = binomial(link = "logit"))
      # cohort_covariates$prop <- predict(prop_res, type = "response")

      setkey(cohort_covariates, fips_state, fips_county)
      cohort_covariates[, ID := .I]
      cohort_matchit <- matchit(formula = prop_formula,
                                data = cohort_covariates,
                                method = "nearest",
                                ratio = 10, replace = T)
      cohort_matched <- match.data(cohort_matchit,
                                   group = "all", distance = "distance",
                                   weights = "match.weights", subclass = "subclass")
      # have to traceback the matching of treatment to control
      match.mat <- cohort_matchit$match.matrix
      match.mat <- data.table(match.mat, keep.rownames = T)
      match.mat <- tidyr::gather(match.mat,
                                 key = "control_count",
                                 value = "control_row", -rn)
      match.mat <- data.table(match.mat)
      match.mat[, ID := as.integer(control_row)]
      match.mat[, treatment_ID := as.integer(rn)]
      match.mat[, c("control_count", "rn", "control_row") := NULL]
      match.mat[, rel_weight := 1 / .N, by = ID]

      cohort_matched <- data.table(cohort_matched)
      cohort_matched <- cohort_matched[, .(
        fips_state, fips_county, treated, match.weights, ID, pct_pop_urban
      )]
      # will end up dividing cohort_matched into unit-specific controls,
      # with their own proportional weights
      cohort_matched <- merge(cohort_matched, match.mat, by = "ID", all = T)
      cohort_matched[treated == 0, match.weights := match.weights * rel_weight]
      cohort_matched[treated == 1, treatment_ID := ID]
      cohort_matched[, rel_weight := NULL]

      cohort_matched[, ref_year := ref.yr]
      cohort_matched[, ref_quarter := ref.qtr]

      control.matched <- rbind(control.matched,
                               cohort_matched)
    }
  }
  control.matched[, treatment_grp := .GRP, by = .(treatment_ID, ref_year, ref_quarter)]
  control.matched[, c("treatment_ID", "ID") := NULL]
  setkey(control.matched, treatment_grp, treated, fips_state, fips_county)
  fwrite(control.matched, prop_output_path)
} else {
  control.matched <- fread(prop_output_path)
}

# Taxable goods only ===========================================================
taxable_pi <- fread(all_goods_pi_path)
taxable_pi <- taxable_pi[sales_tax > 1 | (is.na(sales_tax) & year < 2008)]
taxable_pi <- taxable_pi[year %in% 2006:2014 & !is.na(cpricei)]
flog.info("Base data N: %s", nrow(taxable_pi))

# do `arbitrary` correction for the 2013 Q1 jump in the data
## calculate price index in 2013 Q1 / cpricei in 2012 Q4
taxable_pi[, correction := pricei[year == 2013 & quarter == 1] / pricei[year == 2012 & quarter == 4],
           by = .(store_code_uc, product_module_code)]
## divide price index after 2013 Q1 (inclusive) by above value
taxable_pi[year >= 2013, cpricei := cpricei / correction]

taxable_pi[, cpricei := log(cpricei)]
taxable_pi[, sales_tax := log(sales_tax)]

## get sales weights -----------------------------------------------------------
taxable_pi[, base.sales := sales[year == 2008 & quarter == 1],
           by = .(store_code_uc, product_module_code)]

taxable_pi[, sales := NULL]
taxable_pi <- taxable_pi[!is.na(base.sales) & !is.na(cpricei)]
flog.info("Normalized with weights N: %s", nrow(taxable_pi))


## balance sample on store-module-level from 2006 to 2014 ----------------------
keep_store_modules <- taxable_pi[, list(n = .N),
                                 by = .(store_code_uc, product_module_code)]
keep_store_modules <- keep_store_modules[n == (2014 - 2005) * 4]

setkey(taxable_pi, store_code_uc, product_module_code)
setkey(keep_store_modules, store_code_uc, product_module_code)

taxable_pi <- taxable_pi[keep_store_modules]
setkey(taxable_pi, fips_county, fips_state)
flog.info("Balanced N: %s", nrow(taxable_pi))

taxable_pi_original <- copy(taxable_pi)

## merge treatment, attach event times -----------------------------------------
taxable_pi <- merge(taxable_pi, tr_events,
                    by = c("fips_county", "fips_state"),
                    allow.cartesian = TRUE)

setnames(taxable_pi, "V1", "event_ID")

## define time to event --------------------------------------------------------
taxable_pi[, ref_quarter := ceiling(ref_month / 3)]
taxable_pi[, tt_event := as.integer(4 * year + quarter -
                                      (4 * ref_year + ref_quarter))]

## limit data to three year window around reform ---------------------------------
taxable_pi <- taxable_pi[tt_event >= -8 & tt_event <= 4]

## add pseudo-control group ----------------------------------------------------

### create dataset of never treated counties
control_dt.nt <- merge(taxable_pi_original, control_counties.nt,
                       by = c("fips_state", "fips_county"))

control_dt.nt <- control_dt.nt[, list(
  control.cpricei = weighted.mean(x = cpricei, w = base.sales),
  control.sales_tax = weighted.mean(sales_tax, w = base.sales)
  ), by = .(quarter, year, product_module_code)]

control_dt.nt[, control.type := "No change"]

matched_control_data.nt <- merge(taxable_pi, control_dt.nt,
                              by = c("quarter", "year", "product_module_code"),
                              allow.cartesian = T)

matched_control_data.nt <- matched_control_data.nt[, .(
  control.cpricei, tt_event, event_ID, store_code_uc, product_module_code,
  control.sales_tax, tr_group, base.sales, ref_year, ref_quarter, control.type
  )]

rm(control_dt.nt)

### create dataset of future treated counties (restricted)
control_dt.ft <- merge(taxable_pi_original, control_counties.ft,
                       by = c("fips_state", "fips_county"),
                       allow.cartesian = T)

control_dt.ft <- control_dt.ft[, list(
  control.cpricei = weighted.mean(cpricei, w = base.sales),
  control.sales_tax = weighted.mean(sales_tax, w = base.sales)
  ), by = .(quarter, year, product_module_code, ref_year, ref_quarter, tr_group)]

control_dt.ft[, control.type := "Future change"]

matched_control_data.ft <- merge(taxable_pi, control_dt.ft,
                              by = c("quarter", "year", "product_module_code",
                                     "ref_year", "ref_quarter", "tr_group"))

matched_control_data.ft <- matched_control_data.ft[, .(
  control.cpricei, tt_event, event_ID, store_code_uc, product_module_code,
  tr_group, base.sales, ref_year, ref_quarter, control.sales_tax, control.type
  )]
rm(control_dt.ft)

### created dataset of future treated counties (unrestricted)
control_dt.ftu <- merge(taxable_pi_original, control_counties.ftu,
                        by = c("fips_state", "fips_county", "year", "quarter"),
                        allow.cartesian = T)

control_dt.ftu <- control_dt.ftu[, list(
  control.cpricei = weighted.mean(cpricei, w = base.sales),
  control.sales_tax = weighted.mean(sales_tax, w = base.sales)
  ), by = .(quarter, year, product_module_code, ref_year, ref_quarter, tr_group)]
control_dt.ftu[, control.type := "Future change, unrestricted"]

matched_control_data.ftu <- merge(taxable_pi, control_dt.ftu,
                              by = c("quarter", "year", "product_module_code",
                                     "ref_year", "ref_quarter", "tr_group"))

matched_control_data.ftu <- matched_control_data.ftu[, .(
  control.cpricei, tt_event, event_ID, store_code_uc, product_module_code,
  tr_group, base.sales, ref_year, ref_quarter, control.sales_tax, control.type
  )]

rm(control_dt.ftu)


## combine the three matched groups
matched_control_data <- rbind(matched_control_data.nt,
                              matched_control_data.ft,
                              matched_control_data.ftu)

rm(taxable_pi_original, matched_control_data.nt,
   matched_control_data.ft, matched_control_data.ftu)
gc()

setnames(matched_control_data,
         old = c("control.cpricei", "control.sales_tax"),
         new = c("cpricei",         "sales_tax"))
matched_control_data[, tr_group := paste0(control.type, " (", tolower(tr_group), ")")]

taxable_pi <- rbind(taxable_pi, matched_control_data, fill = T)


## normalize price indices based on time to event ------------------------------
taxable_pi[, normalized.cpricei := cpricei - cpricei[tt_event == -2],
           by = .(store_code_uc, product_module_code, ref_year, ref_quarter,
                  tr_group, event_ID)]
# drops groups for which tt_event == -2 not available
taxable_pi <- taxable_pi[!is.na(normalized.cpricei)]
# note that this is still log cpricei

### this illustrates an issue that we get
# test_collapsed <- taxable_pi[, list(mean.cpricei = weighted.mean(normalized.cpricei, w = base.sales),
#                                     total_sales = sum(base.sales)),
#                              by = .(tt_event, ref_quarter, ref_year, tr_group)]
#
# fwrite(test_collapsed, "Data/test_collapsed_V3.csv")

## aggregate by treatment group ------------------------------------------------

taxable_pi_es_collapsed <- taxable_pi[,
                                      list(mean_pi = weighted.mean(x = normalized.cpricei, w = base.sales),
                                           mean_tax = weighted.mean(sales_tax, w = base.sales, na.rm = T),
                                           n_counties = uniqueN(1000 * fips_state + fips_county),
                                           n_stores = uniqueN(store_code_uc),
                                           total_sales = sum(base.sales)),
                                      by = c("tr_group", "tt_event")
                                      ]

fwrite(taxable_pi_es_collapsed, output.filepath)

rm(taxable_pi)
gc()

