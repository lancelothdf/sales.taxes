#' Author: John Bonney
#'
#' This file creates event-study means plots based on home prices.
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

setwd("/project2/igaarder")
change_of_interest <- "Ever increase"
output.filepath <- "Data/homeprice_all_cohorts_ei_pooled.csv"

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

zillow_path <- "Data/covariates/zillow_long_by_county_clean.csv"

## prepare treatment events for treated and control groups ---------------------
tr_events <- fread(original.eventstudy_tr_path)
tr_events <- tr_events[tr_group == change_of_interest]
tr_events[, treatment_month := 12 * ref_year + ref_month]
## we are only interested in the first event
tr_events[, min.event := (treatment_month == min(treatment_month)),
          by = .(fips_state, fips_county)]
tr_events <- tr_events[min.event == T]
tr_events <- tr_events[, .(fips_county, fips_state, ref_year, ref_month, tr_group, V1)]
# exclude 2012 Q4, 2013 Q1, 2013 Q2 reforms
tr_events <- tr_events[!(ref_year == 2012 & ref_month >= 10) & !(ref_year == 2013 & ref_month %in% 1:6)]

## no change control group
control_counties.nt <- fread(tr_groups_path)
control_counties.nt <- control_counties.nt[tr_group == "No change"]
control_counties.nt <- unique(control_counties.nt[, .(fips_county, fips_state)])

## future restricted control group
control_counties.ft <- data.table(NULL)
for (yr in 2009:2013) {
  for (mon in 1:12) {
    control_counties_spec <- tr_events[
      (ref_year * 12 + ref_month) - (yr * 12 + mon) > 12, # keep if treated over 1 year in future
      .(fips_state, fips_county) # columns to keep
      ]
    control_counties_spec[, ref_year := yr]
    control_counties_spec[, ref_month := mon]

    control_counties.ft <- rbind(control_counties.ft, control_counties_spec)
  }
}

## future unrestricted control group
control_counties.ftu <- data.table(NULL)
for (ref.yr in 2009:2013) {
  for (ref.mon in 1:12) {
    for (cal.yr in 2006:2014) {
      for (cal.mon in 1:12) {
        control_counties_spec <- tr_events[
          (ref_year * 12 + ref_month) > (ref.yr * 12 + ref.mon) &   # treated after focal cohort
            (ref_year * 12 + ref_month) > (cal.yr * 12 + cal.mon),  # not yet treated
          .(fips_state, fips_county) # columns to keep
          ]

        control_counties_spec[, ref_year := ref.yr]
        control_counties_spec[, ref_month := ref.mon]
        control_counties_spec[, year := cal.yr]
        control_counties_spec[, month := cal.mon]

        control_counties.ftu <- rbind(control_counties.ftu, control_counties_spec)
      }
    }
  }
}

## prep the Zillow data --------------------------------------------------------
zillow_dt <- fread(zillow_path)
zillow_dt <- zillow_dt[between(year, 2006, 2014)]

### attach sales
# TODO: this should be just sales for taxable items
taxable_pi <- fread(all_goods_pi_path)
taxable_pi <- taxable_pi[sales_tax > 1 | (is.na(sales_tax) & year < 2008)]
taxable_pi <- taxable_pi[year %in% 2006:2014 & !is.na(cpricei)]

keep_store_modules <- taxable_pi[, list(n = .N),
                                 by = .(store_code_uc, product_module_code)]
keep_store_modules <- keep_store_modules[n == (2014 - 2005) * 4]

setkey(taxable_pi, store_code_uc, product_module_code)
setkey(keep_store_modules, store_code_uc, product_module_code)

taxable_pi <- taxable_pi[keep_store_modules]
taxable_pi <- taxable_pi[year == 2008 & quarter == 1]

taxable_pi <- taxable_pi[, list(base.sales = sum(sales)),
                         by = .(fips_county, fips_state)]

zillow_dt <- merge(zillow_dt, taxable_pi, by = c("fips_county", "fips_state"))
rm(taxable_pi)
gc()

## normalize
zillow_dt[, normalized.homeprice := log(median_home_price) -
            log(median_home_price[year == 2006 & month == 1]),
          by = .(fips_county, fips_state)]
zillow_dt <- zillow_dt[!is.na(normalized.homeprice)]

## balance on county level
keep_counties <- zillow_dt[, list(n = .N), by = .(fips_state, fips_county)]
keep_counties <- keep_counties[n == (2014 - 2005) * 12]

setkey(zillow_dt, fips_county, fips_state)
setkey(keep_counties, fips_county, fips_state)

zillow_dt <- zillow_dt[keep_counties]

# Aggregate --------------------------------------------------------------------

zillow_original <- copy(zillow_dt)

## merge treatment, attach event times -----------------------------------------
zillow_dt <- merge(zillow_dt, tr_events,
                   by = c("fips_county", "fips_state"),
                   allow.cartesian = TRUE)

setnames(zillow_dt, "V1", "event_ID")

## define time to event --------------------------------------------------------
zillow_dt[, tt_event := as.integer(12 * year + month - (12 * ref_year + ref_month))]

## limit data to three year window around reform ---------------------------------
zillow_dt <- zillow_dt[tt_event >= -24 & tt_event <= 12]

## add pseudo-control group ----------------------------------------------------

### create dataset of never treated counties
control_dt.nt <- merge(zillow_original, control_counties.nt,
                       by = c("fips_state", "fips_county"))

control_dt.nt <- control_dt.nt[, list(
  control.homeprice = weighted.mean(normalized.homeprice, w = base.sales)
  ), by = .(month, year)]

control_dt.nt[, control.type := "No change"]

matched_control_data.nt <- merge(zillow_dt, control_dt.nt,
                              by = c("month", "year"),
                              allow.cartesian = T)

matched_control_data.nt <- matched_control_data.nt[, .(
  control.homeprice, tt_event, event_ID, tr_group,
  base.sales, ref_year, ref_month, control.type
  )]

rm(control_dt.nt)

### create dataset of future treated counties (restricted)
control_dt.ft <- merge(zillow_original, control_counties.ft,
                       by = c("fips_state", "fips_county"),
                       allow.cartesian = T)

control_dt.ft <- control_dt.ft[, list(
  control.homeprice = weighted.mean(normalized.homeprice, w = base.sales)
  ), by = .(month, year, ref_year, ref_month)]

control_dt.ft[, control.type := "Future change"]

matched_control_data.ft <- merge(zillow_dt, control_dt.ft,
                              by = c("month", "year", "ref_year", "ref_month"))

matched_control_data.ft <- matched_control_data.ft[, .(
  control.homeprice, tt_event, event_ID, tr_group,
  base.sales, ref_year, ref_month, control.type
  )]
rm(control_dt.ft)

### created dataset of future treated counties (unrestricted)
control_dt.ftu <- merge(zillow_original, control_counties.ftu,
                        by = c("fips_state", "fips_county", "year", "month"),
                        allow.cartesian = T)

control_dt.ftu <- control_dt.ftu[, list(
  control.homeprice = weighted.mean(normalized.homeprice, w = base.sales)
  ), by = .(month, year, ref_year, ref_month)]
control_dt.ftu[, control.type := "Future change, unrestricted"]

matched_control_data.ftu <- merge(zillow_dt, control_dt.ftu,
                              by = c("month", "year", "ref_year", "ref_month"))

matched_control_data.ftu <- matched_control_data.ftu[, .(
  control.homeprice, tt_event, event_ID, tr_group,
  base.sales, ref_year, ref_month, control.type
  )]

rm(control_dt.ftu)

## combine the three matched groups
matched_control_data <- rbind(matched_control_data.nt,
                              matched_control_data.ft,
                              matched_control_data.ftu)

rm(zillow_original, matched_control_data.nt,
   matched_control_data.ft, matched_control_data.ftu)
gc()

setnames(matched_control_data,
         old = c("control.homeprice"),
         new = c("normalized.homeprice"))
matched_control_data[, tr_group := paste0(control.type, " (", tolower(tr_group), ")")]

zillow_dt <- rbind(zillow_dt, matched_control_data, fill = T)


## normalize price indices based on time to event ------------------------------
# zillow_dt[, normalized.homeprice := normalized.homeprice - normalized.homeprice[tt_event == -6],
#            by = .(ref_year, ref_month, tr_group, event_ID)]
#
# # drops groups for which tt_event == -6 not available
# zillow_dt <- zillow_dt[!is.na(normalized.homeprice)]
# # note that this is still log-normalized

## aggregate by treatment group ------------------------------------------------

zillow_es_collapsed <- zillow_dt[, list(
  mean_homeprice = weighted.mean(normalized.homeprice, w = base.sales),
  n_counties = uniqueN(1000 * fips_state + fips_county)
  ), by = c("tr_group", "tt_event")]

fwrite(zillow_es_collapsed, output.filepath)
