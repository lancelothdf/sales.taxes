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

setwd("/project2/igaarder")
change_of_interest <- "Increase only"
output.filepath <- "Data/unemprate_all_cohorts_io_pooled.csv"

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

unemp_path <- "Data/covariates/county_monthly_unemp_clean.csv"

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

## prep the unemp data --------------------------------------------------------
unemp_dt <- fread(unemp_path)
unemp_dt <- unemp_dt[between(year, 2006, 2014)]

### attach sales
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

unemp_dt <- merge(unemp_dt, taxable_pi, by = c("fips_county", "fips_state"))
rm(taxable_pi)
gc()

## normalize
unemp_dt[, normalized.rate := rate] # just for consistency in notation -- we don't actually normalized unemp
unemp_dt <- unemp_dt[!is.na(normalized.rate) & !is.na(base.sales)]

## balance on county level
keep_counties <- unemp_dt[, list(n = .N), by = .(fips_state, fips_county)]
keep_counties <- keep_counties[n == (2014 - 2005) * 12]

setkey(unemp_dt, fips_county, fips_state)
setkey(keep_counties, fips_county, fips_state)

unemp_dt <- unemp_dt[keep_counties]

# Aggregate --------------------------------------------------------------------

unemp_original <- copy(unemp_dt)

## merge treatment, attach event times -----------------------------------------
unemp_dt <- merge(unemp_dt, tr_events,
                   by = c("fips_county", "fips_state"),
                   allow.cartesian = TRUE)

setnames(unemp_dt, "V1", "event_ID")

## define time to event --------------------------------------------------------
unemp_dt[, tt_event := as.integer(12 * year + month - (12 * ref_year + ref_month))]

## limit data to three year window around reform ---------------------------------
unemp_dt <- unemp_dt[tt_event >= -24 & tt_event <= 12]

## add pseudo-control group ----------------------------------------------------

### create dataset of never treated counties
control_dt.nt <- merge(unemp_original, control_counties.nt,
                       by = c("fips_state", "fips_county"))

control_dt.nt <- control_dt.nt[, list(
  control.rate = weighted.mean(normalized.rate, w = base.sales)
), by = .(month, year)]

control_dt.nt[, control.type := "No change"]

matched_control_data.nt <- merge(unemp_dt, control_dt.nt,
                                 by = c("month", "year"),
                                 allow.cartesian = T)

matched_control_data.nt <- matched_control_data.nt[, .(
  control.rate, tt_event, event_ID, tr_group,
  base.sales, ref_year, ref_month, control.type
)]

rm(control_dt.nt)

### create dataset of future treated counties (restricted)
control_dt.ft <- merge(unemp_original, control_counties.ft,
                       by = c("fips_state", "fips_county"),
                       allow.cartesian = T)

control_dt.ft <- control_dt.ft[, list(
  control.rate = weighted.mean(normalized.rate, w = base.sales)
), by = .(month, year, ref_year, ref_month)]

control_dt.ft[, control.type := "Future change"]

matched_control_data.ft <- merge(unemp_dt, control_dt.ft,
                                 by = c("month", "year", "ref_year", "ref_month"))

matched_control_data.ft <- matched_control_data.ft[, .(
  control.rate, tt_event, event_ID, tr_group,
  base.sales, ref_year, ref_month, control.type
)]
rm(control_dt.ft)

### created dataset of future treated counties (unrestricted)
control_dt.ftu <- merge(unemp_original, control_counties.ftu,
                        by = c("fips_state", "fips_county", "year", "month"),
                        allow.cartesian = T)

control_dt.ftu <- control_dt.ftu[, list(
  control.rate = weighted.mean(normalized.rate, w = base.sales)
), by = .(month, year, ref_year, ref_month)]
control_dt.ftu[, control.type := "Future change, unrestricted"]

matched_control_data.ftu <- merge(unemp_dt, control_dt.ftu,
                                  by = c("month", "year", "ref_year", "ref_month"))

matched_control_data.ftu <- matched_control_data.ftu[, .(
  control.rate, tt_event, event_ID, tr_group,
  base.sales, ref_year, ref_month, control.type
)]

rm(control_dt.ftu)

## combine the three matched groups
matched_control_data <- rbind(matched_control_data.nt,
                              matched_control_data.ft,
                              matched_control_data.ftu)

rm(unemp_original, matched_control_data.nt,
   matched_control_data.ft, matched_control_data.ftu)
gc()

setnames(matched_control_data,
         old = c("control.rate"),
         new = c("normalized.rate"))
matched_control_data[, tr_group := paste0(control.type, " (", tolower(tr_group), ")")]

unemp_dt <- rbind(unemp_dt, matched_control_data, fill = T)


## normalize price indices based on time to event ------------------------------
# unemp_dt[, normalized.rate := normalized.rate - normalized.rate[tt_event == -6],
#            by = .(ref_year, ref_month, tr_group, event_ID)]
#
# # drops groups for which tt_event == -6 not available
# unemp_dt <- unemp_dt[!is.na(normalized.rate)]
# # note that this is still log-normalized

## aggregate by treatment group ------------------------------------------------

unemp_es_collapsed <- unemp_dt[, list(
  mean_rate = weighted.mean(normalized.rate, w = base.sales),
  n_counties = uniqueN(1000 * fips_state + fips_county)
), by = c("tr_group", "tt_event")]

fwrite(unemp_es_collapsed, output.filepath)

