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
change_of_interest <- "Ever increase"
output.filepath <- "pi_all_cohorts_pooled_taxable_pm.csv"

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

## future restricted control group
control_counties.ft <- data.table(NULL)
for (yr in 2009:2013) {
  for (qtr in 1:4) {
    control_counties_spec <- control_counties[
      (ref_year * 4 + ref_quarter) - (yr * 4 + qtr) > 4, # keep if treated over 1 year in future
      .(fips_state, fips_county, tr_group) # columns to keep
      ]
    control_counties_spec[, ref_year := yr]
    control_counties_spec[, ref_quarter := qtr]

    control_counties.ft <- rbind(control_counties.ft, control_counties_spec)
  }
}

## future unrestricted control group
control_counties.ftu <- data.table(NULL)
for (ref.yr in 2009:2013) {
  for (ref.qtr in 1:4) {
    for (cal.yr in 2006:2014) {
      for (cal.qtr in 1:4) {
        control_counties_spec <- control_counties[
          (ref_year * 4 + ref_quarter) > (ref.yr * 4 + ref.qtr) &   # treated after focal cohort
            (ref_year * 4 + ref_quarter) > (cal.yr * 4 + cal.qtr),  # not yet treated
          .(fips_state, fips_county, tr_group) # columns to keep
          ]

        control_counties_spec[, ref_year := ref.yr]
        control_counties_spec[, ref_quarter := ref.qtr]
        control_counties_spec[, year := cal.yr]
        control_counties_spec[, quarter := cal.qtr]

        control_counties.ftu <- rbind(control_counties.ftu, control_counties_spec)
      }
    }
  }
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

