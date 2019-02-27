#' Author: John Bonney
#'
#' This file groups tax-change cohorts by years, and focuses on 2009 cohort.

library(tidyverse)
library(data.table)
library(readstata13)
library(sales.taxes)
library(zoo)

setwd("/project2/igaarder")

## useful filepaths ------------------------------------------------------------
eventstudy_tr_path <- "Data/event_study_tr_groups_comprehensive_w2014.csv"
tr_groups_path <- "Data/tr_groups_comprehensive_w2014.csv"
sales_data_path <- "Data/sales_quarterly_2006-2016.csv"
tax_rates_path <- "Data/county_monthly_tax_rates.csv"
quarterly_tax_path <- "Data/quarterly_tax_rates.csv"
module_exemptions_path <- "Data/modules_exemptions_long.csv"
all_goods_pi_path <- "Data/Nielsen/price_quantity_indices_allitems.csv"
taxable_pi_path <- "Data/Nielsen/price_quantity_indices_taxableitems.csv"
expanded.reforms.path <- "Data/tax_reforms_all_incl2014.csv"

## load in and clean data ------------------------------------------------------

# temp function
gi <- function(dt) {
  print(head(dt))
}
all_pi <- fread(all_goods_pi_path)
all_pi <- all_pi[between(year, 2008, 2014)]
gi(all_pi)

## normalize
all_pi[, normalized.cpricei := log(cpricei) - log(cpricei[year == 2008 & quarter == 1]),
       by = .(store_code_uc, product_module_code)]
all_pi[, base.sales := sales[year == 2008 & quarter == 1],
       by = .(store_code_uc, product_module_code)]
all_pi <- all_pi[!is.na(normalized.cpricei) & !is.na(base.sales)]
gi(all_pi)

## balance on store-module level
keep_store_modules <- all_pi[, list(n = .N),
                             by = .(store_code_uc, product_module_code)]
keep_store_modules <- keep_store_modules[n == (2014 - 2007) * 4]
gi(keep_store_modules)

setkey(all_pi, store_code_uc, product_module_code)
setkey(keep_store_modules, store_code_uc, product_module_code)

all_pi <- all_pi[keep_store_modules]
setkey(all_pi, fips_county, fips_state)
gi(all_pi)


## identify 2009 cohort --------------------------------------------------------
tr.events <- fread(eventstudy_tr_path)
tr.events <- tr.events[between(ref_year, 2009, 2014) & tr_group == "Ever increase"]
tr.events[, n_events := .N, by = .(fips_state, fips_county)]
tr.events[, cohort_size := sum(1 / n_events), by = .(ref_year)]
tr.events <- tr.events[, .(fips_county, fips_state, ref_year, n_events, cohort_size)]
gi(tr.events)

setnames(tr.events, "ref_year", "treatment_year")
setkey(tr.events, fips_county, fips_state)

tr.events.09 <- unique(tr.events[treatment_year == 2009], by = c("fips_state", "fips_county"))
gi(tr.events.09)
tr.events.09[, cohort_size := NULL]
setkey(tr.events.09, fips_county, fips_state)

taxable_pi.09 <- all_pi[sales_tax > 1]
taxable_pi.09 <- taxable_pi.09[tr.events.09]
gi(taxable_pi.09)

# at this point, we have the "treatment" group -- goods that are taxable in 2009 Q1 that experience
# a tax change in 2009 Q1

## prepare data for later cohorts ----------------------------------------------
constant.goods.set <- unique(taxable_pi.09[year == 2009]$product_module_code)
gi(constant.goods.set)

all_pi <- all_pi[product_module_code %in% constant.goods.set] # keep goods constant
gi(all_pi)

## identify never treated counties
tr.groups <- fread(tr_groups_path)
never.treated <- tr.groups[tr_group == "No change"]
never.treated <- never.treated[, .(fips_state, fips_county)]
setkey(never.treated, fips_state, fips_county)
never.treated <- all_pi[never.treated]
never.treated[, treatment_year := NA]
never.treated[, n_events := NA]
gi(never.treated)

## combine never treated + later cohorts
all_pi <- all_pi[tr.events[treatment_year != 2009], allow = T]
all_pi <- rbind(all_pi, never.treated, fill = T)
gi(all_pi)

all_pi[, cohort := ifelse(is.na(treatment_year), "No change", as.character(treatment_year))]
all_pi[, event.weight := ifelse(is.na(n_events), 1, 1 / n_events)]

## collapse to product x cohort x time level -----------------------------------
all_pi.collapsed <- all_pi[, list(
  control.cpricei = weighted.mean(normalized.cpricei, w = base.sales * event.weight)
), by = .(year, quarter, cohort, product_module_code)]
gi(all_pi.collapsed)

## rearrange for simple merging all cohorts onto 2009 cohort
all_pi.collapsed <- tidyr::spread(all_pi.collapsed, cohort, control.cpricei)
gi(all_pi.collapsed)

## merge onto the 2009 cohort by product
taxable_pi.09 <- merge(taxable_pi.09, all_pi.collapsed,
                         by = c("year", "quarter", "product_module_code"))
taxable_pi.09[, event.weight := 1 / n_events]
gi(taxable_pi.09)

## aggregate over calendar time ------------------------------------------------

taxable_pi.09.collapsed <- taxable_pi.09[, list(
  mean.cpricei = weighted.mean(normalized.cpricei, w = base.sales * event.weight),
  `2010` = weighted.mean(`2010`, w = base.sales * event.weight),
  `2011` = weighted.mean(`2011`, w = base.sales * event.weight),
  `2012` = weighted.mean(`2012`, w = base.sales * event.weight),
  `2013` = weighted.mean(`2013`, w = base.sales * event.weight),
  `2014` = weighted.mean(`2014`, w = base.sales * event.weight),
  `No change` = weighted.mean(`No change`, w = base.sales * event.weight)
), by = .(year, quarter)]

gi(taxable_pi.09.collapsed)

setnames(taxable_pi.09.collapsed, "mean.cpricei", "2009")
taxable_pi.09.collapsed <- tidyr::gather(taxable_pi.09.collapsed,
                                           key = cohort, value = cpricei,
                                           c(`2009`, `2010`, `2011`,
                                             `2012`, `2013`, `2014`, `No change`))
gi(taxable_pi.09.collapsed)

## attach cohort sizes
n_counties.nochange <- uniqueN(tr.groups[tr_group == "No change"], by = c("fips_state", "fips_county"))
n_counties.other <- unique(tr.events[, .(treatment_year, cohort_size)],
                           by = c("treatment_year", "cohort_size"))
n_counties.other[, treatment_year := as.character(treatment_year)]
n_counties <- rbind(n_counties.other,
                    data.table(treatment_year = "No change", cohort_size = n_counties.nochange))
setnames(n_counties, "treatment_year", "cohort")
taxable_pi.09.collapsed <- merge(taxable_pi.09.collapsed, n_counties,
                                   by = "cohort")

fwrite(taxable_pi.09.collapsed, "Data/pi_2009_cohorts.csv")

