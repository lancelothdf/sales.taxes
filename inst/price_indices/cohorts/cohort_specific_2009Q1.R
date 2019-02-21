#' Author: John Bonney
#'
#' This file groups tax-change cohorts by quarters, and focuses on 2009 Q1 cohort.

library(data.table)
library(readstata13)
library(sales.taxes)
library(zoo)
library(ggplot2)

setwd("/project2/igaarder")

## useful filepaths ------------------------------------------------------------
eventstudy_tr_path <- "Data/event_study_tr_groups_comprehensive.csv"
tr_groups_path <- "Data/tr_groups_comprehensive.csv"
sales_data_path <- "Data/sales_quarterly_2006-2016.csv"
tax_rates_path <- "Data/county_monthly_tax_rates.csv"
quarterly_tax_path <- "Data/quarterly_tax_rates.csv"
module_exemptions_path <- "Data/modules_exemptions_long.csv"
all_goods_pi_path <- "Data/Nielsen/price_quantity_indices_allitems.csv"
taxable_pi_path <- "Data/Nielsen/price_quantity_indices_taxableitems.csv"

## load in and clean data ------------------------------------------------------
all_pi <- fread(all_goods_pi_path)

## normalize
all_pi[, normalized.cpricei := log(cpricei) - log(cpricei[year == 2008 & quarter == 1]),
           by = .(store_code_uc, product_module_code)]
all_pi[, base.sales := sales[year == 2008 & quarter == 1],
           by = .(store_code_uc, product_module_code)]

## balance on store-module level
keep_store_modules <- all_pi[, list(n = .N),
                                 by = .(store_code_uc, product_module_code)]
keep_store_modules <- keep_store_modules[n == (2014 - 2008) * 4]

setkey(all_pi, store_code_uc, product_module_code)
setkey(keep_store_modules, store_code_uc, product_module_code)

all_pi <- all_pi[keep_store_modules]
setkey(all_pi, fips_county, fips_state)

## identify 2009 Q1 cohort -----------------------------------------------------
tr.events <- fread(eventstudy_tr_path)
tr.events <- tr.events[between(ref_year, 2009, 2014) & ref_month %in% 1:3 & tr_group == "Ever increase"]
tr.events[, n_events := .N, by = .(fips_state, fips_county)]
tr.events <- tr.events[, .(fips_county, fips_state, ref_year, n_events)]
setnames(tr.events, "ref_year", "treatment_year")
setkey(tr.events, fips_county, fips_state)

tr.events.09Q1 <- unique(tr.events[treatment_year == 2009], by = c("fips_state", "fips_county"))
setkey(tr.events.09Q1, fips_county, fips_state)

taxable_pi.09Q1 <- all_pi[sales_tax > 1]
taxable_pi.09Q1 <- taxable_pi.09Q1[tr.events.09Q1]

# at this point, we have the "treatment" group -- goods that are taxable in 2009 Q1 that experience
# a tax change in 2009 Q1

## prepare data for later cohorts ----------------------------------------------
constant.goods.set <- unique(taxable_pi.09Q1[year == 2009 & quarter == 1]$product_module_code)

all_pi <- all_pi[product_module_code %in% constant.goods.set] # keep goods constant

## identify never treated counties
tr.groups <- fread(tr_groups_path)
never.treated <- tr.groups[tr_group == "No change"]
never.treated <- never.treated[, .(fips_state, fips_county)]
setkey(never.treated, fips_state, fips_county)
never.treated <- all_pi[never.treated]
never.treated[, treatment_year := NA]
never.treated[, n_events := NA]

## combine never treated + later cohorts
all_pi <- all_pi[tr.events[treatment_year != 2009], allow = T]
all_pi <- rbind(all_pi, never.treated)

all_pi[, cohort := ifelse(is.na(treatment_year), "No change", as.character(treatment_year))]
all_pi[, event.weight := ifelse(is.na(n_events), 1, 1 / n_events)]

## collapse to product x cohort x time level -----------------------------------
all_pi.collapsed <- all_pi[, list(
  control.cpricei = weighted.mean(normalized.cpricei, w = base.sales * event.weight)
), by = .(year, quarter, cohort, product_module_code)]

## rearrange for simple merging all cohorts onto 2009 Q1 cohort
all_pi.collapsed <- tidyr::spread(all_pi.collapsed, cohort, control.cpricei)

## merge onto the 2009 cohort by product
taxable_pi.09Q1 <- merge(taxable_pi.09Q1, all_pi.collapsed,
                         by = c("year", "quarter", "product_module_code"))
taxable_pi.09Q1[, event.weight := 1 / n_events]

## aggregate over calendar time ------------------------------------------------

taxable_pi.09Q1.collapsed <- taxable_pi.09Q1[, list(
  mean.cpricei = weighted.mean(normalized.cpricei, w = base.sales * event.weight),
        `2010` = weighted.mean(`2010`, w = base.sales * event.weight),
        `2011` = weighted.mean(`2011`, w = base.sales * event.weight),
        `2012` = weighted.mean(`2012`, w = base.sales * event.weight),
        `2013` = weighted.mean(`2013`, w = base.sales * event.weight),
        `2014` = weighted.mean(`2014`, w = base.sales * event.weight),
   `No change` = weighted.mean(`No change`, w = base.sales * event.weight)
  ), by = .(year, quarter)]

setnames(taxable_pi.09Q1.collapsed, "mean.cpricei", "2009")
taxable_pi.09Q1.collapsed <- tidyr::gather(taxable_pi.09Q1.collapsed,
                                           key = cohort, value = cpricei,
                                           c(`mean.cpricei`, `2010`, `2011`,
                                             `2012`, `2013`, `2014`, `No change`))

## attach cohort sizes
n_counties.nochange <- uniqueN(tr.groups[tr_group == "No change"], by = c("fips_state", "fips_county"))
n_counties.other <- unique(tr.events[, .(fips_state, fips_county, n_events)],
                           by = c("fips_state", "fips_county", "n_events"))
n_counties.other[, ones := 1]
n_counties.other <- n_counties.other[, list(n_counties = sum(ones / n_events)), by = treatment_year]
n_counties.other[, treatment_year := as.character(treatment_year)]
n_counties <- rbind(n_counties.other,
                    data.table(treatment_year = "No change", n_counties = n_counties.nochange))
setnames(n_counties, "treatment_year", "cohort")
taxable_pi.09Q1.collapsed <- merge(taxable_pi.09Q1.collapsed, n_counties,
                                   by = "cohort")

fwrite(all_pi.collapsed, "Data/pi_2009Q1_cohorts.csv")

