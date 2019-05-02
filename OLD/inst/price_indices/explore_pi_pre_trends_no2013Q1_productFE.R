#' Author: John Bonney
#'
#' This file creates event-study plots based on the tax-inclusive price indices.
#' It is an adaptation of explore_pi_pre_trends.R that removes reforms in
#' 2013 Q1.
#'

library(data.table)
library(readstata13)
library(sales.taxes)
library(zoo)
library(ggplot2)

setwd("/project2/igaarder")

create.new.tr_groups <- F

## useful filepaths ------------------------------------------------------------
eventstudy_tr_path <- "Data/event_study_tr_groups_comprehensive_no2013q1.csv"
tr_groups_path <- "Data/tr_groups_comprehensive_no2013q1.csv"
sales_data_path <- "Data/sales_quarterly_2006-2016.csv"
tax_rates_path <- "Data/county_monthly_tax_rates.csv"
module_exemptions_path <- "Data/modules_exemptions_long.csv"
all_goods_pi_path <- "Data/Nielsen/price_quantity_indices_allitems.csv"
taxable_pi_path <- "Data/Nielsen/price_quantity_indices_taxableitems.csv"

output.all.event.path <- "Data/pi_allgoods_es_no2013Q1_productFE.csv"
output.taxable.event.path <-"Data/pi_taxable_es_no2013Q1_productFE.csv"
output.taxexempt.event.path <-"Data/pi_taxexempt_es_no2013Q1_productFE.csv"

original.eventstudy_tr_path <- "Data/event_study_tr_groups_comprehensive.csv"
original.tr_groups_path <- "Data/tr_groups_comprehensive.csv"

################################################################################
############################# Remove 2013 Q1 reforms ###########################
################################################################################

#' Note: what this is doing is taking the original treatment events/groups and
#'   eliminating 2013 Q1 events. It does not change the treatment status assigned
#'   to the remaining counties (i.e., the control group remains the same, and
#'   counties that experienced a decrease in 2013 Q1 and an increase at another
#'   time period will still not be included in the "increase only" group).

if (create.new.tr_groups) {
  original.events <- fread(original.eventstudy_tr_path)
  original.groups <- fread(original.tr_groups_path)

  new.events <- original.events[ref_year != 2013 | ref_month %in% 4:12]
  control.groups <- original.groups[tr_group == "No change"]
  treated.groups <- unique(new.events[, .(fips_state, fips_county, tr_group)])
  new.groups <- rbind(control.groups[, .(fips_county, fips_state, tr_group)],
                      treated.groups)

  fwrite(new.events, eventstudy_tr_path)
  fwrite(new.groups, tr_groups_path)
}

################################################################################
################ Plots by Event Time (taxable and all goods) ###################
################################################################################

# All goods ====================================================================
all_pi <- fread(all_goods_pi_path)
all_pi <- all_pi[year %in% 2008:2014 & !is.na(cpricei)]
all_pi[, cpricei := log(cpricei)]
all_pi[, sales_tax := log(sales_tax)]

## get sales weights -----------------------------------------------------------
all_pi[, base.sales := sales[year == 2008 & quarter == 1],
       by = .(store_code_uc, product_module_code)]

all_pi[, sales := NULL]
all_pi <- all_pi[!is.na(base.sales)]

## balance panel ---------------------------------------------------------------
all_pi <- balance_panel_data(all_pi, time_vars = c("quarter", "year"),
                             panel_unit = "store_code_uc", n_periods = 28)

## residualize product x calendar time FE (demeaning) --------------------------
#' Demeaning on product x calendar time level should be the same as residualizing
#' out product x calendar time fixed effects.
all_pi[, cpricei := cpricei - weighted.mean(cpricei, w = base.sales),
       by = .(product_module_code, year, quarter)]

all_pi_original <- copy(all_pi)

## merge treatment, attach event times -----------------------------------------
all_pi <- merge_treatment(original_data = all_pi,
                          treatment_data_path = eventstudy_tr_path,
                          merge_by = c("fips_county", "fips_state"))

setnames(all_pi, "V1", "event_ID")

## define time to event --------------------------------------------------------
all_pi[, ref_quarter := ceiling(ref_month / 3)]
all_pi[, tt_event := as.integer(4 * year + quarter -
                                  (4 * ref_year + ref_quarter))]

## limit data to two year window around reform ---------------------------------
all_pi <- all_pi[tt_event >= -4 & tt_event <= 4]

## add pseudo-control group ----------------------------------------------------

### create unique dataset of never treated counties
control_counties <- fread(tr_groups_path)
control_counties <- control_counties[tr_group == "No change"]
control_counties <- unique(control_counties[, .(fips_county, fips_state)])
control_dt <- merge(all_pi_original, control_counties,
                    by = c("fips_state", "fips_county"))

rm(all_pi_original)
gc()

### take the mean for each time period for each product module code
control_dt <- control_dt[,
                         list(control.cpricei = weighted.mean(cpricei, w = base.sales),
                              control.sales_tax = weighted.mean(sales_tax, w = base.sales)),
                         by = .(quarter, year, product_module_code)
                         ]

matched_control_data <- merge(all_pi, control_dt, by = c("quarter", "year", "product_module_code"))
matched_control_data <- matched_control_data[, .(control.cpricei, tt_event, event_ID,
                                                 store_code_uc, product_module_code,
                                                 tr_group, base.sales, ref_year,
                                                 ref_quarter, control.sales_tax)]

setnames(matched_control_data,
         old = c("control.cpricei", "control.sales_tax"),
         new = c("cpricei", "sales_tax"))
matched_control_data[, tr_group := paste0("No change (", tolower(tr_group), ")")]
all_pi <- rbind(all_pi, matched_control_data, fill = T)

## normalize price indices based on time to event ------------------------------
all_pi[, normalized.cpricei := cpricei - cpricei[tt_event == -2],
       by = .(store_code_uc, product_module_code, ref_year, ref_quarter,
              tr_group, event_ID)]
all_pi <- all_pi[!is.na(normalized.cpricei)] # drops groups for which tt_event == -2
# not available

# note that this is the log difference (log was calculated earlier)

## aggregate by treatment group ------------------------------------------------
all_pi_es_collapsed <- all_pi[,
                              list(mean_pi = weighted.mean(normalized.cpricei, w = base.sales),
                                   mean_tax = weighted.mean(sales_tax, w = base.sales),
                                   n_counties = uniqueN(1000 * fips_state + fips_county),
                                   n_stores = uniqueN(store_code_uc)),
                              by = c("tr_group", "tt_event")
                              ]

all_pi_es_collapsed <- add_tr_count(collapsed_data = all_pi_es_collapsed,
                                    tr_group_name = "tr_group",
                                    count_col_name = "n_counties")

fwrite(all_pi_es_collapsed, output.all.event.path)

rm(all_pi)
gc()

# Taxable goods only ===========================================================
taxable_pi <- fread(taxable_pi_path)
taxable_pi <- taxable_pi[year %in% 2008:2014 & !is.na(cpricei)]
taxable_pi[, cpricei := log(cpricei)]
taxable_pi[, sales_tax := log(sales_tax)]

## get sales weights -----------------------------------------------------------
taxable_pi[, base.sales := sales[year == 2008 & quarter == 1],
           by = .(store_code_uc, product_module_code)]

taxable_pi[, sales := NULL]
taxable_pi <- taxable_pi[!is.na(base.sales) & !is.na(cpricei)]

## balance sample on store-level from 2008 to 2014 -----------------------------
taxable_pi <- balance_panel_data(taxable_pi, time_vars = c("quarter", "year"),
                                 panel_unit = "store_code_uc", n_periods = 28)

## residualize product x calendar time FE (demeaning) --------------------------
taxable_pi[, cpricei := cpricei - weighted.mean(cpricei, w = base.sales),
       by = .(product_module_code, year, quarter)]

taxable_pi_original <- copy(taxable_pi)

## merge treatment, attach event times -----------------------------------------
taxable_pi <- merge_treatment(original_data = taxable_pi,
                              treatment_data_path = eventstudy_tr_path,
                              merge_by = c("fips_county", "fips_state"))

setnames(taxable_pi, "V1", "event_ID")

## define time to event --------------------------------------------------------
taxable_pi[, ref_quarter := ceiling(ref_month / 3)]
taxable_pi[, tt_event := as.integer(4 * year + quarter -
                                      (4 * ref_year + ref_quarter))]

## limit data to two year window around reform ---------------------------------
taxable_pi <- taxable_pi[tt_event >= -4 & tt_event <= 4]

## add pseudo-control group ----------------------------------------------------

### create unique dataset of never treated counties
control_counties <- fread(tr_groups_path)
control_counties <- control_counties[tr_group == "No change"]
control_counties <- unique(control_counties[, .(fips_county, fips_state)])
control_dt <- merge(taxable_pi_original, control_counties,
                    by = c("fips_state", "fips_county"))

rm(taxable_pi_original)
gc()

### take the mean for each time period for each product module code
control_dt <- control_dt[,
                         list(control.cpricei = weighted.mean(x = cpricei, w = base.sales),
                              control.sales_tax = weighted.mean(sales_tax, w = base.sales)),
                         by = .(quarter, year, product_module_code)
                         ]

matched_control_data <- merge(taxable_pi, control_dt, by = c("quarter", "year", "product_module_code"))
matched_control_data <- matched_control_data[, .(control.cpricei, tt_event, event_ID,
                                                 store_code_uc, product_module_code, control.sales_tax,
                                                 tr_group, base.sales, ref_year, ref_quarter)]

setnames(matched_control_data,
         old = c("control.cpricei", "control.sales_tax"),
         new = c("cpricei",         "sales_tax"))
matched_control_data[, tr_group := paste0("No change (", tolower(tr_group), ")")]

taxable_pi <- rbind(taxable_pi, matched_control_data, fill = T)

## normalize price indices based on time to event ------------------------------
taxable_pi[, normalized.cpricei := cpricei - cpricei[tt_event == - 2],
           by = .(store_code_uc, product_module_code, ref_year, ref_quarter,
                  tr_group, event_ID)]
# drops groups for which tt_event == -2 not available
taxable_pi <- taxable_pi[!is.na(normalized.cpricei)]
# note that this is still log cpricei

## aggregate by treatment group ------------------------------------------------
taxable_pi_es_collapsed <- taxable_pi[,
                                      list(mean_pi = weighted.mean(x = normalized.cpricei, w = base.sales),
                                           mean_tax = weighted.mean(sales_tax, w = base.sales),
                                           n_counties = uniqueN(1000 * fips_state + fips_county),
                                           n_stores = uniqueN(store_code_uc)),
                                      by = c("tr_group", "tt_event")
                                      ]

taxable_pi_es_collapsed <- add_tr_count(collapsed_data = taxable_pi_es_collapsed,
                                        tr_group_name = "tr_group",
                                        count_col_name = "n_counties")

fwrite(taxable_pi_es_collapsed, output.taxable.event.path)

rm(taxable_pi)
gc()

# Tax exempt goods =============================================================

## get data.table of tax exempt goods ------------------------------------------
all_pi <- fread(all_goods_pi_path)
taxable_pi <- fread(taxable_pi_path)

taxable_pi <- taxable_pi[, .(store_code_uc, product_module_code, quarter, year)]
taxable_pi[, flag := 1]

all_pi <- merge(all_pi, taxable_pi, all.x = T)
all_pi[is.na(flag), flag := 0]

taxexempt_pi <- all_pi[flag != 1]

rm(all_pi, taxable_pi)
gc()

taxexempt_pi <- taxexempt_pi[year %in% 2008:2014 & !is.na(cpricei)]

## get sales weights -----------------------------------------------------------
taxexempt_pi[, base.sales := sales[year == 2008 & quarter == 1],
             by = .(store_code_uc, product_module_code)]
taxexempt_pi[, sales := NULL]
taxexempt_pi[, cpricei := log(cpricei)]
taxexempt_pi[, sales_tax := log(sales_tax)]
taxexempt_pi <- taxexempt_pi[!is.na(base.sales) & !is.na(cpricei)]

## balance sample on store-level from 2008 to 2014 -----------------------------
taxexempt_pi <- balance_panel_data(taxexempt_pi, time_vars = c("quarter", "year"),
                                   panel_unit = "store_code_uc", n_periods = 28)

## residualize product x calendar time FE (demeaning) --------------------------
taxexempt_pi[, cpricei := cpricei - weighted.mean(cpricei, w = base.sales),
           by = .(product_module_code, year, quarter)]

taxexempt_pi_original <- copy(taxexempt_pi)

## merge treatment, attach event times -----------------------------------------
taxexempt_pi <- merge_treatment(original_data = taxexempt_pi,
                                treatment_data_path = eventstudy_tr_path,
                                merge_by = c("fips_county", "fips_state"))

setnames(taxexempt_pi, "V1", "event_ID")

## define time to event --------------------------------------------------------
taxexempt_pi[, ref_quarter := ceiling(ref_month / 3)]
taxexempt_pi[, tt_event := as.integer(4 * year + quarter -
                                        (4 * ref_year + ref_quarter))]

## limit data to two year window around reform ---------------------------------
taxexempt_pi <- taxexempt_pi[tt_event >= -4 & tt_event <= 4]

## add pseudo-control group ----------------------------------------------------

### create unique dataset of never treated counties
control_counties <- fread(tr_groups_path)
control_counties <- control_counties[tr_group == "No change"]
control_counties <- unique(control_counties[, .(fips_county, fips_state)])
control_dt <- merge(taxexempt_pi_original, control_counties,
                    by = c("fips_state", "fips_county"))

rm(taxexempt_pi_original)
gc()

### take the mean for each time period for each product module code
control_dt <- control_dt[,
                         list(control.cpricei = weighted.mean(x = cpricei, w = base.sales),
                              control.sales_tax = weighted.mean(sales_tax, w = base.sales)),
                         by = .(quarter, year, product_module_code)
                         ]

matched_control_data <- merge(taxexempt_pi, control_dt, by = c("quarter", "year", "product_module_code"))
matched_control_data <- matched_control_data[, .(control.cpricei, tt_event, event_ID,
                                                 store_code_uc, product_module_code, control.sales_tax,
                                                 tr_group, base.sales, ref_year, ref_quarter)]
setnames(matched_control_data,
         old = c("control.cpricei", "control.sales_tax"),
         new = c("cpricei",         "sales_tax"))
matched_control_data[, tr_group := paste0("No change (", tolower(tr_group), ")")]
taxexempt_pi <- rbind(taxexempt_pi, matched_control_data, fill = T)

## normalize price indices based on time to event ------------------------------
taxexempt_pi[, normalized.cpricei := cpricei - cpricei[tt_event == -2],
             by = .(store_code_uc, product_module_code, ref_year, ref_quarter,
                    tr_group, event_ID)]
# drops groups for which tt_event == -2 not available
taxexempt_pi <- taxexempt_pi[!is.na(normalized.cpricei)]
# note that this is still log cpricei

## aggregate by treatment group ------------------------------------------------
taxexempt_pi_es_collapsed <- taxexempt_pi[,
                                          list(mean_pi = weighted.mean(x = normalized.cpricei, w = base.sales),
                                               mean_tax = weighted.mean(sales_tax, w = base.sales),
                                               n_counties = uniqueN(1000 * fips_state + fips_county),
                                               n_stores = uniqueN(store_code_uc)),
                                          by = c("tr_group", "tt_event")
                                          ]

taxexempt_pi_es_collapsed <- add_tr_count(collapsed_data = taxexempt_pi_es_collapsed,
                                          tr_group_name = "tr_group",
                                          count_col_name = "n_counties")

fwrite(taxexempt_pi_es_collapsed, output.taxexempt.event.path)

rm(taxexempt_pi)
gc()
