#' Author: John Bonney
#'
#' This file creates event-study plots based on the tax-inclusive price indices.
#'
#' TODO:
#'  * Add pseudo-control group
#'  * Do same thing using Brad and David's package?

# devtools::install_github("setzler/eventStudy/eventStudy")

# library(eventStudy)
library(data.table)
library(readstata13)
library(sales.taxes)
library(zoo)
library(ggplot2)

setwd("/project2/igaarder")
prep_enviro <- T

################################################################################
###### Prepare environment & create datasets of all goods and taxable only #####
################################################################################

## useful filepaths ------------------------------------------------------------
eventstudy_tr_path <- "Data/event_study_tr_groups_comprehensive.csv"
tr_groups_path <- "Data/tr_groups_comprehensive.csv"
sales_data_path <- "Data/sales_quarterly_2006-2016.csv"
tax_rates_path <- "Data/county_monthly_tax_rates.csv"
module_exemptions_path <- "/project2/igaarder/Data/modules_exemptions_long.csv"
all_goods_pi_path <- "Data/Nielsen/price_quantity_indices_allitems.csv"
taxable_pi_path <- "Data/Nielsen/price_quantity_indices_taxableitems.csv"

if (prep_enviro){
  ## create .csv's of taxable and all goods ------------------------------------
  # nonfood_pi <- read.dta13("Data/Nielsen/Price_quantity_indices_nonfood.dta")
  # nonfood_pi <- as.data.table(nonfood_pi)
  # fwrite(nonfood_pi, "Data/Nielsen/price_quantity_indices_nonfood.csv")
  #
  # food_pi <- fread("Data/Nielsen/price_quantity_indices_food.csv")
  # food_pi[, c("fips_state", "fips_county") := NULL]
  #
  # all_pi <- rbind(food_pi, nonfood_pi)
  # rm(nonfood_pi, food_pi)
  # gc()
  #
  # ### attach county and state FIPS codes as well as sales ----------------------
  # sales_data <- fread(sales_data_path)
  # sales_data <- sales_data[, .(store_code_uc, product_module_code, fips_county,
  #                              fips_state, quarter, year, sales)]
  #
  # all_pi <- merge(all_pi, sales_data, by = c("store_code_uc", "quarter", "year",
  #                                            "product_module_code" ))
  # fwrite(all_pi, all_goods_pi_path)
  #
  # rm(sales_data)
  # gc()

  ### create taxable only dataset ----------------------------------------------
  all_pi <- fread(all_goods_pi_path)
  all_pi <- all_goods_pi_path[year %in% 2008:2014]

  county_monthly_tax <- fread(tax_rates_path)
  module_exemptions <- fread(module_exemptions_path)

  county_monthly_tax <- county_monthly_tax[, .(fips_state, fips_county, year,
                                               month, sales_tax)]
  setnames(county_monthly_tax, sales_tax, applicable_tax)
  ### some checks...
  length(unique(module_exemptions$product_module_code))
  length(unique(all_pi$product_module_code))
  ###

  applicable_tax <- merge(module_exemptions, county_monthly_tax,
                          by = c("fips_state", "year", "month"), all = T,
                          allow.cartesian = T)

  applicable_tax[is.na(taxable), applicable_tax := sales_tax.y]
  applicable_tax[taxable == 1 & is.na(sales_tax.x), applicable_tax := sales_tax.y]
  applicable_tax[taxable == 1 & !is.na(sales_tax.x), applicable_tax := sales_tax.x]
  applicable_tax[taxable == 0, applicable_tax := 0]
  applicable_tax[, quarter := ceiling(month / 3)]
  applicable_tax <- applicable_tax[, list(applicable_tax = max(applicable_tax)),
                                   by = .(fips_state, fips_county, year, quarter,
                                          product_module_code)]
  print(table(applicable_tax$applicable_tax, useNA = "always"))
  print(nrow(all_pi)) ## bug check
  print(nrow(applicable_tax)) # bug check
  taxable_pi <- merge(all_pi, applicable_tax,
                      by = c("fips_state", "fips_county", "year", "quarter",
                             "product_module_code"), all.x = T)
  print(nrow(taxable_pi)) ## bug check

  taxable_pi[, rm_missing := max(as.integer(is.na(applicable_tax))),
             by = c("fips_state", "fips_county", "product_module_code")]
  print(table(taxable_pi$rm_missing, useNA = "always"))
  taxable_pi[, rm_nontaxable := max(as.integer(applicable_tax == 0)),
             by = c("fips_state", "fips_county", "product_module_code")]
  print(table(taxable_pi$rm_nontaxable, useNA = "always"))
  taxable_pi <- taxable_pi[rm_nontaxable != 1 & rm_missing != 1]
  print(nrow(taxable_pi)) ## bug check
  taxable_pi <- taxable_pi[, .(store_code_uc, quarter, year, product_group_code,
                               product_module_code, pricei, quantityi, cpricei,
                               fips_state, fips_county, sales)]

  fwrite(taxable_pi, taxable_pi_path)

  rm(county_monthly_tax, module_exemptions, applicable_tax)
  gc()
} # else {
#   all_pi <- fread(all_goods_pi_path)
#   taxable_pi <- fread(taxable_pi_path)
# }


################################################################################
############## Plots by Calendar Time (taxable and all goods) ##################
################################################################################
#
# # All goods ====================================================================
#
# ## balance sample on store-level from 2008 to 2014 -----------------------------
# all_pi <- all_pi[year %in% 2008:2014 & !is.na(cpricei) & !is.na(sales)]
# all_pi <- balance_panel_data(all_pi, time_vars = c("quarter", "year"),
#                              panel_unit = "store_code_uc", n_periods = 28)
#
# ## normalize price index -------------------------------------------------------
# price_base <- all_pi[year == 2008 & quarter == 1]
# price_base <- price_base[, .(store_code_uc, product_module_code, cpricei)]
# price_base[, base.cpricei := cpricei]
# price_base[, cpricei := NULL]
#
# all_pi <- merge(all_pi, price_base, by = c("store_code_uc", "product_module_code"))
# all_pi[, normalized.cpricei := log(cpricei) - log(base.cpricei)]
# all_pi[, base.cpricei := NULL]
#
# rm(price_base)
# gc()
#
# ## merge treatment -------------------------------------------------------------
# all_pi <- merge_treatment(original_data = all_pi,
#                           treatment_data_path = tr_groups_path,
#                           time = "calendar",
#                           merge_by = c("fips_county", "fips_state"))
#
# ## aggregate across treatment groups -------------------------------------------
#
# all_pi_collapsed <- all_pi[, list(
#   mean.cpricei = weighted.mean(x = normalized.cpricei, w = sales),
#   n_counties = uniqueN(1000 * fips_state + fips_county)
#   ), by = c("tr_group", "year", "quarter")]
#
# all_pi_collapsed <- add_tr_count(collapsed_data = all_pi_collapsed,
#                                  tr_group_name = "tr_group",
#                                  count_col_name = "n_counties")
# fwrite(all_pi_collapsed, "Data/pi_all_calendar.csv")
#
# ## prepare plot-----------------------------------------------------------------
# all_pi_collapsed$year_qtr <- as.yearqtr(paste(
#   as.integer(all_pi_collapsed$year), as.integer(all_pi_collapsed$quarter)
#   ), "%Y %q")
#
# all.calendar.plot <- ggplot(data = all_pi_collapsed, mapping = aes(x = year_qtr,
#                                                            y = mean.cpricei,
#                                                            color = tr_count)) +
#   labs(x = "Quarter", y = "Mean normalized ln(index)", color = "Sales tax change",
#        caption = "Weighted by sales in 2008 Q1.") +
#   scale_x_yearqtr(format = "%Y Q%q") +
#   geom_line() +
#   theme_bw()
#
# ggsave("Graphs/pi_all_calendar.png")
#
# # Taxable goods ================================================================
#
# ## balance sample on store-level from 2008 to 2014 -----------------------------
# taxable_pi <- taxable_pi[year %in% 2008:2014 & !is.na(cpricei) & !is.na(sales)]
# taxable_pi <- balance_panel_data(taxable_pi, time_vars = c("quarter", "year"),
#                              panel_unit = "store_code_uc", n_periods = 28)
#
# ## normalize price index -------------------------------------------------------
# price_base <- taxable_pi[year == 2008 & quarter == 1]
# price_base <- price_base[, .(store_code_uc, product_module_code, cpricei)]
# price_base[, base.cpricei := cpricei]
# price_base[, cpricei := NULL]
#
# taxable_pi <- merge(taxable_pi, price_base, by = c("store_code_uc", "product_module_code"))
# taxable_pi[, normalized.cpricei := log(cpricei) - log(base.cpricei)]
# taxable_pi[, base.cpricei := NULL]
#
# rm(price_base)
# gc()
#
# ## merge treatment -------------------------------------------------------------
# taxable_pi <- merge_treatment(original_data = taxable_pi,
#                               treatment_data_path = tr_groups_path,
#                               time = "calendar",
#                               merge_by = c("fips_county", "fips_state"))
#
#
# ## aggregate across treatment groups -------------------------------------------
#
# taxable_pi_collapsed <- taxable_pi[, list(
#   mean.cpricei = weighted.mean(x = normalized.cpricei, w = sales),
#   n_counties = uniqueN(1000 * fips_state + fips_county)
# ), by = c("tr_group", "year", "quarter")]
#
# taxable_pi_collapsed <- add_tr_count(collapsed_data = taxable_pi_collapsed,
#                                      tr_group_name = "tr_group",
#                                      count_col_name = "n_counties")
# fwrite(taxable_pi_collapsed, "Data/taxable_pi_collapsed.csv")
#
# ## prepare plot-----------------------------------------------------------------
# taxable_pi_collapsed$year_qtr <- as.yearqtr(paste(
#   as.integer(taxable_pi_collapsed$year), as.integer(taxable_pi_collapsed$quarter)
# ), "%Y %q")
#
# taxable.calendar.plot <- ggplot(taxable_pi_collapsed, aes(x = year_qtr,
#                                                           y = mean.cpricei,
#                                                           color = tr_count)) +
#   labs(x = "Quarter", y = "Mean normalized ln(index)", color = "Sales tax change",
#        caption = "Weighted by sales in 2008 Q1.") +
#   scale_x_yearqtr(format = "%Y Q%q") +
#   geom_line() +
#   theme_bw()
#
# ggsave("Graphs/pi_taxable_calendar.png")
#
# rm(all_pi, taxable_pi)
# gc()

################################################################################
################ Plots by Event Time (taxable and all goods) ###################
################################################################################

# All goods ====================================================================
all_pi <- fread(all_goods_pi_path)

## balance sample on store-level from 2008 to 2014 -----------------------------
all_pi <- all_pi[year %in% 2008:2014 & !is.na(cpricei) & !is.na(sales)]
all_pi <- balance_panel_data(all_pi, time_vars = c("quarter", "year"),
                             panel_unit = "store_code_uc", n_periods = 28)
all_pi_original <- copy(all_pi)


## merge treatment, attach event times -----------------------------------------
print(head(all_pi))
all_pi <- merge_treatment(original_data = all_pi,
                          treatment_data_path = eventstudy_tr_path,
                          merge_by = c("fips_county", "fips_state"))
setnames(all_pi,"V1", "event_ID")
print(head(all_pi))

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
print(head(control_counties))

control_dt <- merge(all_pi_original, control_counties,
                    by = c("fips_state", "fips_county"))
print(head(control_dt))

rm(all_pi_original)
gc()

### take the mean for each time period
control_dt <- control_dt[,
                         list(control.cpricei = weighted.mean(x = cpricei, w = sales)),
                         by = .(quarter, year)
                         ]
print(head(control_dt))

all_pi <- merge(all_pi, control_dt, by = c("quarter", "year"))
print(head(all_pi))
matched_control_data <- all_pi[, .(control.cpricei, tt_event, tr_group, sales,
                                   ref_year, ref_quarter)]

matched_control_data[, cpricei := control.cpricei]
matched_control_data[, tr_group := paste0("No change (", tr_group, ")")]
matched_control_data[, control.cpricei := NULL]
all_pi <- rbind(all_pi, matched_control_data, fill = T)

## normalize price indices based on time to event ------------------------------
print(head(all_pi))
all_pi[, event_ID := .GRP, by = .(tr_group, event_ID)]
print(length(unique(all_pi$event_ID)))
print(nrow(all_pi))

price_anchors <- all_pi[tt_event == -2]
price_anchors[, base_price := cpricei]
price_anchors <- price_anchors[, .(store_code_uc, product_module_code, base_price,
                                   ref_year, ref_quarter, tr_group)]
print(nrow(unique(price_anchors)))
print(nrow(unique(price_anchors[, .(store_code_uc, product_module_code,
                                    ref_year, ref_quarter, tr_group)])))

print(head(price_anchors))
print(nrow(price_anchors))
print(nrow(unique(all_pi)))
print(nrow(unique(all_pi[, .(store_code_uc, product_module_code,
                             ref_year, ref_quarter, tr_group)])))

all_pi <- merge(all_pi, price_anchors,
                    by = c("store_code_uc", "product_module_code",
                           "ref_year", "ref_quarter", "tr_group"))

all_pi[, normalized.cpricei := log(cpricei) - log(base_price)]
all_pi[, base_price := NULL]

# all_pi <- normalize_price(price_data = all_pi,
#                           time_type = "event",
#                           base_time = -2,
#                           price_var = "cpricei",
#                           new_price_var = "normalized.cpricei")
setorder(all_pi, store_code_uc, product_module_code, tt_event)
print(head(all_pi))

## aggregate by treatment group ------------------------------------------------
print(head(all_pi))
all_pi_es_collapsed <- all_pi[,
  list(mean_pi = weighted.mean(x = normalized.cpricei, w = sales),
       n_counties = uniqueN(1000 * fips_state + fips_county),
       n_stores = uniqueN(store_code_uc)),
   by = c("tr_group", "tt_event")
  ]

fwrite(all_pi_es_collapsed, "Data/pi_allgoods_es.csv")
print(head(all_pi_es_collapsed))

all_pi_es_collapsed <- add_tr_count(collapsed_data = all_pi_es_collapsed,
                                    tr_group_name = "tr_group",
                                    count_col_name = "n_counties")

fwrite(all_pi_es_collapsed, "Data/pi_allgoods_es.csv")

## plot and export result ------------------------------------------------------
all_pi_es_plot <- ggplot(data = all_pi_es_collapsed,
                  mapping = aes(x = tt_event, y = mean_pi, color = tr_count)) +
  labs(x = "Time to event",
       y = "Log normalized price index",
       color = "Sales tax change",
       caption = expression(paste("Weighted by sales in 2008 Q1. ", Y==0, " in ", T==-2))) +
  geom_line() +
  theme_bw()

ggsave("Graphs/pi_allgoods_es.png")

rm(all_pi)
gc()

# Taxable goods only ===========================================================
taxable_pi <- fread(taxable_pi_path)

## balance sample on store-level from 2008 to 2014 -----------------------------
taxable_pi <- taxable_pi[year %in% 2008:2014 & !is.na(cpricei) & !is.na(sales)]
taxable_pi <- balance_panel_data(taxable_pi, time_vars = c("quarter", "year"),
                             panel_unit = "store_code_uc", n_periods = 28)
taxable_pi_original <- copy(taxable_pi)

## merge treatment, attach event times -----------------------------------------
taxable_pi <- merge_treatment(original_data = taxable_pi,
                              treatment_data_path = eventstudy_tr_path,
                              merge_by = c("fips_county", "fips_state"))
setnames(taxable_pi,"V1", "event_ID")

## define time to event --------------------------------------------------------
taxable_pi[, ref_quarter := ceiling(ref_month / 3)]
taxable_pi[, tt_event := as.integer(4 * year + quarter -
                                  (4 * ref_year + ref_quarter))]

## limit data to two year window around reform ---------------------------------
taxable_pi <- taxable_pi[tt_event >= -1 * 4 & tt_event <= 4]

## add pseudo-control group ----------------------------------------------------

### create unique dataset of never treated counties
control_counties <- fread(tr_groups_path)
control_counties <- control_counties[tr_group == "No change"]
control_counties <- unique(control_counties[, .(fips_county, fips_state)])

control_dt <- merge(taxable_pi_original, control_counties,
                    by = c("fips_state", "fips_county"))
rm(taxable_pi_original)
gc()

### take the mean for each time period
control_dt <- control_dt[,
                         list(control.cpricei = weighted.mean(x = cpricei, w = sales)),
                         by = .(quarter, year)
                         ]

taxable_pi <- merge(taxable_pi, control_dt, by = c("quarter", "year"))
matched_control_data <- taxable_pi[, .(control.cpricei, tt_event, tr_group, sales, event_ID)]

matched_control_data[, cpricei := control.cpricei]
matched_control_data[, tr_group := paste0("No change (", tr_group, ")")]
matched_control_data[, control.cpricei := NULL]
taxable_pi <- rbind(taxable_pi, matched_control_data, fill = T)

## normalize price indices based on time to event ------------------------------
taxable_pi[, store_code_uc := .GRP, by = .(tr_group, event_ID)]
taxable_pi <- normalize_price(price_data = taxable_pi,
                              time_type = "event",
                              base_time = -2,
                              price_var = "cpricei",
                              new_price_var = "normalized.cpricei")

## aggregate by treatment group ------------------------------------------------
taxable_pi_es_collapsed <- taxable_pi[,
                              list(mean_pi = weighted.mean(x = normalized.cpricei, w = sales),
                                   n_counties = uniqueN(1000 * fips_state + fips_county),
                                   n_stores = uniqueN(store_code_uc)),
                              by = c("tr_group", "tt_event")
                              ]

taxable_pi_es_collapsed <- add_tr_count(collapsed_data = taxable_pi_es_collapsed,
                                    tr_group_name = "tr_group",
                                    count_col_name = "n_counties")

fwrite(taxable_pi_es_collapsed, "Data/pi_taxablegoods_es.csv")

## plot and export result ------------------------------------------------------
taxable_pi_es_plot <- ggplot(data = taxable_pi_es_collapsed,
                         mapping = aes(x = tt_event, y = mean_pi, color = tr_count)) +
  labs(x = "Time to event",
       y = "Log normalized price index",
       color = "Sales tax change",
       caption = expression(paste("Weighted by sales in 2008 Q1. ", Y==0, " in ", T==-2))) +
  geom_line() +
  theme_bw()

ggsave("Graphs/pi_taxablegoods_es.png")

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

## balance sample on store-level from 2008 to 2014 -----------------------------
taxexempt_pi <- taxexempt_pi[year %in% 2008:2014 & !is.na(cpricei) & !is.na(sales)]
taxexempt_pi <- balance_panel_data(taxexempt_pi, time_vars = c("quarter", "year"),
                                 panel_unit = "store_code_uc", n_periods = 28)
taxexempt_pi_original <- copy(taxexempt_pi)

## merge treatment, attach event times -----------------------------------------
taxexempt_pi <- merge_treatment(original_data = taxexempt_pi,
                              treatment_data_path = eventstudy_tr_path,
                              merge_by = c("fips_county", "fips_state"))
setnames(taxexempt_pi,"V1", "event_ID")

## define time to event --------------------------------------------------------
taxexempt_pi[, ref_quarter := ceiling(ref_month / 3)]
taxexempt_pi[, tt_event := as.integer(4 * year + quarter -
                                      (4 * ref_year + ref_quarter))]

## limit data to two year window around reform ---------------------------------
taxexempt_pi <- taxexempt_pi[tt_event >= -1 * 4 & tt_event <= 4]

## add pseudo-control group ----------------------------------------------------

### create unique dataset of never treated counties
control_counties <- fread(tr_groups_path)
control_counties <- control_counties[tr_group == "No change"]
control_counties <- unique(control_counties[, .(fips_county, fips_state)])

control_dt <- merge(taxexempt_pi_original, control_counties,
                    by = c("fips_state", "fips_county"))
rm(taxexempt_pi_original)
gc()

### take the mean for each time period
control_dt <- control_dt[,
                         list(control.cpricei = weighted.mean(x = cpricei, w = sales)),
                         by = .(quarter, year)
                         ]

taxexempt_pi <- merge(taxexempt_pi, control_dt, by = c("quarter", "year"))
matched_control_data <- taxexempt_pi[, .(control.cpricei, tt_event, tr_group, sales, event_ID)]

matched_control_data[, cpricei := control.cpricei]
matched_control_data[, tr_group := paste0("No change (", tr_group, ")")]
matched_control_data[, control.cpricei := NULL]
taxexempt_pi <- rbind(taxexempt_pi, matched_control_data, fill = T)

## normalize price indices based on time to event ------------------------------
taxexempt_pi[, store_code_uc := .GRP, by = .(tr_group, event_ID)]
taxexempt_pi <- normalize_price(price_data = taxexempt_pi,
                              time_type = "event",
                              base_time = -2,
                              price_var = "cpricei",
                              new_price_var = "normalized.cpricei")

## aggregate by treatment group ------------------------------------------------
taxexempt_pi_es_collapsed <- taxexempt_pi[,
                                      list(mean_pi = weighted.mean(x = normalized.cpricei, w = sales),
                                           n_counties = uniqueN(1000 * fips_state + fips_county),
                                           n_stores = uniqueN(store_code_uc)),
                                      by = c("tr_group", "tt_event")
                                      ]

taxexempt_pi_es_collapsed <- add_tr_count(collapsed_data = taxexempt_pi_es_collapsed,
                                        tr_group_name = "tr_group",
                                        count_col_name = "n_counties")

fwrite(taxexempt_pi_es_collapsed, "Data/pi_taxexemptgoods_es.csv")

## plot and export result ------------------------------------------------------
taxexempt_pi_es_plot <- ggplot(data = taxexempt_pi_es_collapsed,
                             mapping = aes(x = tt_event, y = mean_pi, color = tr_count)) +
  labs(x = "Time to event",
       y = "Log normalized price index",
       color = "Sales tax change",
       caption = expression(paste("Weighted by sales in 2008 Q1. ", Y==0, " in ", T==-2))) +
  geom_line() +
  theme_bw()

ggsave("Graphs/pi_taxexemptgoods_es.png")

rm(taxexempt_pi)
gc()

