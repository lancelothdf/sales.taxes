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

################################################################################
###### Prepare environment & create datasets of all goods and taxable only #####
################################################################################

## useful filepaths ------------------------------------------------------------
tr_data_path <- "Data/event_study_tr_groups_comprehensive.csv"
tr_groups_path <- "Data/tr_groups_comprehensive.csv"
sales_data_path <- "Data/sales_quarterly_2006-2016.csv"
tax_rates_path <- "Data/county_monthly_tax_rates.csv"
module_exemptions_path <- "/project2/igaarder/Data/modules_exemptions_long.csv"

## create .csv's of taxable and all goods --------------------------------------
nonfood_pi <- read.dta13("Data/Nielsen/Price_quantity_indices_nonfood.dta")
nonfood_pi <- as.data.table(nonfood_pi)
fwrite(nonfood_pi, "Data/Nielsen/price_quantity_indices_nonfood.csv")

food_pi <- fread("Data/Nielsen/price_quantity_indices_food.csv")
food_pi[, c("fips_state", "fips_county") := NULL]

all_pi <- rbind(food_pi, nonfood_pi)
rm(nonfood_pi, food_pi)
gc()

### attach county and state FIPS codes as well as sales ------------------------
sales_data <- fread(sales_data_path)
sales_data <- sales_data[, .(store_code_uc, product_module_code, fips_county,
                             fips_state, quarter, year, sales)]

all_pi <- merge(all_pi, sales_data, by = c("store_code_uc", "quarter", "year",
                                           "product_module_code" ))
fwrite(all_pi, "Data/Nielsen/price_quantity_indices_allitems.csv")

rm(sales_data)
gc()

### create taxable only dataset ------------------------------------------------
county_monthly_tax <- fread(tax_rates_path)
module_exemptions <- fread(module_exemptions_path)

county_monthly_tax <- county_monthly_tax[, .(fips_state, fips_county, year,
                                             month, sales_tax)]
applicable_tax <- merge(module_exemptions, county_monthly_tax,
                        by = c("fips_state", "year", "month"),
                        allow.cartesian = T)

applicable_tax[is.na(taxable), applicable_tax := sales_tax.y]
applicable_tax[taxable == 1 & is.na(sales_tax.x), applicable_tax := sales_tax.y]
applicable_tax[taxable == 1 & !is.na(sales_tax.x), applicable_tax := sales_tax.x]
applicable_tax[taxable == 0, applicable_tax := 0]
applicable_tax[, quarter := ceiling(month / 3)]
applicable_tax <- applicable_tax[, list(applicable_tax = max(applicable_tax)),
                                 by = .(fips_state, fips_county, year, quarter,
                                        product_module_code)]

taxable_pi <- merge(all_pi, applicable_tax,
                    by = c("fips_state", "fips_county", "year", "quarter",
                           "product_module_code"), all.x = T)

taxable_pi[, rm_missing := max(as.integer(is.na(applicable_tax))),
           by = c("fips_state", "fips_county", "product_module_code")]
taxable_pi[, rm_nontaxable := max(as.integer(applicable_tax == 0)),
           by = c("fips_state", "fips_county", "product_module_code")]
taxable_pi <- taxable_pi[rm_nontaxable != 1 & rm_missing != 1]
taxable_pi <- taxable_pi[, .(store_code_uc, quarter, year, product_group_code,
                             product_module_code, pricei, quantityi, cpricei,
                             fips_state, fips_county)]

fwrite(taxable_pi, "Data/Nielsen/price_quantity_indices_taxableitems.csv")

rm(county_monthly_tax, module_exemptions, applicable_tax)
gc()

################################################################################
############## Plots by Calendar Time (taxable and all goods) ##################
################################################################################

# All goods ====================================================================

## balance sample on store-level from 2008 to 2014 -----------------------------
all_pi <- all_pi[year %in% 2008:2014 & !is.na(cpricei) & !is.na(sales)]
all_pi <- balance_panel_data(all_pi, time_vars = c("quarter", "year"),
                             panel_unit = "store_code_uc", n_periods = 28)

## merge treatment -------------------------------------------------------------
all_pi <- merge_treatment(original_data = all_pi,
                          treatment_data_path = tr_groups_path,
                          time = "calendar",
                          merge_by = c("fips_county", "fips_state"))

## normalize price index -------------------------------------------------------
price_base <- all_pi[year == 2008 & quarter == 1]
price_base <- price_base[, .(store_code_uc, product_module_code, cpricei)]
price_base[, base.cpricei := cpricei]
price_base[, cpricei := NULL]

all_pi <- merge(all_pi, price_base, by = c("store_code_uc", "product_module_code"))
all_pi[, normalized.cpricei := log(cpricei) - log(base.cpricei)]
all_pi[, base.cpricei := NULL]

rm(price_base)
gc()

## aggregate across treatment groups -------------------------------------------

all_pi_collapsed <- all_pi[, list(
  mean.cpricei = weighted.mean(x = base.cpricei, w = sales),
  n_counties = uniqueN(1000 * fips_state + fips_county)
  ), by = c("tr_group", "year", "quarter")]

all_pi_collapsed <- add_tr_count(collapsed_data = all_pi_collapsed,
                                 tr_group_name = "tr_group",
                                 count_col_name = "n_counties")
fwrite(all_pi_collapsed, "Data/pi_all_calendar.csv")

## prepare plot-----------------------------------------------------------------
all_pi_collapsed$year_qtr <- as.yearqtr(paste(
  as.integer(all_pi_collapsed$year), as.integer(all_pi_collapsed$quarter)
  ), "%Y %q")

all.calendar.plot <- ggplot(data = all_pi_collapsed, mapping = aes(x = year_qtr,
                                                           y = mean.cpricei,
                                                           color = tr_count)) +
  labs(x = "Quarter", y = "Mean normalized ln(index)", color = "Sales tax change",
       caption = note) +
  scale_x_yearqtr(format = "%Y Q%q") +
  geom_line() +
  theme_bw()

ggsave("Graphs/pi_all_calendar.png")

# Taxable goods ================================================================

## balance sample on store-level from 2008 to 2014 -----------------------------
taxable_pi <- taxable_pi[year %in% 2008:2014 & !is.na(cpricei) & !is.na(sales)]
taxable_pi <- balance_panel_data(taxable_pi, time_vars = c("quarter", "year"),
                             panel_unit = "store_code_uc", n_periods = 28)

## merge treatment -------------------------------------------------------------
taxable_pi <- merge_treatment(original_data = taxable_pi,
                              treatment_data_path = tr_groups_path,
                              time = "calendar",
                              merge_by = c("fips_county", "fips_state"))

## normalize price index -------------------------------------------------------
price_base <- taxable_pi[year == 2008 & quarter == 1]
price_base <- price_base[, .(store_code_uc, product_module_code, cpricei)]
price_base[, base.cpricei := cpricei]
price_base[, cpricei := NULL]

taxable_pi <- merge(taxable_pi, price_base, by = c("store_code_uc", "product_module_code"))
taxable_pi[, normalized.cpricei := log(cpricei) - log(base.cpricei)]
taxable_pi[, base.cpricei := NULL]

rm(price_base)
gc()

## aggregate across treatment groups -------------------------------------------

taxable_pi_collapsed <- taxable_pi[, list(
  mean.cpricei = weighted.mean(x = base.cpricei, w = sales),
  n_counties = uniqueN(1000 * fips_state + fips_county)
), by = c("tr_group", "year", "quarter")]

taxable_pi_collapsed <- add_tr_count(collapsed_data = taxable_pi_collapsed,
                                     tr_group_name = "tr_group",
                                     count_col_name = "n_counties")
fwrite("Data/taxable_pi_collapsed.csv")

## prepare plot-----------------------------------------------------------------
taxable_pi_collapsed$year_qtr <- as.yearqtr(paste(
  as.integer(taxable_pi_collapsed$year), as.integer(taxable_pi_collapsed$quarter)
), "%Y %q")

taxable.calendar.plot <- ggplot(taxable_pi_collapsed, aes(x = year_qtr,
                                                          y = mean.cpricei,
                                                          color = tr_count)) +
  labs(x = "Quarter", y = "Mean normalized ln(index)", color = "Sales tax change",
       caption = note) +
  scale_x_yearqtr(format = "%Y Q%q") +
  geom_line() +
  theme_bw()

ggsave("Graphs/pi_taxable_calendar.png")

################################################################################
################ Plots by Event Time (taxable and all goods) ###################
################################################################################

## load in price index data (variables: "store_code_uc", "quarter", "year",
##                              "product_group_code", "product_module_code",
##                              "pricei", "quantityi", "cpricei", "fips_state",
##                              "fips_county")

# pi_data <- fread(pi_data_path)
# pi_data <- pi_data[year %in% 2008:2014] # years for which we have tax rates
#
# ## merge on sales weights ------------------------------------------------------
# sales_weights <- fread("Data/sales_quarterly_2006-2016.csv")
# sales_weights <- sales_weights[year == 2008 & quarter == 1]
# sales_weights <- sales_weights[, .(store_code_uc, product_module_code, sales)]
#
# pi_data <- merge(pi_data, sales_weights,
#                  by = c("store_code_uc", "product_module_code"))
# rm(sales_weights)
# gc()
#
# ## balance on store-level ------------------------------------------------------
# pi_data <- balance_panel_data(pi_data, time_vars = c("quarter", "year"),
#                               panel_unit = "store_code_uc", n_periods = 28)
#
# ## attach event times ----------------------------------------------------------
# pi_data <- merge_treatment(original_data = pi_data,
#                            treatment_data_path = tr_data_path,
#                            merge_by = c("fips_county", "fips_state"))
#
# ## define time to event --------------------------------------------------------
# pi_data[, ref_quarter := ceiling(ref_month / 4)]
# pi_data[, tt_event := as.integer(4 * year + quarter -
#                                    (4 * ref_year + ref_quarter))]
#
# ## normalize price indices based on time to event ------------------------------
# pi_data <- normalize_price(price_data = pi_data,
#                            time_type = "event",
#                            base_time = -2,
#                            price_var = "cpricei",
#                            new_price_var = "normalized.cpricei")
#
# # *** NOTE: currently, we do not residualize at all. ***
#
# ## limit data to two year window around reform ---------------------------------
# pi_data <- pi_data[tt_event >= -1 * 4 & tt_event <= 4]
# pi_data <- pi_data[!is.na(normalized.cpricei)]
#
# ## add pseudo-control group ----------------------------------------------------
#
# ### create unique dataset of never treated counties
# control_counties <- fread(tr_groups_path)
# control_counties <- control_counties[tr_group == "No change"]
# control_counties <- unique(control_counties[, .(fips_county, fips_state)])
#
# control_dt <- merge(pi_data, control_counties,
#                     by = c("fips_state", "fips_county"))
#
# ### take the mean for each time period
# control_dt <- control_dt[,
#   list(control.cpricei = weighted.mean(x = normalized.cpricei, w = sales)),
#   by = .(quarter, year)
#   ]
#
# pi_data <- merge(pi_data, control_dt, by = c("quarter", "year"))
# matched_control_data <- pi_data[, .(control.cpricei, tt_event, tr_group, sales)]
#
# matched_control_data[, normalized.cpricei := control.cpricei]
# matched_control_data[, tr_group := paste0("No change (", tr_group, ")")]
# matched_control_data[, control.cpricei := NULL]
# pi_data <- rbind(pi_data, matched_control_data, fill = T)
#
# ## aggregate by treatment group ------------------------------------------------
# pi_collapsed <- pi_data[,
#   list(mean_pi = weighted.mean(x = normalized.cpricei, w = sales),
#        n_counties = uniqueN(1000 * fips_state + fips_county),
#        n_stores = uniqueN(store_code_uc)),
#    by = c("tr_group", "tt_event")
#   ]
#
# pi_collapsed <- add_tr_count(collapsed_data = pi_collapsed,
#                              tr_group_name = "tr_group",
#                              count_col_name = "n_counties")
#
# fwrite(pi_collapsed, "Data/pi_sw_control_es.csv")
#
# ## plot and export result ------------------------------------------------------
# pi_plot <- ggplot(data = pi_collapsed,
#                   mapping = aes(x = tt_event, y = mean_pi, color = tr_count)) +
#   labs(x = "Time to event",
#        y = "Log normalized price index",
#        color = "Sales tax change",
#        note = expression(paste("Weighted by sales in 2008 Q1. ", Y==0, " in ", T==-2))) +
#   geom_line() +
#   theme_bw()
#
# ggsave("Graphs/pi_sw_control_es.png")


