#' Maintained by: John Bonney
#' Last modified: 12/6/2018
#'
#' Graphs:
#' Plot log of sales quarterly for ALL GOODS (not just tax-exempt goods)
#' Note that this is an adaptation of graph_sales_quarterly.R
#'

rm(list=ls())
wd <- "/project2/igaarder"
setwd(wd)

library(sales.taxes)
library(readstata13)
library(data.table)
library(zoo)
library(ggplot2)

sales_panel <- fread("Data/Nielsen/allyears_module_store_level.csv")
# we only want stores that are balanced from Jan 2008 to Dec 2014 (84 months)
sales_panel <- balance_panel_data(sales_panel,
                                  panel_unit = "store_code_uc",
                                  n_periods = 84)
# # remove tax-exempt items
# county_monthly_tax <- fread("Data/county_monthly_tax_rates.csv")
# county_monthly_tax <- county_monthly_tax[, .(fips_state, fips_county, year, month, sales_tax)]
# sales_panel <- merge_tax_rates(sales_data = sales_panel,
#                                keep_taxable_only = T,
#                                county_monthly_tax_data = county_monthly_tax)
# Now aggregate to seasons
sales_panel <- months_to_quarters(monthly_data = sales_panel,
                                  month_var = "month",
                                  collapse_by = c("fips_state", "fips_county", "product_group_code",
                                                  "store_code_uc", "product_module_code"),
                                  collapse_var = "sales")
# fwrite(sales_panel, "Data/Nielsen/allyears_module_store_quarterly.csv")
# sales_panel <- fread("Data/Nielsen/allyears_module_store_quarterly.csv")

sales_panel <- make_fixed_weights(panel_data = sales_panel,
                                  weight_time = list(year = 2008, quarter = 1),
                                  weight_var = "sales",
                                  panel_unit_vars = c("fips_state", "fips_county", "store_code_uc", "product_module_code"))
sales_panel[, ln_total_sales := log(sales / sales.weight)]

# product_by_county_sales <- fread("Data/Nielsen/product_by_county_sales.csv")
# merge county population on to sales_panel for weights
preprocessed_sales <- sales_panel
# county_pop <- fread("Data/county_population.csv")
# preprocessed_sales <- merge(product_by_county_sales,
#                                  county_pop,
#                                  by = c("fips_state", "fips_county"))

compr_control_counties <- fread("Data/tr_groups_comprehensive.csv")
compr_control_counties <- compr_control_counties[tr_group == "No change"]
compr_control_counties <- unique(compr_control_counties[, .(fips_county, fips_state)])

for (resid_type in c("A", "B", "C", "D", "E")){
  # Question is: ok to residualize first? Should be...
  if (resid_type == "A"){
    product_by_county_sales <- remove_time_trends(copy(preprocessed_sales),
                                                  outcome_var = "ln_total_sales",
                                                  month_var = "quarter",
                                                  year_var = "year",
                                                  month_dummies = FALSE,
                                                  calendar_time = FALSE,
                                                  product_group_trend = FALSE,
                                                  census_region_trends = TRUE,
                                                  weight_var = NULL)

  } else if (resid_type == "B"){
    product_by_county_sales <- remove_time_trends(copy(preprocessed_sales),
                                                  outcome_var = "ln_total_sales",
                                                  month_var = "quarter",
                                                  year_var = "year",
                                                  month_dummies = FALSE,
                                                  calendar_time = FALSE,
                                                  product_group_trend = TRUE,
                                                  census_region_trends = TRUE,
                                                  weight_var = NULL)
  } else if (resid_type == "C"){
    product_by_county_sales <- remove_time_trends(copy(preprocessed_sales),
                                                  outcome_var = "ln_total_sales",
                                                  month_var = "quarter",
                                                  year_var = "year",
                                                  month_dummies = TRUE,
                                                  calendar_time = FALSE,
                                                  product_group_trend = FALSE,
                                                  census_region_trends = TRUE,
                                                  weight_var = NULL)
  } else if (resid_type == "D"){
    product_by_county_sales <- remove_time_trends(copy(preprocessed_sales),
                                                  outcome_var = "ln_total_sales",
                                                  month_var = "quarter",
                                                  year_var = "year",
                                                  month_dummies = TRUE,
                                                  calendar_time = FALSE,
                                                  product_group_trend = TRUE,
                                                  census_region_trends = TRUE,
                                                  weight_var = NULL)
  } else if (resid_type == "E"){
    product_by_county_sales <- remove_time_trends(copy(preprocessed_sales),
                                                  outcome_var = "ln_total_sales",
                                                  month_var = "quarter",
                                                  year_var = "year",
                                                  month_dummies = FALSE,
                                                  calendar_time = TRUE,
                                                  product_group_trend = FALSE,
                                                  census_region_trends = TRUE,
                                                  weight_var = NULL)
  }
  product_by_county_sales[, ln_total_sales := ln_total_sales_residual]
  compr_outfile <- paste0("Graphs/log_sales_residualized_compr_qly_allgoods_", resid_type, "region.png")
  compr_es_outfile <- paste0("Graphs/log_sales_es_residualized_compr_qly_allgoods_", resid_type, "region.png")
  restr_outfile <- paste0("Graphs/log_sales_residualized_restr_qly_allgoods_", resid_type, "region.png")
  restr_es_outfile <- paste0("Graphs/log_sales_es_residualized_restr_qly_allgoods_", resid_type, "region.png")
  ### COMPREHENSIVE DEFINITION ###
  # sales_application(product_by_county_sales,
  #                   treatment_data_path = "Data/tr_groups_comprehensive.csv",
  #                   time = "calendar",
  #                   fig_outfile = compr_outfile,
  #                   quarterly = T,
  #                   pop_weights = F)

  ### event study-like ###
  sales_application(product_by_county_sales,
                    treatment_data_path = "Data/event_study_tr_groups_comprehensive.csv",
                    time = "event",
                    fig_outfile = compr_es_outfile,
                    quarterly = T,
                    pop_weights = F,
                    create_es_control = T,
                    control_counties = compr_control_counties)

  # ### RESTRICTIVE DEFINITION ###
  # sales_application(product_by_county_sales,
  #                   treatment_data_path = "Data/tr_groups_restrictive.csv",
  #                   time = "calendar",
  #                   fig_outfile = restr_outfile,
  #                   quarterly = T,
  #                   pop_weights = F)
  #
  # ### event study-like ###
  # sales_application(product_by_county_sales,
  #                   treatment_data_path = "Data/event_study_tr_groups_restrictive.csv",
  #                   time = "event",
  #                   fig_outfile = restr_es_outfile,
  #                   quarterly = T,
  #                   pop_weights = F)
}

# ### COMPREHENSIVE DEFINITION ###
# sales_application(product_by_county_sales,
#                   treatment_data_path = "Data/tr_groups_comprehensive.csv",
#                   time = "calendar",
#                   fig_outfile = "Graphs/log_sales_trends_compr2.png")
#
# ### event study-like ###
# sales_application(product_by_county_sales,
#                   treatment_data_path = "Data/event_study_tr_groups_comprehensive.csv",
#                   time = "event",
#                   fig_outfile = "Graphs/log_sales_trends_es_compr2.png")
#
# ### RESTRICTIVE DEFINITION ###
# sales_application(product_by_county_sales,
#                   treatment_data_path = "Data/tr_groups_restrictive.csv",
#                   time = "calendar",
#                   fig_outfile = "Graphs/log_sales_trends_restr2.png")
#
# ### event study-like ###
# sales_application(product_by_county_sales,
#                   treatment_data_path = "Data/event_study_tr_groups_restrictive.csv",
#                   time = "event",
#                   fig_outfile = "Graphs/log_sales_trends_es_restr2.png")
