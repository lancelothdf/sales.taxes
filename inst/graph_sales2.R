#' Maintained by: John Bonney
#' Last modified: 11/12/2018
#'
#' Graphs:
#' Plot log of sales by calendar time
#'

rm(list=ls())
wd <- "/project2/igaarder"
setwd(wd)

library(sales.taxes)
library(readstata13)
library(data.table)
library(zoo)
library(ggplot2)

best_selling_modules <- fread("Data/best_selling_modules.csv")
sales_panel <- data.table(NULL)
for (year in 2008:2014){
  for (rn in c("I", "II", "III", "IV", "V", "VI")){
    filename <- paste0("/project2/igaarder/Data/Nielsen/", year,
                       "_monthly_master_file_part", rn, ".dta")
    print(paste("Loading", filename, "..."))
    data_part <- as.data.table(read.dta13(filename))
    print("Keeping only best selling products")
    data_part <- keep_best_selling_products(data_part,
                                            module_name_ad = "product_module_code",
                                            products_data = best_selling_modules,
                                            module_name_pd = "Module")
    store_id_file <- paste0("/project2/igaarder/Data/Nielsen/stores_",
                            year, ".dta")
    print(paste("Loading", store_id_file, "..."))

    store_id <- as.data.table(read.dta13(store_id_file))

    # this is not good form and may cause errors in the future if these files
    # are altered.
    setnames(store_id, old = "fips_state_code", new = "fips_state")
    setnames(store_id, old = "fips_county_code", new = "fips_county")
    print(paste("Merging", store_id_file, "and", filename))
    data_part <- merge(data_part, store_id, by = "store_code_uc", all.x = T)

    # keep stores we care about
    print(paste("Dropping unnecessary stores"))
    data_part <- data_part[channel_code %in% c("M", "F", "D")]

    data_part[, year := year]
    print(paste("Binding sales_panel and", filename))
    sales_panel <- rbind(sales_panel, data_part)
  }
}

# sales_panel <- combine_scanner_data(folder = "Data/Nielsen/",
#                                     file_tail = "_module_store_level",
#                                     file_type = "dta",
#                                     years = 2008:2014,
#                                     filters = 'channel_code %in% c("M", "F", "D")',
#                                     select_modules = T,
#                                     modules_data = best_selling_modules)

fwrite(sales_panel, file = "Data/Nielsen/allyears_module_store_level.csv")
# we only want stores that are balanced from Jan 2008 to Dec 2014 (84 months)
sales_panel <- balance_panel_data(sales_panel,
                                  panel_unit = "store_code_uc",
                                  n_periods = 84)

# Aggregate to county x product level

product_by_county_sales <- sales_panel[, list(ln_total_sales = log(sum(sales)),
                                              n_stores = .N),
                                       by = c("fips_state", "fips_county",
                                              "product_module_code", "product_group_code",
                                              "month", "year")]

county_monthly_tax <- fread("Data/county_monthly_tax_rates.csv")
county_monthly_tax <- county_monthly_tax[, .(fips_state, fips_county, year, month, sales_tax)]
# remove tax-exempt items
product_by_county_sales <- merge_tax_rates(sales_data = product_by_county_sales,
                                    keep_taxable_only = T,
                                    county_monthly_tax_data = county_monthly_tax)
fwrite(product_by_county_sales, "Data/Nielsen/product_by_county_sales_taxable.csv")

product_by_county_sales <- fread("Data/Nielsen/product_by_county_sales_taxable.csv")

# product_by_county_sales <- fread("Data/Nielsen/product_by_county_sales.csv")

# merge county population on to sales_panel for weights
county_pop <- fread("Data/county_population.csv")
preprocessed_sales <- merge(product_by_county_sales,
                                 county_pop,
                                 by = c("fips_state", "fips_county"))

# this would be a good place to residualize
for (resid_type in c("A", "B", "C", "D", "E")){
  # Question is: ok to residualize first? Should be...
  if (resid_type == "A"){
    product_by_county_sales <- remove_time_trends(copy(preprocessed_sales),
                                                  outcome_var = "ln_total_sales",
                                                  month_var = "month",
                                                  year_var = "year",
                                                  month_dummies = FALSE,
                                                  calendar_time = FALSE,
                                                  product_group_trend = FALSE,
                                                  weight_var = "population")

  } else if (resid_type == "B"){
    product_by_county_sales <- remove_time_trends(copy(preprocessed_sales),
                                                  outcome_var = "ln_total_sales",
                                                  month_var = "month",
                                                  year_var = "year",
                                                  month_dummies = FALSE,
                                                  calendar_time = FALSE,
                                                  product_group_trend = TRUE,
                                                  weight_var = "population")
  } else if (resid_type == "C"){
    product_by_county_sales <- remove_time_trends(copy(preprocessed_sales),
                                                  outcome_var = "ln_total_sales",
                                                  month_var = "month",
                                                  year_var = "year",
                                                  month_dummies = TRUE,
                                                  calendar_time = FALSE,
                                                  product_group_trend = FALSE,
                                                  weight_var = "population")
  } else if (resid_type == "D"){
    product_by_county_sales <- remove_time_trends(copy(preprocessed_sales),
                                                  outcome_var = "ln_total_sales",
                                                  month_var = "month",
                                                  year_var = "year",
                                                  month_dummies = TRUE,
                                                  calendar_time = FALSE,
                                                  product_group_trend = TRUE,
                                                  weight_var = "population")
  } else if (resid_type == "E"){
    product_by_county_sales <- remove_time_trends(copy(preprocessed_sales),
                                                  outcome_var = "ln_total_sales",
                                                  month_var = "month",
                                                  year_var = "year",
                                                  month_dummies = FALSE,
                                                  calendar_time = TRUE,
                                                  product_group_trend = FALSE,
                                                  weight_var = "population")
  }
  product_by_county_sales[, ln_total_sales := ln_total_sales_residual]
  compr_outfile <- paste0("Graphs/log_sales_residualized_compr_", resid_type, ".png")
  compr_es_outfile <- paste0("Graphs/log_sales_es_residualized_compr_", resid_type, ".png")
  restr_outfile <- paste0("Graphs/log_sales_residualized_restr_", resid_type, ".png")
  restr_es_outfile <- paste0("Graphs/log_sales_es_residualized_restr_", resid_type, ".png")
  ### COMPREHENSIVE DEFINITION ###
  sales_application(product_by_county_sales,
                    treatment_data_path = "Data/tr_groups_comprehensive.csv",
                    time = "calendar",
                    fig_outfile = compr_outfile)

  ### event study-like ###
  sales_application(product_by_county_sales,
                    treatment_data_path = "Data/event_study_tr_groups_comprehensive.csv",
                    time = "event",
                    fig_outfile = compr_es_outfile)

  ### RESTRICTIVE DEFINITION ###
  sales_application(product_by_county_sales,
                    treatment_data_path = "Data/tr_groups_restrictive.csv",
                    time = "calendar",
                    fig_outfile = restr_outfile)

  ### event study-like ###
  sales_application(product_by_county_sales,
                    treatment_data_path = "Data/event_study_tr_groups_restrictive.csv",
                    time = "event",
                    fig_outfile = restr_es_outfile)
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
