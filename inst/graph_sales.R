#' Maintained by: John Bonney
#' Last modified: 11/12/2018
#'
#' Graphs:
#' Plot log of sales by calendar time
#'

rm(list=ls())
wd <- "/project2/igaarder"
setwd(wd)

#install.packages("readstata13")
library(readstata13)
library(magrittr)
library(data.table)
library(zoo)
library(ggplot2)

best_selling_modules <- fread("Data/best_selling_modules.csv")

sales_panel <- combine_scanner_data(folder = "Data/Nielsen/",
                                    file_tail = "_module_store_level",
                                    file_type = "dta",
                                    years = 2008:2014,
                                    filters = 'channel_code %in% c("M", "F", "D")',
                                    select_modules = T,
                                    modules_data = best_selling_modules)

fwrite(sales_panel, file = "Data/Nielsen/allyears_module_store_level.csv")
# we only want stores that are balanced from Jan 2008 to Dec 2014 (84 months)
sales_panel <- balance_panel_data(sales_panel,
                                  panel_unit = "store_code_uc",
                                  n_periods = 84)

# Aggregate to county x product level
product_by_county_sales <- sales_panel[, list(ln_total_sales = log(sum(sales)),
                                              n_stores = .N),
                                       by = c("fips_state", "fips_county",
                                              "product_module_code",
                                              "month", "year")]
# TODO: only keep non-exempt goods?

# merge county population on to sales_panel for weights
county_pop <- fread("Data/county_population.csv")
product_by_county_sales <- merge(product_by_county_sales,
                                 county_pop,
                                 by = c("fips_state", "fips_county"))
### COMPREHENSIVE DEFINITION ###
sales_application(product_by_county_sales,
                  treatment_data_path = "Data/tr_groups_comprehensive.csv",
                  time = "calendar",
                  fig_outfile = "Graphs/log_sales_trends_compr.png")

    ### event study-like ###
sales_application(product_by_county_sales,
                  treatment_data_path = "Data/event_study_tr_groups_comprehensive.csv",
                  time = "event",
                  fig_outfile = "Graphs/log_sales_trends_es_compr.png")

### RESTRICTIVE DEFINITION ###
sales_application(product_by_county_sales,
                  treatment_data_path = "Data/tr_groups_restrictive.csv",
                  time = "calendar",
                  fig_outfile = "Graphs/log_sales_trends_restr.png")

### event study-like ###
sales_application(product_by_county_sales,
                  treatment_data_path = "Data/event_study_tr_groups_restrictive.csv",
                  time = "event",
                  fig_outfile = "Graphs/log_sales_trends_es_restr.png")
