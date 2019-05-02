#' Maintained by: John Bonney
#' Last modified: 11/21/2018
#'
#' Graphs:
#' Plot log of normalized prices by event time
#'

rm(list=ls())
wd <- "/project2/igaarder"
setwd(wd)

library(sales.taxes)
library(readstata13)
library(data.table)
library(zoo)
library(ggplot2)

county_monthly_tax <- fread("Data/county_monthly_tax_rates.csv")
county_monthly_tax <- county_monthly_tax[, .(fips_state, fips_county, year, month, sales_tax)]

all_nielsen_data <- fread("Data/Nielsen/allyears_module_store_level.csv")
all_nielsen_data[, price := sales / quantity]

## incorporate tax rates
all_nielsen_data <- merge_tax_rates(sales_data = all_nielsen_data,
                                    keep_taxable_only = T,
                                    county_monthly_tax_data = county_monthly_tax)
all_nielsen_data <- balance_panel_data(all_nielsen_data,
                                       panel_unit = "store_code_uc",
                                       n_periods = 84)
all_nielsen_data[, price_w_tax := (1 + applicable_tax) * price]
# creates a weighted sales variable called sales.weight
all_nielsen_data <- make_fixed_weights(all_nielsen_data,
                                       weight_time = list(year = 2008, month = 1),
                                       weight_var = "sales",
                                       panel_unit_vars = c("fips_state", "fips_county", "store_code_uc", "product_module_code"))

county_pop <- fread("Data/county_population.csv")

## at this point, we have
## all_nielsen_data -- contains price and price_w_tax on module-store-month level

county_module_weights <- all_nielsen_data[year == 2008 & month == 1,
                                          list(cty_base_sales = sum(sales)),
                                          by = c("fips_state", "fips_county", "product_module_code")]

for (resid_type in c("C", "D", "E", "A", "B")){
  ## Weighted plots
  ### COMPREHENSIVE DEFINITION ###
  compr_graph_name <- paste0("Graphs/log_price_trends_compr_posttax_es_resid_", resid_type, ".png")
  es_price_application(all_nielsen_data,
                       treatment_data_path = "Data/event_study_tr_groups_comprehensive.csv",
                       county_pop_data = NULL,
                       pre_post_periods = 12, # 12 months pre and post
                       county_sales_weights = county_module_weights,
                       weighting_var = "cty_base_sales",
                       price_var = "mld_price_w_tax",
                       resid_type = resid_type,
                       w_tax = T,
                       fig_outfile = compr_graph_name)
  gc()

  ### RESTRICTIVE DEFINITION ###
  # restr_graph_name <- paste0("Graphs/log_price_trends_restr_posttax_es_resid_", resid_type, ".png")
  # es_price_application(all_nielsen_data,
  #                      treatment_data_path = "Data/event_study_tr_groups_restrictive.csv",
  #                      county_pop_data = NULL,
  #                      county_sales_weights = county_module_weights,
  #                      weighting_var = "cty_base_sales",
  #                      price_var = "mld_price_w_tax",
  #                      resid_type = resid_type,
  #                      w_tax = T,
  #                      fig_outfile = restr_graph_name)
}



