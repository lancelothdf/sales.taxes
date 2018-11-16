#' Maintained by: John Bonney
#' Last modified: 11/12/2018
#'
#' Graphs:
#' Plot log of normalized prices by calendar time
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
all_nielsen_data <- balance_panel_data(all_nielsen_data,
                                       panel_unit = "store_code_uc",
                                       n_periods = 84)
all_nielsen_data[, price := sales / quantity]

# Merge on the tax rates (including exemptions)
all_nielsen_data <- merge_tax_rates(sales_data = all_nielsen_data,
                                    county_monthly_tax_data = county_monthly_tax)
all_nielsen_data[, price_w_tax := (1 + applicable_tax) * price]
all_nielsen_data <- normalize_price(price_data = all_nielsen_data,
                    time_type = "calendar",
                    base_time = c(2008, 1),
                    price_var = "price",
                    new_price_var = "normalized_price")

all_nielsen_data <- normalize_price(price_data = all_nielsen_data,
                                    time_type = "calendar",
                                    base_time = c(2008, 1),
                                    price_var = "price_w_tax",
                                    new_price_var = "normalized_price_w_tax")

# Aggregate to county x product level
product_by_county_prices <- all_nielsen_data[, list(mld_price = mean(normalized_price),
                                                    mld_price_w_tax = mean(normalized_price_w_tax),
                                                    n_stores = .N),
                                       by = c("fips_state", "fips_county",
                                              "product_module_code",
                                              "month", "year")]
fwrite(product_by_county_prices, "Data/Nielsen/product_by_county_prices.csv")

county_pop <- fread("Data/county_population.csv")
product_by_county_prices <- merge(product_by_county_prices,
                                  county_pop,
                                  by = c("fips_state", "fips_county"))


### At this point, the data is ready (for unweighted plots)
### COMPREHENSIVE DEFINITION ###
price_application(product_by_county_prices,
                  treatment_data_path = "Data/tr_groups_comprehensive.csv",
                  time = "calendar",
                  w_tax = F,
                  fig_outfile = "Graphs/log_price_trends_compr_pretax.png")
price_application(product_by_county_prices,
                  treatment_data_path = "Data/tr_groups_comprehensive.csv",
                  time = "calendar",
                  w_tax = T,
                  fig_outfile = "Graphs/log_price_trends_compr_posttax.png")
### event study-like ###
price_application(product_by_county_prices,
                  treatment_data_path = "Data/event_study_tr_groups_comprehensive.csv",
                  time = "event",
                  w_tax = F,
                  fig_outfile = "Graphs/log_sales_trends_es_compr_pretax.png")
price_application(product_by_county_prices,
                  treatment_data_path = "Data/event_study_tr_groups_comprehensive.csv",
                  time = "event",
                  w_tax = T,
                  fig_outfile = "Graphs/log_sales_trends_es_compr_posttax.png")

### RESTRICTIVE DEFINITION ###
price_application(product_by_county_prices,
                  treatment_data_path = "Data/tr_groups_restrictive.csv",
                  time = "calendar",
                  w_tax = F,
                  fig_outfile = "Graphs/log_price_trends_restr_pretax.png")
price_application(product_by_county_prices,
                  treatment_data_path = "Data/tr_groups_restrictive.csv",
                  time = "calendar",
                  w_tax = T,
                  fig_outfile = "Graphs/log_price_trends_restr_posttax.png")
### event study-like ###
price_application(product_by_county_prices,
                  treatment_data_path = "Data/event_study_tr_groups_restrictive.csv",
                  time = "event",
                  w_tax = F,
                  fig_outfile = "Graphs/log_sales_trends_es_restr_pretax.png")
price_application(product_by_county_prices,
                  treatment_data_path = "Data/event_study_tr_groups_restrictive.csv",
                  time = "event",
                  w_tax = T,
                  fig_outfile = "Graphs/log_sales_trends_es_restr_posttax.png")

