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
all_nielsen_data <- balance_panel_data(all_nielsen_data,
                                       panel_unit = "store_code_uc",
                                       n_periods = 84)
all_nielsen_data[, price := sales / quantity]

## incorporate tax rates
all_nielsen_data <- merge_tax_rates(sales_data = all_nielsen_data,
                                    county_monthly_tax_data = county_monthly_tax)
all_nielsen_data[, price_w_tax := (1 + applicable_tax) * price]
# creates a weighted sales variable called sales.weight
all_nielsen_data <- make_fixed_weights(all_nielsen_data,
                                       weight_time = list(year = 2008, month = 1),
                                       weight_var = "sales",
                                       panel_unit_vars = c("fips_state", "fips_county", "store_code_uc", "product_module_code"))

county_pop <- fread("Data/county_population.csv")

### At this point, the data is ready (for unweighted, calendar plots)
### COMPREHENSIVE DEFINITION ###
price_application(product_by_county_prices,
                  treatment_data_path = "Data/tr_groups_comprehensive.csv",
                  time = "calendar",
                  weighting_var = "population",
                  pretax_var = "mld_price",
                  posttax_var = "mld_price_w_tax",
                  w_tax = F,
                  fig_outfile = "Graphs/log_price_trends_compr_pretax.png")
price_application(product_by_county_prices,
                  treatment_data_path = "Data/tr_groups_comprehensive.csv",
                  time = "calendar",
                  weighting_var = "population",
                  pretax_var = "mld_price",
                  posttax_var = "mld_price_w_tax",
                  w_tax = T,
                  fig_outfile = "Graphs/log_price_trends_compr_posttax.png")

### RESTRICTIVE DEFINITION ###
price_application(product_by_county_prices,
                  treatment_data_path = "Data/tr_groups_restrictive.csv",
                  time = "calendar",
                  weighting_var = "population",
                  pretax_var = "mld_price",
                  posttax_var = "mld_price_w_tax",
                  w_tax = F,
                  fig_outfile = "Graphs/log_price_trends_restr_pretax.png")
price_application(product_by_county_prices,
                  treatment_data_path = "Data/tr_groups_restrictive.csv",
                  time = "calendar",
                  weighting_var = "population",
                  pretax_var = "mld_price",
                  posttax_var = "mld_price_w_tax",
                  w_tax = T,
                  fig_outfile = "Graphs/log_price_trends_restr_posttax.png")

## Weighted plots
### COMPREHENSIVE DEFINITION ###
price_application(product_by_county_prices,
                  treatment_data_path = "Data/tr_groups_comprehensive.csv",
                  time = "calendar",
                  weighting_var = "total_sales",
                  pretax_var = "mld_price.wtd",
                  posttax_var = "mld_price_w_tax.wtd",
                  w_tax = F,
                  fig_outfile = "Graphs/log_price_trends_compr_pretax_wtd.png")
price_application(product_by_county_prices,
                  treatment_data_path = "Data/tr_groups_comprehensive.csv",
                  time = "calendar",
                  weighting_var = "total_sales",
                  pretax_var = "mld_price.wtd",
                  posttax_var = "mld_price_w_tax.wtd",
                  w_tax = T,
                  fig_outfile = "Graphs/log_price_trends_compr_posttax_wtd.png")

### RESTRICTIVE DEFINITION ###
price_application(product_by_county_prices,
                  treatment_data_path = "Data/tr_groups_restrictive.csv",
                  time = "calendar",
                  weighting_var = "total_sales",
                  pretax_var = "mld_price.wtd",
                  posttax_var = "mld_price_w_tax.wtd",
                  w_tax = F,
                  fig_outfile = "Graphs/log_price_trends_restr_pretax_wtd.png")
price_application(product_by_county_prices,
                  treatment_data_path = "Data/tr_groups_restrictive.csv",
                  time = "calendar",
                  weighting_var = "total_sales",
                  pretax_var = "mld_price.wtd",
                  posttax_var = "mld_price_w_tax.wtd",
                  w_tax = T,
                  fig_outfile = "Graphs/log_price_trends_restr_posttax_wtd.png")


## Next up: event time plots
# all_nielsen_data[, c("normalized_price", "normalized_price_w_tax") := NULL]

# ultimately need to normalize price by event t=-1 -- have to figure out how to
# connect event t=-1 prices to all other prices for normalization...

# may be fastest to merge on the treatments (m:m merge)...
