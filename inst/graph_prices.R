#' Maintained by: John Bonney
#' Last modified: 11/16/2018
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
# creates a weighted sales variable called sales.weight
all_nielsen_data <- make_fixed_weights(all_nielsen_data,
                                       weight_time = list(year = 2008, month = 1),
                                       weight_var = "sales",
                                       panel_unit_vars = c("fips_state", "fips_county", "store_code_uc", "product_module_code"))

# Aggregate to county x product level
product_by_county_prices <- all_nielsen_data[, list(mld_price = mean(normalized_price),
                                                    mld_price_w_tax = mean(normalized_price_w_tax),
                                                    mld_price.wtd = weighted.mean(x = normalized_price,
                                                                                  w = sales.weight),
                                                    mld_price_w_tax.wtd = weighted.mean(x = normalized_price_w_tax,
                                                                                        w = sales.weight),
                                                    total_sales = sum(sales),
                                                    n_stores = .N),
                                       by = c("fips_state", "fips_county",
                                              "product_module_code",
                                              "month", "year")]
fwrite(product_by_county_prices, "Data/Nielsen/product_by_county_prices.csv")

# product_by_county_prices <- fread("Data/Nielsen/product_by_county_prices.csv")
county_pop <- fread("Data/county_population.csv")
product_by_county_prices <- merge(product_by_county_prices,
                                  county_pop,
                                  by = c("fips_state", "fips_county"))


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
