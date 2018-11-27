#' Maintained by: John Bonney
#'
#' Graphs:
#' Plot residualized log of normalized prices by calendar time
#'

rm(list=ls())
wd <- "/project2/igaarder"
setwd(wd)

library(sales.taxes)
library(readstata13)
library(data.table)
library(zoo)
library(ggplot2)

# county_monthly_tax <- fread("Data/county_monthly_tax_rates.csv")
# county_monthly_tax <- county_monthly_tax[, .(fips_state, fips_county, year, month, sales_tax)]
#
# all_nielsen_data <- fread("Data/Nielsen/allyears_module_store_level.csv")
# all_nielsen_data[, price := sales / quantity]
#
# # Merge on the tax rates (including exemptions)
# all_nielsen_data <- merge_tax_rates(sales_data = all_nielsen_data,
#                                     keep_taxable_only = T,
#                                     county_monthly_tax_data = county_monthly_tax)
# all_nielsen_data <- balance_panel_data(all_nielsen_data,
#                                        panel_unit = "store_code_uc",
#                                        n_periods = 84)
# all_nielsen_data[, price_w_tax := (1 + applicable_tax) * price]
# all_nielsen_data <- normalize_price(price_data = all_nielsen_data,
#                                     time_type = "calendar",
#                                     base_time = c(2008, 1),
#                                     price_var = "price",
#                                     new_price_var = "normalized_price")
#
#
# all_nielsen_data <- normalize_price(price_data = all_nielsen_data,
#                                     time_type = "calendar",
#                                     base_time = c(2008, 1),
#                                     price_var = "price_w_tax",
#                                     new_price_var = "normalized_price_w_tax")
# # creates a weighted sales variable called sales.weight
# all_nielsen_data <- make_fixed_weights(all_nielsen_data,
#                                        weight_time = list(year = 2008, month = 1),
#                                        weight_var = "sales",
#                                        panel_unit_vars = c("fips_state", "fips_county", "store_code_uc", "product_module_code"))
#
# # Aggregate to county x product level
# product_by_county_prices <- all_nielsen_data[, list(mld_price = mean(normalized_price),
#                                                     mld_price_w_tax = mean(normalized_price_w_tax),
#                                                     mld_price.wtd = weighted.mean(x = normalized_price,
#                                                                                   w = sales.weight),
#                                                     mld_price_w_tax.wtd = weighted.mean(x = normalized_price_w_tax,
#                                                                                         w = sales.weight),
#                                                     total_sales = sum(sales),
#                                                     n_stores = .N),
#                                              by = c("fips_state", "fips_county",
#                                                     "product_module_code", "product_group_code",
#                                                     "month", "year")]
#
# product_by_county_prices[, cty_base_sales := max(total_sales * as.integer(year == 2008 & month == 1)),
#                          by = c("fips_state", "fips_county", "product_module_code")]
# fwrite(product_by_county_prices, "Data/Nielsen/product_by_county_prices.csv")

product_by_county_prices <- fread("Data/Nielsen/product_by_county_prices.csv")

county_pop <- fread("Data/county_population.csv")
product_by_county_prices <- merge(product_by_county_prices,
                                  county_pop,
                                  by = c("fips_state", "fips_county"))
################################
### basic linear time trend ###
###############################

### COMPREHENSIVE DEFINITION ###
# price_application(product_by_county_prices,
#                   treatment_data_path = "Data/tr_groups_comprehensive.csv",
#                   time = "calendar",
#                   weighting_var = "cty_base_sales",
#                   pretax_var = "mld_price.wtd",
#                   posttax_var = "mld_price_w_tax.wtd",
#                   resid_type = "A",
#                   w_tax = T,
#                   fig_outfile = "Graphs/log_price_trends_compr_posttax_A.png")
#
# ### RESTRICTIVE DEFINITION ###
# price_application(product_by_county_prices,
#                   treatment_data_path = "Data/tr_groups_restrictive.csv",
#                   time = "calendar",
#                   weighting_var = "cty_base_sales",
#                   pretax_var = "mld_price.wtd",
#                   posttax_var = "mld_price_w_tax.wtd",
#                   resid_type = "A",
#                   w_tax = T,
#                   fig_outfile = "Graphs/log_price_trends_restr_posttax_A.png")

#####################################
### group-level linear time trend ###
####################################

### COMPREHENSIVE DEFINITION ###
price_application(product_by_county_prices,
                  treatment_data_path = "Data/tr_groups_comprehensive.csv",
                  time = "calendar",
                  weighting_var = "cty_base_sales",
                  pretax_var = "mld_price.wtd",
                  posttax_var = "mld_price_w_tax.wtd",
                  resid_type = "B",
                  w_tax = T,
                  fig_outfile = "Graphs/log_price_trends_compr_posttax_B.png")

### RESTRICTIVE DEFINITION ###
price_application(product_by_county_prices,
                  treatment_data_path = "Data/tr_groups_restrictive.csv",
                  time = "calendar",
                  weighting_var = "cty_base_sales",
                  pretax_var = "mld_price.wtd",
                  posttax_var = "mld_price_w_tax.wtd",
                  resid_type = "B",
                  w_tax = T,
                  fig_outfile = "Graphs/log_price_trends_restr_posttax_B.png")

###########################################
### linear time trend and month effects ###
###########################################

# ### COMPREHENSIVE DEFINITION ###
# price_application(product_by_county_prices,
#                   treatment_data_path = "Data/tr_groups_comprehensive.csv",
#                   time = "calendar",
#                   weighting_var = "cty_base_sales",
#                   pretax_var = "mld_price.wtd",
#                   posttax_var = "mld_price_w_tax.wtd",
#                   resid_type = "C",
#                   w_tax = T,
#                   fig_outfile = "Graphs/log_price_trends_compr_posttax_C.png")
#
# ### RESTRICTIVE DEFINITION ###
# price_application(product_by_county_prices,
#                   treatment_data_path = "Data/tr_groups_restrictive.csv",
#                   time = "calendar",
#                   weighting_var = "cty_base_sales",
#                   pretax_var = "mld_price.wtd",
#                   posttax_var = "mld_price_w_tax.wtd",
#                   resid_type = "C",
#                   w_tax = T,
#                   fig_outfile = "Graphs/log_price_trends_restr_posttax_C.png")

##################################################################
### linear time trend and month effects on product-group level ###
##################################################################

### COMPREHENSIVE DEFINITION ###
price_application(product_by_county_prices,
                  treatment_data_path = "Data/tr_groups_comprehensive.csv",
                  time = "calendar",
                  weighting_var = "cty_base_sales",
                  pretax_var = "mld_price.wtd",
                  posttax_var = "mld_price_w_tax.wtd",
                  resid_type = "D",
                  w_tax = T,
                  fig_outfile = "Graphs/log_price_trends_compr_posttax_D.png")

### RESTRICTIVE DEFINITION ###
price_application(product_by_county_prices,
                  treatment_data_path = "Data/tr_groups_restrictive.csv",
                  time = "calendar",
                  weighting_var = "cty_base_sales",
                  pretax_var = "mld_price.wtd",
                  posttax_var = "mld_price_w_tax.wtd",
                  resid_type = "D",
                  w_tax = T,
                  fig_outfile = "Graphs/log_price_trends_restr_posttax_D.png")


#############################
### calendar time effects ###
#############################

### COMPREHENSIVE DEFINITION ###
# price_application(product_by_county_prices,
#                   treatment_data_path = "Data/tr_groups_comprehensive.csv",
#                   time = "calendar",
#                   weighting_var = "cty_base_sales",
#                   pretax_var = "mld_price.wtd",
#                   posttax_var = "mld_price_w_tax.wtd",
#                   resid_type = "E",
#                   w_tax = T,
#                   fig_outfile = "Graphs/log_price_trends_compr_posttax_E.png")
#
# ### RESTRICTIVE DEFINITION ###
# price_application(product_by_county_prices,
#                   treatment_data_path = "Data/tr_groups_restrictive.csv",
#                   time = "calendar",
#                   weighting_var = "cty_base_sales",
#                   pretax_var = "mld_price.wtd",
#                   posttax_var = "mld_price_w_tax.wtd",
#                   resid_type = "E",
#                   w_tax = T,
#                   fig_outfile = "Graphs/log_price_trends_restr_posttax_E.png")
