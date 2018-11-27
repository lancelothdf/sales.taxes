library(data.table)
library(sales.taxes)
test_scanner_data <- fread("C:/Users/John Bonney/Desktop/Magne_projects/sales_taxes/output/server/sample_test_subset.csv")
# This dataset is Wyoming in 2008 and 2009. I think I should add fake tax rates for the tests.

county_monthly_tax <- fread("C:/Users/John Bonney/Dropbox/Sales tax/Data/county_monthly_tax_rates.csv")
county_monthly_tax <- county_monthly_tax[, .(fips_state, fips_county, year, month, sales_tax)]

test_scanner_data[, price := sales / quantity]

# Merge on the tax rates (including exemptions)
module_exemptions <- "C:/Users/John Bonney/Desktop/Magne_projects/sales_taxes/output/server/modules_exemptions_long.csv"
test_scanner_data <- merge_tax_rates(sales_data = test_scanner_data,
                                     keep_taxable_only = T,
                                     county_monthly_tax_data = county_monthly_tax,
                                     module_exemptions_path = module_exemptions)
test_scanner_data[, price_w_tax := (1 + applicable_tax) * price]
test_scanner_data <- normalize_price(price_data = test_scanner_data,
                                    time_type = "calendar",
                                    base_time = c(2008, 1),
                                    price_var = "price",
                                    new_price_var = "normalized_price")


test_scanner_data <- normalize_price(price_data = test_scanner_data,
                                    time_type = "calendar",
                                    base_time = c(2008, 1),
                                    price_var = "price_w_tax",
                                    new_price_var = "normalized_price_w_tax")
test_scanner_data <- make_fixed_weights(test_scanner_data,
                                        weight_time = list(year = 2008, month = 1),
                                        weight_var = "sales",
                                        panel_unit_vars = c("fips_state", "fips_county", "store_code_uc", "product_module_code"))

# Aggregate to county x product level
product_by_county_prices <- test_scanner_data[, list(mld_price = mean(normalized_price),
                                                    mld_price_w_tax = mean(normalized_price_w_tax),
                                                    mld_price.wtd = weighted.mean(x = normalized_price,
                                                                                  w = sales.weight),
                                                    mld_price_w_tax.wtd = weighted.mean(x = normalized_price_w_tax,
                                                                                        w = sales.weight),
                                                    total_sales = sum(sales),
                                                    n_stores = .N),
                                             by = c("fips_state", "fips_county",
                                                    "product_module_code", "product_group_code",
                                                    "month", "year")]

product_by_county_prices[, cty_base_sales := max(total_sales * as.integer(year == 2008 & month == 1)),
                         by = c("fips_state", "fips_county", "product_module_code")]

county_pop <- fread("C:/Users/John Bonney/Dropbox/Sales tax/Data/county_population.csv")
product_by_county_prices <- merge(product_by_county_prices,
                                  county_pop,
                                  by = c("fips_state", "fips_county"))
################################
### basic linear time trend ###
###############################

### COMPREHENSIVE DEFINITION ###
price_application(product_by_county_prices,
                  treatment_data_path = "C:/Users/John Bonney/Desktop/Magne_projects/sales_taxes/output/tr_groups_comprehensive.csv",
                  time = "calendar",
                  weighting_var = "cty_base_sales",
                  pretax_var = "mld_price.wtd",
                  posttax_var = "mld_price_w_tax.wtd",
                  resid_type = "A",
                  w_tax = T,
                  fig_outfile = NULL)

#####################################
### group-level linear time trend ###
####################################

### COMPREHENSIVE DEFINITION ###
price_application(product_by_county_prices,
                  treatment_data_path = "C:/Users/John Bonney/Desktop/Magne_projects/sales_taxes/output/tr_groups_comprehensive.csv",
                  time = "calendar",
                  weighting_var = "cty_base_sales",
                  pretax_var = "mld_price.wtd",
                  posttax_var = "mld_price_w_tax.wtd",
                  resid_type = "B",
                  w_tax = T,
                  fig_outfile = NULL)

###########################################
### linear time trend and month effects ###
###########################################

### COMPREHENSIVE DEFINITION ###
price_application(product_by_county_prices,
                  treatment_data_path = "Data/tr_groups_comprehensive.csv",
                  time = "calendar",
                  weighting_var = "cty_base_sales",
                  pretax_var = "mld_price.wtd",
                  posttax_var = "mld_price_w_tax.wtd",
                  resid_type = "C",
                  w_tax = T,
                  fig_outfile = "Graphs/log_price_trends_compr_posttax_C.png")

### RESTRICTIVE DEFINITION ###
price_application(product_by_county_prices,
                  treatment_data_path = "Data/tr_groups_restrictive.csv",
                  time = "calendar",
                  weighting_var = "cty_base_sales",
                  pretax_var = "mld_price.wtd",
                  posttax_var = "mld_price_w_tax.wtd",
                  resid_type = "C",
                  w_tax = T,
                  fig_outfile = "Graphs/log_price_trends_restr_posttax_C.png")

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
price_application(product_by_county_prices,
                  treatment_data_path = "Data/tr_groups_comprehensive.csv",
                  time = "calendar",
                  weighting_var = "cty_base_sales",
                  pretax_var = "mld_price.wtd",
                  posttax_var = "mld_price_w_tax.wtd",
                  resid_type = "E",
                  w_tax = T,
                  fig_outfile = "Graphs/log_price_trends_compr_posttax_E.png")

### RESTRICTIVE DEFINITION ###
price_application(product_by_county_prices,
                  treatment_data_path = "Data/tr_groups_restrictive.csv",
                  time = "calendar",
                  weighting_var = "cty_base_sales",
                  pretax_var = "mld_price.wtd",
                  posttax_var = "mld_price_w_tax.wtd",
                  resid_type = "E",
                  w_tax = T,
                  fig_outfile = "Graphs/log_price_trends_restr_posttax_E.png")
