#' Test sales.taxes functions.
#'
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

test_scanner_data <- make_fixed_weights(test_scanner_data,
                                       weight_time = list(year = 2008, month = 1),
                                       weight_var = "sales",
                                       panel_unit_vars = c("fips_state", "fips_county", "store_code_uc", "product_module_code"))

county_pop <- fread("C:/Users/John Bonney/Dropbox/Sales tax/Data/county_population.csv")

## at this point, we have
## all_nielsen_data -- contains price and price_w_tax on module-store-month level

county_module_weights <- test_scanner_data[year == 2008 & month == 1,
                                          list(cty_base_sales = sum(sales)),
                                          by = c("fips_state", "fips_county", "product_module_code")]


county_pop <- fread("C:/Users/John Bonney/Dropbox/Sales tax/Data/county_population.csv")
product_by_county_prices <- merge(test_scanner_data,
                                  county_pop,
                                  by = c("fips_state", "fips_county"))

# test combine_scanner_data

price_data <- merge_treatment(original_data = product_by_county_prices,
                              treatment_data_path = "C:/Users/John Bonney/Desktop/Magne_projects/sales_taxes/output/event_study_tr_groups_comprehensive.csv",
                              time = "event",
                              merge_by = c("fips_county", "fips_state"))

price_data[, tt_event := as.integer(12 * year + month - (12 * ref_year + ref_month))]

price_data <- normalize_price(price_data = price_data,
                              time_type = "event",
                              base_time = -1,
                              price_var = "price",
                              new_price_var = "normalized_price")




