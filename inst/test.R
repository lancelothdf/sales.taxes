#' Test sales.taxes functions.
#'
library(data.table)
test_scanner_data <- fread("C:/Users/John Bonney/Desktop/Magne_projects/sales_taxes/output/server/sample_test_subset.csv")
# This dataset is Wyoming in 2008 and 2009. I think I should add fake tax rates for the tests.

county_monthly_tax <- fread("C:/Users/John Bonney/Dropbox/Sales tax/Data/county_monthly_tax_rates.csv")
county_monthly_tax <- county_monthly_tax[, .(fips_state, fips_county, year, month, sales_tax)]

test_scanner_data[, price := sales / quantity]

# Merge on the tax rates (including exemptions)
module_exemptions <- "C:/Users/John Bonney/Desktop/Magne_projects/sales_taxes/output/server/modules_exemptions_long.csv"
test_scanner_data <- merge_tax_rates(sales_data = test_scanner_data,
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

# Aggregate to county x product level
product_by_county_prices <- test_scanner_data[, list(mld_price = mean(normalized_price),
                                                    mld_price_w_tax = mean(normalized_price_w_tax),
                                                    n_stores = .N),
                                             by = c("fips_state", "fips_county",
                                                    "product_module_code",
                                                    "month", "year")]

county_pop <- fread("C:/Users/John Bonney/Dropbox/Sales tax/Data/county_population.csv")
product_by_county_prices <- merge(product_by_county_prices,
                                  county_pop,
                                  by = c("fips_state", "fips_county"))

# test combine_scanner_data

price_application(product_by_county_prices,
                  treatment_data_path = "C:/Users/John Bonney/Desktop/Magne_projects/sales_taxes/output/tr_groups_comprehensive.csv",
                  time = "calendar",
                  w_tax = F,
                  fig_outfile = NULL)
price_application(product_by_county_prices,
                  treatment_data_path = "C:/Users/John Bonney/Desktop/Magne_projects/sales_taxes/output/tr_groups_comprehensive.csv",
                  time = "calendar",
                  w_tax = T,
                  fig_outfile = NULL)
