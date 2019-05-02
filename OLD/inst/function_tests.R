#' Create tests to test package functions.

# Test merge_tax_rates -- with "keep taxable only" as true/false, remove_tax_missing T/F

test_merge_tax_rates <- function(){

  sales_data <- data.table(expand.grid(fips_county = 1:2,
                                       fips_state = 1:2,
                                       year = 2012:2013,
                                       month = 1,
                                       product_module_code = c("ham", "cheese")))

  module_exemptions <- data.table(fips_state = rep(1:2, 4),
                                  year = rep(2012:2013, each = 4),
                                  month = 1,
                                  product_module_code = rep(c("ham", "ham", "cheese", "cheese"), 2)) # state-product-year dataset
  module_exemptions[, sales_tax := c(NA, .05, NA, NA, NA, .05, NA, NA)]
  module_exemptions[, taxable := 1] #first, test all taxable

  county_monthly_tax_data <- data.table(expand.grid(fips_county = 1:2,
                                                    fips_state = 1:2,
                                                    month = 1,
                                                    year = 2012:2013)) #county-year dataset that has county, state, year, tax rate
  county_monthly_tax_data[, sales_tax := c(.01, .02, .03, .04, .015, .025, .035, .045)]

  merge_tax_rates(sales_data = sales_data,
                  county_monthly_tax_data = county_monthly_tax_data,
                  keep_taxable_only = F,
                  remove_tax_missing = T,
                  module_exemptions = module_exemptions)
}

# Test balance_panel_data

# Test normalize price

# test make fixed weights

# test remove_time_trends

# test merge_treatment

# test add_tr_count
