#' Analyze outcome for one county, one product
#'
#' @description \code{one_cty_one_prod} analyzes the given data for a single
#'     county for a single product type.
#' @param product_data Dataset to be analyzed (data.table). Should have columns
#'     with \code{fips_county}, \code{fips_state}, \code{product_module_code}
#' @param fips_state The state fips code of the desired state (integer)
#' @param fips_county The county fips code of the desired county (integer)
#' @param product_module_code The product module code of the desired product
#'     (integer)
#'

one_cty_one_prod <- function(product_data,
                             fips_state,
                             fips_county,
                             product_module_code,
                             month_of_reform,
                             year_of_reform,
                             county_monthly_tax_data){
  assertDataTable(product_data)
  assertInteger(fips_county)
  assertInteger(fips_state)
  assertInteger(product_module_code)
  assertInteger(month_of_reform)
  assertInteger(year_of_reform)
  assertSubset(c("fips_county", "fips_state", "store_code_uc", "quantity", "sales",
                 "product_module_code", "year", "month"), names(product_data))

  # first, extract the data for that subset
  one_cty_prod <- product_data[fips_state == fips_state &
                                 fips_county == fips_county &
                                 product_module_code == product_module_code]
  one_cty_prod <- balance_panel_data(one_cty_prod,
                                    panel_unit = "store_code_uc",
                                    n_periods = 84)
  print(paste("Number of stores:", length(unique(one_cty_prod$store_code_uc))))

  if (!price %in% names(one_cty_prod)){
    one_cty_prod[, price := sales / quantity]
  }

  # then merge on tax
  one_cty_prod <- merge_tax_rates(sales_data = one_cty_prod,
                                      keep_taxable_only = F,
                                      county_monthly_tax_data = county_monthly_tax_data)

  # then calculate post-tax price
  one_cty_prod[, price_w_tax := (1 + applicable_tax) * price]

  # then, collapse
  one_cty_prod_collapsed <- one_cty_prod[, list(mean_price = mean(price),
                                                mean_price.taxed = mean(price_w_tax),
                                                total_sales = sum(sales),
                                                n_stores = .N),
                                         by = c("month", "year")]

  #TODO: what should this return? A list? Need to make graphs and stuff...

}
