#' Merge tax rates (county-product level)
#'
#' @description \code{merge_tax_rates} merges the applicable tax rates to a
#'     county-product level dataset, including exemptions or reduced rates.
#' @param sales_data The county-product level dataset, including quantities and/
#'     or sales information (but not tax rates) (data.table)
#' @param county_monthly_tax_data A dataset of county-by-month tax rates
#'     (data.table)
#' @param module_exemptions A dataset of state-product-year level exemptions/
#'     special tax rates (data.table) If left \code{NULL} (default), the program
#'     will read a dataset from the provided \code{module_exemptions_path}.
#' @param module_exemptions_path The path to the .csv file containing a long
#'     dataset of state-product level exemptions/special tax rates. The default
#'     is \code{"/project2/igaarder/Data/modules_exemptions_long.csv"}. Don't
#'     change this unless you know what you are doing (character)
#'

merge_tax_rates <- function(sales_data,
                            county_monthly_tax_data,
                            keep_taxable_only = F,
                            remove_tax_missing = T,
                            module_exemptions = NULL,
                            module_exemptions_path = "/project2/igaarder/Data/modules_exemptions_long.csv"){
  assertDataTable(sales_data)
  assertCharacter(module_exemptions_path)
  assertDataTable(county_monthly_tax_data)
  assertDataTable(module_exemptions, null.ok = T)
  assertSubset(c("fips_state", "fips_county", "year", "month", "sales_tax"),
               names(county_monthly_tax_data))
  assertSubset(c("fips_state", "fips_county", "year", "month", "product_module_code"),
               names(sales_data))
  if (is.null(module_exemptions)){
    module_exemptions <- fread(module_exemptions_path)
  }
  assertSubset(c("fips_state", "year", "month", "sales_tax", "taxable", "product_module_code"),
               names(module_exemptions))

  applicable_tax <- merge(module_exemptions, county_monthly_tax_data,
                          by = c("fips_state", "year", "month"),
                          allow.cartesian = T)
  applicable_tax[is.na(taxable), applicable_tax := sales_tax.y]
  applicable_tax[taxable == 1 & is.na(sales_tax.x), applicable_tax := sales_tax.y]
  applicable_tax[taxable == 1 & !is.na(sales_tax.x), applicable_tax := sales_tax.x]
  applicable_tax[taxable == 0, applicable_tax := 0]

  # TODO: What are the "tax_status" variables?
  # table(module_exemptions$tax_status, module_exemptions$taxable)

  applicable_tax <- applicable_tax[, .(fips_state, fips_county, year, month, product_module_code, applicable_tax)]

  sales_data <- merge(sales_data, applicable_tax,
                      by = c("fips_state", "fips_county", "year", "month",
                             "product_module_code"), all.x = T)
  if (keep_taxable_only){
    sales_data <- sales_data[applicable_tax != 0]
  }
  if (remove_tax_missing){
    sales_data <- sales_data[!is.na(applicable_tax)]
  }
  return(sales_data)
}
