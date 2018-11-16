#' Identify tax-exempted (or tax-reduced) goods
#'
#' @description \code{identify_exemptions} identifies tax-exempt goods and marks
#'     their tax rates as zero.
#'
#' requires tidyr

identify_exemptions <- function(sales_data,
                                module_exemptions_path = "/project2/igaarder/Data/modules_exemptions_long.csv",
                                county_monthly_tax_data){
  module_exemptions <- fread(module_exemptions_path)
  applicable_tax <- merge(module_exemptions, county_monthly_tax_data,
                          by = c("fips_state", "year", "month"),
                          allow.cartesian = T)
  applicable_tax[is.na(taxable), applicable_tax := sales_tax.y]
  applicable_tax[taxable == 1 & is.na(sales_tax.x), applicable_tax := sales_tax.y]
  applicable_tax[taxable == 1 & !is.na(sales_tax.x), applicable_tax := sales_tax.x]
  applicable_tax[taxable == 0, applicable_tax := 0]

  # TODO: What are the "tax_status" variables?
  table(module_exemptions$tax_status, module_exemptions$taxable)

  applicable_tax <- applicable_tax[, .(fips_state, fips_county, year, month, product_module_code, applicable_tax)]

  sales_data <- merge(sales_data, applicable_tax,
                      by = c("fips_state", "fips_county", "year", "month",
                             "product_module_code"))

}
