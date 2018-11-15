#' Merge tax rates to a panel dataset of sales
#'
#' @description \code{merge_tax_rates} merges applicable tax rates to a dataset
#'     on the product-county level or finer.
#'
#' requires tidyr

identify_exemptions <- function(sales_data,
                                county_monthly_tax_data){

  # 0 is always exempt, 1 is always taxable, 2 is different but always constant rate,
  # 3 is status change, 4 is different and changing.
  sales_data <- merge(sales_data, county_monthly_tax_data,
                      by = c("fips_state", "fips_county", "year", "month"))
  # currently, the tax variable is called `sales_tax'
  sales_data[]


  #TODO: why are some NA?

  # Lance will send me 1) panel of county-level sales tax rates and 2) list of
  # exemptions/special tax rates for various products.
  # At that point, I will connect the county-level sales tax rates to the products,
  # except for where exemptions apply, in which case I will apply the exemption
  # or the reduced rate.
}
