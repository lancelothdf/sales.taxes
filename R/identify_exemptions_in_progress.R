#' Identify tax-exempted (or tax-reduced) goods
#'
#' @description \code{identify_exemptions} identifies tax-exempt goods and marks
#'     their tax rates as zero.
#'
#' requires tidyr

identify_exemptions <- function(sales_data,
                                module_exemptions_path = "/project2/igaarder/Data/Nielsen/List_modules_sample_with_description.xlsx"){
  # Step 1: import the module exemptions file (wide or long)
  module_exemptions <- read.xlsx(module_exemptions_path, sheetIndex = 1)
  module_exemptions <- gather(module_exemptions, AL:WY, key = "state", value = "taxable")
  module_exemptions <- as.data.table(module_exemptions)[, .(Module, state, taxable)]
  setnames(module_exemptions, old = "Module", new = "product_module_code")
  module_exemptions[, taxable := as.integer(taxable)]

  fips_codes <- structure(list(state = c("AL", "AK", "AZ", "AR", "CA", "CO",
                                         "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS",
                                         "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE",
                                         "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA",
                                         "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI",
                                         "WY", "AS", "GU", "MP", "PR", "UM", "VI"),
                               fips_state = c(1L, 2L, 4L, 5L, 6L, 8L, 9L, 10L, 11L, 12L, 13L, 15L, 16L, 17L, 18L,
                                                 19L, 20L, 21L, 22L, 23L, 24L, 25L, 26L, 27L, 28L, 29L, 30L, 31L,
                                                 32L, 33L, 34L, 35L, 36L, 37L, 38L, 39L, 40L, 41L, 42L, 44L, 45L,
                                                 46L, 47L, 48L, 49L, 50L, 51L, 53L, 54L, 55L, 56L, 60L, 66L, 69L,
                                                 72L, 74L, 78L)), row.names = c(NA, -57L), class = "data.frame")
  module_exemptions <- merge(module_exemptions, as.data.table(fips_codes),
                             by = c("state"))
  # 0 is always exempt, 1 is always taxable, 2 is different but always constant rate,
  # 3 is status change, 4 is different and changing.


  #TODO: why are some NA?

  # Lance will send me 1) panel of county-level sales tax rates and 2) list of
  # exemptions/special tax rates for various products.
  # At that point, I will connect the county-level sales tax rates to the products,
  # except for where exemptions apply, in which case I will apply the exemption
  # or the reduced rate.
}
