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
#' @param product_name The name of the product (for graph labeling) (character)
#' @param month_of_reform The month of the tax reform (integer)
#' @param year_of_reform The year of the tax reform (integer)
#' @param county_monthly_tax_data Dataset with tax rates (data.table)
#' @param rm_month_effects Remove month seasonality? (logical)
#' @param module_exemptions_path The path to the .csv file containing a long
#'     dataset of state-product level exemptions/special tax rates. The default
#'     is "/project2/igaarder/Data/modules_exemptions_long.csv"
#' @return A list including a \code{price_graph}, a \code{sales_graph}, and the
#'     data collapsed to event-time level (\code{})
#'

one_cty_one_prod <- function(product_data,
                             fips_state,
                             fips_county,
                             product_module_code,
                             product_name,
                             month_of_reform,
                             year_of_reform,
                             county_monthly_tax_data,
                             rm_month_effects = FALSE,
                             module_exemptions_path = '/project2/igaarder/Data/modules_exemptions_long.csv'){
  assertDataTable(product_data)
  assertIntegerish(fips_county)
  assertIntegerish(fips_state)
  assertIntegerish(product_module_code)
  assertIntegerish(month_of_reform)
  assertIntegerish(year_of_reform)
  assertCharacter(product_name)
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

  if (!"price" %in% names(one_cty_prod)){
    one_cty_prod[, price := sales / quantity]
  }

  # then merge on tax
  one_cty_prod <- merge_tax_rates(sales_data = one_cty_prod,
                                  keep_taxable_only = F,
                                  county_monthly_tax_data = county_monthly_tax_data,
                                  module_exemptions_path = module_exemptions_path)

  # then calculate post-tax price
  one_cty_prod[, price_w_tax := (1 + applicable_tax) * price]
  if (any(one_cty_prod$applicable_tax == 0)){
    warning("This product does not appear to be taxable")
    print(head(one_cty_prod))
  }
  one_cty_prod[, event_time := year * 12 + month -
                 (year_of_reform * 12 + month_of_reform)]
  one_cty_prod <- make_fixed_weights(panel_data = one_cty_prod,
                                     weight_time = list(year = 2008,
                                                        month = 1),
                                     weight_var = "sales",
                                     panel_unit_vars = "store_code_uc")
  note <- NULL
  if (rm_month_effects){
    pre_data <- one_cty_prod[event_time < 0]
    price_model <- lm(data = pre_data, formula = as.formula(price ~ factor(month)))
    taxed_model <- lm(data = pre_data, formula = as.formula(price_w_tax ~ factor(month)))
    quantity_model <- lm(data = pre_data, formula = as.formula(quantity ~ factor(month)))
    sales_model <- lm(data = pre_data, formula = as.formula(sales ~ factor(month)))
    one_cty_prod[, price_predicted := predict(price_model, newdata = one_cty_prod, type = 'response')]
    one_cty_prod[, price := price - price_predicted]
    one_cty_prod[, price_w_tax_predicted := predict(taxed_model, newdata = one_cty_prod, type = 'response')]
    one_cty_prod[, price_w_tax := price_w_tax - price_w_tax_predicted]
    one_cty_prod[, quantity_predicted := predict(quantity_model, newdata = one_cty_prod, type = 'response')]
    one_cty_prod[, quantity := quantity - quantity_predicted]
    one_cty_prod[, sales_predicted := predict(sales_model, newdata = one_cty_prod, type = 'response')]
    one_cty_prod[, sales := sales - sales_predicted]

    note <- "Note: Month effects have estimated on the pre-period and residualized out."
  }

  one_cty_prod <- one_cty_prod[event_time <= 24 & event_time >= -24]


  # then, collapse
  one_cty_prod_collapsed <- one_cty_prod[, list(mean_price = weighted.mean(x = price,
                                                                           w = sales.weight),
                                                mean_price.taxed = weighted.mean(x = price_w_tax,
                                                                                 w = sales.weight),
                                                total_sales = sum(sales),
                                                total_quantity = sum(quantity),
                                                n_stores = .N),
                                         by = c("event_time")]

  y_lab <- paste0("Price of ", product_name)
  y_lab_s <- paste0("Sales of ", product_name)
  y_lab_q <- paste0("Quantity of ", product_name)

  price_graph <- ggplot(data = one_cty_prod_collapsed,
                        mapping = aes(x = event_time)) +
    geom_line(mapping = aes(y = mean_price, color = "Pre-tax")) +
    geom_line(mapping = aes(y = mean_price.taxed, color = "Post-tax")) +
    labs(x = "Month (relative to event time)", y = y_lab, color = "Price type", caption = note) +
    geom_vline(xintercept = 0) +
    theme_bw()

  sales_graph <- ggplot(data = one_cty_prod_collapsed,
                        mapping = aes(x = event_time, y = total_sales)) +
    geom_line() +
    labs(x = "Month (relative to event time)", y = y_lab_s, caption = note) +
    geom_vline(xintercept = 0) +
    theme_bw()

  quantity_graph <- ggplot(data = one_cty_prod_collapsed,
                        mapping = aes(x = event_time, y = total_quantity)) +
    geom_line() +
    labs(x = "Month (relative to event time)", y = y_lab_q, caption = note) +
    geom_vline(xintercept = 0) +
    theme_bw()

  return(list(price_graph = price_graph,
              sales_graph = sales_graph,
              quantity_graph = quantity_graph,
              collapsed_data = one_cty_prod_collapsed))
}
