#' Analyze outcome for one county, one product
#'
#' @description \code{one_cty_one_prod} analyzes the given data for a single
#'     county for a single product type. The user must provide identifying
#'     information for the county and for either a product module code or a UPC
#'     (but not both).
#' @param product_data Dataset to be analyzed (data.table). Should have columns
#'     with \code{fips_county}, \code{fips_state}, \code{product_module_code}
#' @param fips_state The state fips code of the desired state (integer)
#' @param fips_county The county fips code of the desired county (integer)
#' @param product_module_code The product module code of the desired product
#'     (integer)
#' @param upc The name of the UPC code (if product is given on UPC level) (integer).
#'     \code{upc} may be a list if you would like to average over multiple
#'     UPCs (if, for example, a brand replaces a product with another, similar
#'     product)
#' @param product_name The name of the product (for graph labeling) (character)
#' @param month_of_reform The month of the tax reform (integer)
#' @param year_of_reform The year of the tax reform (integer)
#' @param county_monthly_tax_data Dataset with tax rates (data.table)
#' @param rm_month_effects Remove month seasonality? (logical)
#' @param module_exemptions_path The path to the .csv file containing a long
#'     dataset of state-product level exemptions/special tax rates. The default
#'     is "/project2/igaarder/Data/modules_exemptions_long.csv"
#' @param balance_panel Force the panel to be balanced? (logical)
#' @return A list including a \code{price_graph}, a \code{sales_graph}, and the
#'     data collapsed to event-time level (\code{})
#'

#Hmm, how can this be modified to handle UPC level data?
# Could have option allowing user to choose whether level == "upc" or "product",
# then based on what the user specifies
one_cty_one_prod <- function(product_data,
                             fips_state,
                             fips_county,
                             product_module_code = NULL,
                             upc = NULL,
                             product_name,
                             month_of_reform,
                             year_of_reform,
                             county_monthly_tax_data,
                             rm_month_effects = FALSE,
                             module_exemptions_path = '/project2/igaarder/Data/modules_exemptions_long.csv',
                             balance_panel = T){
  #TODO: add checks for upc argument being integerish
  assertDataTable(product_data)
  assertIntegerish(fips_county)
  assertIntegerish(fips_state)
  assertIntegerish(product_module_code, null.ok = T)
  assertIntegerish(month_of_reform)
  assertIntegerish(year_of_reform)
  assertCharacter(product_name)
  assertLogical(balance_panel)
  assertNumeric(upc, null.ok = T)
  if (is.null(upc) & is.null(product_module_code)){
    stop("Both `upc` and `product_module_code` cannot be NULL. Exactly one must be provided.")
  } else if (!is.null(upc) & !is.null(product_module_code)){
    stop("One of `upc` and `product_module_code` must be NULL. Either keep all of a product type or all of a UPC. See documentation.")
  }

  # I define a variable with a different name containing fips_state and
  # fips_county (etc.) values because using the same name as the data.table
  # column causes issues

  fs_val <- fips_state
  fc_val <- fips_county
  if (!is.null(upc)){
    assertSubset(c("fips_county", "fips_state", "store_code_uc", "quantity", "sales",
                   "upc", "year", "month"), names(product_data))
    upc_val <- upc
    one_cty_prod <- product_data[fips_state == fs_val &
                                 fips_county == fc_val &
                                 upc %in% upc_val]
  } else if (!is.null(product_module_code)){
    assertSubset(c("fips_county", "fips_state", "store_code_uc", "quantity", "sales",
                   "product_module_code", "year", "month"), names(product_data))
    pmc_val <- product_module_code
    one_cty_prod <- product_data[fips_state == fs_val &
                                 fips_county == fc_val &
                                 product_module_code == pmc_val]
  }

  # TODO: fix what happens when balance_panel = F (currently doesn't work)
  if (min(one_cty_prod$year) < 2008 | max(one_cty_prod$year) > 2014){
    warning("Dropping all data from before 2008 and after 2014")
    one_cty_prod <- one_cty_prod[year >= 2008 & year <= 2014]
  }
  if (balance_panel){
    one_cty_prod <- balance_panel_data(one_cty_prod,
                                       panel_unit = "store_code_uc",
                                       n_periods = 84)
  }
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
  one_cty_prod_collapsed <- one_cty_prod[, list(mean_price = mean(price),
                                                mean_price.taxed = mean(price_w_tax),
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
