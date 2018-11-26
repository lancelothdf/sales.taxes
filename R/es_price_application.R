#' Make event study graphs for prices (temporary file)

es_price_application <- function(price_data,
                                 treatment_data_path,
                                 county_pop_data = NULL,
                                 county_sales_weights = NULL,
                                 weighting_var,
                                 price_var,
                                 w_tax,
                                 fig_outfile = NULL){
  assertDataTable(price_data)
  assertCharacter(treatment_data_path)
  assertDataTable(county_pop_data, null.ok = T)
  assertDataTable(county_sales_weights, null.ok = T)
  assertCharacter(weighting_var)
  assertCharacter(price_var)
  assertLogical(w_tax)
  on.exit(traceback(1))

  print("Merging treatment...")
  # Before normalizing, need to identify event times...
  price_data <- merge_treatment(original_data = price_data,
                                treatment_data_path = treatment_data_path,
                                time = "event",
                                merge_by = c("fips_county", "fips_state"))
  print("Treatment merged!")
  # now we have time of treatment attached

  price_data[, tt_event := as.integer(12 * year + month - (12 * ref_year + ref_month))]
  # now we have an event time variable
  print("Normalizing pre-tax price...")
  price_data <- normalize_price(price_data = price_data,
                                time_type = "event",
                                base_time = -1,
                                price_var = "price",
                                new_price_var = "normalized_price")
  print("Normalizing post-tax price...")
  price_data <- normalize_price(price_data = price_data,
                                time_type = "event",
                                base_time = -1,
                                price_var = "price_w_tax",
                                new_price_var = "normalized_price_w_tax")

  print("Aggregating to county x product level...")
  # Aggregate to county x product level
  product_by_county_prices <- price_data[, list(mld_price = mean(normalized_price),
                                                mld_price_w_tax = mean(normalized_price_w_tax),
                                                mld_price.wtd = weighted.mean(x = normalized_price,
                                                                              w = sales.weight),
                                                mld_price_w_tax.wtd = weighted.mean(x = normalized_price_w_tax,
                                                                                    w = sales.weight),
                                                total_sales = sum(sales),
                                                n_stores = .N),
                                               by = c("fips_state", "fips_county",
                                                      "product_module_code",
                                                      "tt_event", "tr_group")]

  product_by_county_prices <- product_by_county_prices[tt_event >= -24 & tt_event <= 24]

  print("Adding weights...")
  if (!is.null(county_sales_weights)){
    product_by_county_prices <- merge(product_by_county_prices,
                                      county_sales_weights,
                                      by = c("fips_state", "fips_county",
                                             "product_module_code"))
  }


  if (!is.null(county_pop_data)){
    product_by_county_prices <- merge(product_by_county_prices,
                                      county_pop,
                                      by = c("fips_state", "fips_county"))
  }

  price_data[, weights := get(weighting_var)]
  price_data[, price_var := get(price_var)]

  if (w_tax){
    y_label <- "Mean normalized ln(price) (post-tax)"
  } else {
    y_label <- "Mean normalized ln(price) (pre-tax)"
  }

  print("Collapsing and graphing...")
  price_panel_not_na <- price_panel[!is.na(price_var)]
  # price_panel[, tt_event := as.integer(12 * year + month - (12 * ref_year + ref_month))]
  # price_panel <- price_panel[tt_event >= -24 & tt_event <= 24]
  es_price_collapsed <- price_panel_not_na[, list(mean_ln_price = weighted.mean(x = price_var,
                                                                                w = weights),
                                                  n_counties = uniqueN(1000 * fips_state + fips_county),
                                                  n_stores = sum(n_stores)),
                                           by = c("tr_group", "tt_event")]
  # add number of counties
  es_price_collapsed <- add_tr_count(collapsed_data = es_price_collapsed,
                                     tr_group_name = "tr_group",
                                     count_col_name = "n_counties")

  log_prices <- ggplot(data = es_price_collapsed, mapping = aes(x = tt_event,
                                                                y = mean_ln_price,
                                                                color = tr_count)) +
    labs(x = "Month", y = y_label, color = "Sales tax change",
         caption = "Note: Price is normalized by subtracting the log of the price in event time t-1 from the log price")
  geom_line() +
    theme_bw()
  if (!is.null(fig_outfile)){
    ggsave(fig_outfile, plot = log_prices)
  }


  return(log_prices)
}
