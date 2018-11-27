#' Make event study graphs for prices (temporary file)

price_application <- function(price_data,
                              treatment_data_path,
                              time,
                              weighting_var,
                              pretax_var,
                              posttax_var,
                              w_tax,
                              resid_type = NULL,
                              fig_outfile = NULL){
  assertDataTable(price_data)
  assertCharacter(treatment_data_path)
  assertCharacter(time)
  assertLogical(w_tax)
  assertSubset(resid_type, c("A", "B", "C", "D", "E"))

  if (!is.null(resid_type) & !w_tax){
    stop("Currently, residualization has only been implemented for post-tax prices")
  }

  note <- "Note: Price is normalized by subtracting the log of the price in Jan 2008 from the log price"
  posttax_y <- "Mean normalized ln(price) (post-tax)"

  price_data[, weights := get(weighting_var)]
  price_data[, pretax_price_var := get(pretax_var)]
  price_data[, posttax_price_var := get(posttax_var)]

  if (!is.null(resid_type)){
    posttax_y <- "Mean normalized residual ln(price) (post-tax)"
    if (resid_type == "A"){
      price_panel <- remove_time_trends(input_data = price_data,
                                        outcome_var = "posttax_price_var",
                                        month_var = "month",
                                        year_var = "year",
                                        month_dummies = FALSE,
                                        calendar_time = FALSE,
                                        product_group_trend = FALSE,
                                        weight_var = "weights")
      price_panel[, posttax_price_var_old := posttax_price_var]
      price_panel[, posttax_price_var := posttax_price_var_residual]
      note <- "Note: Price is normalized by subtracting the log of the price in Jan 2008 from the log price.
      A linear time trend has been residualized out."
    } else if (resid_type == "B"){
      price_panel <- remove_time_trends(input_data = price_data,
                                        outcome_var = "posttax_price_var",
                                        month_var = "month",
                                        year_var = "year",
                                        month_dummies = FALSE,
                                        calendar_time = FALSE,
                                        product_group_trend = TRUE,
                                        weight_var = "weights")
      price_panel[, posttax_price_var_old := posttax_price_var]
      price_panel[, posttax_price_var := posttax_price_var_residual]
      note <- "Note: Price is normalized by subtracting the log of the price in Jan 2008 from the log price.
      A linear time trend has been residualized out on each product-group level."
    } else if (resid_type == "C"){
      price_panel <- remove_time_trends(input_data = price_data,
                                        outcome_var = "posttax_price_var",
                                        month_var = "month",
                                        year_var = "year",
                                        month_dummies = TRUE,
                                        calendar_time = FALSE,
                                        product_group_trend = FALSE,
                                        weight_var = "weights")
      price_panel[, posttax_price_var_old := posttax_price_var]
      price_panel[, posttax_price_var := posttax_price_var_residual]
      note <- "Note: Price is normalized by subtracting the log of the price in Jan 2008 from the log price.
      Residualization was done by removing a linear time trend as well as month-of-year effects."
    } else if (resid_type == "D"){
      price_panel <- remove_time_trends(input_data = price_data,
                                        outcome_var = "posttax_price_var",
                                        month_var = "month",
                                        year_var = "year",
                                        month_dummies = TRUE,
                                        calendar_time = FALSE,
                                        product_group_trend = TRUE,
                                        weight_var = "weights")
      price_panel[, posttax_price_var_old := posttax_price_var]
      price_panel[, posttax_price_var := posttax_price_var_residual]
      note <- "Note: Price is normalized by subtracting the log of the price in Jan 2008 from the log price.
      Residualization was done by removing a linear time trend as well as month-of-year effects
      on each product-group level."
    } else if (resid_type == "E"){
      price_panel <- remove_time_trends(input_data = price_data,
                                        outcome_var = "posttax_price_var",
                                        month_var = "month",
                                        year_var = "year",
                                        month_dummies = FALSE,
                                        calendar_time = TRUE,
                                        product_group_trend = FALSE,
                                        weight_var = "weights")
      price_panel[, posttax_price_var_old := posttax_price_var]
      price_panel[, posttax_price_var := posttax_price_var_residual]
      note <- "Note: Price is normalized by subtracting the log of the price in Jan 2008 from the log price.
      Residualization was done by removing calendar time (month-year) effects."
    }
  }

  price_panel <- merge_treatment(original_data = price_panel,
                                 treatment_data_path = treatment_data_path,
                                 time = time,
                                 merge_by = c("fips_county", "fips_state"))


  if (!w_tax){
      price_panel_not_na <- price_panel[!is.na(pretax_price_var)]
      price_collapsed <- price_panel_not_na[, list(mean_ln_price = weighted.mean(x = pretax_price_var,
                                                                                 w = weights),
                                            n_counties = uniqueN(1000 * fips_state + fips_county),
                                            n_stores = sum(n_stores)),
                                     by = c("tr_group", "year", "month")]

      price_collapsed <- add_tr_count(collapsed_data = price_collapsed,
                                      tr_group_name = "tr_group",
                                      count_col_name = "n_counties")

      price_collapsed$year_month <- as.yearmon(paste(as.integer(price_collapsed$year),
                                                     as.integer(price_collapsed$month)),
                                               "%Y %m")

      # Then it can be easily graphed across time periods.

      log_prices <- ggplot(data = price_collapsed, mapping = aes(x = year_month,
                                                                 y = mean_ln_price,
                                                                 color = tr_count)) +
        labs(x = "Month", y = "Mean normalized ln(price) (pre-tax)", color = "Sales tax change",
             caption = note) +
        scale_x_yearmon(format = "%b %Y") +
        geom_line() +
        theme_bw()
      if (!is.null(fig_outfile)){
        ggsave(fig_outfile, plot = log_prices)
      }
  } else if (w_tax){
      price_panel_not_na <- price_panel[!is.na(posttax_price_var)]
      price_collapsed <- price_panel_not_na[, list(mean_ln_price = weighted.mean(x = posttax_price_var,
                                                                                 w = weights),
                                            n_counties = uniqueN(1000 * fips_state + fips_county),
                                            n_stores = sum(n_stores)),
                                     by = c("tr_group", "year", "month")]

      price_collapsed <- add_tr_count(collapsed_data = price_collapsed,
                                      tr_group_name = "tr_group",
                                      count_col_name = "n_counties")

      price_collapsed$year_month <- as.yearmon(paste(as.integer(price_collapsed$year),
                                                     as.integer(price_collapsed$month)),
                                               "%Y %m")

      # Then it can be easily graphed across time periods.

      log_prices <- ggplot(data = price_collapsed, mapping = aes(x = year_month,
                                                                 y = mean_ln_price,
                                                                 color = tr_count)) +
        labs(x = "Month", y = posttax_y, color = "Sales tax change",
             caption = note) +
        scale_x_yearmon(format = "%b %Y") +
        geom_line() +
        theme_bw()
      if (!is.null(fig_outfile)){
        ggsave(fig_outfile, plot = log_prices)
      }
  }
  return(log_prices)
}
