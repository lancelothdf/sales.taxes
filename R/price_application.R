#' Make event study graphs for prices (temporary file)

price_application <- function(price_data,
                              treatment_data_path,
                              time,
                              weighting_var,
                              pretax_var,
                              posttax_var,
                              w_tax,
                              fig_outfile = NULL){
  assertDataTable(price_data)
  assertCharacter(treatment_data_path)
  assertCharacter(time)
  assertLogical(w_tax)

  price_data[, weights := get(weighting_var)]
  price_data[, pretax_price_var := get(pretax_var)]
  price_data[, posttax_price_var := get(posttax_var)]
  if (time == "calendar"){
    price_panel <- merge_treatment(original_data = price_data,
                                   treatment_data_path = treatment_data_path,
                                   time = time,
                                   merge_by = c("fips_county", "fips_state"))
  }

  if (!w_tax){
    if (time == "calendar"){
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
             caption = "Note: Price is normalized by subtracting the log of the price in Jan 2008 from the log price") +
        scale_x_yearmon(format = "%b %Y") +
        geom_line() +
        theme_bw()
      if (!is.null(fig_outfile)){
        ggsave(fig_outfile, plot = log_prices)
      }
    } else if (time == "event"){
      price_panel_not_na <- price_panel[!is.na(pretax_price_var)]
      # price_panel[, tt_event := as.integer(12 * year + month - (12 * ref_year + ref_month))]
      # price_panel <- price_panel[tt_event >= -24 & tt_event <= 24]
      es_price_collapsed <- price_panel_not_na[, list(mean_ln_price = weighted.mean(x = pretax_price_var,
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
        labs(x = "Month", y = "Mean normalized ln(price) (pre-tax)", color = "Sales tax change",
             caption = "Note: Price is normalized by subtracting the log of the price in event time t-1 from the log price")
      geom_line() +
        theme_bw()
      if (!is.null(fig_outfile)){
        ggsave(fig_outfile, plot = log_prices)
      }
    } else {
      stop("`time' must be either 'calendar' or 'event'")
    }
  } else if (w_tax){
    if (time == "calendar"){
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
        labs(x = "Month", y = "Mean normalized ln(price) (post-tax)", color = "Sales tax change",
             caption = "Note: Price is normalized by subtracting the log of the price in Jan 2008 from the log price") +
        scale_x_yearmon(format = "%b %Y") +
        geom_line() +
        theme_bw()
      if (!is.null(fig_outfile)){
        ggsave(fig_outfile, plot = log_prices)
      }
    } else if (time == "event"){
      price_panel_not_na <- price_panel[!is.na(posttax_price_var)]
      # price_panel[, tt_event := as.integer(12 * year + month - (12 * ref_year + ref_month))]
      # price_panel <- price_panel[tt_event >= -24 & tt_event <= 24]
      es_price_collapsed <- price_panel_not_na[, list(mean_ln_price = weighted.mean(x = posttax_price_var,
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
        labs(x = "Month", y = "Mean normalized ln(price) (post-tax)", color = "Sales tax change",
             caption = "Note: Price is normalized by subtracting the log of the price in event time t-1 from the log price")
      geom_line() +
        theme_bw()
      if (!is.null(fig_outfile)){
        ggsave(fig_outfile, plot = log_prices)
      }
    } else {
      stop("`time' must be either 'calendar' or 'event'")
    }
  }

  return(log_prices)
}
