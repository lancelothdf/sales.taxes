#' Make event study graphs for sales (temporary file)

sales_application <- function(sales_data,
                              treatment_data_path,
                              time,
                              fig_outfile = NULL){
  assertDataTable(sales_data)
  assertCharacter(treatment_data_path)
  assertCharacter(time)

  sales_panel_ct <- merge_treatment(original_data = sales_data,
                                    treatment_data_path = treatment_data_path,
                                    time = time,
                                    merge_by = c("fips_county", "fips_state"))

  if (time == "calendar"){
    sales_collapsed <- sales_panel_ct[, list(mean_log_sales = weighted.mean(x = ln_total_sales,
                                                                            w = population),
                                             n_counties = uniqueN(1000 * fips_state + fips_county),
                                             n_stores = sum(n_stores)),
                                      by = c("tr_group", "year", "month")]

    sales_collapsed <- add_tr_count(collapsed_data = sales_collapsed,
                                    tr_group_name = "tr_group",
                                    count_col_name = "n_counties")

    sales_collapsed$year_month <- as.yearmon(paste(as.integer(sales_collapsed$year),
                                                   as.integer(sales_collapsed$month)),
                                             "%Y %m")

    # Then it can be easily graphed across time periods.

    log_sales <- ggplot(data = sales_collapsed, mapping = aes(x = year_month,
                                                              y = mean_log_sales,
                                                              color = tr_count)) +
      labs(x = "Month", y = "Mean ln(sales)", color = "Sales tax change") +
      scale_x_yearmon(format = "%b %Y") +
      geom_line() +
      theme_bw()
    if (!is.null(fig_outfile)){
      ggsave(fig_outfile, plot = log_sales)
    }
  } else if (time == "event"){
    sales_panel_es[, tt_event := as.integer(12 * year + month - (12 * ref_year + ref_month))]
    sales_panel_es <- sales_panel_es[tt_event >= -24 & tt_event <= 24]
    es_sales_collapsed <- sales_panel_es[, list(mean_log_sales = weighted.mean(x = ln_total_sales,
                                                                               w = population),
                                                n_counties = uniqueN(1000 * fips_state + fips_county),
                                                n_stores = sum(n_stores)),
                                         by = c("tr_group", "tt_event")]
    # add number of counties
    es_sales_collapsed <- add_tr_count(collapsed_data = es_sales_collapsed,
                                       tr_group_name = "tr_group",
                                       count_col_name = "n_counties")

    log_sales <- ggplot(data = es_sales_collapsed, mapping = aes(x = tt_event,
                                                                    y = mean_log_sales,
                                                                    color = tr_count)) +
      labs(x = "Time to reform", y = "Mean ln(sales)", color = "Sales tax change") +
      geom_line() +
      theme_bw()
    if (!is.null(fig_outfile)){
      ggsave(fig_outfile, plot = log_sales)
    }
  } else {
    stop("`time' must be either 'calendar' or 'event'")
  }
  return(log_sales)
}
