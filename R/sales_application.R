#' Make event study graphs for sales (temporary file)

sales_application <- function(sales_data,
                              treatment_data_path,
                              time,
                              pre_post_periods = NULL,
                              fig_outfile = NULL,
                              quarterly = F,
                              pop_weights,
                              create_es_control = F,
                              control_counties = NULL){
  assertDataTable(sales_data)
  assertCharacter(treatment_data_path)
  assertCharacter(time)
  assertCharacter(fig_outfile, null.ok = T)
  assertLogical(quarterly)
  assertLogical(pop_weights)
  assertLogical(create_es_control)
  assertDataTable(control_counties, null.ok = T)
  assertNumeric(pre_post_periods, null.ok = T)

  fig_note <- NULL

  if (pop_weights & create_es_control){
    stop("Using population weights and creating an event study control is currently not an option.")
  }
  if (time == "calendar" & create_es_control){
    print("Note: You specified `time == 'calendar'` and `create_es_control == T`. Control groups are always automatically included in calendar time graphs.")
  }
  if (create_es_control & is.null(control_counties) & time == "event"){
    stop("To create an event study control group, a data.table of control observations must be provided")
  }
  if (time == "event" & is.null(pre_post_periods)){
    stop("User must specify how many pre- (and, implicitly, post-) periods to include in the event plot.")
  }

  if (quarterly){
    time_var <- "quarter"
  } else {
    time_var <- "month"
  }

  sales_panel <- merge_treatment(original_data = sales_data,
                                 treatment_data_path = treatment_data_path,
                                 time = time,
                                 merge_by = c("fips_county", "fips_state"))
  if (quarterly & time == "event"){
    sales_panel[, ref_quarter := ceiling(ref_month / 3)]
  }

  if (time == "calendar"){
    if (pop_weights){
      sales_collapsed <- sales_panel[, list(mean_log_sales = weighted.mean(x = ln_total_sales,
                                                                           w = population),
                                            n_counties = uniqueN(1000 * fips_state + fips_county)),
                                     by = c("tr_group", "year", time_var)]
    } else {
      sales_collapsed <- sales_panel[, list(mean_log_sales = mean(ln_total_sales),
                                            n_counties = uniqueN(1000 * fips_state + fips_county)),
                                     by = c("tr_group", "year", time_var)]
    }


    sales_collapsed <- add_tr_count(collapsed_data = sales_collapsed,
                                    tr_group_name = "tr_group",
                                    count_col_name = "n_counties")

    if (!quarterly){
      sales_collapsed$year_month <- as.yearmon(paste(as.integer(sales_collapsed$year),
                                                     as.integer(sales_collapsed$month)),
                                               "%Y %m")

      log_sales <- ggplot(data = sales_collapsed, mapping = aes(x = year_month,
                                                                y = mean_log_sales,
                                                                color = tr_count)) +
        labs(x = "Month", y = "Mean ln(sales)", color = "Sales tax change") +
        scale_x_yearmon(format = "%b %Y") +
        geom_line() +
        theme_bw()
    } else {
      sales_collapsed$year_qtr <- as.yearqtr(paste(as.integer(sales_collapsed$year),
                                                     as.integer(sales_collapsed$quarter)),
                                               "%Y %q")

      log_sales <- ggplot(data = sales_collapsed, mapping = aes(x = year_qtr,
                                                                y = mean_log_sales,
                                                                color = tr_count)) +
        labs(x = "Quarter", y = "Mean ln(sales)", color = "Sales tax change") +
        scale_x_yearqtr(format = "%Y-%q") +
        geom_line() +
        theme_bw()
    }

    if (!is.null(fig_outfile)){
      ggsave(fig_outfile, plot = log_sales)
    }
  } else if (time == "event"){
    if (!quarterly){
      sales_panel[, tt_event := as.integer(12 * year + month - (12 * ref_year + ref_month))]
      sales_panel <- sales_panel[tt_event >= -1 * pre_post_periods & tt_event <= pre_post_periods]

    } else {
      sales_panel[, tt_event := as.integer(3 * year + quarter - (3 * ref_year + ref_quarter))]
      sales_panel <- sales_panel[tt_event >= -1 * pre_post_periods & tt_event <= pre_post_periods]
    }
    if (create_es_control){
      time_var <- ifelse(quarterly, "quarter", "month")
      sales_panel <- create_pseudo_control(es_data = sales_panel,
                                           original_data = sales_data,
                                           control_counties = control_counties,
                                           time_var = time_var)
      fig_note <- "Note: The control groups are created by merge the 'no change' group average to treated counties
      by calendar time, then taking the event-time averages over these calendar-time averages."
    }

    if (pop_weights){
      es_sales_collapsed <- sales_panel[, list(mean_log_sales = weighted.mean(x = ln_total_sales,
                                                                              w = population),
                                               n_counties = uniqueN(1000 * fips_state + fips_county)),
                                        by = c("tr_group", "tt_event")]
    } else {
      es_sales_collapsed <- sales_panel[, list(mean_log_sales = mean(ln_total_sales),
                                               n_counties = uniqueN(1000 * fips_state + fips_county)),
                                        by = c("tr_group", "tt_event")]
    }

    # add number of counties
    es_sales_collapsed <- add_tr_count(collapsed_data = es_sales_collapsed,
                                       tr_group_name = "tr_group",
                                       count_col_name = "n_counties")

    log_sales <- ggplot(data = es_sales_collapsed, mapping = aes(x = tt_event,
                                                                 y = mean_log_sales,
                                                                 color = tr_count)) +
      labs(x = "Time to reform", y = "Mean ln(sales)", color = "Sales tax change", caption = fig_note) +
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
