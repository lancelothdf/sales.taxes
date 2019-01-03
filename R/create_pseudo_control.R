#' Create pseudo-control observations for event-study plots
#'
#' @description \code{create_pseudo_control} creates a set of control observations
#'     for event study graphs based on the control average at the corresponding
#'     calendar time.
#'
#' @param es_data The dataset of event-study like data (data.table)
#' @param original_data The original data (that still includes the control
#'     group data) (data.table)
#' @param control_counties A dataset of identifying information (state and county
#'     fips codes) for control counties (data.table)
#' @param time_var The type of time variable (should be \code{"quarter"} or \code{"month"})
#'
#' @return A modified version of \code{es_data} that has matched each treated
#'     observation to a control observation mean and added those "matched"
#'     observations as new observations in the dataset.
#'

create_pseudo_control <- function(es_data, original_data, control_counties, time_var){
  assertDataTable(es_data)
  assertDataTable(original_data)
  assertSubset(time_var, c("quarter", "month"))
  assertSubset(c("tt_event", time_var, "year"), names(es_data))
  assertSubset(c("fips_state", "fips_county"), names(control_counties))
  assertSubset(c(time_var, "year"), names(original_data))

  control_dt <- merge(original_data, control_counties, by = c("fips_state",
                                                              "fips_county"))
  ## Take the mean for each time period
  control_dt <- control_dt[, list(control_ln_total_sales = mean(ln_total_sales)),
                           by = c(time_var, "year")]

  es_data <- merge(es_data, control_dt, by = c(time_var, "year"))
  matched_control_data <- es_data[, .(control_ln_total_sales, tt_event, tr_group)]

  matched_control_data[, ln_total_sales := control_ln_total_sales]
  matched_control_data[, tr_group := paste0("No change (", tr_group, ")")]
  matched_control_data[, control_ln_total_sales := NULL]
  es_data <- rbind(es_data, matched_control_data, fill = T)
  return(es_data)
}
