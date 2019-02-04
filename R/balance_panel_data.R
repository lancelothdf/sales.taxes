#' Balance panel data.
#'
#' @description This function takes a possibly unbalanced panel and returns
#'     a balanced panel over a given time period.
#' @param panel_data The data to be balanced (data.table).
#' @param n_periods The number of time periods in the dataset (numeric).
#' @param time_vars A vector of the variables that together uniquely identify
#'     time periods (e.g., default is \code{c("month", "year")}, referring to
#'     a dataset with monthly observations that has \code{month} and \code{year}
#'     columns) (character).
#' @param panel_unit The name of the column identifying the panel unit (character).
#'

balance_panel_data <- function(panel_data, n_periods,
                               time_vars = c("month", "year"), panel_unit){
  assertDataTable(panel_data)
  assertNumeric(n_periods)
  assertCharacter(panel_unit)
  assertCharacter(time_vars)
  assertSubset(panel_unit, names(panel_data))
  assertSubset(time_vars, names(panel_data))

  to_keep <- unique(panel_data, by = c(time_vars, panel_unit))
  to_keep[, count := .N, by = eval(panel_unit)]
  to_keep <- unique(to_keep[count == n_periods, eval(parse(text = panel_unit))])
  original_length <- nrow(panel_data)
  panel_data <- panel_data[eval(parse(text = panel_unit)) %in% to_keep]
  n_dropped <- original_length - nrow(panel_data)

  if (n_dropped > 0){
    warning(paste0(n_dropped, " out of ", original_length, " observations dropped when balancing panel"))
  }
  return(panel_data)
}
