#' Balance panel data.
#'
#' @description This function takes a possibly unbalanced panel and returns
#'     a balanced panel over a given time period.
#' @param panel_data The data to be balanced (data.table).
#' @param n_periods The number of time periods in the dataset (numeric).
#' @param panel_unit The name of the panel unit (character).
#'

balance_panel_data <- function(panel_data, n_periods, panel_unit){
  assertDataTable(panel_data)
  assertNumeric(n_periods)
  assertCharacter(panel_unit)
  assertSubset(panel_unit, names(panel_data))


  to_keep <- unique(panel_data, by = c("month", "year", panel_unit)) # check this
  to_keep[, count := .N, by = eval(panel_unit)]
  to_keep <- unique(to_keep[count == n_periods, eval(parse(text = panel_unit))])
  panel_data <- panel_data[eval(parse(text = panel_unit)) %in% to_keep]
  return(panel_data)
}

