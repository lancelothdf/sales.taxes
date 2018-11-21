#' Create weights based on a fixed time period
#'
#' @description \code{make_fixed_weights} creates a weight for panel units
#'     based on a specific time period in the sample.
#' @param panel_data The data for which weights are to be calculated (data.table)
#' @param weight_time The time period(s) that correspond to the time period of
#'     the weights, as a list. For example, if the time period were
#'     \code{year == 2008 & month == 1}, it would be correct to have
#'     \code{weight_time = list(year = 2008, month = 1)} (list)
#' @param weight_var The name of the variable in \code{panel_data} that will
#'     be used to create the weight (character)
#' @param panel_unit_vars The name(s) of the variables in \code{panel_data} that
#'     uniquely identify the panel units for which the weights are to be
#'     calculated (character)
#'

make_fixed_weights <- function(panel_data, weight_time, weight_var,
                               panel_unit_vars){
  assertDataTable(panel_data)
  assertList(weight_time)
  assertCharacter(weight_var)
  assertCharacter(panel_unit_vars)

  assertSubset(weight_var, names(panel_data))
  assertSubset(panel_unit_vars, names(panel_data))
  assertSubset(names(weight_time), names(panel_data))

  time_conditions <- ""
  for (x in 1:length(weight_time)){
    time_conditions <- paste0(time_conditions,
                              names(weight_time)[x],
                              " == ",
                              weight_time[[x]])
    if (x < length(weight_time)){
      time_conditions <- paste0(time_conditions, " & ")
    }
  }
  weight_data <- eval(parse(text = paste0("panel_data[",
                                          time_conditions,
                                          "]")))
  eval(parse(text = paste0("weight_data[,", weight_var, ".weight := ", weight_var, "]")))
  weight_data <- eval(parse(text = paste0("weight_data[, .(",
                                          paste(panel_unit_vars, collapse = ", "),
                                          ",", weight_var, ".weight)]")))
  panel_data <- merge(panel_data, weight_data, by = panel_unit_vars)
  return(panel_data)
}
