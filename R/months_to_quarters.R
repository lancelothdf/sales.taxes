#' Aggregate monthly data to quarterly data
#'
#' @description \code{months_to_quarters} takes a monthly dataset and sums up
#'     a given variable to the quarterly level.
#' @param monthly_data Dataset on month-level (data.table)
#' @param month_var The name of the month variable in \code{monthly_data} (character)
#' @param collapse_by The name(s) of the variable(s) that help define the
#'     collapse (in addition to the seasons). Ideally, these variables will be
#'     unique on the quarter-level (character)
#' @param collapse_var The name of the variable to be summed up over the seasons (character)
#' @param weights Not currently in use.

months_to_quarters <- function(monthly_data, month_var, collapse_by,
                               collapse_var, weights = F){
  assertDataTable(monthly_data)
  assertCharacter(month_var)
  assertCharacter(collapse_var)
  assertSubset(month_var, names(monthly_data))
  assertSubset(collapse_var, names(monthly_data))
  assertSubset("year", names(monthly_data))
  assertNumeric(monthly_data[, get(month_var)])
  warning("`months_to_quarters()` currently aggregates by summing over months")

  quarters <- data.table(month = 1:12, quarter = rep(1:4, each = 3))
  quarters[, (month_var) := month]

  if (weights){
    stop("Weights have not yet been incorporated")
  }

  monthly_data <- merge(monthly_data, quarters, by = month_var)
  quarterly_data <- monthly_data[, .(sum(get(collapse_var))),
                                 by = c("quarter", "year", collapse_by)]
  quarterly_data[, (collapse_var) := V1]
  quarterly_data[, V1 := NULL]
  return(quarterly_data)
}
