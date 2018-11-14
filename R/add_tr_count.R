#' Collapse data while adding treatment count variable.
#'
#' @description \code{add_tr_count} takes collapsed data and adds a treatment
#'     group variable that reflects the number of observations in that treatment
#'     bin. If the counts are unequal within treatment bins, the function gives
#'     a warning and adds an asterisk (*) after the count variable, which now
#'     represents the minimum of all the counts within each treatment group bin.
#' @param collapsed_data Data that has been aggregated and includes a treatment
#'     group variable as well as a count variable on the treatment group level
#'     (data.table)
#' @param tr_group_name The name of the column in \code{collapsed_data} that
#'     contains the names of the corresponding treatment groups (character)
#' @param count_col_name The name of the column in \code{collapsed_data} that
#'     contains the treatment group-level observation counts (character)

add_tr_count <- function(collapsed_data, tr_group_name, count_col_name){
  assertDataTable(collapsed_data)
  assertCharacter(tr_group_name)
  assertCharacter(count_col_name)
  assertSubset(tr_group_name, names(collapsed_data))
  assertSubset(count_col_name, names(collapsed_data))

  collapsed_data[, tr_match := ifelse(
    max(get(count_col_name)) == min(get(count_col_name)), ")", "*)"),
    by = get(tr_group_name)]
  collapsed_data[, tr_count := paste0(get(tr_group_name),
                                      " (n = ",
                                      get(count_col_name),
                                      tr_match)]
  if ("*)" %in% collapsed_data$tr_match){
    print("Warning: count not equal across years within treatment groups.")
    print(with(collapsed_data, table(get(tr_group_name), get(count_col_name))))
  }

  collapsed_data[, tr_match := NULL]
  return(collapsed_data)
}
