#' Merge treatment onto county-level sales data.
#'
#' @description This function takes a dataset without treatment info and merges
#'     another dataset with treatment information onto the first dataset.
#'
#' @param original_data The panel data (without treatment status) (data.table)
#' @param treatment_data_path The path to the treatment data (character)
#' @param time Either \code{"calendar"} or \code{"event"} (character)
#' @param merge_by A vector of the names of the units to merge
#'     \code{original_data} and the treatment data by (character)
#'
#' allow by to be either "calendar" or "event"
#' merge_by = c("fips_county", "fips_state")


merge_treatment <- function(original_data,
                            treatment_data_path,
                            time = "calendar",
                            merge_by){
  assertDataTable(original_data)
  assertCharacter(treatment_data_path)
  assertCharacter(time)
  assertCharacter(merge_by)
  if (!all(merge_by %in% names(original_data))){
    stop("`merge_by' variables must be columns in `original_data'")
  }
  # I originally intended the function to be different for calendar time and
  # event time data. However, that has yet to prove necessary.
  if (time == "calendar" | time == "event"){
    tr_groups <- fread(treatment_data_path)
    if (!all(merge_by %in% names(tr_groups))){
      stop("`merge_by' variables must be columns in the treatment data")
    }
    original_data[, original := 1]
    tr_groups[, additional := 1]

    # by calendar time
    # Note that such an m:m merge is usually bad form. However, in this case,
    # we want to merge every store-module-time observation to all possible
    # times of treatment.

    cal_panel_data <- merge(original_data, tr_groups,
                            by = merge_by,
                            all = TRUE,
                            allow.cartesian = TRUE)

    cal_panel_data <- cal_panel_data[!is.na(original) & !is.na(additional)]
    cal_panel_data[, original := NULL]
    cal_panel_data[, additional := NULL]
    return(cal_panel_data)
  } else {
    stop("`time' must be either 'calendar' or 'event'")
  }
}


