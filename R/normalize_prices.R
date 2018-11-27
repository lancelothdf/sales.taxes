#' Normalize prices
#'
#' @description \code{normalize_price} takes a dataset with prices and dates
#'     or event times and normalizes, using a given date/event time as the
#'     base.
#' @param price_data A dataset that includes prices to be normalized as well as
#'     times of each price (data.table)
#' @param time_type Either \code{"calendar"} or \code{"event"}. The type of time
#'     variable used in the data (character)
#' @param base_time The time of the base price. If \code{time_type = "event"},
#'     \code{base_time} should be a single integer corresponding to the event time.
#'     If \code{time_type = "calendar"}, \code{base_time} should be a vector of
#'     two integers, the first corresponding to the year, the second corresponding
#'     to the month (numeric)
#' @param price_var The name of the original price variable found in
#'     \code{price_data} (character)
#' @param new_price_var The desired name of the new, normalized price variable
#'     (default is just \code{"normalized_price"})
#'

normalize_price <- function(price_data, time_type, base_time, price_var, new_price_var = "normalized_price"){
  assertDataTable(price_data)
  assertCharacter(time_type)
  assertIntegerish(base_time)
  assertCharacter(price_var)
  assertCharacter(new_price_var)
  assertSubset(time_type, c("event", "calendar"))
  assertSubset(price_var, names(price_data))
  assertSubset(c("store_code_uc", "product_module_code"), names(price_data))

  if (time_type == "event"){
    assertSubset("tt_event", names(price_data))
    price_anchors <- price_data[tt_event == base_time]

  } else if (time_type == "calendar"){
    assertSubset(c("year", "month"), names(price_data))
    assertSubset(base_time[1], 1990:2020)
    assertSubset(base_time[2], 1:12)
    base_year <- base_time[1]
    base_month <- base_time[2]
    price_anchors <- price_data[year == base_year & month == base_month]
  }

  price_anchors[, base_price := get(price_var)]

  # eval(parse(
  #   text = paste("price_anchors[, base_price :=", price_var, "]")
  # ))

  if (time_type == "event"){
    #TODO: the following code is for debugging
    # print(paste("Number of rows in price_anchors:", nrow(price_anchors[, .(store_code_uc, product_module_code,
    #                                                                        ref_year, ref_month)])))
    # print(paste("Number of unique rows in price_anchors:", nrow(unique(price_anchors[, .(store_code_uc, product_module_code,
    #                                                                                      ref_year, ref_month)]))))
    # print(paste("Number of unique rows (including base price) in price_anchors:", nrow(unique(price_anchors[, .(store_code_uc, product_module_code,
    #                                                                                                             ref_year, ref_month, base_price)]))))
    # fwrite(price_anchors, "/project2/igaarder/Data/price_anchors_check.csv")

    # print("Debug check 1 (normalize_prices): ready to merge price_anchors")
    price_anchors <- price_anchors[, .(store_code_uc, product_module_code, base_price,
                                       ref_year, ref_month, tr_group)]
    # price_dups <- price_anchors[duplicated(price_anchors, by = c("store_code_uc",
    #                                                              "product_module_code",
    #                                                              "ref_year",
    #                                                              "ref_month"))]
    # print(head(price_anchors[order(store_code_uc, product_module_code, ref_year, ref_month)], 30))

    # Need to be careful with merging the price anchors on...
    price_data <- merge(price_data, price_anchors,
                        by = c("store_code_uc", "product_module_code",
                               "ref_year", "ref_month", "tr_group"))
  } else if (time_type == "calendar"){
    price_anchors <- price_anchors[, .(store_code_uc, product_module_code, base_price)]
    # It's ok to just merge on store and product, since store x product gives
    # enough information to match base price
    price_data <- merge(price_data, price_anchors,
                        by = c("store_code_uc", "product_module_code"))
  }
  price_data[, (new_price_var) := log(get(price_var)) - log(base_price)]
  # eval(parse(text = paste0("price_data[, ", new_price_var, " := log(", price_var,
  #                          ") - log(base_price)]")))
  price_data[, base_price := NULL]
  return(price_data)
}
