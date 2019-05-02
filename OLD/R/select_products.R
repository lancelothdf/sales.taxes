#' Keep only certain product modules specified by the user
#'
#' @description Filter product modules to keep only those found in a vector
#'     provided by the user.
#' @param all_data The data to be filtered, keeping only the best-selling
#'     products found in \code{products}. Must contain the
#'     \code{module_name_col} column (data.table)
#' @param module_name_col The name of the column in \code{all_data}
#'     containing the identifying module names or numbers (character)
#' @param products A vector of product module codes to keep (need not be unique)
#'     (must be numeric)
#' @return A dataset that has dropped all products not in the vector of product
#'     module codes provided by the user


select_products <- function(all_data,
                            module_name_col,
                            products){
  assertCharacter(module_name_col)
  assertDataTable(all_data)
  assertNumeric(products)

  if (!module_name_col %in% names(all_data)){
    stop(paste(module_name_col, "must be the name of a column in `all_data'"))
  }

  all_data <- all_data[get(module_name_col) %in% unique(products)]
  return(all_data)
}
