#' Keep only best-selling product modules
#'
#' @description Filter product modules to keep only those found on the list
#'     of best-selling modules.
#' @param all_data The data to be filtered, keeping only the best-selling
#'     products found in \code{products_data}. Must contain the
#'     \code{module_name_ad} column (data.table)
#' @param module_name_ad The name of the column in \code{all_data}
#'     containing the identifying module names or numbers (character)
#' @param products_data A table of the best-selling products to keep. Must
#'     contain the \code{module_name_pd} column (data.table)
#' @param module_name_pd The name of the column in \code{products_data}
#'     containing the identifying module names or numbers (character)


keep_best_selling_products <- function(all_data,
                                       module_name_ad,
                                       products_data,
                                       module_name_pd){
  assertCharacter(module_name_pd)
  assertCharacter(module_name_ad)
  assertDataTable(products_data)
  assertDataTable(all_data)

  if (!module_name_pd %in% names(products_data)){
    stop(paste(module_name_pd, "must be the name of a column in `products_data'"))
  }
  if (!module_name_ad %in% names(all_data)){
    stop(paste(module_name_ad, "must be the name of a column in `all_data'"))
  }

  keep_modules <- products_data[, module_name_pf]
  all_data <- all_data[get(module_name_ad) %in% keep_modules]
  return(all_data)

}
