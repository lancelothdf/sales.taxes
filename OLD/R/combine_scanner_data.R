#' Function to combine Nielsen scanner data.
#'
#' @description This function combines year-level Nielsen data cross years,
#'     keeping only a subset of stores specified by the user. The user also
#'     specifies for which years he would like the data.
#' @param folder The folder in which the scanner data and the year-level store
#'     information are contained (if they aren't in the working directory) (character).
#' @param file_tail The name of the scanner data files (that follows the year). For
#'     example, if the 2010 file was called \code{2010_scanner_data.dta}, the
#'     \code{file_stem} argument would be \code{"_scanner_data"}. (character)
#' @param file_type The type of the file. Currently works for either \code{.dta},
#'     \code{.csv}, and \code{.txt} files. (character)
#' @param years A vector of years for which the scanner data should be returned
#'     (numeric).
#' @param filters A string containing the filtering conditions (e.g., if I wanted to keep
#'     stores in fips_state == 4 and fips_county == 18, it would be
#'     \code{"fips_state == 4 & fips_county == 18"}) (character)
#' @param store_info_folder The folder containing the store information files,
#'     if different than \code{folder}. (character)
#' @param store_info_prefix The prefix for the store information files. (character)
#' @param store_info_filetype The filetype of the store information files, if
#'     different than \code{file_type}. (character)
#' @param select_modules Do you want to specify specific product modules to
#'     keep? (logical)
#' @param modules_to_keep If \code{select_modules = TRUE}, the dataset containing
#'     the modules to be kept (data.table)
#'

combine_scanner_data <- function(folder = NULL,
                                 file_tail,
                                 file_type,
                                 years,
                                 filters = NULL,
                                 store_info_folder = folder,
                                 store_info_prefix = "stores_",
                                 store_info_filetype = file_type,
                                 select_modules = F,
                                 modules_to_keep = NULL){
  assertCharacter(folder, null.ok = T)
  assertCharacter(file_tail)
  assertCharacter(file_type)
  assertNumeric(years)
  assertCharacter(filters, null.ok = T)
  assertCharacter(store_info_folder, null.ok = T)
  assertCharacter(store_info_prefix)
  assertCharacter(store_info_filetype)
  assertLogical(select_modules)
  assertDataTable(modules_to_keep, null.ok = T)

  read_file <- function(file, type){
    if (type %in% c("dta", ".dta")){
      as.data.table(read.dta13(file))
    } else if (type %in% c("csv", ".csv", "txt", ".txt")){
      fread(file)
    } else {
      stop("File type must be either 'dta', 'csv', or 'txt' and must be a character." )
    }
  }

  if (substr(folder, nchar(folder), nchar(folder)) != "/"){
    folder <- paste0(folder, "/")
  }
  if (substr(file_type, 1, 1) != "."){
    file_type <- paste0(".", file_type)
  }

  sales_panel <- data.table(NULL)

  for (year in years){
    nielsen_file <- paste0(folder, year, file_stem, file_type)
    sales_data <- read_file(nielsen_file, file_type)

    if (select_modules){
      sales_data <- select_products(sales_data,
                                    module_name_col = "product_module_code",
                                    products = modules_to_keep)
    }

    # Merge onto store identifiers
    store_id_file <- paste0(folder, store_info_prefix,
                            year, store_info_filetype)

    store_id <- read_file(store_id_file, store_info_filetype)

    # this is not good form and may cause errors in the future if these files
    # are altered.
    setnames(store_id, old = "fips_state_code", new = "fips_state")
    setnames(store_id, old = "fips_county_code", new = "fips_county")
    sales_data <- merge(sales_data, store_id, by = "store_code_uc", all.x = T)

    # keep stores we care about
    sales_data <- eval(parse(text = paste0("sales_data[", filters, "]")))

    sales_data[, year := year]
    sales_panel <- rbind(sales_panel, sales_data)
  }
  return(sales_panel)
}
