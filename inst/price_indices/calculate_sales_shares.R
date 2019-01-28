#' Author: John Bonney
#' Purpose: Create dataset from 2006-2016 on quarterly level

library(sales.taxes)
library(data.table)
library(readstata13)

setwd("/project2/igaarder")

best_selling_modules <- fread("Data/best_selling_modules.csv")
keep_modules <- unique(best_selling_modules[, .(Module)][[1]])

for (year in 2006:2016){
  sales_panel <- data.table(NULL)
  for (rn in c("I", "II", "III", "IV", "V", "VI")){
    filename <- paste0("/project2/igaarder/Data/Nielsen/", year,
                       "_monthly_master_file_part", rn, ".dta")
    print(paste("Loading", filename, "..."))
    data_part <- as.data.table(read.dta13(filename))
    print("Keeping only best selling products")
    # keep only best selling modules
    data_part <- data_part[product_module_code %in% keep_modules]
    store_id_file <- paste0("/project2/igaarder/Data/Nielsen/stores_",
                            year, ".dta")
    print(paste("Loading", store_id_file, "..."))

    store_id <- as.data.table(read.dta13(store_id_file))

    # this is not good form and may cause errors in the future if these files
    # are altered.
    setnames(store_id, old = "fips_state_code", new = "fips_state")
    setnames(store_id, old = "fips_county_code", new = "fips_county")
    print(paste("Merging", store_id_file, "and", filename))
    data_part <- merge(data_part, store_id, by = "store_code_uc", all.x = T)

    # keep stores we care about
    print(paste("Dropping unnecessary stores"))
    data_part <- data_part[channel_code %in% c("M", "F", "D")]

    data_part[, year := year]
    print(paste("Binding sales_panel and", filename))
    sales_panel <- rbind(sales_panel, data_part)
  }
  print(paste("Converting", year, "data to quarterly"))
  sales_panel <- months_to_quarters(monthly_data = sales_panel, month_var = "month",
                                    collapse_by = c("fips_state", "fips_county", "product_group_code",
                                                    "store_code_uc", "product_module_code"),
                                    collapse_var = "sales")
  annual_filename <- paste0("Data/sales_quarterly_", year, ".csv")
  fwrite(sales_panel, annual_filename)
}

sales_combined <- data.table(NULL)
for (year in 2006:2016){
  annual_filename <- paste0("Data/sales_quarterly_", year, ".csv")
  annual_sales <- fread(annual_filename)
  sales_combined <- rbind(sales_combined, annual_sales)
}

fwrite(sales_quarterly, "Data/sales_quarterly_2006-2016.csv")
