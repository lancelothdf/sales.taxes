#' Author: John Bonney
#'
#' Clean the Nielsen household panel data to create a data set on the
#' consumer-product-store-quarter level.

library(data.table)
library(futile.logger)
library(readstata13)

setwd("/project2/igaarder/Data/Nielsen/Household_panel")

## products file is same across years
products_file <- "HMS/Master_Files/Latest/products.tsv"
products <- fread(products_file)
products <- products[, .(upc, upc_ver_uc, product_module_code, product_group_code)]

for (yr in 2006:2016) {
  next
  ## necessary filepaths
  folderpath <- paste0("HMS/", yr, "/Annual_Files/")
  panelists_file <- paste0(folderpath, "panelists_", yr, ".tsv")
  purchases_file <- paste0(folderpath, "purchases_", yr, ".tsv")
  trips_file <- paste0(folderpath, "trips_", yr, ".tsv")
  store_file <- paste0("/project2/igaarder/Data/Nielsen/stores_", yr, ".dta")

  ## start with purchases data
  flog.info("Loading in purchases data for %s", yr)
  purchases <- fread(purchases_file)
  purchases <- purchases[, .(trip_code_uc, upc, upc_ver_uc, total_price_paid)]

  ## merge on the products data to get product module codes (and product group codes)
  flog.info("Merging products data to purchases for %s", yr)
  purchases <- merge(purchases, products, by = c("upc", "upc_ver_uc"))

  ## sum expenditures over UPCs to the product module level
  flog.info("Summing expenditures over UPCs for %s", yr)
  purchases <- purchases[, list(
    total_expenditures = sum(total_price_paid)
  ), by = .(trip_code_uc, product_module_code, product_group_code)]

  ## merge on the trip data
  flog.info("Loading in trips data for %s", yr)
  trips <- fread(trips_file)
  trips[, purchase_date := as.Date(purchase_date, "%Y-%m-%d")]
  trips[, month := month(purchase_date)]
  setnames(trips, "panel_year", "year")
  trips <- trips[, .(trip_code_uc, household_code, store_code_uc, year, month)]

  flog.info("Merging trips data to purchases for %s", yr)
  purchases <- merge(purchases, trips, by = "trip_code_uc")

  ## collapse expenditures to the quarterly level
  flog.info("Collapsing expenditures to the quarterly level for %s", yr)
  purchases[, quarter := ceiling(month / 3)]
  purchases <- purchases[, list(
    total_expenditures = sum(total_expenditures)
  ), by = .(household_code, product_module_code, product_group_code,
            store_code_uc, quarter, year)]

  ## merge on the retailer info to get channel type and county FIPS codes
  flog.info("Loading in store data for %s", yr)
  stores <- read.dta13(store_file)
  setDT(stores)

  stores <- stores[, .(store_code_uc, channel_code, fips_state_code, fips_county_code)]
  setnames(stores,
           old = c("fips_state_code", "fips_county_code"),
           new = c("fips_state", "fips_county"))

  flog.info("Merging store data to purchases for %s", yr)
  purchases <- merge(purchases, stores, by = "store_code_uc", all.x = T)

  ## merge on some individual information?
  flog.info("Loading in panelists data for %s", yr)
  panelists <- fread(panelists_file)
  panelists <- panelists[, .(Household_Cd, Panel_Year, Projection_Factor, Projection_Factor_Magnet)]
  setnames(panelists,
           old = c("Household_Cd", "Panel_Year", "Projection_Factor", "Projection_Factor_Magnet"),
           new = c("household_code", "year", "projection_factor", "projection_factor_magnet"))
  flog.info("Merging panelists data to purchases for %s", yr)
  purchases <- merge(purchases, panelists, by = c("household_code", "year"))

  ## save the final dataset
  flog.info("Saving cleaned dataset for %s", yr)
  output.path <- paste0("cleaning/purchases_", yr, ".csv")
  fwrite(purchases, output.path)
}

## link all these annual files
best_selling_modules <- fread("/project2/igaarder/Data/best_selling_modules.csv")
keep_modules <- unique(best_selling_modules[, .(Module)][[1]])

purchases.all <- data.table(NULL)
for (yr in 2006:2016) {
  annual.path <- paste0("cleaning/purchases_", yr, ".csv")
  annual.file <- fread(annual.path)

  ## Calculate total expenditure per consumer in each quarter (across modules)
  annual.file[, sum_total_exp := sum(total_expenditures),
              by = .(household_code, year, quarter, store_code_uc)]

  ## Subset to just the best-selling modules
  annual.file <- annual.file[product_module_code %in% keep_modules]
  ## attach
  purchases.all <- rbind(purchases.all, annual.file)
}
fwrite(purchases.all, "cleaning/consumer_panel_2006-2016.csv")
