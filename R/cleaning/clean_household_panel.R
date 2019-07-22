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
  ## necessary filepaths
  folderpath <- paste0("HMS/", yr, "/Annual_Files/")
  panelists_file <- paste0(folderpath, "panelists_", yr, ".tsv")
  purchases_file <- paste0(folderpath, "purchases_", yr, ".tsv")
  trips_file <- paste0(folderpath, "trips_", yr, ".tsv")
  store_file <- paste0("/project2/igaarder/Data/Nielsen/stores_", yr, ".dta")
  store_file2 <- paste0("/project2/igaarder/Data/Nielsen/stores_", yr - 1, ".dta")
  
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
  trips[, year := year(purchase_date)]
  trips <- trips[, .(trip_code_uc, household_code, store_code_uc,
                     year, month, panel_year)]
  
  flog.info("Merging trips data to purchases for %s", yr)
  purchases <- merge(purchases, trips, by = "trip_code_uc")
  
  ## Collapse by quarter and year (since is total there wont be problem)
  purchases[, quarter := ceiling(month / 3)]
  purchases <- purchases[, list(
    total_expenditures = sum(total_expenditures) , panel_year = max(panel_year)
  ), by = .(household_code, product_module_code, product_group_code,
            store_code_uc, quarter, year)]
  
  ## merge on the retailer info to get channel type and county FIPS codes
  flog.info("Loading in store data for %s", yr)
  stores <- read.dta13(store_file)
  setDT(stores)
  stores[, year := yr]
  
  if (yr > 2006) {
    stores2 <- read.dta13(store_file2)
    setDT(stores2)
    stores2[, year := (yr - 1) ] # TODO: end of prior year or beginning of next year?
    
    
    stores <- rbind(stores, stores2)
  }
  
  stores <- stores[, .(store_code_uc, channel_code, fips_state_code,
                       fips_county_code, year)]
  setnames(stores,
           old = c("fips_state_code", "fips_county_code"),
           new = c("fips_state", "fips_county"))
  
  flog.info("Merging store data to purchases for %s", yr)
  purchases <- merge(purchases, stores, by = c("store_code_uc", "year"), all.x = T)
  
  
  ## merge on some individual information?
  flog.info("Loading in panelists data for %s", yr)
  panelists <- fread(panelists_file)
  panelists <- panelists[, .(Household_Cd, Panel_Year, Projection_Factor,
                             Projection_Factor_Magnet, Household_Income)]
  setnames(panelists,
           old = c("Household_Cd", "Panel_Year", "Projection_Factor",
                   "Projection_Factor_Magnet", "Household_Income"),
           new = c("household_code", "year", "projection_factor",
                   "projection_factor_magnet", "household_income"))
  flog.info("Merging panelists data to purchases for %s", yr)
  purchases <- merge(purchases, panelists, by = c("household_code", "year"), all.x = T)
  
  ## save the final dataset
  flog.info("Saving cleaned dataset for panel year %s", yr)
  output.path <- paste0("cleaning/purchases_", yr, ".csv")
  fwrite(purchases, output.path)

}

## link all these annual files
purchases.all <- data.table(NULL)
for (yr in 2006:2016) {
  flog.info("Appending %s data to master file", yr)
  annual.path <- paste0("cleaning/purchases_", yr, ".csv")
  annual.file <- fread(annual.path)
  
  ## attach
  purchases.all <- rbind(purchases.all, annual.file)
}



## collapse expenditures to the quarterly level
purchases.full <- data.table(NULL)
for (yr in 2006:2016) {
  
  purchase.yr <- purchases.all[year == yr]
  flog.info("Collapsing expenditures to the quarterly level for year %s", yr)
  purchase.yr <- purchase.yr[, list(
    total_expenditures = sum(total_expenditures), 
    projection_factor = mean(projection_factor, na.rm = T),
    projection_factor_magnet = mean(projection_factor_magnet, na.rm = T),
    household_income = mean(household_income, na.rm = T)
  ), by = .(household_code, product_module_code, product_group_code,
            store_code_uc, channel_code, fips_county, fips_state,
            quarter, year)  ]
  ## attach
  purchases.full <- rbind(purchases.full, purchase.yr)
}
rm(purchases.all)
## Calculate total expenditure per consumer in each quarter (across modules)
purchases.full[, sum_total_exp := sum(total_expenditures),
               by = .(household_code, year, quarter, store_code_uc)]

## Subset to just the best-selling modules
best_selling_modules <- fread("/project2/igaarder/Data/best_selling_modules.csv")
keep_modules <- unique(best_selling_modules[, .(Module)][[1]])

purchases.full <- purchases.full[product_module_code %in% keep_modules]
#fwrite(purchases.full, "cleaning/consumer_panel_2006-2016.csv")

purchases.full <- fread("cleaning/consumer_panel_2006-2016.csv")
## merge on price indices and tax rates
all_goods_pi_path <- "../../all_nielsen_data_2006_2016_quarterly.csv"
all_pi <- fread(all_goods_pi_path)
all_pi <- all_pi[, .(store_code_uc, product_module_code,
                     year, quarter, pricei, cpricei, sales_tax)]

purchases.full <- merge(
  purchases.full, all_pi,
  by = c("store_code_uc", "product_module_code", "year", "quarter"),
  all.x = T
)
fwrite(purchases.full, "cleaning/consumer_panel_2006-2016.csv")