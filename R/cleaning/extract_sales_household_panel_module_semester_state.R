#' Author: John Bonney & Santiago Lacouture
#'
#' Clean the Nielsen household panel data to create a data set on the
#' consumer-module-semester level. Then extract the total sales weighted by the projection factor 
#' To Match 
#' 
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
  trips <- trips[, .(trip_code_uc, household_code, store_code_uc, store_zip3,
                     year, month, panel_year)]

  flog.info("Merging trips data to purchases for %s", yr)
  purchases <- merge(purchases, trips, by = "trip_code_uc")

  ## Collapse by month and year (since is total there wont be problem)
  purchases <- purchases[, list(
    total_expenditures = sum(total_expenditures) , panel_year = max(panel_year)
  ), by = .(household_code, product_module_code, store_code_uc, month, year)]

  ## Keep purchases greater than 0
  purchases<-purchases[total_expenditures > 0]


  ## merge on some individual information?
  flog.info("Loading in panelists data for %s", yr)
  panelists <- fread(panelists_file)
  panelists <- panelists[, .(Household_Cd, Panel_Year, Projection_Factor,
                             Projection_Factor_Magnet, Household_Income,
                             Fips_State_Cd, Fips_County_Cd, Panelist_ZipCd, Region_Cd)]
  setnames(panelists,
           old = c("Household_Cd", "Panel_Year", "Projection_Factor", "Fips_State_Cd", "Fips_County_Cd",
                   "Projection_Factor_Magnet", "Household_Income", "Panelist_ZipCd", "Region_Cd"),
           new = c("household_code", "year", "projection_factor", "fips_state_code", "fips_county_code",
                   "projection_factor_magnet", "household_income", "zip_code", "region_code"))
  flog.info("Merging panelists data to purchases for %s", yr)
  purchases <- merge(purchases, panelists, by = c("household_code", "year"), all.x = T)

  ## Collapse by household-module-semester
  # Create Semester
  purchases[, semester := ceiling(month/6)]
  
  
  # Collapse and keep variables we are interested at
  purchases <- purchases[, list(
    total_expenditures = sum(total_expenditures) , panel_year = max(panel_year)
  ), by = .(household_code, product_module_code, projection_factor, region_code,
            fips_state_code, fips_county_code, semester, year)]

  ## save the final dataset
  flog.info("Saving cleaned dataset for panel year %s", yr)
  output.path <- paste0("cleaning/purchases_s_m_", yr, ".csv")
  fwrite(purchases, output.path)

}

## collapse expenditures to the semester level and link all these annual files
purchases.full <- data.table(NULL)
for (yr in 2006:2016) {
  
  annual.path <- paste0("cleaning/purchases_s_m_", yr, ".csv")
  purchase.yr <- fread(annual.path)
  
  ## attach
  flog.info("Appending %s data to master file", yr)
  purchases.full <- rbind(purchases.full, purchase.yr)
  
}

## Retrieve household info for purchases in the last month of previous year but same panel_year
household.cols <- c("fips_county_code", "fips_state_code", "projection_factor", "region_code")
purchases.full[, (household.cols) := lapply(.SD, as.numeric), 
               .SDcols = household.cols][,(household.cols) := lapply(.SD, mean, na.rm = T),
                                         by = .(household_code, year), .SDcols = household.cols] 

## Calculate total expenditures across consumers in each semester in a given module within a state
purchases.full[, .(total_sales = sum(total_expenditures, weights = projection_factor)),
               by = .(year, semester, product_module_code, fips_state_code)]



## Export
fwrite(purchases.full, "cleaning/consumer_sales_semester_state_2006-2016.csv")