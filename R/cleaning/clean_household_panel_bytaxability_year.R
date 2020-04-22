#' Author: Santiago Lacouture
#'
#' Clean the Nielsen household panel data to create a data set on the
#' consumer-year-taxability(food) level. Match taxability to HH by their location. 


library(data.table)
library(futile.logger)
library(readstata13)

setwd("/project2/igaarder/Data/Nielsen/Household_panel")

## Start with basic files used across years: taxability on best-selling modules

# Identify taxability of module: import
taxability_panel <- fread("/project2/igaarder/Data/taxability_state_panel.csv")
# For now, make reduced rate another category
taxability_panel[, taxability := ifelse(!is.na(reduced_rate), 2, taxability)]
# We will use taxability as of December 2014
taxability_panel <- taxability_panel[(month==12 & year==2014),][, .(product_module_code, product_group_code,
                                         fips_state, taxability)]
head(taxability_panel)

## products file is same across years
products_file <- "HMS/Master_Files/Latest/products.tsv"
products <- fread(products_file)
products <- products[, .(upc, upc_ver_uc, product_module_code, product_group_code, department_code)]

purchases.full <- data.table(NULL)
for (yr in 2006:2016) {
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
  ), by = .(trip_code_uc, product_module_code, product_group_code, department_code)]

  ## merge on the trip data
  flog.info("Loading in trips data for %s", yr)
  trips <- fread(trips_file)
  trips <- trips[, .(trip_code_uc, household_code, panel_year)]

  flog.info("Merging trips data to purchases for %s", yr)
  purchases <- merge(purchases, trips, by = "trip_code_uc")

  ## Collapse by year (across stores: since is total there wont be problem)
  purchases <- purchases[, list(total_expenditures = sum(total_expenditures))
                         , by = .(household_code, product_module_code, product_group_code, department_code,
                                  panel_year)]

  ## Keep purchases greater than 0 for efficiency
  purchases<-purchases[total_expenditures > 0]

  # To make valid inferences, drop magnet products
  purchases<-purchases[department_code != 99]
  
  
  ## merge on some individual information
  flog.info("Loading in panelists data for %s", yr)
  panelists <- fread(panelists_file)
  panelists <- panelists[, .(Household_Cd, Panel_Year, Projection_Factor,
                             Household_Income,  Fips_State_Cd, Fips_County_Cd, Panelist_ZipCd, Region_Cd)]
  panelists[, Projection_Factor := Projection_Factor/sum(Projection_Factor)] #Normalize that year projection factor
  setnames(panelists,
           old = c("Household_Cd", "Panel_Year", "Projection_Factor", "Fips_State_Cd", "Fips_County_Cd",
                   "Household_Income", "Panelist_ZipCd", "Region_Cd"),
           new = c("household_code", "panel_year", "projection_factor", "fips_state", "fips_county",
                   "household_income", "zip_code", "region_code"))
  flog.info("Merging panelists data to purchases for %s", yr)
  purchases <- merge(purchases, panelists, by = c("household_code", "panel_year"))
  
  # Calculate household total expenditures
  purchases[, hh_expenditures := sum(total_expenditures), by = .(household_code, panel_year)]
  
  ## Identify purchases by type
  
  # Collapse by department and keep aside, before taxability
  purchases[, food := ifelse(department_code <= 6 & department_code >= 1, 1, 0)]
  purchases.food <- purchases[, .(expenditures = sum(total_expenditures)),
                              by = .(household_code, panel_year, projection_factor, fips_state, fips_county, 
                                     household_income, zip_code, region_code, food, hh_expenditures)]
  # reshape to get a hh level data
  purchases.food <- dcast(purchases.food, household_code +  fips_county + fips_state + zip_code + panel_year + 
                           projection_factor + region_code + hh_expenditures + household_income ~ food, fun=sum,
                         value.var = "expenditures")
  setnames(purchases.food, c("0", "1"),
           c("expenditures_food", "expenditures_nonfood"))
  
  
  ## Identify purchases by taxability
  # merge By HH state. This will drop all other products
  flog.info("Merging taxability for %s", yr)
  purchases <- merge(purchases, taxability_panel, by = "fips_state")
  
  # Collapse to taxability
  purchases.tax <- purchases[, .(expenditures = sum(total_expenditures)),
                         by = .(household_code, panel_year, projection_factor, fips_state, fips_county, 
                                household_income, zip_code, region_code, taxability, hh_expenditures)]
  # reshape to get a hh level data
  purchases.tax <- dcast(purchases.tax, household_code +  fips_county + fips_state + zip_code + panel_year + 
                        projection_factor + region_code + hh_expenditures + household_income ~ taxability, fun=sum,
                        value.var = "expenditures")
  setnames(purchases.food, c("0", "1", "2"),
           c("expenditures_exempt", "expenditures_taxable", "expenditures_reduced"))
  
  # Merge to get final data
  flog.info("Merging sources for %s", yr)
  purchases <- merge(purchases.tax, purchases.food, by = c("household_code", "panel_year", "projection_factor", 
                                                           "fips_state", "fips_county", "household_income", "zip_code",
                                                           "region_code", "hh_expenditures"))
  
  ## save the final dataset: append to full
  flog.info("Saving cleaned dataset for panel year %s", yr)
  purchases.full <- rbind(purchases.full, purchases)
  fwrite(purchases.full, "cleaning/consumer_panel_bytaxability_year_2006-2016.csv")
  
}
