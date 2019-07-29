#' Author: John Bonney & Santiago Lacouture
#'
#' Clean the Nielsen household panel data to create a data set on the
#' consumer-month level. Match sales tax to HH by their location. 
#' Divide consumption by taxability of product and by location of store (same 3 digit or not)

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
  ), by = .(household_code, product_module_code, product_group_code, store_zip3,
            store_code_uc, month, year)]
  
  ## Keep purchases greater than 0
  purchases<-purchases[total_expenditures > 0]
  
  
  ## merge on some individual information?
  flog.info("Loading in panelists data for %s", yr)
  panelists <- fread(panelists_file)
  panelists <- panelists[, .(Household_Cd, Panel_Year, Projection_Factor,
                             Projection_Factor_Magnet, Household_Income,
                             fips_state_code, fips_county_code, panelist_zip_code)]
  setnames(panelists,
           old = c("Household_Cd", "Panel_Year", "Projection_Factor",
                   "Projection_Factor_Magnet", "Household_Income", "panelist_zip_code"),
           new = c("household_code", "year", "projection_factor",
                   "projection_factor_magnet", "household_income", "zip_code"))
  flog.info("Merging panelists data to purchases for %s", yr)
  purchases <- merge(purchases, panelists, by = c("household_code", "year"), all.x = T)
  
  ## For computational efficiency collapse by type of store: same or different 3 zip code
  # Module X HH X type_of_store X month level
  
  # Retrieve 3 digit zip for hh
  purchases <- purchases[, hh_zip3 := substring(zip_code, 1, 3)]
  # Compare store and hh
  purchases <- purchases[, same_3zip_store := (hh_zip3 == store_zip3)]
  # Collapse
  purchases <- purchases[, list(
    total_expenditures = sum(total_expenditures) , panel_year = max(panel_year)
  ), by = .(household_code, product_module_code, product_group_code, same_3zip_store, 
            household_income, projection_factor, household_income, fips_state_code, fips_county_code,
            month, year)]
  
  ## save the final dataset
  flog.info("Saving cleaned dataset for panel year %s", yr)
  output.path <- paste0("cleaning/purchases_m_", yr, ".csv")
  fwrite(purchases, output.path)

}

## collapse expenditures to the monthly level and link all these annual files
purchases.full <- data.table(NULL)
for (yr in 2006:2016) {
  
  annual.path <- paste0("cleaning/purchases_m_", yr, ".csv")
  purchase.yr <- fread(annual.path)
  
  purchase.yr <- purchase.yr[, list(
    total_expenditures = sum(total_expenditures), 
    projection_factor = mean(projection_factor, na.rm = T),
    projection_factor_magnet = mean(projection_factor_magnet, na.rm = T),
    household_income = mean(household_income, na.rm = T)
  ), by = .(household_code, product_module_code, product_group_code,
            same_3zip_store, fips_county, fips_state, zip_code,
            month, year)  ]
  ## attach
  flog.info("Appending %s data to master file", yr)
  purchases.full <- rbind(purchases.full, purchase.yr) 

}

## Calculate total expenditure per consumer in each month (across stores and modules)
purchases.full[, sum_total_exp_month := sum(total_expenditures),
               by = .(household_code, year, month)]

## Subset to just the best-selling modules
best_selling_modules <- fread("/project2/igaarder/Data/best_selling_modules.csv")
keep_modules <- unique(best_selling_modules[, .(Module)][[1]])

purchases.full <- purchases.full[product_module_code %in% keep_modules]

## Identify taxability of module: import
taxability_panel <- fread("/project2/igaarder/Data/taxability_state_panel.csv")
taxability_panel <- taxability_panel[, .(product_module_code, product_group_code, 
                                         fips_state, taxability, month, year)]

purchases.full <- merge(
  purchases.full, taxability_panel,
  by = c("fips_state", "product_module_code", "product_group_code", "year", "month"),
  all.x = T
)
## Collapse by household X type of store X taxability of module
purchases.full <- purchases.full[, list(
            total_expenditures = sum(total_expenditures),
            sum_total_exp_month = mean(sum_total_exp_month, na.rm = T),
            projection_factor = mean(projection_factor, na.rm = T),
            projection_factor_magnet = mean(projection_factor_magnet, na.rm = T),
            household_income = mean(household_income, na.rm = T)
          ), by = .(household_code, taxability, fips_county, fips_state, zip_code,
                    same_3zip_store, quarter, year)  ]
## reshape to get a hh X taxability of module data
purchases.full <- dcast(purchases.full, household_code + taxability + fips_county + fips_state + 
                          zip_code + quarter + year + projection_factor + projection_factor_magnet + 
                          sum_total_exp_month + household_income ~ same_3zip_store, value.var = "total_expenditures")
setnames(purchases.full,
         old = c("total_expenditures_same_3zip_store0", "total_expenditures_same_3zip_store1"),
         new = c("expenditures_diff3", "expenditures_same3"))
## reshape to get a hh data
purchases.full <- dcast(purchases.full, household_code + fips_county + fips_state + zip_code + quarter
                        + year + projection_factor + projection_factor_magnet + sum_total_exp_month + 
                          household_income ~ taxability, value.var = c("expenditures_diff3","expenditures_same3"))

#fwrite(purchases.full, "cleaning/consumer_panel_2006-2016.csv")

#purchases.full <- fread("cleaning/consumer_panel_2006-2016.csv")
## merge on tax rates
all_goods_pi_path <- "../../monthly_taxes_county_5zip_2008_2014.csv"
all_pi <- fread(all_goods_pi_path)
all_pi <- all_pi[, .(sales_tax, year, month, fips_county, fips_state, zip_code )]

purchases.full <- merge(
  purchases.full, all_pi,
  by = c("fips_county", "fips_state", "zip_code", "year", "month"),
  all.x = T
)
fwrite(purchases.full, "cleaning/consumer_panel_m_hh_2006-2016.csv")