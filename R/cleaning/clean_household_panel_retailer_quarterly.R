#' Author: John Bonney
#'
#' Clean the Nielsen household panel data to create a data set on the
#' consumer-quarter level.
#' only use purchases at stores from the retailer data

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
    total_expenditures = sum(total_expenditures) , panel_year = mean(panel_year)
  ), by = .(household_code, product_module_code, product_group_code,
            store_code_uc, quarter, year)]
  
  ## Keep purchases greater than 0
  purchases<-purchases[total_expenditures > 0]
  
  ## merge on the retailer info to get channel type and county FIPS codes
  flog.info("Loading in store data for %s", yr)
  stores <- read.dta13(store_file)
  setDT(stores)
  stores[, year := yr]
  
  if (yr > 2006) {
    stores2 <- read.dta13(store_file2)
    setDT(stores2)
    stores2[, year := (yr - 1) ]
    
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
                             Projection_Factor_Magnet, Household_Income,
                             Fips_State_Cd, Fips_County_Cd, Panelist_ZipCd, Region_Cd)]
  setnames(panelists,
           old = c("Household_Cd", "Panel_Year", "Projection_Factor", "Fips_State_Cd", "Fips_County_Cd",
                   "Projection_Factor_Magnet", "Household_Income", "Panelist_ZipCd", "Region_Cd"),
           new = c("household_code", "year", "projection_factor", "hh_fips_state_code", "hh_fips_county_code",
                   "projection_factor_magnet", "household_income", "hh_zip_code", "hh_region_code"))
  flog.info("Merging panelists data to purchases for %s", yr)
  purchases <- merge(purchases, panelists, by = c("household_code", "year"), all.x = T)
  
  ## Compute total expense in that quarter (across stores and modules)
  purchases.full[, sum_total_exp := sum(total_expenditures),
                 by = .(household_code, year, quarter)] 
  ## Keep purchases made in retailer data stores
  purchases.full[!is.na(channel_code)]
  
  ## save the final dataset
  flog.info("Saving cleaned dataset for panel year %s", yr)
  output.path <- paste0("cleaning/purchases_retail_q_", yr, ".csv")
  fwrite(purchases, output.path)

}

## collapse expenditures to the quarterly level and link all these annual files
purchases.full <- data.table(NULL)
for (yr in 2006:2016) {
  
  annual.path <- paste0("cleaning/purchases_retail_q_", yr, ".csv")
  purchase.yr <- fread(annual.path)
  purchase.yr <- purchase.yr[, list(
    total_expenditures = sum(total_expenditures), 
    projection_factor = mean(projection_factor, na.rm = T),
    projection_factor_magnet = mean(projection_factor_magnet, na.rm = T),
    household_income = mean(household_income, na.rm = T),
    sum_total_exp = sum(sum_total_exp)
  ), by = .(household_code, product_module_code, product_group_code,
            store_code_uc, channel_code, fips_county, fips_state, hh_fips_state_code, hh_fips_county_code,
            hh_zip_code, hh_region_code, quarter, year)]
  ## attach
  flog.info("Appending %s data to master file", yr)
  purchases.full <- rbind(purchases.full, purchase.yr) 

}

## Calculate total expenditure per consumer in each quarter in each store (across modules)
purchases.full[, sum_total_exp_retailer := sum(total_expenditures),
               by = .(household_code, year, quarter, store_code_uc)]

## Identify taxability of module: import
taxability_panel <- fread("/project2/igaarder/Data/taxability_state_panel.csv")
taxability_panel <- taxability_panel[, .(product_module_code, product_group_code,
                                         fips_state, taxability, month, year)]
setnames(taxability_panel,
         old = c("fips_state"),
         new = c("fips_state_code"))
# Collapse taxability to the quarter as rounding the mean 
taxability_panel[, quarter := ceiling(month / 3)]
taxability_panel <- taxability_panel[, list(taxability = round(mean(taxability))) , 
                                     by =.(product_module_code, product_group_code,
                                           fips_state_code, quarter, year)]
purchases.full <- merge(
  purchases.full, taxability_panel,
  by = c("fips_state_code", "product_module_code", "product_group_code", "year", "quarter"),
  all.x = T
)
# Assign unknown to purchases out of best selling module (taxability only identified for best selling)
purchases.full$taxability[is.na(purchases.full$taxability)] <- 2

## Collapse by household X store X taxability of module x quarter
purchases.full <- purchases.full[, list(
  total_expenditures = sum(total_expenditures)
), by = .(household_code, taxability, fips_county_code, fips_state_code,
          hh_fips_county_code, hh_fips_state_code, hh_zip_code, hh_region_code,
          store_code_uc, quarter, year, sum_total_exp, projection_factor, sum_total_exp_retailer,
          projection_factor_magnet, household_income) ]

fwrite(purchases.full, "cleaning/consumer_panel_retailer_q_hh_2006-2016.csv")

purchases <- fread("cleaning/consumer_panel_retailer_q_hh_2006-2016.csv")
## Reshape to get a household x store x quarter data set
purchases.full <- dcast(purchases.full, household_code + fips_county_code + fips_state_code + store_code_uc +
                          hh_fips_county_code + hh_fips_state_code + hh_zip_code + hh_region_code +
                          quarter + year + projection_factor + projection_factor_magnet + sum_total_exp +
                          sum_total_exp_retailer + household_income ~ taxability, fun=sum,
                        value.var = "total_expenditures")
setnames(purchases.full,
         old = c("0", "1", "2"),
         new = c("expenditures_non_taxable", "expenditures_taxable", "expenditures_unknown"))


## merge on tax rates by store
taxes_path <- "../../county_monthly_tax_rates_2008_2014.csv"
taxes <- fread(taxes_path)
taxes <- taxes[, .(sales_tax, year, month, fips_county, fips_state )]


setnames(taxes,
         old = c("fips_state", "fips_county"),
         new = c("fips_state_code", "fips_county_code"))
# Collapse rates to the quarter as the mean 
taxes[, quarter := ceiling(month / 3)]
taxes <- taxes[, list(sales_tax = mean(sales_tax)) , 
                 by =.(fips_county_code, fips_state_code, quarter, year)]

# Merge
purchases.full <- merge(purchases.full, taxes,
  by = c("fips_county_code", "fips_state_code", "year", "quarter"),
  all.x = T
)

## collapse to get a hh X quarter data
purchases.full <- purchases.full[, list(
  expenditures_non_taxable = sum(expenditures_non_taxable),
  expenditures_taxable = sum(expenditures_taxable),
  expenditures_unknown = sum(expenditures_unknown),
  sales_tax = weighted.mean(sales_tax, sum_total_exp_retailer, na.rm = T)
), by = .(household_code,
          hh_fips_county_code, hh_fips_state_code, hh_zip_code, hh_region_code,
          quarter, year, sum_total_exp, projection_factor,
          projection_factor_magnet, household_income) ]

## Create interest variables
purchases.full <- purchases.full[, ln_sales_tax := log1p(sales_tax)]


fwrite(purchases.full, "cleaning/consumer_panel_retailer_q_hh_2006-2016.csv")