#' Author: John Bonney & Santiago Lacouture
#'
#' Clean the Nielsen household panel data to create a data set on the
#' consumer-quarter level. Match sales tax to HH by their location. 
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

  ## Collapse by quarter and year (since is total there wont be problem)
  purchases[, quarter := ceiling(month / 3)]
  purchases <- purchases[, list(
    total_expenditures = sum(total_expenditures) , panel_year = max(panel_year)
  ), by = .(household_code, product_module_code, product_group_code,
            store_zip3, quarter, year)]

  ## Keep purchases greater than 0 for efficiency
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

  ## For computational efficiency collapse by type of store: same or different 3 zip code
  # Module X HH X type_of_store X month level

  # Retrieve 3 digit zip for hh
  purchases <- purchases[, hh_zip3 := substring(zip_code, 1, 3)]
  # Compare store and hh
  purchases <- purchases[, same_3zip_store := (hh_zip3 == store_zip3)]
  # Collapse
  purchases <- purchases[, list(
    total_expenditures = sum(total_expenditures) , panel_year = max(panel_year)
  ), by = .(household_code, product_module_code, product_group_code, same_3zip_store, zip_code,
            household_income, projection_factor,projection_factor_magnet, household_income, region_code,
            fips_state_code, fips_county_code, quarter, year)]

  ## save the final dataset
  flog.info("Saving cleaned dataset for panel year %s", yr)
  output.path <- paste0("cleaning/purchases_q_", yr, ".csv")
  fwrite(purchases, output.path)

}

## collapse expenditures to the quarter level and link all these annual files
purchases.full <- data.table(NULL)
for (yr in 2006:2016) {

  annual.path <- paste0("cleaning/purchases_q_", yr, ".csv")
  purchase.yr <- fread(annual.path)

  purchase.yr <- purchase.yr[, list(
    total_expenditures = sum(total_expenditures),
    projection_factor = mean(projection_factor, na.rm = T),
    projection_factor_magnet = mean(projection_factor_magnet, na.rm = T),
    household_income = mean(household_income, na.rm = T)
  ), by = .(household_code, product_module_code, product_group_code, region_code,
            same_3zip_store, fips_county_code, fips_state_code, zip_code,
            quarter, year)  ]
  ## attach
  flog.info("Appending %s data to master file", yr)
  purchases.full <- rbind(purchases.full, purchase.yr)

}

## Calculate total expenditure per consumer in each quarter (across stores and modules)
purchases.full[, sum_total_exp_quarter := sum(total_expenditures),
               by = .(household_code, year, quarter)]

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

## Collapse by household X type of store X taxability of module
purchases.full <- purchases.full[, list(
            total_expenditures = sum(total_expenditures)
          ), by = .(household_code, taxability, fips_county_code, fips_state_code, zip_code,
                    same_3zip_store, quarter, year, sum_total_exp_quarter, projection_factor,
                    projection_factor_magnet, household_income, region_code) ]
## reshape to get a hh X taxability of module data
purchases.full <- dcast(purchases.full, household_code + taxability + fips_county_code + fips_state_code +
                          zip_code + quarter + year + projection_factor + projection_factor_magnet +
                          sum_total_exp_quarter + household_income + region_code ~ same_3zip_store, fun=sum,
                          value.var = "total_expenditures")

setnames(purchases.full,
         old = c("FALSE", "TRUE", "NA"),
         new = c("expenditures_diff3", "expenditures_same3", "expenditures_unkn3"))
## reshape to get a hh data
purchases.full <- dcast(purchases.full, household_code + fips_county_code + fips_state_code + zip_code + quarter
                        + year + projection_factor + projection_factor_magnet + sum_total_exp_quarter + region_code +
                          household_income ~ taxability,  fun=sum, value.var = c("expenditures_diff3", "expenditures_same3", "expenditures_unkn3"))
## Balance panel for proper estimations
#### Balance the panel: Key step for proper estimation
flog.info("Building skeleton")

# Collapse to hh that appeared at least once
possible.purchases <- purchases.full[, list(N_obs = .N), by = .(household_code)]
possible.purchases <- possible.purchases[N_obs > 0]
possible.purchases[N_obs > 0]
# Expand by quarter (old fashioned: CJ does not work in this case because of dimensionality)
possible.purchases.q <- data.table(NULL)
for (i in 1:4) {
  possible.purchases.t <- possible.purchases[ , quarter := i ]
  possible.purchases.q <- rbind(possible.purchases.q, possible.purchases.t)
  
}
# remove used data for space
rm(possible.purchases)
# Expand by year
possible.purchases.full <- data.table(NULL)
for (i in 2006:2016) {
  possible.purchases.t <- possible.purchases.q[ , year := i ]
  possible.purchases.full <- rbind(possible.purchases.full, possible.purchases.t)
}
rm(possible.purchases.q)
# merge
flog.info("Merging to balance panel")
purchases.full <- merge(purchases.full, possible.purchases.full, by = c("household_code", "quarter", "year"), all.y = T)
rm(possible.purchases.full)
# assign purchases of 0 to those moments
expenditure.cols <- c("expenditures_diff3_0", "expenditures_diff3_1", "expenditures_diff3_2", 
                    "expenditures_same3_0", "expenditures_same3_1", "expenditures_same3_2", 
                    "expenditures_unkn3_0", "expenditures_unkn3_1", "expenditures_unkn3_2")
for (Y in expenditure.cols) {
  purchases.full <- purchases.full[, get(Y) := ifelse(is.na(purchases.full$get(Y)),
                                                      0, purchases.full$get(Y))]
}

# retrieve total purchases
purchases.full[, sum_total_exp_quarter := mean(sum_total_exp_quarter, na.rm = T),
               by = .(household_code, year, quarter)]


## merge on tax rates
all_goods_pi_path <- "../../monthly_taxes_county_5zip_2008_2014.csv"
all_pi <- fread(all_goods_pi_path)
all_pi <- all_pi[, .(sales_tax, year, month, fips_county, fips_state, zip_code )]
setnames(all_pi,
         old = c("fips_state", "fips_county"),
         new = c("fips_state_code", "fips_county_code"))
# Collapse rates to the quarter as the mean 
all_pi[, quarter := ceiling(month / 3)]
all_pi <- all_pi[, list(sales_tax = mean(sales_tax)) , 
                                     by =.(zip_code, fips_county_code,
                                           fips_state_code, quarter, year)]
purchases.full <- merge(
  purchases.full, all_pi,
  by = c("fips_county_code", "fips_state_code", "zip_code", "year", "quarter"),
  all.x = T
)

## Create interest variables
purchases.full <- purchases.full[, ln_sales_tax := log1p(sales_tax)]
purchases.full <- purchases.full[, expenditure_taxable := expenditures_diff3_1 + expenditures_same3_1 + expenditures_unkn3_1]
purchases.full <- purchases.full[, expenditure_non_taxable := expenditures_diff3_0 + expenditures_same3_0 + expenditures_unkn3_0]
purchases.full <- purchases.full[, expenditure_unknown := expenditures_diff3_2 + expenditures_same3_2 + expenditures_unkn3_2]
purchases.full <- purchases.full[, expenditure_same3 := expenditures_same3_0 + expenditures_same3_1 + expenditures_same3_2]
purchases.full <- purchases.full[, expenditure_diff3 := expenditures_diff3_0 + expenditures_diff3_1 + expenditures_diff3_2]


fwrite(purchases.full, "cleaning/consumer_panel_q_hh_2006-2016.csv")