#' Author: John Bonney & Santiago Lacouture
#'
#' Clean the Nielsen household panel data to create a data set on the
#' consumer-group-quarter level. Match sales tax to HH and product by their location. 
#' Divide consumption by taxability of product and by location of store (same 3 digit or not)


library(data.table)
library(futile.logger)
library(readstata13)

setwd("/project2/igaarder/Data/Nielsen/Household_panel")

# # products file is same across years
# products_file <- "HMS/Master_Files/Latest/products.tsv"
# products <- fread(products_file)
# products <- products[, .(upc, upc_ver_uc, product_module_code, product_group_code)]
# 
# for (yr in 2006:2016) {
#   ## necessary filepaths
#   folderpath <- paste0("HMS/", yr, "/Annual_Files/")
#   panelists_file <- paste0(folderpath, "panelists_", yr, ".tsv")
#   purchases_file <- paste0(folderpath, "purchases_", yr, ".tsv")
#   trips_file <- paste0(folderpath, "trips_", yr, ".tsv")
#   store_file <- paste0("/project2/igaarder/Data/Nielsen/stores_", yr, ".dta")
#   store_file2 <- paste0("/project2/igaarder/Data/Nielsen/stores_", yr - 1, ".dta")
# 
#   ## start with purchases data
#   flog.info("Loading in purchases data for %s", yr)
#   purchases <- fread(purchases_file)
#   purchases <- purchases[, .(trip_code_uc, upc, upc_ver_uc, total_price_paid)]
# 
#   ## merge on the products data to get product module codes (and product group codes)
#   flog.info("Merging products data to purchases for %s", yr)
#   purchases <- merge(purchases, products, by = c("upc", "upc_ver_uc"))
# 
#   ## sum expenditures over UPCs to the product module level
#   flog.info("Summing expenditures over UPCs for %s", yr)
#   purchases <- purchases[, list(
#     total_expenditures = sum(total_price_paid)
#   ), by = .(trip_code_uc, product_module_code, product_group_code)]
# 
#   ## merge on the trip data
#   flog.info("Loading in trips data for %s", yr)
#   trips <- fread(trips_file)
#   trips[, purchase_date := as.Date(purchase_date, "%Y-%m-%d")]
#   trips[, month := month(purchase_date)]
#   trips[, year := year(purchase_date)]
#   trips <- trips[, .(trip_code_uc, household_code, store_code_uc, store_zip3,
#                      year, month, panel_year)]
# 
#   flog.info("Merging trips data to purchases for %s", yr)
#   purchases <- merge(purchases, trips, by = "trip_code_uc")
# 
#   ## Collapse by quarter and year (since is total there wont be problem)
#   purchases[, quarter := ceiling(month / 3)]
#   purchases <- purchases[, list(
#     total_expenditures = sum(total_expenditures) , panel_year = max(panel_year)
#   ), by = .(household_code, product_module_code, product_group_code,
#             store_zip3, quarter, year)]
# 
#   ## Keep purchases greater than 0 for efficiency
#   purchases<-purchases[total_expenditures > 0]
# 
# 
#   ## merge on some individual information?
#   flog.info("Loading in panelists data for %s", yr)
#   panelists <- fread(panelists_file)
#   panelists <- panelists[, .(Household_Cd, Panel_Year, Projection_Factor,
#                              Projection_Factor_Magnet, Household_Income,
#                              Fips_State_Cd, Fips_County_Cd, Panelist_ZipCd, Region_Cd)]
#   setnames(panelists,
#            old = c("Household_Cd", "Panel_Year", "Projection_Factor", "Fips_State_Cd", "Fips_County_Cd",
#                    "Projection_Factor_Magnet", "Household_Income", "Panelist_ZipCd", "Region_Cd"),
#            new = c("household_code", "year", "projection_factor", "fips_state_code", "fips_county_code",
#                    "projection_factor_magnet", "household_income", "zip_code", "region_code"))
#   flog.info("Merging panelists data to purchases for %s", yr)
#   purchases <- merge(purchases, panelists, by = c("household_code", "year"), all.x = T)
# 
#   ## For computational efficiency collapse by type of store: same or different 3 zip code
#   # Module X HH X type_of_store X month level
# 
#   # Retrieve 3 digit zip for hh
#   purchases <- purchases[, hh_zip3 := substring(zip_code, 1, 3)]
#   # Compare store and hh
#   purchases <- purchases[, same_3zip_store := (hh_zip3 == store_zip3)]
#   # Collapse
#   purchases <- purchases[, list(
#     total_expenditures = sum(total_expenditures) , panel_year = max(panel_year)
#   ), by = .(household_code, product_module_code, product_group_code, same_3zip_store, zip_code,
#             household_income, projection_factor,projection_factor_magnet, household_income,
#             fips_state_code, fips_county_code, region_code, quarter, year)]
# 
#   ## save the final dataset
#   flog.info("Saving cleaned dataset for panel year %s", yr)
#   output.path <- paste0("cleaning/purchases_q_", yr, ".csv")
#   fwrite(purchases, output.path)
# 
# }

## collapse expenditures to the quarter level and link all these annual files
purchases.full <- data.table(NULL)
for (yr in 2006:2016) {

  annual.path <- paste0("cleaning/purchases_q_", yr, ".csv")
  purchase.yr <- fread(annual.path)

  ## attach
  flog.info("Appending %s data to master file", yr)
  purchases.full <- rbind(purchases.full, purchase.yr)

}

## Collapse to the quarter (there can be doubles)
## Collapse to the quarter (there can be doubles)
purchases.full <- purchases.full[, list(
  total_expenditures = sum(total_expenditures),
  projection_factor = mean(projection_factor, na.rm = T),
  projection_factor_magnet = mean(projection_factor_magnet, na.rm = T),
  household_income = mean(household_income, na.rm = T),
  fips_county_code = mean(fips_county_code, na.rm = T),
  fips_state_code = mean(fips_state_code, na.rm = T),
  zip_code = mean(zip_code, na.rm = T),
  region_code = mean(region_code, na.rm = T)
), by = .(household_code, product_module_code, product_group_code,
          same_3zip_store, quarter, year) ]

## Calculate total expenditure per consumer in each quarter (across stores and modules)
purchases.full[, sum_total_exp_quarter := sum(total_expenditures),
               by = .(household_code, year, quarter)]


## Keep only best selling modules
best_selling_modules <- fread("/project2/igaarder/Data/best_selling_modules.csv")
keep_modules <- unique(best_selling_modules[, .(Module)][[1]])
purchases.full <- purchases.full[product_module_code %in% keep_modules]
rm(keep_modules)
rm(best_selling_modules)


## reshape to get a hh X module data
purchases.full <- dcast(purchases.full, household_code + product_group_code + fips_county_code + fips_state_code +
                          zip_code + quarter + year + projection_factor + projection_factor_magnet + region_code +
                          sum_total_exp_quarter + household_income + product_module_code ~ same_3zip_store, fun=sum,
                          value.var = "total_expenditures")

setnames(purchases.full,
         old = c("FALSE", "TRUE", "NA"),
         new = c("expenditures_diff3", "expenditures_same3", "expenditures_unkn3"))

#### Balance the panel: Key step for proper estimation
flog.info("Building skeleton")

# Collapse to hh x module that appeared at least once
possible.purchases <- purchases.full[, list(N_obs = .N), by = .(household_code, product_module_code, product_group_code)]
possible.purchases <- possible.purchases[N_obs > 0]
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
# minor changes for efficiency
possible.purchases.full <- data.table(possible.purchases.full, key = c("household_code", "product_module_code","product_group_code", "quarter", "year"))
purchases.full <- data.table(purchases.full, key = c("household_code", "product_module_code","product_group_code", "quarter", "year"))


purchases.full <- merge(possible.purchases.full, purchases.full, all.x = T)
rm(possible.purchases.full)
# assign purchases of 0 to those modules
purchases.full[, expenditures_diff3 := ifelse(is.na(expenditures_diff3),
                                    0, expenditures_diff3)]
purchases.full[, expenditures_same3 := ifelse(is.na(expenditures_same3),
                                             0, expenditures_same3)]
purchases.full[, expenditures_unkn3 := ifelse(is.na(expenditures_unkn3),
                                             0, expenditures_unkn3)]
# retrieve household data
household.cols <- c("fips_county_code", "fips_state_code", "zip_code", "projection_factor", 
                 "projection_factor_magnet", "region_code", "household_income")
purchases.full[, (household.cols) := lapply(.SD, as.numeric), 
               .SDcols = household.cols][,(household.cols) := lapply(.SD, mean, na.rm = T),
                                         by = .(household_code, year), .SDcols = household.cols] 


# retrieve total purchases
purchases.full[, sum_total_exp_quarter := mean(sum_total_exp_quarter, na.rm = T),
               by = .(household_code, year, quarter)]



## Identify taxability of module: import
taxability_panel <- fread("/project2/igaarder/Data/taxability_state_panel.csv")

setnames(taxability_panel,
         old = c("fips_state"),
         new = c("fips_state_code"))
# Collapse taxability to the quarter as rounding the mean and reduced rate as the mean
taxability_panel[, quarter := ceiling(month / 3)]
taxability_panel <- taxability_panel[, list(taxability = round(mean(taxability)),
                                            reduced_rate = mean(reduced_rate, na.rm = T)) , 
                                     by =.(product_module_code, product_group_code,
                                           fips_state_code, quarter, year)]


purchases.full <- merge(
  purchases.full, taxability_panel,
  by = c("fips_state_code", "product_module_code", "product_group_code", "year", "quarter"),
  all.x = T
)
# Assign unknown to purchases out of best selling module (taxability only identified for best selling)
purchases.full$taxability[is.na(purchases.full$taxability)] <- 2
# Keep only products for which we know the tax rate
purchases.full <- purchases.full[taxability != 2]


## merge on tax rates at household
all_goods_pi_path <- "../../monthly_taxes_county_5zip_2008_2014.csv"
all_pi <- fread(all_goods_pi_path)
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

# Impute tax rate to exempt items and to reduced rate items
purchases.full$sales_tax[purchases.full$taxability == 0 & !is.na(purchases.full$sales_tax)] <- 0
purchases.full[, sales_tax:= ifelse(!is.na(purchases.full$reduced_rate) & !is.na(purchases.full$sales_tax),
                                    reduced_rate, sales_tax)]

# computing the log tax before collapsing
purchases.full <- purchases.full[, ln_sales_tax := log1p(sales_tax)]

## Collapse to the group:
# Taxability as the mode within the group
# Expenditures as the sum
# Tax as a weighted mean 
# ad-hoc mode function
mode <- function(x) {
  x <- x[!is.na(x)]
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
# Building weights for tax: sales within group
purchases.full[, sales_weight := sum(expenditures_diff3 + expenditures_same3 + expenditures_unkn3)
               , by =.(quarter, year, product_group_code, product_module_code)]
purchases.full[, sales_weight := mean(sales_weight) , by =.(product_group_code, product_module_code)]
purchases.full[, sales_weight := sales_weight/sum(sales_weight) , by =.(product_group_code)]
purchases.full <- purchases.full[, list(
  expenditures_diff3 = sum(expenditures_diff3),
  expenditures_same3 = sum(expenditures_same3),
  expenditures_unkn3 = sum(expenditures_unkn3),
  taxability = mode(taxability),
  ln_sales_tax = weighted.mean(ln_sales_tax,sales_weight)
), by = .(household_code, fips_county_code, fips_state_code, product_group_code,
          zip_code, region_code, quarter, year, sum_total_exp_quarter, projection_factor,
          projection_factor_magnet, household_income) ]


## Create interest variables
purchases.full <- purchases.full[, expenditures := expenditures_diff3 + expenditures_same3 + expenditures_unkn3]

## Share
purchases.sample[, share_expenditures := expenditures/sum_total_exp_quarter]

## Logarithms
# Expenditures
purchases.sample <- purchases.sample[, ln_expenditures := log(expenditures)]
purchases.sample$ln_expenditures[is.infinite(purchases.sample$ln_expenditures)] <- NA

purchases.sample[, ln_expenditures_taxable := ifelse(taxability == 1, ln_expenditures, NA)]
purchases.sample[, ln_expenditures_non_taxable := ifelse(taxability == 0, ln_expenditures, NA)]

# Share
purchases.sample <- purchases.sample[, ln_share := log(share_expenditures)]
purchases.sample$ln_share[is.infinite(purchases.sample$ln_share)] <- NA

purchases.sample[, ln_share_taxable := ifelse(taxability == 1, ln_share, NA)]
purchases.sample[, ln_share_non_taxable := ifelse(taxability == 0, ln_share, NA)]

fwrite(purchases.full, "cleaning/consumer_panel_q_hh_group_2006-2016.csv")