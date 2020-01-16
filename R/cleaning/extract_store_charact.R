#' This code retrieves variables to refine the sample of estimation as a robustness exploration.
#' First we retrieve the channel type of the store to restrict only to groceries
#' Second we try to identify "chains" as in DellaVigna and Gentzkow (2019) to exclude these
#' Third, we extract a store characteristics based on their consumers. Similarly to DellaVigna and Gentzkow (2019)

library(data.table)
library(lfe)
library(futile.logger)
library(AER)
library(readstata13)


setwd("/project2/igaarder")

## Open all data
stores.all <- data.table(NULL)
for (yr in 2006:2016) {
  
  ## Store File path
  path <- paste0("Data/Nielsen/stores_", yr, ".dta")
  
  ## Open year file
  stores.yr <- as.data.table(read.dta13(path))
  stores.yr[, year := yr]
  
  ## append to previous years
  stores.all <- rbind(stores.all, stores.yr)
  rm(stores.yr)
}

#### 1. Keep only 3 big types of stores ------
stores.all <- stores.all[channel_code %in% c("F", "M", "D")]

length(unique(stores.all$store_code_uc))

#### 2. Follow DellaVigna and Gentzkow identification of Chains -------
# Main difference:they focus in 2006 to 2014 period not 2006-2016

### A. Stores Exclusion
## 3. stores without any consumer purchases in the Homescan data. Identifyied w. blank retailer_code as said in Guide note of table
# We start with this exclusion to avoid problems later. This is probably what they do, although they mention it in a different order
stores.dg <- stores.all[!is.na(retailer_code)]

## 0.  Define a chain to be a unique combination of two identifiers in the Nielsen data: parent code and retailer code
stores.dg <- stores.dg[, chain := .GRP, by = .(retailer_code, parent_code)]

## 1. Exclude stores that switch chains over time
stores.dg[, chain.mn := mean(chain), by = .(store_code_uc)]
stores.dg <- stores.dg[ chain == chain.mn]

## 2. Exclude stores in the sample for fewer than 104 weeks
stores.dg[, years := .N, by = .(store_code_uc)]
stores.dg <- stores.dg[ years > 2]


### B. Chain exclusions

## 1. Exclude Chains that are present less than 8 years (here 10)
stores.dg[, years := uniqueN(year), by = .(chain)]
stores.dg <- stores.dg[ years > 9]


## 2. Exclude cases where the same retailer code appears for stores with different parent codes, keep the parent code associated with the majority
#     of such stores, and further exclude cases in which this retailer code-parent code combination accounts for less than 80% of the stores with 
#     a given retailer code.
stores.dg[, mn.parent_code := mean(parent_code), by = .(retailer_code)]
stores.dg[, p.parent_code := .N / sum(.N), by = .(retailer_code)]
stores.dg <- stores.dg[(parent_code != mn.parent_code & p.parent_code == max(p.parent_code, na.rm = T)) | p.parent_code >= .8 ]

## 3. Exclude chains in which 60% or more of stores belonging to a retailer code-parent code combination change either parent code or retailer code.
stores.dg[, mn.parent := mean(parent_code), by = .(store_code_uc)]
stores.dg[, mn.retailer := mean(retailer_code), by = .(store_code_uc)]
stores.dg[, change := ifelse(parent_code == mn.parent & retailer_code == mn.retailer,
                             0, 1)]
stores.dg[, p.change := mean(change), by = .(chain)]
stores.dg <- stores.dg[p.change < .6]

length(unique(stores.dg$store_code_uc))

#### Mark this sample and merge to all stores sample
stores.dg <- stores.dg[, c("year", "store_code_uc")]
stores.dg[, DGsample := 1]

stores.all <- merge(stores.all, stores.dg, by = c("year", "store_code_uc"), all.x = T)
rm(stores.dg)

## Simplify the file to avoid loading by amount of data
stores.all <- stores.all[, c("year", "store_code_uc", "channel_code", "DGsample")]



#### 3. Follow DellaVigna and Gentzkow store characteristics identification ----

## Need to identify consumers for each store
# Reset working directory to work with the Consumer PAnel
setwd("/project2/igaarder/Data/Nielsen/Household_panel")

full.purchases <- data.table(NULL)
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
  
  ## sum expenditures over UPCs to the trip level
  flog.info("Summing expenditures over UPCs for %s", yr)
  purchases <- purchases[, list(
    total_expenditures = sum(total_price_paid)
  ), by = .(trip_code_uc)]
  
  ## merge on the trip data
  flog.info("Loading in trips data for %s", yr)
  trips <- fread(trips_file)
  trips[, purchase_date := as.Date(purchase_date, "%Y-%m-%d")]
  trips[, month := month(purchase_date)]
  trips[, year := year(purchase_date)]
  trips <- trips[, .(trip_code_uc, household_code, store_code_uc,
                     year, month)]
  
  flog.info("Merging trips data to purchases for %s", yr)
  purchases <- merge(purchases, trips, by = "trip_code_uc")
  rm(trips)
  
  ## Collapse by household (since is total there wont be problem)
  purchases <- purchases[, list(
    total_expenditures = sum(total_expenditures),
    n_trips = .N), 
    by = .(household_code, store_code_uc)]
  
  ## merge on some individual info
  flog.info("Loading in panelists data for %s", yr)
  panelists <- fread(panelists_file)
  panelists <- panelists[, .(Household_Cd, Panelist_ZipCd)]
  setnames(panelists,
           old = c("Household_Cd", "Panelist_ZipCd"),
           new = c("household_code", "zip_code"))
  flog.info("Merging panelists data to purchases for %s", yr)
  purchases <- merge(purchases, panelists, by = c("household_code"), all.x = T)
  rm(panelists)
  
  ## append to final data across years
  full.purchases <- rbind(full.purchases, purchases)
  rm(purchases)
}

## Finally, collapse across years
full.purchases <- full.purchases[, .(total_expenditures = sum(total_expenditures),
                                     n_trips = sum(n_trips)),
                                 by = .(household_code, store_code_uc, zip_code)]
## Make this compatible
full.purchases[, zip_code := as.integer(zip_code)]

## Merge zip code info to the household
zip_data <- fread("../../consumer_zip_code_data.csv", colClasses = c(av_hh_income = "integer", per_bachelor_25 = "double", median_age = "double"))
zip_data[, zip_code := as.integer(zip_code)]
full.purchases <- merge(full.purchases, zip_data, by = "zip_code")
rm(zip_data)

## Collapse at the store level
store_costumer_ch <- full.purchases[, .(av_hh_income_sales = weighted.mean(av_hh_income, na.rm = T, w = total_expenditures),
                                        per_bachelor_25_sales = weighted.mean(per_bachelor_25, na.rm = T, w = total_expenditures),
                                        median_age_sales = weighted.mean(median_age, na.rm = T, w = total_expenditures),
                                        av_hh_income_trips = weighted.mean(av_hh_income, na.rm = T, w = n_trips),
                                        per_bachelor_25_trips = weighted.mean(per_bachelor_25, na.rm = T, w = n_trips),
                                        median_age_trips = weighted.mean(median_age, na.rm = T, w = n_trips),
                                        n_households = .N,
                                        n_trips = sum(n_trips)
                                        ), by = c("store_code_uc")]

## Merge info to store data
stores_all <- merge(stores_all, store_costumer_ch, by = "store_code_uc", all.x = T)

## Save this File
fwrite(stores.all, "../stores_all.csv")
