##### Santiago Lacouture
#' Sales Taxes
#' Replication File. Updated on 8/16/2022
#' Step 0: Cleaning stores data by consumers

#' This code retrieves variables to refine the sample of estimation as a robustness exploration.
#' First we retrieve the channel type of the store to restrict only to groceries
#' Second we try to identify "chains" as in DellaVigna and Gentzkow (2019) to exclude these
#' Third, we extract a store characteristics based on their consumers. Similarly to DellaVigna and Gentzkow (2019)
#' Update 31-1-19: The Number of trips is itself weigthed by the projection factor of the household

library(data.table)
library(lfe)
library(futile.logger)
library(AER)
library(readstata13)
library(geodist)

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
nrow(stores.all)

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

## Make these exclusions clearer
stores.dg <- stores.dg[, n_year := .N, by = .(store_code_uc)]

length(unique(stores.dg$store_code_uc))

#### Mark this sample and merge to all stores sample
stores.dg <- stores.dg[, c("year", "store_code_uc", "chain")]
stores.dg[, DGsample := 1]

stores.all <- merge(stores.all, stores.dg, by = c("year", "store_code_uc"), all.x = T)
rm(stores.dg)

## Simplify the file to avoid loading by amount of data
stores.all <- stores.all[, c("year", "store_code_uc", "channel_code", "DGsample", "retailer_code", "parent_code", "chain", "fips_state_code", "fips_county_code")]
nrow(stores.all)
ncol(stores.all)


#### 3. Follow DellaVigna and Gentzkow store characteristics identification ----

## Need to identify consumers for each store
# Reset working directory to work with the Consumer PAnel
full.purchases <- data.table(NULL)
for (yr in 2006:2016) {
  ## necessary filepaths
  folderpath <- paste0("Data/Nielsen/Household_panel/HMS/", yr, "/Annual_Files/")
  panelists_file <- paste0(folderpath, "panelists_", yr, ".tsv")
  purchases_file <- paste0(folderpath, "purchases_", yr, ".tsv")
  trips_file <- paste0(folderpath, "trips_", yr, ".tsv")
  store_file <- paste0("Data/Nielsen/stores_", yr, ".dta")

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
  panelists <- panelists[, .(Household_Cd, Panelist_ZipCd, Projection_Factor)]
  setnames(panelists,
           old = c("Household_Cd", "Panelist_ZipCd", "Projection_Factor"),
           new = c("household_code", "zip_code", "projection"))
  flog.info("Merging panelists data to purchases for %s", yr)
  
  # Normalize projection factor within year
  panelists[, projection := projection/sum(projection)]
  
  purchases <- merge(purchases, panelists, by = c("household_code"), all.x = T)
  rm(panelists)
  
  ## append to final data across years
  full.purchases <- rbind(full.purchases, purchases)
  rm(purchases)
}


## Finally, collapse across years sum, weigthing by projection factor
full.purchases <- full.purchases[, .(total_expenditures = sum(total_expenditures*projection)/sum(projection),
                                     n_trips = sum(n_trips*projection)/sum(projection),
                                     r_n_trips= sum(n_trips)),
                                 by = .(household_code, store_code_uc, zip_code)]
nrow(full.purchases)
## Make this compatible
full.purchases[, zip_code := as.integer(zip_code)]

## Merge zip code info to the household
zip_data <- fread("Data/consumer_zip_code_data.csv")
zip_data[, zip_code := as.integer(zip_code)]
zip_data[, av_hh_income := as.integer(av_hh_income)]
zip_data[, per_bachelor_25 := as.double(per_bachelor_25)]
zip_data[, median_age := as.double(median_age)]
full.purchases <- merge(full.purchases, zip_data, by = "zip_code")
rm(zip_data)

## Collapse at the store level
store_costumer_ch <- full.purchases[, .(av_hh_income_sales = weighted.mean(av_hh_income, na.rm = T, w = total_expenditures),
                                        per_bachelor_25_sales = weighted.mean(per_bachelor_25, na.rm = T, w = total_expenditures),
                                        median_age_sales = weighted.mean(median_age, na.rm = T, w = total_expenditures),
                                        per65_sales = weighted.mean(per65, na.rm = T, w = total_expenditures),
                                        per_black_sales = weighted.mean(per_black, na.rm = T, w = total_expenditures),
                                        per_hisp_sales = weighted.mean(per_hisp, na.rm = T, w = total_expenditures),
                                        x_sales = weighted.mean(x, na.rm = T, w = total_expenditures),
                                        y_sales = weighted.mean(y, na.rm = T, w = total_expenditures),
                                        av_hh_income_trips = weighted.mean(av_hh_income, na.rm = T, w = n_trips),
                                        per_bachelor_25_trips = weighted.mean(per_bachelor_25, na.rm = T, w = n_trips),
                                        median_age_trips = weighted.mean(median_age, na.rm = T, w = n_trips),
                                        per65_trips = weighted.mean(per65, na.rm = T, w = n_trips),
                                        per_black_trips = weighted.mean(per_black, na.rm = T, w = n_trips),
                                        per_hisp_trips = weighted.mean(per_hisp, na.rm = T, w = n_trips),
                                        x_trips = weighted.mean(x, na.rm = T, w = n_trips),
                                        y_trips = weighted.mean(y, na.rm = T, w = n_trips),
                                        n_households = .N,
                                        n_trips = sum(r_n_trips)
                                        ), by = c("store_code_uc")]
rm(full.purchases)
length(unique(store_costumer_ch$store_code_uc))
nrow(store_costumer_ch)
ncol(store_costumer_ch)
#### Competition measures
## identify stores with location for efficiency. Need to merge with chains identified
stores_loc <- store_costumer_ch[!is.nan(x_sales) & !is.na(x_sales)]

chain.data <- stores.all[, .(chain = max(chain, na.rm = T)), by = c("store_code_uc")]
chain.data[, chain := ifelse(is.infinite(chain), max(chain, na.rm = T) + .I, chain)] # Create fake chain number for unidentified stores
stores_loc <- merge(stores_loc, chain.data, by = c("store_code_uc"), all.x = T)
length(unique(stores_loc$store_code_uc))
rm(chain.data)
nrow(stores_loc)
ncol(stores_loc)


stores_loc_data_sales <- as.matrix(stores_loc[, c("x_sales", "y_sales")])
stores_loc_data_trips <- as.matrix(stores_loc[, c("x_trips", "y_trips")])

# Calculate distances
distances_sales <- geodist(stores_loc_data_sales)
distances_trips <- geodist(stores_loc_data_trips)

dim(distances_sales)

# Check thresholds
distances_10_sales <- distances_sales <= 10000
distances_5_sales <- distances_sales <= 5000

distances_10_trips <- distances_trips <= 10000
distances_5_trips <- distances_trips <= 5000

# Chain matrix: 1 if same chain
chains <- stores_loc[["chain"]]
print(length(chains))
same_chain <- matrix(0, length(chains), length(chains))
for (i in 1:length(chains)) {
  val.i <- chains[i]
  if (!is.na(val.i)) {
    for (j in 1:length(chains)) {
      same_chain[i,j] <- (val.i == chains[j])
    }
  }
}
# Remove elements nolonger used to save memory
rm(chains, distances_trips, distances_sales, stores_loc_data_sales, stores_loc_data_trips)

diff_chain <- 1 - same_chain
# Create distances matrix for same and diff chain
# First calculate for whole matrix,
# then sum across columns (substract 1 for diagonal when the same store is included)

# 10 km - sales
distances_10_sales_same <- distances_10_sales * same_chain
distances_10_sales_diff <- distances_10_sales * diff_chain
dim(distances_10_sales_same)
distances_10_sales <- colSums(distances_10_sales) - 1
distances_10_sales_same <- colSums(distances_10_sales_same) - 1
distances_10_sales_diff <- colSums(distances_10_sales_diff)
length(distances_10_sales_same)
# 5 km - sales
distances_5_sales_same <- distances_5_sales * same_chain
distances_5_sales_diff <- distances_5_sales * diff_chain
dim(distances_5_sales_same)
distances_5_sales <- colSums(distances_5_sales) - 1
distances_5_sales_diff <- colSums(distances_5_sales_diff)
distances_5_sales_same <- colSums(distances_5_sales_same) - 1
length(distances_5_sales_same)
# 10 km - trips
distances_10_trips_same <- distances_10_trips * same_chain
distances_10_trips_diff <- distances_10_trips * diff_chain
dim(distances_10_trips_same)
distances_10_trips <- colSums(distances_10_trips) - 1
distances_10_trips_same <- colSums(distances_10_trips_same) - 1
distances_10_trips_diff <- colSums(distances_10_trips_diff)
length(distances_10_trips_same)
# 5 km - trips
distances_5_trips_same <- distances_5_trips * same_chain
distances_5_trips_diff <- distances_5_trips * diff_chain
dim(distances_5_trips_same)
distances_5_trips <- colSums(distances_5_trips) - 1
distances_5_trips_same <- colSums(distances_5_trips_same) - 1
distances_5_trips_diff <- colSums(distances_5_trips_diff)
length(distances_5_trips_same)

# Remove elements nolonger used to save memory
rm(same_chain, diff_chain)
# Put all data together, should have preserved order
distances <- data.table(distances_10_sales, distances_5_sales, distances_10_trips, distances_5_trips,
                        distances_10_sales_same, distances_5_sales_same, distances_10_trips_same, distances_5_trips_same,
                        distances_10_sales_diff, distances_5_sales_diff, distances_10_trips_diff, distances_5_trips_diff)
ncol(distances)
nrow(distances)

# Remove elements nolonger used to save memory
rm(distances_10_sales, distances_5_sales, distances_10_trips, distances_5_trips,
    distances_10_sales_same, distances_5_sales_same, distances_10_trips_same, distances_5_trips_same,
    distances_10_sales_diff, distances_5_sales_diff, distances_10_trips_diff, distances_5_trips_diff)

# Put data together
stores_loc <- cbind(data.table(store_code_uc = stores_loc$store_code_uc), distances)
nrow(stores_loc)
ncol(stores_loc)

##### Merge all info to store data
stores.all <- merge(stores.all, store_costumer_ch, by = "store_code_uc", all.x = T)
stores.all <- merge(stores.all, stores_loc, by = "store_code_uc", all.x = T)
# Remove elements nolonger used to save memory
rm(stores_loc, store_costumer_ch)
nrow(stores.all)
ncol(stores.all)

## Save this File
fwrite(stores.all, "Data/Replication/stores_all.csv")

### 4. Identify stores in retail data -----------------
our.data <- fread("Data/Replication/all_pi.csv")
nrow(our.data)

## Collapse to the store level
our.data <- our.data[, .(N_semesters = .N), by = .(store_code_uc, product_module_code)] ## First across years
our.data <- our.data[, .(N_modules = .N), by = .(store_code_uc)]
nrow(our.data)
## Seems to break here... but saved above

## Merge info to store data
stores.all <- merge(stores.all, our.data, by = "store_code_uc", all.x = T)
nrow(stores.all)
ncol(stores.all)

## Save this File
fwrite(stores.all, "Data/Replication/stores_all.csv")
