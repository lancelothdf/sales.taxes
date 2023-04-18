##### Santiago Lacouture
#' Sales Taxes
#' Replication File. Updated on 04/18/2023
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


### NOTE: at the bottom, when output files, we included full path because output has to be saved on Midway3 instead of Midway2
setwd("/project/igaarder")


## Save this File
stores.all <- fread("/project/igaarder/Data/Replication_v4/stores_all.csv")
nrow(stores.all)

### 4. Identify stores in retail data -----------------
our.data <- fread("/project/igaarder/Data/Replication_v4/all_pi.csv")
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
fwrite(stores.all, "/project/igaarder/Data/Replication_v4/stores_all.csv")
