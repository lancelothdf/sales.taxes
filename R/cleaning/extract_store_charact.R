#' This code retrieves variables to refine the sample of estimation as a robustness exploration.
#' First we retrieve the channel type of the store to restrict only to groceries
#' Second we try to identify "chains" as in DellaVigna and Gentzkow (2019) to exclude these
#' 

library(data.table)
library(lfe)
library(futile.logger)
library(AER)


setwd("/project2/igaarder")

## Open all data
stores.all <- data.table(NULL)
for (yr in 2006:2016) {
  
  ## Store File path
  path <- paste0("Data/Nielsen/stores_", yr, ".dta")
  
  ## Open year file
  stores.yr <- read.dta13(path)
  
  ## append to previous years
  stores <- rbind(stores.all, stores.yr)
  rm(stores.yr)
}

length(unique(stores.all$store_code_uc))

#### Follow DellaVigna and Gentzkow identification of Chains
# Main difference:they focus in 2006 to 2014 period not 2006-2016


## 0.  Define a chain to be a unique combination of two identifiers in the Nielsen data: parent code and retailer code
stores.dg <- stores.all[, chain := .GRP, by = .(retailer_code, parent_code)]

### A. Stores Exclusion
## 1. Exclude stores that switch chains over time
stores.dg[, chain.mn := mean(chain), by = .(store_code_uc)]
stores.dg <- stores.dg[ chain == chain.mn]

## 2. Exclude stores in the sample for fewer than 104 weeks
stores.dg[, years := .N, by = .(store_code_uc)]
stores.dg <- stores.dg[ years > 2]

### B. Chain exclusions

## 1. Exclude Chains that are present less than 8 years
stores.dg[, years := .N, by = .(chain)]
stores.dg <- stores.dg[ years > 7]


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
stores.dg[, c("year", "store_code_uc")]
stores.dg[, DGsample := 1]

stores.all <- merge(stores.all, stores.dg, by = c("year", "store_code_uc"), all.x = T)

## Save this File
fwrite(stores.all, "Data/Nielsen/stores_all.csv")
