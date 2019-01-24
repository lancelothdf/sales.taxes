#' Author: John Bonney
#' Purpose: Create national price index from store-category-level price indices
#'

library(readstata13)
library(data.table)

setwd("/project2/igaarder")

## load and convert .dta to .csv
pi_data <- read.dta13("Data/Nielsen/Price_quantity_indices_food.dta")
pi_data <- as.data.table(pi_data)
fwrite(pi_data, "Data/Nielsen/price_quantity_indices_food.csv")

print(paste0("N (raw): ", nrow(pi_data)))

## get rid of the observations for 2006
pi_data <- pi_data[year >= 2007]
print(paste0("N (dropping 2006): ", nrow(pi_data)))

## balance on store-level
stores <- unique(pi_data, by = c("year", "quarter", "store_code_uc"))
stores <- stores[, list(n = .N), by = store_code_uc]
keep_stores <- stores[n == (2016 - 2006) * 4][["store_code_uc"]]

pi_data <- pi_data[store_code_uc %in% keep_stores]
print(paste0("N (balancing on store-level): ", nrow(pi_data)))

## balance on module X store level
# store_modules <- pi_data[, list(n = .N), by = .(store_code_uc, product_module_code)]
# keep_store_modules <- store_modules[n == (2016 - 2006) * 4]
# keep_store_modules[, balanced := TRUE]
#
# pi_data <- merge(pi_data, keep_store_modules,
#                  by = c("store_code_uc", "product_module_code"))
# pi_data <- pi_data[balanced == TRUE]
# print(paste0("N (balancing on store-module-level): ", nrow(pi_data)))


## renormalize index so that it equals 1 in 2007 Q1
base_pi <- pi_data[year == 2007 & quarter == 1]
base_pi[, base_cpricei := cpricei]
base_pi <- base_pi[, .(store_code_uc, product_module_code, base_cpricei)]

pi_data <- merge(pi_data, base_pi,
                 by = c("store_code_uc", "product_module_code"))

pi_data[, cpricei := cpricei / base_pi]

## merge sales shares onto cleaned price indices
sales_data <- fread("Data/national_sales_shares.csv")

 # TODO: see what kind of a merge it is doing (I assume inner merge) -- we need
 # to keep Q4 2006 sales
pi_data <- merge(pi_data, sales_data,
                 by = c("store_code_uc", "quarter", "year", "product_module_code"))
fwrite(pi_data, "Data/Nielsen/price_quantity_indices_food.csv")

## calculate the price indices
setkey(pi_data, store_code_uc, product_module_code, year, quarter)
# create the exponent
pi_data[, s_average := (sales_share + shift(sales_share, 1, type = "lag")) / 2,
        by = .(store_code_uc, product_module_code)]
# create the base
pi_data[, pi_change := cpricei / shift(cpricei, 1, type = "lag"),
        by = .(store_code_uc, product_module_code)]

# compute P_t / P_{t-1}
print(nrow(pi_data))
pi_data <- pi_data[!is.na(pi_change) & !is.na(s_average)]
national_pi <- pi_data[, list(national_index = prod(pi_change^s_average)),
                       by = .(quarter, year)]

fwrite(national_pi, "Data/national_price_index.csv")
