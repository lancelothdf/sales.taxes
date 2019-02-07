#' Author: John Bonney
#' Purpose: Create national price index from store-category-level price indices
#'
#' Note that this is a reworking of create_national_price_index.R, it merely
#'     incorporates the new geometric price indices.
#'

library(readstata13)
library(data.table)

## for testing
# pi_data <- data.table(expand.grid(year = 2006:2008, quarter = 1:4,
#                                   store_code_uc = c("A", "B"),
#                                   product_module_code = c("goat", "cheese")))
# pi_data[, geocpricei := runif(nrow(pi_data), 100, 300)]
#
# sales_data <- pi_data[, .(year, quarter, store_code_uc, product_module_code)]
# sales_data[, sales := runif(nrow(sales_data), 1000, 10000)]

setwd("/project2/igaarder")

## load and convert .dta to .csv -----------------------------------------------
pi_data <- read.dta13("Data/Nielsen/Geo_prices_quantity_indices_food.dta")
pi_data <- as.data.table(pi_data)
fwrite(pi_data, "Data/Nielsen/Geo_price_quantity_indices_food.csv")

print(paste0("N (raw): ", nrow(pi_data)))
print(paste0("N stores (raw): ", uniqueN(pi_data$store_code_uc)))
print(paste0("N store-products (raw): ", uniqueN(pi_data[, .(store_code_uc, product_module_code)])))

## get rid of the observations for 2006 Q3 and earlier -------------------------
pi_data <- pi_data[year >= 2007 | (year == 2006 & quarter == 4)]

print(paste0("N (dropping 2006 Q3 and earlier): ", nrow(pi_data)))
print(paste0("N stores (dropping 2006 Q3 and earlier): ", uniqueN(pi_data$store_code_uc)))
print(paste0("N store-products (dropping 2006 Q3 and earlier): ",
             uniqueN(pi_data[, .(store_code_uc, product_module_code)])))

## merge sales shares onto price indices ---------------------------------------
sales_data <- fread("Data/sales_quarterly_2006-2016.csv")

pi_data <- merge(pi_data, sales_data, by = c("store_code_uc", "product_module_code",
                                             "quarter", "year"))
rm(sales_data)
gc()

print(paste0("N (merging on sales): ", nrow(pi_data)))
print(paste0("N stores (merging on sales): ", uniqueN(pi_data$store_code_uc)))
print(paste0("N store-products (merging on sales): ",
             uniqueN(pi_data[, .(store_code_uc, product_module_code)])))

fwrite(pi_data, "Data/Nielsen/Geo_price_quantity_indices_food.csv")

## balance on store-level ------------------------------------------------------
stores <- unique(pi_data, by = c("year", "quarter", "store_code_uc"))
stores <- stores[, list(n = .N), by = store_code_uc]
keep_stores <- stores[n == (2016 - 2006) * 4 + 1][["store_code_uc"]]
pi_data <- pi_data[store_code_uc %in% keep_stores]

print(paste0("N (balancing on store-level): ", nrow(pi_data)))
print(paste0("N stores (balancing on store-level): ", uniqueN(pi_data$store_code_uc)))
print(paste0("N store-products (balancing on store-level): ",
             uniqueN(pi_data[, .(store_code_uc, product_module_code)])))

## renormalize index so that it equals 1 in 2006 Q4 ----------------------------
base_pi <- pi_data[year == 2006 & quarter == 4]
base_pi[, base_geocpricei := geocpricei]
base_pi <- base_pi[, .(store_code_uc, product_module_code, base_geocpricei)]

pi_data <- merge(pi_data, base_pi,
                 by = c("store_code_uc", "product_module_code"))

pi_data[, geocpricei := geocpricei / base_geocpricei]

## compute sales shares --------------------------------------------------------
pi_data[, national_sales := sum(sales), by = .(quarter, year)]
pi_data[, sales_share := sales / national_sales]  # this is our S_{j,r}^t
fwrite(pi_data, "Data/Nielsen/Geo_price_quantity_indices_food.csv")

## calculate the price indices -------------------------------------------------
setkey(pi_data, store_code_uc, product_module_code, year, quarter)

### create the exponent
pi_data[, s_average := (sales_share + shift(sales_share, 1, type = "lag")) / 2,
        by = .(store_code_uc, product_module_code)]
### create the base
pi_data[, pi_change := geocpricei / shift(geocpricei, 1, type = "lag"),
        by = .(store_code_uc, product_module_code)]

### compute P_t / P_{t-1}
national_pi <- pi_data[, list(national_ratio = prod(pi_change^s_average)),
                       by = .(quarter, year)]
national_pi[year == 2006 & quarter == 4, national_ratio := 1]
# compute the P_t as a cumulative product
national_pi[, national_index := cumprod(national_ratio)]

print(national_pi[])
fwrite(national_pi, "Data/national_pi_geo.csv")
