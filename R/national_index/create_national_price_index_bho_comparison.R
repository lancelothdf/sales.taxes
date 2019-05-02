#' Author: John Bonney
#' Purpose: Create national price index from store-category-level price indices
#'

library(readstata13)
library(data.table)

## for testing
# pi_data <- data.table(expand.grid(year = 2006:2008, quarter = 1:4,
#                                   store_code_uc = c("A", "B"),
#                                   product_module_code = c("goat", "cheese")))
# pi_data[, cpricei := runif(nrow(pi_data), 100, 300)]
#
# sales_data <- pi_data[, .(year, quarter, store_code_uc, product_module_code)]
# sales_data[, sales := runif(nrow(sales_data), 1000, 10000)]

setwd("/project2/igaarder")

## load and convert .dta to .csv
pi_data <- read.dta13("Data/Nielsen/Price_quantity_indices_food.dta")
pi_data <- as.data.table(pi_data)
nonfood_pi <- read.dta13("Data/Nielsen/Price_quantity_indices_nonfood.dta")
pi_data <- rbind(pi_data, nonfood_pi)
rm(nonfood_pi)

print(paste0("N (raw): ", nrow(pi_data)))
print(paste0("N stores (raw): ", length(unique(pi_data$store_code_uc))))
print(paste0("N store-products (raw): ", nrow(unique(pi_data[, .(store_code_uc, product_module_code)]))))

## merge sales shares onto price indices
sales_data <- fread("Data/sales_quarterly_2006-2016.csv")

# this should be an inner merge
nrow_base <- nrow(pi_data)
pi_data <- merge(pi_data, sales_data,
                 by = c("store_code_uc", "quarter", "year", "product_module_code"))

if (nrow_base != nrow(pi_data)){
  warning("Merging sales to indices changes the number of observations")
  print(paste0("N before merge: ", nrow_base))
  print(paste0("N after merge: ", nrow(pi_data)))
  pi_data <- pi_data[!is.na(cpricei)]
}

rm(sales_data)

## balance on store-level
# stores <- unique(pi_data, by = c("year", "quarter", "store_code_uc"))
# stores <- stores[, list(n = .N), by = store_code_uc]
# keep_stores <- stores[n == (2016 - 2006) * 4 + 1][["store_code_uc"]]
# pi_data <- pi_data[store_code_uc %in% keep_stores]
#
# print(paste0("N (balancing on store-level): ", nrow(pi_data)))
# print(paste0("N stores (balancing on store-level): ", length(unique(pi_data$store_code_uc))))
# print(paste0("N store-products (balancing on store-level): ",
#              nrow(unique(pi_data[, .(store_code_uc, product_module_code)]))))

## balance on module X store level
store_modules <- pi_data[, list(n = .N), by = .(store_code_uc, product_module_code)]
keep_store_modules <- store_modules[n == (2016 - 2005) * 4]
keep_store_modules[, balanced := TRUE]

pi_data <- merge(pi_data, keep_store_modules,
                 by = c("store_code_uc", "product_module_code"))
pi_data[, balanced := ifelse(is.na(balanced), FALSE, balanced)]
pi_data <- pi_data[balanced == TRUE]
print(paste0("N (balancing on store-module-level): ", nrow(pi_data)))
print(paste0("N stores (balancing on store-module-level): ",
             length(unique(pi_data$store_code_uc))))
print(paste0("N store-products (balancing on store-module-level): ",
             nrow(unique(pi_data[, .(store_code_uc, product_module_code)]))))

## renormalize index so that it equals 1 in 2006 Q1
base_pi <- pi_data[year == 2006 & quarter == 1]
base_pi[, base_cpricei := cpricei]
base_pi <- base_pi[, .(store_code_uc, product_module_code, base_cpricei)]

pi_data <- merge(pi_data, base_pi,
                 by = c("store_code_uc", "product_module_code"))

pi_data[, cpricei := cpricei / base_cpricei]

## compute sales shares
pi_data[, national_sales := sum(sales), by = .(quarter, year)]
pi_data[, sales_share := sales / national_sales]  # this is our S_{j,r}^t

## calculate the price indices
setkey(pi_data, store_code_uc, product_module_code, year, quarter)

### create the exponent
pi_data[, s_average := (sales_share + shift(sales_share, 1, type = "lag")) / 2,
        by = .(store_code_uc, product_module_code)]
### create the base
pi_data[, pi_change := cpricei / shift(cpricei, 1, type = "lag"),
        by = .(store_code_uc, product_module_code)]

### compute P_t / P_{t-1}
national_pi <- pi_data[, list(national_ratio = prod(pi_change^s_average)),
                       by = .(quarter, year)]
national_pi[year == 2006 & quarter == 1, national_ratio := 1]
# compute the P_t as a cumulative product
national_pi[, national_index := cumprod(national_ratio)]

print(national_pi[])
fwrite(national_pi, "Data/national_pi_quarterly_bho_comparison.csv")
