#' Author: John Bonney
#' Purpose: Create national price index from county-level aggregated price
#'     indices (weighting by county population).

library(readstata13)
library(data.table)

setwd("/project2/igaarder")

## for testing
# pi_data <- data.table(expand.grid(year = 2006:2016, quarter = 1:4,
#                                   store_code_uc = c("A", "B"),
#                                   product_module_code = c("goat", "cheese")))
# pi_data[, cpricei := runif(nrow(pi_data), 100, 300)]
#
# sales_data <- pi_data[, .(year, quarter, store_code_uc, product_module_code)]
# sales_data[, sales := runif(nrow(sales_data), 1000, 10000)]

## load sales data to compute sales shares
sales_data <- fread("Data/sales_quarterly_2006-2016.csv")

## build sales shares for each module at the national level for each quarter
national_sales <- sales_data[, list(total_sales = sum(sales)),
                             by = .(quarter, year, product_module_code)]
national_sales[, total_national_sales := sum(total_sales), by = .(quarter, year)]
national_sales[, sales_share := total_sales / total_national_sales]

## grab county/state information to attach to stores
# stores_to_counties <- unique(sales_data[, .(store_code_uc, fips_state, fips_county, quarter, year)])

rm(sales_data) # free up memory
gc()

## load price index data

# pi_data <- read.dta13("Data/Nielsen/Price_quantity_indices_food.dta")
# pi_data <- as.data.table(pi_data)
# fwrite(pi_data, "Data/Nielsen/price_quantity_indices_food.csv")

# print(paste0("N (raw): ", nrow(pi_data)))
# print(paste0("N stores (raw): ", length(unique(pi_data$store_code_uc))))
# print(paste0("N store-products (raw): ",
#              nrow(unique(pi_data[, .(store_code_uc, product_module_code)]))))

## merge county variable
# pi_data <- merge(pi_data, stores_to_counties, by = c("store_code_uc", "quarter", "year"))
# fwrite(pi_data, "Data/Nielsen/price_quantity_indices_food.csv")
pi_data <- fread("Data/Nielsen/price_quantity_indices_food.csv")

print(paste0("N (merging counties): ", nrow(pi_data)))
print(paste0("N stores (merging counties): ", length(unique(pi_data$store_code_uc))))
print(paste0("N store-products (merging counties): ",
             nrow(unique(pi_data[, .(store_code_uc, product_module_code)]))))

## keep only 2006 Q4 and after
pi_data <- pi_data[year >= 2007 | (year == 2006 & quarter == 4)]

print(paste0("N (dropping 2006 Q3 and earlier): ", nrow(pi_data)))
print(paste0("N stores (dropping 2006 Q3 and earlier): ", length(unique(pi_data$store_code_uc))))
print(paste0("N store-products (dropping 2006 Q3 and earlier): ",
             nrow(unique(pi_data[, .(store_code_uc, product_module_code)]))))

## merge on sales shares
head(pi_data)
head(national_sales)
pi_data <- merge(pi_data, national_sales, by = c("quarter", "year", "product_module_code"))
pi_data <- pi_data[!is.na(sales_share) & !is.na(cpricei)]

print(paste0("N (after merging sales): ", nrow(pi_data)))
print(paste0("N stores (after merging sales): ", length(unique(pi_data$store_code_uc))))
print(paste0("N store-products (after merging sales): ",
             nrow(unique(pi_data[, .(store_code_uc, product_module_code)]))))

## balance panel on store-level
stores <- unique(pi_data, by = c("year", "quarter", "store_code_uc"))
stores <- stores[, list(n = .N), by = store_code_uc]
keep_stores <- stores[n == (2016 - 2006) * 4 + 1][["store_code_uc"]]
pi_data <- pi_data[store_code_uc %in% keep_stores]

print(paste0("N (balancing on store-level): ", nrow(pi_data)))
print(paste0("N stores (balancing on store-level): ", length(unique(pi_data$store_code_uc))))
print(paste0("N store-products (balancing on store-level): ",
             nrow(unique(pi_data[, .(store_code_uc, product_module_code)]))))

## build a county-level price index using the Beraja et al. formula
setkey(pi_data, store_code_uc, product_module_code, year, quarter)

### create the exponent
pi_data[, s_average := (sales_share + shift(sales_share, 1, type = "lag")) / 2,
        by = .(store_code_uc, product_module_code)]

### create the base
pi_data[, pi_change := cpricei / shift(cpricei, 1, type = "lag"),
        by = .(store_code_uc, product_module_code)]

### compute P^c_t / P^c_{t-1}
county_pi <- pi_data[, list(county_ratio = prod(pi_change^s_average)),
                       by = .(quarter, year, fips_state, fips_county)]
county_pi[year == 2006 & quarter == 4, county_ratio := 1]
## take cumulative product
county_pi[, county_index := cumprod(county_ratio)]

## average county-level indices, weighting by county population
county_pop <- fread("Data/county_population.csv")
county_pi <- merge(county_pi, county_pop, by = c("fips_state", "fips_county"))

national_pi <- county_pi[, list(
  national_index = weighted.mean(x = county_index, w = population)
  ), by = .(quarter, year)]

print(national_pi[])
fwrite(national_pi, "Data/national_pi_popweights.csv")
