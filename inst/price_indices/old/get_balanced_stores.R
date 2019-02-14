#' Balanced sample -- create balanced sample of stores

library(data.table)

setwd("/project2/igaarder")

## load sales data to compute sales shares
sales_data <- fread("Data/sales_quarterly_2006-2016.csv")

## build sales shares for each module at the national level for each quarter
national_sales <- sales_data[, list(total_sales = sum(sales)),
                             by = .(quarter, year, product_module_code)]
national_sales[, total_national_sales := sum(total_sales), by = .(quarter, year)]
national_sales[, sales_share := total_sales / total_national_sales]

rm(sales_data) # free up memory
gc()

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

## renormalize index so that it equals 1 in 2006 Q4
base_pi <- pi_data[year == 2006 & quarter == 4]
base_pi[, base_cpricei := cpricei]
base_pi <- base_pi[, .(store_code_uc, product_module_code, base_cpricei)]

pi_data <- merge(pi_data, base_pi,
                 by = c("store_code_uc", "product_module_code"))

pi_data[, cpricei := cpricei / base_cpricei]

## merge on sales shares
pi_data <- merge(pi_data, national_sales, by = c("quarter", "year", "product_module_code"))
pi_data <- pi_data[!is.na(sales_share) & !is.na(cpricei)]


## balance panel on store-level
stores <- unique(pi_data, by = c("year", "quarter", "store_code_uc"))
stores <- stores[, list(n = .N), by = store_code_uc]
keep_stores <- stores[n == (2016 - 2006) * 4 + 1][["store_code_uc"]]
pi_data <- pi_data[store_code_uc %in% keep_stores]

store_info <- unique(pi_data[, .(store_code_uc, fips_state, fips_county)])
write.csv(store_info, "Data/balanced_stores_sample_for_pi.csv")
