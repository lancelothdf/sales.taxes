#' Aggregate price indices using a simple 2006 Q4 sales-weighted average.
#'

library(data.table)

setwd("/project2/igaarder")

###########################################
####### balancing on store-level ##########
###########################################

## load sales data
sales_data <- fread("Data/sales_quarterly_2006-2016.csv")
## keep only 2006 Q4 sales
sales_data <- sales_data[year == 2006 & quarter == 4]
sales_data <- sales_data[, .(store_code_uc, product_module_code, sales)]

## load price indices
pi_data <- fread("Data/Nielsen/price_quantity_indices_food.csv")
pi_data <- pi_data[year >= 2007 | (year == 2006 & quarter == 4)]

## balance on store-level
stores <- unique(pi_data, by = c("year", "quarter", "store_code_uc"))
stores <- stores[, list(n = .N), by = store_code_uc]
keep_stores <- stores[n == (2016 - 2006) * 4 + 1][["store_code_uc"]]
pi_data <- pi_data[store_code_uc %in% keep_stores]

## merge on sales data
pi_data <- merge(pi_data, sales_data, by = c("product_module_code", "store_code_uc"))

## create sales-weighted averages
national_avg <- pi_data[, list(natl_avg = weighted.mean(x = cpricei, w = sales)),
                        by = .(year, quarter)]
fwrite(national_avg, "Data/simple_sales_weighted_pi.csv")

##################################################
####### balancing on store-module-level ##########
##################################################

## balance on module X store level
store_modules <- pi_data[, list(n = .N), by = .(store_code_uc, product_module_code)]
keep_store_modules <- store_modules[n == (2016 - 2006) * 4 + 1]
keep_store_modules[, balanced := TRUE]

pi_data <- merge(pi_data, keep_store_modules,
                 by = c("store_code_uc", "product_module_code"))
pi_data[, balanced := ifelse(is.na(balanced), FALSE, balanced)]
pi_data <- pi_data[balanced == TRUE]

national_avg2 <- pi_data[, list(natl_avg = weighted.mean(x = cpricei, w = sales)),
                        by = .(year, quarter)]
fwrite(national_avg2, "Data/simple_sales_weighted_pi_modbalanced.csv")
