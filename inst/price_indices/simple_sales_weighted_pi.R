#' Aggregate price indices using a simple 2006 Q4 sales-weighted average.
#'

library(data.table)

setwd("/project2/igaarder")

## load sales data
sales_data <- fread("Data/sales_quarterly_2006-2016.csv")
## keep only 2006 Q4 sales
sales_data <- sales_data[year == 2006 & quarter == 4]
sales_data <- sales_data[, .(store_code_uc, product_module_code, sales)]

## load price indices
pi_data <- fread("Data/Nielsen/price_quantity_indices_food.csv")
pi_data <- pi_data[year >= 2007 | (year == 2006 & quarter == 4)]

## merge on sales data
pi_data <- merge(pi_data, sales_data, by = c("product_module_code", "store_code_uc"))

## create sales-weighted averages
national_avg <- pi_data[, list(natl_avg = weighted.mean(x = cpricei, w = sales)),
                        by = .(year, quarter)]
fwrite(national_avg, "Data/simple_sales_weighted_pi.csv")
