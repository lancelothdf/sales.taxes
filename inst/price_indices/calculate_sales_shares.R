#' Author: John Bonney
#' Purpose: Calculate share of national sales for each store-product category

library(sales.taxes)
library(data.table)

setwd("/project2/igaarder")

## Create data.table from raw data of total national quarterly sales

sales_data <- fread("Data/Nielsen/allyears_module_store_level.csv")
sales_data <- months_to_quarters(monthly_data = sales_data, month_var = "month",
                                 collapse_by = c("fips_state", "fips_county", "product_group_code",
                                                 "store_code_uc", "product_module_code"),
                                 collapse_var = "sales")

national_sales <- sales_data[, .(total_sales = sum(sales)), by = .(quarter, year)]

## calculate sales shares for each store X product X quarter
sales_data <- merge(sales_data, national_sales, by = c("quarter", "year"))
sales_data[, sales_share := sales / national_sales] # this is our S_{j,r}^t
fwrite(sales_data, "Data/national_sales_shares.csv")
