#' Author: John Bonney
#' Purpose: Calculate share of national sales for each store-product category
#'
#' Note: Originally, this was intended to calculate store-product shares of
#' national sales. However, doing so at this point prevented the shares from
#' summing to 1 in the balanced panel (which they should, as weights). The
#' only useful thing this script did was convert the data from months to
#' quarters. This script remains temporarily but will be deleted in the future.

library(sales.taxes)
library(data.table)

setwd("/project2/igaarder")

## Create data.table from raw data of total national quarterly sales

sales_data <- fread("Data/Nielsen/allyears_module_store_level.csv")
sales_data <- months_to_quarters(monthly_data = sales_data, month_var = "month",
                                 collapse_by = c("fips_state", "fips_county", "product_group_code",
                                                 "store_code_uc", "product_module_code"),
                                 collapse_var = "sales")

fwrite(sales_data, "Data/national_sales_shares.csv")
