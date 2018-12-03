#' Graph Maricopa county prices of one product (fips 04-013) milk (3625)
#'

rm(list=ls())
wd <- "/project2/igaarder"
setwd(wd)

library(sales.taxes)
library(readstata13)
library(data.table)
library(zoo)
library(ggplot2)

# read in tax data
county_monthly_tax <- fread("Data/county_monthly_tax_rates.csv")
county_monthly_tax <- county_monthly_tax[, .(fips_state, fips_county, year, month, sales_tax)]

# limit data to Maricopa County, AZ and product to refrigerated milk
all_nielsen_data <- fread("Data/Nielsen/allyears_module_store_level.csv")
maricopa_milk <- all_nielsen_data[, fips_state == 4 & fips_county == 13 &
                                    product_module_code == 3625]
maricopa_milk[, price := sales / quantity]

maricopa_milk <- merge_tax_rates(sales_data = maricopa_milk,
                                 keep_taxable_only = T,
                                 county_monthly_tax_data = county_monthly_tax)

maricopa_milk <- make_fixed_weights(all_nielsen_data,
                                       weight_time = list(year = 2008, month = 1),
                                       weight_var = "sales",
                                       panel_unit_vars = c("fips_state", "fips_county", "store_code_uc", "product_module_code"))

