# get maricopa milk

rm(list=ls())
wd <- "/project2/igaarder"
setwd(wd)

library(sales.taxes)
library(readstata13)
library(data.table)

# limit data to Maricopa County, AZ and product to refrigerated milk
all_nielsen_data <- fread("Data/Nielsen/allyears_module_store_level.csv")
print(nrow(all_nielsen_data))
print(head(all_nielsen_data))
sapply(all_nielsen_data, class)

print(unique(all_nielsen_data$fips_state))
print(unique(all_nielsen_data$product_module_code))
print(unique(all_nielsen_data[, fips_state == 4]$fips_county))

print(nrow(all_nielsen_data[, fips_state == 4 & fips_county == 19]))
print(nrow(all_nielsen_data[, product_module_code == 7260]))
print(nrow(all_nielsen_data[, fips_state == 4 & fips_county == 19 & product_module_code == 7260]))
print(nrow(all_nielsen_data[, fips_state == 4 & fips_county == 19 & product_module_code == 7260 ]))

maricopa_tp <- all_nielsen_data[, fips_state == 4 & fips_county == 19 & product_module_code == 7260]

print(nrow(maricopa_tp))

fwrite(maricopa_tp, "Data/maricopa_tp.csv")
