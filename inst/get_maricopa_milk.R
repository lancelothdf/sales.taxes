# get maricopa milk

rm(list=ls())
wd <- "/project2/igaarder"
setwd(wd)

library(sales.taxes)
library(readstata13)
library(data.table)

# limit data to Maricopa County, AZ and product to refrigerated milk
all_nielsen_data <- fread("Data/Nielsen/allyears_module_store_level.csv")
maricopa_milk <- all_nielsen_data[, fips_state == 4 & fips_county == 13 &
                                    product_module_code == 3625]

fwrite(maricopa_milk, "Data/maricopa_milk.csv")
