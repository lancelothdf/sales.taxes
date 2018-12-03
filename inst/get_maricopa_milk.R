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
sapply(all_nielsen_data, class)
c_fs <- class(all_nielsen_data$fips_state)
c_fc <- class(all_nielsen_data$fips_county)
c_pmc <- class(all_nielsen_data$product_module_code)

maricopa_milk <- eval(parse(text = paste0(
  "all_nielsen_data[, fips_state == as.", c_fs, "(", 4, ") & fips_county == as.", c_fc, "(", 13,
                                          ") & product_module_code == as.", c_pmc, "(", 3625, ")]")))
print(nrow(maricopa_milk))

fwrite(maricopa_milk, "Data/maricopa_milk.csv")
