# get maricopa toilet paper (brand-level)

rm(list=ls())
wd <- "/project2/igaarder"
setwd(wd)

library(sales.taxes)
library(readstata13)
library(data.table)

tp_sales_panel <- data.table(NULL)
for (year in 2006:2016){
  filename <- paste0("/project2/igaarder/Data/Nielsen/", year, "_monthly_toilet_tissue.dta")
  annual_tp <- as.data.table(read.dta13(filename))
  store_id_file <- paste0("/project2/igaarder/Data/Nielsen/stores_", year, ".dta")
  store_id <- as.data.table(read.dta13(store_id_file))
  setnames(store_id, old = "fips_state_code", new = "fips_state")
  setnames(store_id, old = "fips_county_code", new = "fips_county")
  annual_tp <- merge(annual_tp, store_id, by = "store_code_uc", all.x = T)
  annual_tp <- annual_tp[channel_code %in% c("M", "F", "D")]

  annual_tp[, year := year]
  tp_sales_panel <- rbind(tp_sales_panel, annual_tp)
}

fwrite(tp_sales_panel, file = "Data/Nielsen/allyears_toilet_tissue.csv")

# limit data to Maricopa County, AZ

print(unique(tp_sales_panel$fips_state))
print(unique(tp_sales_panel$product_module_code))
print(unique(tp_sales_panel[fips_state == 4, fips_county]))

maricopa_tp <- tp_sales_panel[fips_state == 4 & fips_county == 13]

print(nrow(maricopa_tp))

fwrite(maricopa_tp, "Data/maricopa_tp_det.csv")
