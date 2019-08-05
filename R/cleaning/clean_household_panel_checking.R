

library(data.table)
library(futile.logger)
library(readstata13)

setwd("/project2/igaarder/Data/Nielsen/Household_panel")
purchases.full <- data.table(NULL)
for (yr in 2006:2016) {
  
  annual.path <- paste0("cleaning/purchases_q_", yr, ".csv")
  purchase.yr <- fread(annual.path)
  purchase.yr <- purchase.yr[sample(nrow(purchase.yr), 1000)]
  purchase.yr <- purchase.yr[, list(
    total_expenditures = sum(total_expenditures),
    projection_factor = mean(projection_factor, na.rm = T),
    projection_factor_magnet = mean(projection_factor_magnet, na.rm = T),
    household_income = mean(household_income, na.rm = T)
  ), by = .(household_code, product_module_code, product_group_code, region_code,
            same_3zip_store, fips_county_code, fips_state_code, zip_code,
            year)  ]
  ## attach
  flog.info("Appending %s data to master file", yr)
  purchases.full <- rbind(purchases.full, purchase.yr)
  
}
output.results.file <- "../../../../../home/slacouture/HMS/sample_data.csv"
fwrite(purchases.full, output.results.file)


