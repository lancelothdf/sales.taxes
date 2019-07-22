#' Author: John Bonney
#'
#' Clean the Nielsen household panel data to create a data set on the
#' consumer-product-store-quarter level.

library(data.table)
library(futile.logger)
library(readstata13)

setwd("/project2/igaarder/Data/Nielsen/Household_panel")


## collapse expenditures to the quarterly level and link all these annual files
purchases.full <- data.table(NULL)
for (yr in 2006:2016) {
  
  annual.path <- paste0("cleaning/purchases_", yr, ".csv")
  purchase.yr <- fread(annual.path)
  
  purchase.yr <- purchase.yr[, list(
    total_expenditures = sum(total_expenditures), 
    projection_factor = mean(projection_factor, na.rm = T),
    projection_factor_magnet = mean(projection_factor_magnet, na.rm = T),
    household_income = mean(household_income, na.rm = T)
  ), by = .(household_code, product_module_code, product_group_code,
            store_code_uc, channel_code, fips_county, fips_state,
            quarter, year)  ]
  ## attach
  flog.info("Appending %s data to master file", yr)
  purchases.full <- rbind(purchases.full, purchase.yr) 

}

## Calculate total expenditure per consumer in each quarter in each store (across modules)
purchases.full[, sum_total_exp := sum(total_expenditures),
               by = .(household_code, year, quarter, store_code_uc)]
## Calculate total expenditure per consumer in each quarter (across stores and modules)
purchases.full[, sum_total_exp := sum(total_expenditures),
               by = .(household_code, year, quarter)]

## Subset to just the best-selling modules
best_selling_modules <- fread("/project2/igaarder/Data/best_selling_modules.csv")
keep_modules <- unique(best_selling_modules[, .(Module)][[1]])

purchases.full <- purchases.full[product_module_code %in% keep_modules]
#fwrite(purchases.full, "cleaning/consumer_panel_2006-2016.csv")

#purchases.full <- fread("cleaning/consumer_panel_2006-2016.csv")
## merge on price indices and tax rates
all_goods_pi_path <- "../../all_nielsen_data_2006_2016_quarterly.csv"
all_pi <- fread(all_goods_pi_path)
all_pi <- all_pi[, .(store_code_uc, product_module_code,
                     year, quarter, pricei, cpricei, sales_tax)]

purchases.full <- merge(
  purchases.full, all_pi,
  by = c("store_code_uc", "product_module_code", "year", "quarter"),
  all.x = T
)
fwrite(purchases.full, "cleaning/consumer_panel_2006-2016.csv")