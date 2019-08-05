#' Author: John Bonney & Santiago Lacouture
#'
#' Clean the Nielsen household panel data to create a data set on the
#' consumer-quarter level. Match sales tax to HH by their location. 
#' Divide consumption by taxability of product and by location of store (same 3 digit or not)

library(data.table)
library(futile.logger)
library(readstata13)

setwd("/project2/igaarder/Data/Nielsen/Household_panel")

#### Start with quarterly data and add up

## collapse expenditures to the semester level and link all these annual files
purchases.full <- data.table(NULL)
for (yr in 2006:2016) {

  annual.path <- paste0("cleaning/purchases_q_", yr, ".csv")
  purchase.yr <- fread(annual.path)
  purchase.yr[ semester := ceiling(quarter/2)]
  purchase.yr <- purchase.yr[, list(
    total_expenditures = sum(total_expenditures),
    projection_factor = mean(projection_factor, na.rm = T),
    projection_factor_magnet = mean(projection_factor_magnet, na.rm = T),
    household_income = mean(household_income, na.rm = T)
  ), by = .(household_code, product_module_code, product_group_code, region_code,
            same_3zip_store, fips_county_code, fips_state_code, zip_code,
            semester, year)  ]
  ## attach
  flog.info("Appending %s data to master file", yr)
  purchases.full <- rbind(purchases.full, purchase.yr)

}

## Calculate total expenditure per consumer in each semester (across stores and modules)
purchases.full[, sum_total_exp_semester := sum(total_expenditures),
               by = .(household_code, year, semester)]

## Identify taxability of module: import
taxability_panel <- fread("/project2/igaarder/Data/taxability_state_panel.csv")
taxability_panel <- taxability_panel[, .(product_module_code, product_group_code,
                                         fips_state, taxability, month, year)]
setnames(taxability_panel,
         old = c("fips_state"),
         new = c("fips_state_code"))
# Collapse taxability to the semester as rounding the mean 
taxability_panel[, semester := ceiling(month / 6)]
taxability_panel <- taxability_panel[, list(taxability = round(mean(taxability))) , 
                                     by =.(product_module_code, product_group_code,
                                           fips_state_code, semester, year)]


purchases.full <- merge(
  purchases.full, taxability_panel,
  by = c("fips_state_code", "product_module_code", "product_group_code", "year", "semester"),
  all.x = T
)
# Assign unknown to purchases out of best selling module (taxability only identified for best selling)
purchases.full$taxability[is.na(purchases.full$taxability)] <- 2

## Collapse by household X type of store X taxability of module
purchases.full <- purchases.full[, list(
            total_expenditures = sum(total_expenditures)
          ), by = .(household_code, taxability, fips_county_code, fips_state_code, zip_code,
                    same_3zip_store, semester, year, sum_total_exp_quarter, projection_factor,
                    projection_factor_magnet, household_income, region_code) ]
## reshape to get a hh X taxability of module data
purchases.full <- dcast(purchases.full, household_code + taxability + fips_county_code + fips_state_code +
                          zip_code + semester + year + projection_factor + projection_factor_magnet +
                          sum_total_exp_quarter + household_income + region_code ~ same_3zip_store, fun=sum,
                          value.var = "total_expenditures")

setnames(purchases.full,
         old = c("FALSE", "TRUE"),
         new = c("expenditures_diff3", "expenditures_same3"))
## reshape to get a hh data
purchases.full <- dcast(purchases.full, household_code + fips_county_code + fips_state_code + zip_code + semester
                        + year + projection_factor + projection_factor_magnet + sum_total_exp_quarter + region_code +
                          household_income ~ taxability,  fun=sum, value.var = c("expenditures_diff3","expenditures_same3"))

## merge on tax rates
all_goods_pi_path <- "../../monthly_taxes_county_5zip_2008_2014.csv"
all_pi <- fread(all_goods_pi_path)
all_pi <- all_pi[, .(sales_tax, year, month, fips_county, fips_state, zip_code )]
setnames(all_pi,
         old = c("fips_state", "fips_county"),
         new = c("fips_state_code", "fips_county_code"))
# Collapse rates to the quarter as the mean 
all_pi[, semester := ceiling(month / 6)]
all_pi <- all_pi[, list(sales_tax = mean(sales_tax)) , 
                                     by =.(zip_code, fips_county_code,
                                           fips_state_code, semester, year)]
purchases.full <- merge(
  purchases.full, all_pi,
  by = c("fips_county_code", "fips_state_code", "zip_code", "year", "semester"),
  all.x = T
)

## Create interest variables
purchases.full <- purchases.full[, ln_sales_tax := log1p(sales_tax)]
purchases.full <- purchases.full[, expenditure_taxable := expenditures_diff3_1 + expenditures_same3_1]
purchases.full <- purchases.full[, expenditure_non_taxable := expenditures_diff3_0 + expenditures_same3_0]
purchases.full <- purchases.full[, expenditure_unknown := expenditures_diff3_2 + expenditures_same3_2]
purchases.full <- purchases.full[, expenditure_same3 := expenditures_same3_0 + expenditures_same3_1 + expenditures_same3_2]
purchases.full <- purchases.full[, expenditure_diff3 := expenditures_diff3_0 + expenditures_diff3_1 + expenditures_diff3_2]


fwrite(purchases.full, "cleaning/consumer_panel_s_hh_2006-2016.csv")