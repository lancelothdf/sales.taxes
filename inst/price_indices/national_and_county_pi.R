#' They are under "/project2/igaarder/Data/Nielsen/Price_quantity_indices_county_food.dta"
#' and "project2/igaarder/Data/Nielsen//Price_quantity_indices_national_food.dta"
#' So there would be two graphs to make (Only for sample balanced across stores -
#' we can forget about the other sample for now since it did not seem to make a big difference).
#'
#' Graph 1:
#'   Step 1:  Aggregate the price indices across module at the county-level by
#'     using the CPI index formula (for weights use the national share of sales
#'     for each module so that weights are the same across counties)
#'   Step 2: Do a simple weighted average of indices across counties using
#'     population as weights
#'
#' Graph 2:
#'   Just aggregate the national indices across modules using the CPI index
#'   formula (for weights, use - again - the national share of sales for each module)

library(readstata13)
library(data.table)

setwd("/project2/igaarder")

##############################################
############ prepare sales weights ###########
##############################################

## load county price indices
county_pi <- read.dta13("Data/Nielsen/Price_quantity_indices_county_food.dta")
county_pi <- as.data.table(county_pi)
county_pi[, fips_state := as.integer(fips_state)]
county_pi[, fips_county := as.integer(fips_county)]
fwrite(county_pi, "Data/Nielsen/Price_quantity_indices_county_food.csv")

## load sales data to compute sales shares
sales_data <- fread("Data/sales_quarterly_2006-2016.csv")

## keep only sales in the relevant food modules
food_pmc <- unique(county_pi[, .(product_module_code)][[1]])
sales_data <- sales_data[product_module_code %in% food_pmc]

## build sales shares for each module at the national level for each quarter
national_sales <- sales_data[, list(total_sales = sum(sales)),
                             by = .(quarter, year, product_module_code)]
national_sales[, total_national_sales := sum(total_sales), by = .(quarter, year)]
national_sales[, sales_share := total_sales / total_national_sales]

rm(sales_data) # free up memory
gc()

##############################################
################### Graph 1 ##################
##############################################

## county price index data already loaded

## keep only 2006 Q4 and after
county_pi <- county_pi[year >= 2007 | (year == 2006 & quarter == 4)]

## renormalize index so that it equals 1 in 2006 Q4
base_pi <- county_pi[year == 2006 & quarter == 4]
base_pi[, base_cpricei := cpricei]
base_pi <- base_pi[, .(fips_state, fips_county, product_module_code, base_cpricei)]

county_pi <- merge(county_pi, base_pi, by = c("fips_state", "fips_county", "product_module_code"))
county_pi[, cpricei := cpricei / base_cpricei]

## merge on sales shares
county_pi <- merge(county_pi, national_sales, by = c("quarter", "year", "product_module_code"))

## build a county-level price index using the Beraja et al. formula
setkey(county_pi, fips_state, fips_county, product_module_code, year, quarter)

### create the exponent
county_pi[, s_average := (sales_share + shift(sales_share, 1, type = "lag")) / 2,
        by = .(fips_state, fips_county, product_module_code)]

### create the base
county_pi[, pi_change := cpricei / shift(cpricei, 1, type = "lag"),
        by = .(fips_state, fips_county, product_module_code)]

### compute P^c_t / P^c_{t-1}
county_pi <- county_pi[, list(county_ratio = prod(pi_change^s_average)),
                     by = .(quarter, year, fips_state, fips_county)]
county_pi[year == 2006 & quarter == 4, county_ratio := 1]
## take cumulative product
county_pi[, county_index := cumprod(county_ratio), by = .(fips_state, fips_county)]

## average county-level indices, weighting by county population
county_pop <- fread("Data/county_population.csv")
county_pi <- merge(county_pi, county_pop, by = c("fips_state", "fips_county"))

national_pi <- county_pi[, list(
  national_index = weighted.mean(x = county_index, w = population, na.rm = T)
), by = .(quarter, year)]

print(national_pi[])
fwrite(national_pi, "Data/national_pi_popweights.csv")

##############################################
################### Graph 2 ##################
##############################################

## load in national price indices
national_pi2 <- read.dta13("Data/Nielsen/Price_quantity_indices_national_food.dta")
national_pi2 <- as.data.table(national_pi2)
fwrite(national_pi2, "Data/Nielsen/Price_quantity_indices_national_food.csv")

## keep only 2006 Q4 and after
national_pi2 <- national_pi2[year >= 2007 | (year == 2006 & quarter == 4)]

## renormalize index so that it equals 1 in 2006 Q4
base_pi2 <- national_pi2[year == 2006 & quarter == 4]
base_pi2[, base_cpricei := cpricei]
base_pi2 <- base_pi2[, .(product_module_code, base_cpricei)]

national_pi2 <- merge(national_pi2, base_pi2, by = "product_module_code")
national_pi2[, cpricei := cpricei / base_cpricei]

## merge on sales shares
national_pi2 <- merge(national_pi2, national_sales, by = c("quarter", "year", "product_module_code"))

## build a national-level price index using the Beraja et al. formula
setkey(national_pi2, product_module_code, year, quarter)

### create the exponent
national_pi2[, s_average := (sales_share + shift(sales_share, 1, type = "lag")) / 2,
          by = product_module_code]

### create the base
national_pi2[, pi_change := cpricei / shift(cpricei, 1, type = "lag"),
          by = product_module_code]

### compute P_t / P_{t-1}
national_pi2 <- national_pi2[, list(natl_ratio = prod(pi_change^s_average)),
                       by = .(quarter, year)]
national_pi2[year == 2006 & quarter == 4, natl_ratio := 1]
## take cumulative product
national_pi2[, natl_index := cumprod(natl_ratio)]

print(national_pi2[])
fwrite(national_pi2, "Data/national_pi2.csv")
