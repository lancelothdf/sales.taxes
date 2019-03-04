#' Author: John Bonney
#' Purpose: Create national price index from store-category-level price indices,
#' aggregating over counties.
#'

library(readstata13)
library(data.table)

## for testing
# pi_data <- data.table(expand.grid(year = 2006:2008, month = 1:12,
#                                   store_code_uc = c("A", "B"),
#                                   product_module_code = c(36, 42)))
# pi_data[, store_code_uc := as.character(store_code_uc)]
# pi_data[, cpricei := runif(nrow(pi_data), 100, 300)]
# pi_data[, geocpricei := rnorm(nrow(pi_data), 200, 50)]
#
# sales_data <- pi_data[, .(year, month, store_code_uc, product_module_code)]
# sales_data[, sales := runif(nrow(sales_data), 1000, 10000)]

setwd("/project2/igaarder")

################################################################################
############################### No balancing ###################################
################################################################################

## load and convert .dta to .csv
pi_data <- read.dta13("Data/Nielsen/Monthly_county_price_quantity_indices_food.dta")
pi_data <- as.data.table(pi_data)
setnames(pi_data, old = c("fips_state_code", "fips_county_code"),
                  new = c("fips_state",      "fips_county"))
# fwrite(pi_data, "Data/Nielsen/monthly_county_price_quantity_indices_food.csv")

## get rid of the observations for 2006 Nov and earlier
pi_data <- pi_data[year >= 2007 | (year == 2006 & month == 12)]

## renormalize index so that it equals 1 in Dec 2006
base_pi <- pi_data[year == 2006 & month == 12]
setnames(base_pi,
         old = c("cpricei", "geocpricei"),
         new = c("base_cpricei", "base_geocpricei"))
base_pi <- base_pi[, .(fips_state, fips_county, product_module_code, base_cpricei, base_geocpricei)]

pi_data <- merge(pi_data, base_pi, by = c("fips_state", "fips_county", "product_module_code"))

pi_data[, cpricei := cpricei / base_cpricei]
pi_data[, geocpricei := geocpricei / base_geocpricei]

## merge sales shares onto cleaned price indices
sales_data <- fread("Data/sales_monthly_2006-2016.csv")
sales_data <- sales_data[, list(sales = sum(sales)),
                         by = .(fips_state, fips_county, month, year)]

 # this should be an inner merge
nrow_base <- nrow(pi_data)
pi_data <- merge(pi_data, sales_data, by = c("month", "year", "fips_state",
                                             "fips_county", "product_module_code"))

if (nrow_base != nrow(pi_data)){
  warning("Merging sales to indices changes the number of observations")
  print(paste0("N before merge: ", nrow_base))
  print(paste0("N after merge: ", nrow(pi_data)))
  pi_data <- pi_data[!is.na(cpricei) | !is.na(geocpricei)]
}

## compute sales shares
pi_data[, national_sales := sum(sales), by = .(month, year)]
pi_data[, sales_share := sales / national_sales]  # this is our S_{j,r}^t

## calculate the price indices
setkey(pi_data, fips_state, fips_county, product_module_code, year, month)

### create the exponent
pi_data[, s_average := (sales_share + shift(sales_share, 1, type = "lag")) / 2,
        by = .(fips_state, fips_county, product_module_code)]
### create the base
pi_data[, pi_change := cpricei / shift(cpricei, 1, type = "lag"),
        by = .(fips_state, fips_county, product_module_code)]
pi_data[, geo.pi_change := geocpricei / shift(geocpricei, 1, type = "lag"),
        by = .(fips_state, fips_county, product_module_code)]

### compute P_t / P_{t-1}
national_pi <- pi_data[, list(cpricei.ratio = prod(pi_change^s_average),
                              geocpricei.ratio = prod(geo.pi_change^s_average)),
                       by = .(month, year)]
national_pi[year == 2006 & month == 12, cpricei.ratio := 1]
national_pi[year == 2006 & month == 12, geocpricei.ratio := 1]

# compute the P_t as a cumulative product
national_pi[, national.cpricei.unbal := cumprod(cpricei.ratio)]
national_pi[, national.geocpricei.unbal := cumprod(geocpricei.ratio)]

print(national_pi[])
fwrite(
  national_pi[, .(year, month, national.cpricei.unbal, national.geocpricei.unbal)],
  "Data/national_pi_county_monthly.csv"
  )

rm(list=ls())
gc()

################################################################################
################### Balancing on module x county level #########################
################################################################################

## load and convert .dta to .csv
pi_data <- read.dta13("Data/Nielsen/Monthly_county_price_quantity_indices_food.dta")
pi_data <- as.data.table(pi_data)
setnames(pi_data, old = c("fips_state_code", "fips_county_code"),
         new = c("fips_state",      "fips_county"))
# fwrite(pi_data, "Data/Nielsen/monthly_county_price_quantity_indices_food.csv")

## get rid of the observations for 2006 Nov and earlier
pi_data <- pi_data[year >= 2007 | (year == 2006 & month == 12)]

print(paste0("N (raw): ", nrow(pi_data)))
print(paste0("N counties (raw): ",
             length(unique(pi_data$fips_state * 1000 + pi_data$fips_county))))
print(paste0("N county-products (raw): ",
             uniqueN(pi_data[, .(fips_state, fips_county, product_module_code)])))

## balance on module X county level
cty_modules <- pi_data[, list(n = .N), by = .(fips_state, fips_county, product_module_code)]
keep_cty_modules <- cty_modules[n == (2016 - 2006) * 12 + 1]
keep_cty_modules[, balanced := TRUE]

pi_data <- merge(pi_data, keep_cty_modules,
                 by = c("fips_state", "fips_county", "product_module_code"))
pi_data[, balanced := ifelse(is.na(balanced), FALSE, balanced)]
pi_data <- pi_data[balanced == TRUE]

print(paste0("N (balancing on county-module-level): ", nrow(pi_data)))
print(paste0("N counties (balancing on county-module-level): ",
             length(unique(pi_data$fips_state * 1000 + pi_data$fips_county))))
print(paste0("N county-products (balancing on county-module-level): ",
             uniqueN(pi_data[, .(fips_state, fips_county, product_module_code)])))

## renormalize index so that it equals 1 in Dec 2006
base_pi <- pi_data[year == 2006 & month == 12]
setnames(base_pi,
         old = c("cpricei", "geocpricei"),
         new = c("base_cpricei", "base_geocpricei"))
base_pi <- base_pi[, .(fips_state, fips_county, product_module_code, base_cpricei, base_geocpricei)]

pi_data <- merge(pi_data, base_pi, by = c("fips_state", "fips_county", "product_module_code"))

pi_data[, cpricei := cpricei / base_cpricei]
pi_data[, geocpricei := geocpricei / base_geocpricei]

## merge sales shares onto cleaned price indices
sales_data <- fread("Data/sales_monthly_2006-2016.csv")
sales_data <- sales_data[, list(sales = sum(sales)),
                         by = .(fips_state, fips_county, month, year)]

# this should be an inner merge
nrow_base <- nrow(pi_data)
pi_data <- merge(pi_data, sales_data, by = c("month", "year", "fips_state",
                                             "fips_county", "product_module_code"))

if (nrow_base != nrow(pi_data)){
  warning("Merging sales to indices changes the number of observations")
  print(paste0("N before merge: ", nrow_base))
  print(paste0("N after merge: ", nrow(pi_data)))
  pi_data <- pi_data[!is.na(cpricei) | !is.na(geocpricei)]
}

## compute sales shares
pi_data[, national_sales := sum(sales), by = .(month, year)]
pi_data[, sales_share := sales / national_sales]  # this is our S_{j,r}^t

## calculate the price indices
setkey(pi_data, fips_state, fips_county, product_module_code, year, month)

### create the exponent
pi_data[, s_average := (sales_share + shift(sales_share, 1, type = "lag")) / 2,
        by = .(fips_state, fips_county, product_module_code)]
### create the base
pi_data[, pi_change := cpricei / shift(cpricei, 1, type = "lag"),
        by = .(fips_state, fips_county, product_module_code)]
pi_data[, geo.pi_change := geocpricei / shift(geocpricei, 1, type = "lag"),
        by = .(fips_state, fips_county, product_module_code)]

### compute P_t / P_{t-1}
national_pi <- pi_data[, list(cpricei.ratio = prod(pi_change^s_average),
                              geocpricei.ratio = prod(geo.pi_change^s_average)),
                       by = .(month, year)]
national_pi[year == 2006 & month == 12, cpricei.ratio := 1]
national_pi[year == 2006 & month == 12, geocpricei.ratio := 1]

# compute the P_t as a cumulative product
national_pi[, national.cpricei.bal := cumprod(cpricei.ratio)]
national_pi[, national.geocpricei.bal := cumprod(geocpricei.ratio)]

print(national_pi[])

## merge onto the unbalanced indices
national_pi.old <- fread("Data/national_pi_monthly_county.csv")
national_pi.all <- merge(
  national_pi.old,
  national_pi[, .(year, month, national.cpricei.bal, national.geocpricei.bal)],
  by = c("year", "month")
  )
fwrite(national_pi.all, "Data/national_pi_monthly_county.csv")
