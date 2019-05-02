#' Author: John Bonney
#' Purpose: Create national price index from store-category-level price indices
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
######################### Balancing on store level #############################
################################################################################

## load and convert .dta to .csv
pi_data <- read.dta13("Data/Nielsen/Monthly_price_quantity_indices_food.dta")
pi_data <- as.data.table(pi_data)
fwrite(pi_data, "Data/Nielsen/monthly_price_quantity_indices_food.csv")

print(paste0("N (raw): ", nrow(pi_data)))
print(paste0("N stores (raw): ", length(unique(pi_data$store_code_uc))))
print(paste0("N store-products (raw): ", uniqueN(pi_data[, .(store_code_uc, product_module_code)])))

## get rid of the observations for 2006 Nov and earlier
pi_data <- pi_data[year >= 2007 | (year == 2006 & month == 12)]

print(paste0("N (dropping 2006 Nov and earlier): ", nrow(pi_data)))
print(paste0("N stores (dropping 2006 Nov and earlier): ", length(unique(pi_data$store_code_uc))))
print(paste0("N store-products (dropping 2006 Nov and earlier): ",
             uniqueN(pi_data[, .(store_code_uc, product_module_code)])))

## balance on store-level
stores <- unique(pi_data, by = c("year", "month", "store_code_uc"))
stores <- stores[, list(n = .N), by = store_code_uc]
keep_stores <- stores[n == (2016 - 2006) * 12 + 1][["store_code_uc"]]
pi_data <- pi_data[store_code_uc %in% keep_stores]

print(paste0("N (balancing on store-level): ", nrow(pi_data)))
print(paste0("N stores (balancing on store-level): ", length(unique(pi_data$store_code_uc))))
print(paste0("N store-products (balancing on store-level): ",
             uniqueN(pi_data[, .(store_code_uc, product_module_code)])))

## renormalize index so that it equals 1 in Dec 2006
base_pi <- pi_data[year == 2006 & month == 12]
base_pi[, base_cpricei := cpricei]
base_pi[, base_geocpricei := geocpricei]
base_pi <- base_pi[, .(store_code_uc, product_module_code, base_cpricei, base_geocpricei)]

pi_data <- merge(pi_data, base_pi,
                 by = c("store_code_uc", "product_module_code"))

pi_data[, cpricei := cpricei / base_cpricei]
pi_data[, geocpricei := geocpricei / base_geocpricei]

## merge sales shares onto cleaned price indices
sales_data <- fread("Data/sales_monthly_2006-2016.csv")

 # this should be an inner merge
nrow_base <- nrow(pi_data)
pi_data <- merge(pi_data, sales_data,
                 by = c("store_code_uc", "month", "year", "product_module_code"))

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
setkey(pi_data, store_code_uc, product_module_code, year, month)

### create the exponent
pi_data[, s_average := (sales_share + shift(sales_share, 1, type = "lag")) / 2,
        by = .(store_code_uc, product_module_code)]
### create the base
pi_data[, pi_change := cpricei / shift(cpricei, 1, type = "lag"),
        by = .(store_code_uc, product_module_code)]
pi_data[, geo.pi_change := geocpricei / shift(geocpricei, 1, type = "lag"),
        by = .(store_code_uc, product_module_code)]

### compute P_t / P_{t-1}
national_pi <- pi_data[, list(cpricei.ratio = prod(pi_change^s_average),
                              geocpricei.ratio = prod(geo.pi_change^s_average)),
                       by = .(month, year)]
national_pi[year == 2006 & month == 12, cpricei.ratio := 1]
national_pi[year == 2006 & month == 12, geocpricei.ratio := 1]

# compute the P_t as a cumulative product
national_pi[, national.cpricei.storebal := cumprod(cpricei.ratio)]
national_pi[, national.geocpricei.storebal := cumprod(geocpricei.ratio)]

print(national_pi[])
fwrite(national_pi, "Data/national_pi_monthly.csv")

rm(list=ls())
gc()

################################################################################
#################### Balancing on module x store level #########################
################################################################################

## read in csv
pi_data <- fread("Data/Nielsen/monthly_price_quantity_indices_food.csv")

## get rid of the observations for 2006 Nov and earlier
pi_data <- pi_data[year >= 2007 | (year == 2006 & month == 12)]

## balance on module X store level
store_modules <- pi_data[, list(n = .N), by = .(store_code_uc, product_module_code)]
keep_store_modules <- store_modules[n == (2016 - 2006) * 12 + 1]
keep_store_modules[, balanced := TRUE]

pi_data <- merge(pi_data, keep_store_modules,
                 by = c("store_code_uc", "product_module_code"))
pi_data[, balanced := ifelse(is.na(balanced), FALSE, balanced)]
pi_data <- pi_data[balanced == TRUE]

print(paste0("N (balancing on store-module-level): ", nrow(pi_data)))
print(paste0("N stores (balancing on store-module-level): ",
             length(unique(pi_data$store_code_uc))))
print(paste0("N store-products (balancing on store-module-level): ",
             uniqueN(pi_data[, .(store_code_uc, product_module_code)])))

## renormalize index so that it equals 1 in Dec 2006
base_pi <- pi_data[year == 2006 & month == 12]
base_pi[, base_cpricei := cpricei]
base_pi[, base_geocpricei := geocpricei]
base_pi <- base_pi[, .(store_code_uc, product_module_code, base_cpricei, base_geocpricei)]

pi_data <- merge(pi_data, base_pi,
                 by = c("store_code_uc", "product_module_code"))

pi_data[, cpricei := cpricei / base_cpricei]
pi_data[, geocpricei := geocpricei / base_geocpricei]

## merge sales shares onto cleaned price indices
sales_data <- fread("Data/sales_monthly_2006-2016.csv")

# this should be an inner merge
nrow_base <- nrow(pi_data)
pi_data <- merge(pi_data, sales_data,
                 by = c("store_code_uc", "month", "year", "product_module_code"))

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
setkey(pi_data, store_code_uc, product_module_code, year, month)

### create the exponent
pi_data[, s_average := (sales_share + shift(sales_share, 1, type = "lag")) / 2,
        by = .(store_code_uc, product_module_code)]
### create the base
pi_data[, pi_change := cpricei / shift(cpricei, 1, type = "lag"),
        by = .(store_code_uc, product_module_code)]
pi_data[, geo.pi_change := geocpricei / shift(geocpricei, 1, type = "lag"),
        by = .(store_code_uc, product_module_code)]

### compute P_t / P_{t-1}
national_pi <- pi_data[, list(cpricei.ratio = prod(pi_change^s_average),
                              geocpricei.ratio = prod(geo.pi_change^s_average)),
                       by = .(month, year)]
national_pi[year == 2006 & month == 12, cpricei.ratio := 1]
national_pi[year == 2006 & month == 12, geocpricei.ratio := 1]

# compute the P_t as a cumulative product
national_pi[, national.cpricei.storeprodbal := cumprod(cpricei.ratio)]
national_pi[, national.geocpricei.storeprodbal := cumprod(geocpricei.ratio)]
print(national_pi[])

national_pi.old <- fread("Data/national_pi_monthly.csv")
national_pi.all <- merge(national_pi.old, national_pi, by = c("year", "month"))
fwrite(national_pi.all, "Data/national_pi_monthly.csv")
