## Sales taxes Project. Household Panel
# Exploring Data Set Unbalance
# This code runs basic statistics to test for unbalance
# Author: Santiago Lacouture


library(data.table)
library(lfe)
library(futile.logger)
library(AER)
library(multcomp)
library(psych)
library(ggplot2)

setwd("/project2/igaarder/Data/Nielsen/Household_panel")

## Open Data
purchases.full <- fread("cleaning/consumer_panel_2006-2016_ids.csv")

### Identify households missing in each quarter

## Step 1. Keep data actually used (sales tax)
# Drop "magnet" purchases: 
purchases.full[, sum(is.na(projection_factor))]
# 2016154 obs
purchases.nomagnet <- purchases.full[!is.na(projection_factor)]

# Drop purchases without sales tax data
purchases.full[, sum(is.na(sales_tax))]
# More than 70%: 181247984 obs
purchases.retail <- purchases.nomagnet[!is.na(sales_tax)]

## Step 2. "balance" the data
purchases.retail$quarter_of_year <- factor(with(purchases.retail, 
                                             interaction(year, quarter)))

# Make a "new" set with codes of all household X store X module that appear at least once in data
flog.info("Building expanding master")
# Collapse to hh x store x module that appeared at least once
possible.purchases <- purchases.retail[, list(N_obs = .N), by = .(household_code, product_module_code, store_code_uc)]
possible.purchases <- possible.purchases[N_obs > 0]
possible.purchases[N_obs > 0]
# Number Check:
length(unique(possible.purchases$household_code))
length(unique(possible.purchases$product_module_code))
length(unique(possible.purchases$store_code_uc))

# Expand by quarter (old fashioned: CJ does not work in this case because of dimensionality)
possible.purchases.q <- possible.purchases[ , quarter := 1]
for (i in 2:4) {
  possible.purchases.t <- possible.purchases[ , quarter := i ]
  possible.purchases.q <- rbind(possible.purchases.q, possible.purchases.t)
  
}
# Expand by year
possible.purchases.full <- possible.purchases.q[ , year := 2008]
for (i in 2009:2014) {
  possible.purchases.t <- possible.purchases.q[ , year := i ]
  possible.purchases.full <- rbind(possible.purchases.full, possible.purchases.t)
}
# Number Check:
length(unique(possible.purchases.full$household_code))
length(unique(possible.purchases.full$product_module_code))
length(unique(possible.purchases.full$store_code_uc))
length(unique(possible.purchases.full$year))
unique(possible.purchases.full$year)

# merge with existing data
flog.info("Merging to expand")
purchases.retail <- merge(purchases.retail, possible.purchases.full, by = c("store_code_uc", "household_code", "product_module_code", "quarter", "year"), all.y = T)
# Number Check:
length(unique(purchases.retail$household_code))
length(unique(purchases.retail$product_module_code))
length(unique(purchases.retail$store_code_uc))
length(unique(purchases.retail$year))

## Now I have to drop real missings: households that actually do not appear in a quarter
flog.info("Dropping 'Real' Missings") # Just to check
purchases.retail <- purchases.retail[, missing := mean(is.na(total_expenditures)), 
                                  by = .(household_code, quarter, year) ]
purchases.retail <- purchases.retail[missing != 1]
# Number Check:
length(unique(purchases.retail$household_code))
length(unique(purchases.retail$product_module_code))
length(unique(purchases.retail$store_code_uc))
length(unique(purchases.retail$year))

## Now I build a new variable for attition of module x store by transform expenses
flog.info("Building 'Attrition' variable")
purchases.retail <- purchases.retail[, purchased := !is.na(total_expenditures)]


## Create again the IDs for estimation
purchases.retail$module_by_time <- factor(with(purchases.retail, 
                                             interaction(year, quarter, product_module_code )))

purchases.retail <- within(purchases.retail,{household_by_store_by_module<-as.numeric(
  factor(paste0(product_module_code,
                store_code_uc,
                household_code)))}) 

## Extrapolate sales tax for new observations
flog.info("Extrapolating variables for missings")
purchases.retail <- purchases.retail[, ln_sales_tax := mean(ln_sales_tax, na.rm = T), 
                                     by = .(product_module_code, store_code_uc, quarter, year)]

purchases.retail <- purchases.retail[, projection_factor := as.integer(median(projection_factor, na.rm = T)), 
                                     by = .(household_code, quarter, year)]

## Estimate desired especification on this new variable

# Log Share of Expenditure
formula0 <- as.formula(paste0(
  "purchased ~ ln_sales_tax | module_by_time + household_by_store_by_module"
))

flog.info("Estimating Balance")
res0 <- felm(data = purchases.retail,
             formula = formula0,
             weights = purchases.retail$projection_factor,
             na.omit)

flog.info("Writing results...")
res0.dt <- data.table(coef(summary(res0)), keep.rownames=T)
res0.dt[, outcome := "purchased"]
res0.dt[, Rsq := summary(res0)$r.squared]
res0.dt[, adj.Rsq := summary(res0)$adj.r.squared]
res0.dt[, specification := "Basic"]
res0.dt[, N := sum((!is.na(purchases.retail$purchased)))]
LRdiff_res <- res0.dt ### Create table LRdiff_res in which we store all results (we start with the results we had just stored in res1.dt)
LRdiff_res$N_hholds <- length(unique(purchases.retail$household_code))
LRdiff_res$N_modules <- length(unique(purchases.retail$product_module_code))
LRdiff_res$N_stores <- length(unique(purchases.retail$store_code_uc))
LRdiff_res$N_counties <- uniqueN(purchases.retail, by = c("fips_state", "fips_county"))
LRdiff_res$N_years <- uniqueN(purchases.retail, by = c("year"))
LRdiff_res$N_county_modules <- uniqueN(purchases.retail, by = c("fips_state", "fips_county",
                                                                "product_module_code"))
LRdiff_res$N_store_modules <- uniqueN(purchases.retail, by = c("store_code_uc", "product_module_code"))
LRdiff_res$N_state_modules <- uniqueN(purchases.retail, by = c("fips_state", "product_module_code"))
LRdiff_res$N_hholds_modules <- uniqueN(purchases.retail, by = c("household_code", "product_module_code"))
LRdiff_res$N_hholds_stores <- uniqueN(purchases.retail, by = c("household_code", "store_code_uc"))
LRdiff_res$N_hholds_modules_stores <- length(unique(purchases.retail$household_by_store_by_module))
LRdiff_res$N_module_time <- length(unique(purchases.retail$module_by_time))

fwrite(LRdiff_res, "../../../../../home/slacouture/HMS/Basic_balance_results.csv")