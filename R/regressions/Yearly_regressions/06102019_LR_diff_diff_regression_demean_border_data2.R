### In this R-file we explore a specification meant to estimate long-run pass-through and sales/quantity response to tax changes

library(data.table)
library(lfe)
library(futile.logger)
library(AER)
library(splitstackshape)


setwd("/project2/igaarder")

## useful filepaths ------------------------------------------------------------
FE_pindex_path <- "Data/Nielsen/Pindex_FE_yearly_all_years.csv"
output_yearly <- "Data/Nielsen/yearly_nielsen_data.csv"
#taxable_pi_path <- "Data/Nielsen/price_quantity_indices_taxableitems_2006-2016.csv"
#eventstudy_tr_path <- "Data/event_study_tr_groups_comprehensive_firstonly_no2012q4_2013q1q2.csv"
#tr_groups_path <- "Data/tr_groups_comprehensive_firstonly_no2012q4_2013q1q2.csv"

covariates.nhgis.path <- "Data/covariates/nhgis_county_clean.csv"
covariates.qcew.path <- "Data/covariates/qcew_clean.csv"
census.regions.path <- "Data/covariates/census_regions.csv"

tax.path <- "Data/county_monthly_tax_rates.csv"

zillow_path <- "Data/covariates/zillow_long_by_county_clean.csv"
zillow_state_path <- "Data/covariates/zillow_long_by_state_clean.csv"
unemp.path <- "Data/covariates/county_monthly_unemp_clean.csv"
border.path <- "Data/border_counties.csv"



######### Regression analysis ##NOTE: Make sure that we only include 2008-2014 in the regressions!!
## run the analysis on price ---------------------------------------------------
yearly_data <- fread(output_yearly)
yearly_data <- yearly_data[year >= 2008 & year <= 2014,]



################################
## Include some covariates
#List of unique counties in the sales data
list.counties <- data.frame(unique(yearly_data[,c('fips_state','fips_county')]))

covariates.nhgis <- fread(covariates.nhgis.path)
census.regions <- fread(census.regions.path)
census.regions <- merge(list.counties, census.regions, by = c("fips_state"),
                        all.x = T)
census.regions$Division <- census.regions$Region*10 + census.regions$Division


###
#yearly_data <- merge(yearly_data, covariates.nhgis, by = c("fips_state", "fips_county"), all.x = T)
yearly_data <- merge(yearly_data, census.regions, by = c("fips_state", "fips_county"), all.x = T)

yearly_data[, region_by_time := .GRP, by = .(Region, year)]
yearly_data[, division_by_time := .GRP, by = .(Division, year)]
yearly_data[, reg_by_module_by_time := .GRP, by = .(Region, product_module_code, year)]
yearly_data[, div_by_module_by_time := .GRP, by = .(Division, product_module_code, year)]


########## Select a subset of variables to reduce size of dataset
yearly_data <- yearly_data[, c("fips_state", "fips_county", "store_code_uc", "year", "product_module_code", "ln_cpricei", "ln_cpricei2", "ln_quantity", "ln_quantity2", "ln_sales_tax", "base.sales", "store_module", "yr", "state_by_module", "module_by_time", "reg_by_module_by_time", "div_by_module_by_time")]


######## Import and prep house price and unemployment data

### Start with house prices
# First build a frame to make sure we can assign every county a home price
all_counties <- unique(yearly_data[, .(fips_state, fips_county)])
county_skeleton <- data.table(NULL)
for (X in 2006:2016) {
  for (Y in 1:12) {
    all_counties[, year := X]
    all_counties[, month := Y]
    county_skeleton <- rbind(county_skeleton, all_counties)
  }
}

## prep house price data
zillow_dt <- fread(zillow_path)
zillow_dt <- zillow_dt[between(year, 2006, 2016)]
zillow_dt <- zillow_dt[, .(fips_state, fips_county, median_home_price, year, month)]
zillow_dt <- merge(county_skeleton, zillow_dt, all.x = T,
                   by = c("fips_state", "fips_county", "year", "month"))

## prep state-level house prices (for when county-level is missing)
zillow_state_dt <- fread(zillow_state_path)
zillow_state_dt <- zillow_state_dt[between(year, 2006, 2016)]
zillow_state_dt <- zillow_state_dt[, .(fips_state, median_home_price, year, month)]
setnames(zillow_state_dt, "median_home_price", "state_median_home_price")
zillow_state_dt$month <- as.integer(round(zillow_state_dt$month))

zillow_dt <- merge(zillow_dt, zillow_state_dt, all.x = T,
                   by = c("fips_state", "year", "month"))
zillow_dt[is.na(median_home_price), median_home_price := state_median_home_price]
zillow_dt[, state_median_home_price := NULL]


## collapse to years
zillow_dt <- zillow_dt[, list(ln_home_price = log(mean(median_home_price))),
                       by = .(year, fips_state, fips_county)]

##
yearly_data <- merge(yearly_data, zillow_dt, by = c("fips_state", "fips_county", "year"), all.x = T)


### Unemployment data
unemp.data <- fread(unemp.path)
unemp.data <- unemp.data[, c("fips_state", "fips_county", "year", "month", "rate")]
unemp.data <- unemp.data[, list(unemp = mean(rate)), by = .(year, fips_state, fips_county)]
unemp.data <- unemp.data[year >= 2006 & year <= 2016,]
unemp.data <- unemp.data[, ln_unemp := log(unemp)]

##
yearly_data <- merge(yearly_data, unemp.data, by = c("fips_state", "fips_county", "year"), all.x = T)



########## Import data with border counties
border.counties <- fread(border.path)

border.counties[ , fips_state := floor(fips_county/1000)]
border.counties[, fips_county := fips_county - fips_state*1000]

#Only keep counties that are in the Nielsen dataset
border.counties <- merge(list.counties, border.counties,by = c("fips_state", "fips_county"), all.x = T)

#Only keep pairs for which both counties are in the data
setDT(border.counties)
keep_counties <- border.counties[, list(n = .N),
                                  by = .(bordindx)]
keep_counties <- keep_counties[n == 2]

setkey(border.counties, bordindx)
setkey(keep_counties, bordindx)

border.counties <- border.counties[keep_counties]
border.counties <- border.counties[, c("fips_state", "fips_county", "bordindx")]

#Count the number of times each county appears in the data set (number of pairs that it is a part of)
n_counties <- border.counties[, list(n = .N),
                                  by = .(fips_state, fips_county)]
border.counties[, id := seq_len(.N), by = .(fips_state, fips_county)] ##Create an ID within each county (that will map into a border index bordindx).  Later we will duplicate each observation in yearly_data as many times as this county appears in border pairs and will create the same id --> so that we can then assign a bordindx to each

## Duplicate each observation in yearly_data as many times as this county appears in the border dataset
yearly_data <- merge(yearly_data, n_counties, by = c("fips_state", "fips_county"), all.x = TRUE) #Merge number of times each county appears in list of border pairs
yearly_data <- yearly_data[!is.na(n)]
yearly_data <- expandRows(yearly_data, "n", drop = FALSE) #Duplicate observations as many times as county appears in border county pairs
yearly_data <- yearly_data[ , id := seq_len(.N), by = .(store_code_uc, product_module_code, year)] #Create the same ID as in border.counties.


## Merge yearly_data with the bordindx IDs
yearly_data <- merge(yearly_data, border.counties, by = c("fips_state", "fips_county", "id"), all.x = T)

## Generate some pair-specific FE
yearly_data[, pair_by_time := .GRP, by = .(bordindx, year)]
yearly_data[, pair_by_module_by_time := .GRP, by = .(bordindx, year, product_module_code)]

## Generate some regressions weights (want to divide usual weights by number of times the county appears in the dataset)
yearly_data[, weight := base.sales/n]



####
#################################################################################
#################################################################################
#############################################
## Demean the data


## module-store mean
yearly_data[, mean_ltax_ms := weighted.mean(ln_sales_tax, w = weight), by = .(store_code_uc, product_module_code)]
print("Calculated store-module mean ln_sales_tax")

yearly_data[, mean_lcpricei_ms := weighted.mean(ln_cpricei, w = weight), by = .(store_code_uc, product_module_code)]
print("Calculated store-module mean ln_cpricei")

yearly_data[, mean_lcpricei2_ms := weighted.mean(ln_cpricei2, w = weight), by = .(store_code_uc, product_module_code)]
print("Calculated store-module mean ln_cpricei2")

yearly_data[, mean_lquantity_ms := weighted.mean(ln_quantity, w = weight), by = .(store_code_uc, product_module_code)]
print("Calculated store-module mean ln_quantity")

yearly_data[, mean_lquantity2_ms := weighted.mean(ln_quantity2, w = weight), by = .(store_code_uc, product_module_code)]
print("Calculated store-module mean ln_quantity2")

yearly_data[, mean_lunemp_ms := weighted.mean(ln_unemp, w = weight), by = .(store_code_uc, product_module_code)]
print("Calculated store-module mean ln_unemp")

yearly_data[, mean_lhprice_ms := weighted.mean(ln_home_price, w = weight), by = .(store_code_uc, product_module_code)]
print("Calculated store-module mean ln_home_price")



##
fwrite(yearly_data, "Data/Nielsen/demeaned_yearly_data.csv")
print("Saved the mean by module-border-year")
##
#yearly_data <- fread("Data/Nielsen/demeaned_yearly_data.csv")


### To obtain residuals after two-way fixed effect: substract mean on each dimension then add sample mean (works because the sample is balanced).
## Create demeaned variables
yearly_data[, ln_sales_tax_ms := ln_sales_tax - mean_ltax_ms]
yearly_data[, ln_cpricei_ms := ln_cpricei - mean_lcpricei_ms]
yearly_data[, ln_cpricei2_ms := ln_cpricei2 - mean_lcpricei2_ms]
yearly_data[, ln_quantity_ms := ln_quantity - mean_lquantity_ms]
yearly_data[, ln_quantity2_ms := ln_quantity2 - mean_lquantity2_ms]
yearly_data[, ln_unemp_ms := ln_unemp - mean_lunemp_ms]
yearly_data[, ln_home_price_ms := ln_home_price - mean_lhprice_ms]




#### Run regressions with pair-specific FEs
#################################################################################
#################################################################################
#############################################
### First: regress price and quantity on tax rate after controling for econ. conditions
list.outcomes <- c("ln_cpricei", "ln_cpricei2", "ln_quantity", "ln_quantity2", "ln_home_price", "ln_unemp")

for(j in 1:length(list.outcomes)) {

  #
  outcome.j <- list.outcomes[j]
  outcome.j.ms <- paste(outcome.j, "_ms", sep = "")

  ## no FE (only store_module and time)
  price_formula1 <- as.formula(paste0(
    outcome.j.py, " ~ ln_sales_tax_py | pair_by_time + module_by_time | 0 | state_by_module "
  ))



  price_res1 <- felm(data = yearly_data,
                     formula = price_formula1,
                     weights = yearly_data$weight)

  if(j == 1) {
    LRdiff_res <-
      data.table(
        outcome = outcome.j,
        estimate = coef(summary(price_res1))["ln_sales_tax_ms", "Estimate"],
        se = coef(summary(price_res1))["ln_sales_tax_ms", "Cluster s.e."],
        pval = coef(summary(price_res1))["ln_sales_tax_ms", "Pr(>|t|)"],
        Rsq = summary(price_res1)$r.squared,
        adj.Rsq = summary(price_res1)$adj.r.squared,
        time_controls = "Module by time + pair by time FE"
      )

  } else {

    LRdiff_res <-
      rbind(LRdiff_res,
            data.table(
              outcome = outcome.j,
              estimate = coef(summary(price_res1))["ln_sales_tax_ms", "Estimate"],
              se = coef(summary(price_res1))["ln_sales_tax_ms", "Cluster s.e."],
              pval = coef(summary(price_res1))["ln_sales_tax_ms", "Pr(>|t|)"],
              Rsq = summary(price_res1)$r.squared,
              adj.Rsq = summary(price_res1)$adj.r.squared,
              time_controls = "Module by time + pair by time FE"
            )
      )
  }

  print(paste("Done - ", outcome.j, "pair X year + mod X year FE"))
  fwrite(LRdiff_res, "Data/LRdiff_results_county_pairs.csv")


  ## with module-time FE
  price_formula <- as.formula(paste0(
    outcome.j.ms, " ~ ln_sales_tax_ms | pair_by_module_by_time | 0 | state_by_module "
  ))

  price_res <- felm(data = yearly_data,
                    formula = price_formula,
                    weights = yearly_data$weight)

  LRdiff_res <-rbind(
    LRdiff_res,
    data.table(
      outcome = outcome.j,
      estimate = coef(summary(price_res))["ln_sales_tax_ms", "Estimate"],
      se = coef(summary(price_res))["ln_sales_tax_ms", "Cluster s.e."],
      pval = coef(summary(price_res))["ln_sales_tax_ms", "Pr(>|t|)"],
      Rsq = summary(price_res)$r.squared,
      adj.Rsq = summary(price_res)$adj.r.squared,
      time_controls = "pair by module by time FE"
    )
  )

  print(paste("Done - ", outcome.j, "mod X pair X year FE"))
  fwrite(LRdiff_res, "Data/LRdiff_results_county_pairs.csv")
}


## summary values --------------------------------------------------------------
## Some of these values are needed to adjust standard errors to reflect the correct number of FEs
LRdiff_res$N_obs <- nrow(yearly_data)
LRdiff_res$N_modules <- length(unique(yearly_data$product_module_code))
LRdiff_res$N_stores <- length(unique(yearly_data$store_code_uc))
LRdiff_res$N_counties <- uniqueN(yearly_data, by = c("fips_state", "fips_county"))
LRdiff_res$N_years <- uniqueN(yearly_data, by = c("year")) # should be 7
LRdiff_res$N_county_modules <- uniqueN(yearly_data, by = c("fips_state", "fips_county",
                                                           "product_module_code"))
LRdiff_res$N_county_pairs <- length(unique(yearly_data$bordindx))
LRdiff_res$N_store_modules <- uniqueN(yearly_data, by = c("store_code_uc", "product_module_code"))
LRdiff_res$N_module_years <- uniqueN(yearly_data, by = c("product_module_code", "year"))
LRdiff_res$N_pair_years <- uniqueN(yearly_data, by = c("bordindx", "year"))
LRdiff_res$N_pair_year_modules <- uniqueN(yearly_data, by = c("bordindx", "year", "product_module_code"))

fwrite(LRdiff_res, "Data/LRdiff_results_county_pairs.csv")

