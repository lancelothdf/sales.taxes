#' Author: Lancelot Henry de Frahan and John Bonney
#'
#'


library(data.table)
library(lfe)
library(futile.logger)
library(AER)
library(readstata13)
library(multcomp)



setwd("/project2/igaarder")
prep_enviro <- F


### OUTPUT
output.results.file <- "Data/LRdiff_monthly_results_preferred_cumulative.csv"


## useful filepaths ------------------------------------------------------------
all_goods_pi_path <- "Data/Nielsen/Monthly_data_2006-2016.csv"
taxable_pi_path <- "Data/Nielsen/price_quantity_indices_taxableitems_2006-2016.csv"
eventstudy_tr_path <- "Data/event_study_tr_groups_comprehensive_firstonly_no2012q4_2013q1q2.csv"
tr_groups_path <- "Data/tr_groups_comprehensive_firstonly_no2012q4_2013q1q2.csv"
sales_data_path <- "Data/sales_monthly_2006-2016.csv"
monthly_tax_path <- "Data/Nielsen/sales_tax_rate_monthly_product_level.dta"



## Prepare the monthly data ----------------------------------------------------

if (prep_enviro){
  ## create .csv's of taxable and all goods ------------------------------------
  flog.info("Reading in nonfood price indices...")
  nonfood_pi <- read.dta13("Data/Nielsen/Monthly_price_quantity_indices_nonfood.dta")
  nonfood_pi <- as.data.table(nonfood_pi)
  nonfood_pi[, c("cspricei", "squantityi", "spricei", "geocpricei",
                 "geoquantityi", "geopricei") := NULL] # to save space
  # fwrite(nonfood_pi, "Data/Nielsen/Monthly_price_quantity_indices_nonfood.dta")

  flog.info("Reading in food price indices...")
  food_pi <- fread("Data/Nielsen/monthly_price_quantity_indices_food.csv")
  food_pi[, c("cspricei", "squantityi", "spricei", "geocpricei",
              "geoquantityi", "geopricei") := NULL]

  flog.info("Binding food and nonfood data...")
  all_pi <- rbind(food_pi, nonfood_pi)
  all_pi <- all_pi[year %in% 2006:2014]
  rm(nonfood_pi, food_pi)
  gc()

  ### attach county and state FIPS codes, sales, and tax rates -----------------
  flog.info("Reading in sales data...")
  sales_data <- fread(sales_data_path)
  sales_data <- sales_data[, .(store_code_uc, product_module_code, fips_county,
                               fips_state, month, year, sales)]
  sales_data <- sales_data[year %in% 2006:2014]

  flog.info("Merging price and sales data...")
  all_pi <- merge(all_pi, sales_data, by = c("store_code_uc", "month", "year",
                                             "product_module_code" ))
  rm(sales_data)
  gc()

  flog.info("Reading in tax rates...")
  all.tax <- read.dta13(monthly_tax_path)
  setDT(all.tax)

  flog.info("Merging on tax rates...")
  all_pi <- merge(all_pi, all.tax,
                  by = c("fips_county", "fips_state", "product_module_code",
                         "year", "month", "product_group_code"),
                  all.x = T)

  fwrite(all_pi, all_goods_pi_path)

  rm(all.tax)
  gc()

} else {
  all_pi <- fread(all_goods_pi_path)
}


## prep Census region/division data
geo_dt <- structure(list(
  fips_state = c(1L, 2L, 4L, 5L, 6L, 8L, 9L, 10L, 12L, 13L, 15L, 16L, 17L, 18L,
                 19L, 20L, 21L, 22L, 23L, 24L, 25L, 26L, 27L, 28L, 29L, 30L,
                 31L, 32L, 33L, 34L, 35L, 36L, 37L, 38L, 39L, 40L, 41L, 42L,
                 44L, 45L, 46L, 47L, 48L, 49L, 50L, 51L, 53L, 54L, 55L, 56L),
  region = c(3L, 4L, 4L, 3L, 4L, 4L, 1L, 3L, 3L, 3L, 4L, 4L, 2L, 2L, 2L, 2L, 3L,
             3L, 1L, 3L, 1L, 2L, 2L, 3L, 2L, 4L, 2L, 4L, 1L, 1L, 4L, 1L, 3L, 2L,
             2L, 3L, 4L, 1L, 1L, 3L, 2L, 3L, 3L, 4L, 1L, 3L, 4L, 3L, 2L, 4L),
  division = c(6L, 9L, 8L,  7L, 9L, 8L, 1L, 5L, 5L, 5L, 9L, 8L, 3L, 3L, 4L, 4L,
               6L, 7L, 1L, 5L, 1L, 3L, 4L, 6L, 4L, 8L, 4L, 8L, 1L, 2L, 8L, 2L,
               5L, 4L, 3L,  7L, 9L, 2L, 1L, 5L, 4L, 6L, 7L, 8L, 1L, 5L, 9L, 5L, 3L, 8L)),
  class = "data.frame", row.names = c(NA, -50L))
setDT(geo_dt)


# Start with event-no-event case ===============================================

## prep the data ---------------------------------------------------------------

all_pi <- all_pi[year %in% 2006:2014 & !is.na(cpricei)]


## take logs
all_pi[, ln_cpricei := log(cpricei)]
all_pi[, ln_sales_tax := log(sales_tax)]
all_pi[, ln_quantity := log(sales) - log(pricei)]
all_pi[, ln_sales := log(sales)]



### Keep only relevant variables
all_pi <- all_pi[, c("ln_cpricei", "ln_sales_tax", "ln_quantity", "ln_sales", "store_code_uc", "product_module_code", "fips_state", "fips_county", "year", "month", "sales")]



## balance on store-module level
keep_store_modules <- all_pi[, list(n = .N),
                             by = .(store_code_uc, product_module_code)]
keep_store_modules <- keep_store_modules[n == (2014 - 2005) * 12]

setkey(all_pi, store_code_uc, product_module_code)
setkey(keep_store_modules, store_code_uc, product_module_code)

all_pi <- all_pi[keep_store_modules]


## get sales weights
all_pi[, base.sales := sales[year == 2008 & month == 1],
       by = .(store_code_uc, product_module_code)]

all_pi[, sales := NULL]
all_pi <- all_pi[!is.na(base.sales)]




## Collapse the data the county-level to reduce size of the dataset
all_pi <-  all_pi[, list(ln_cpricei = weighted.mean(ln_cpricei, w = base.sales), ln_sales_tax = weighted.mean(ln_sales_tax, w = base.sales), ln_quantity = weighted.mean(ln_quantity, w = base.sales), ln_sales = weighted.mean(ln_sales, w = base.sales), base.sales = sum(base.sales)),
                                        by = .(fips_state, fips_county, product_module_code, year, month)]
# We weigh all means by base.sales so that we get analogous of running the regression at the store-level with base sales as weights


## merge on the census region/division info
all_pi <- merge(all_pi, geo_dt, by = "fips_state")


all_pi[, county_ID := .GRP, by = .(fips_state, fips_county)]
all_pi[, county_by_module := .GRP, by = .(county_ID, product_module_code)]
all_pi[, cal_time := 12 * year + month]
all_pi[, module_by_time := .GRP, by = .(product_module_code, cal_time)]
all_pi[, module_by_state := .GRP, by = .(product_module_code, fips_state)]
all_pi[, region_by_module_by_time := .GRP, by = .(region, product_module_code, cal_time)]
all_pi[, division_by_module_by_time := .GRP, by = .(division, product_module_code, cal_time)]




#######################################################
## take first differences of outcomes and treatment
all_pi <- all_pi[order(county_ID, product_module_code, cal_time),] ##Sort on store by year-month (in ascending order)


all_pi[, D.ln_cpricei := ln_cpricei - shift(ln_cpricei, n=1, type="lag"),
       by = .(county_ID, product_module_code)]

all_pi[, D.ln_quantity := ln_quantity - shift(ln_quantity, n=1, type="lag"),
       by = .(county_ID, product_module_code)]

all_pi[, D.ln_sales := ln_sales - shift(ln_sales, n=1, type="lag"),
       by = .(county_ID, product_module_code)]

all_pi[, D.ln_sales_tax := ln_sales_tax - shift(ln_sales_tax, n=1, type="lag"),
       by = .(county_ID, product_module_code)]



## generate lags and leads of ln_sales_tax
for (lag.val in 1:24) {
  lag.X <- paste0("L", lag.val, ".D.ln_sales_tax")
  all_pi[, (lag.X) := shift(D.ln_sales_tax, n=lag.val, type="lag"),
         by = .(county_ID, product_module_code)]
  
  lead.X <- paste0("F", lag.val, ".D.ln_sales_tax")
  all_pi[, (lead.X) := shift(D.ln_sales_tax, n=lag.val, type="lead"),
         by = .(county_ID, product_module_code)]
}



### Estimation ---------------------------------------------------
all_pi <- all_pi[between(year, 2008, 2014)]
all_pi <- all_pi[ year >= 2009 | (year == 2008 & month >= 2)] ## First month of 2008, the difference was imputed not real data - so we drop it


formula_lags <- paste0("L", 1:24, ".D.ln_sales_tax", collapse = "+")
formula_leads <- paste0("F", 1:24, ".D.ln_sales_tax", collapse = "+")
formula_RHS <- paste0("D.ln_sales_tax + ", formula_lags, "+", formula_leads)



outcomes <- c("D.ln_cpricei", "D.ln_quantity", "D.ln_sales")
econ.outcomes <- c("D.ln_unemp", "D.ln_home_price")
FE_opts <- c("module_by_time", "region_by_module_by_time", "division_by_module_by_time")


## for linear hypothesis tests
lead.vars <- paste(paste0("F", 24:1, ".D.ln_sales_tax"), collapse = " + ")
lag.vars <- paste(paste0("L", 24:1, ".D.ln_sales_tax"), collapse = " + ")
lead.lp.restr <- paste(lead.vars, "= 0")
lag.lp.restr <- paste(lag.vars, "+ D.ln_sales_tax = 0")
total.lp.restr <- paste(lag.vars, "+", lead.vars, "+ D.ln_sales_tax = 0")


### First: run regression and estimate leads and lags directly 
## No Econ controls (but we run the placebos)
LRdiff_res <- data.table(NULL)
for (Y in c(outcomes)) {
  for (FE in FE_opts) {
    
    formula1 <- as.formula(paste0(
      Y, "~", formula_RHS, "| ", FE, " | 0 | module_by_state"
    ))
    flog.info("Estimating with %s as outcome with %s FE.", Y, FE)
    res1 <- felm(formula = formula1, data = all_pi,
                 weights = all_pi$base.sales)
    flog.info("Finished estimating with %s as outcome with %s FE.", Y, FE)
    
    
    ## attach results
    flog.info("Writing results...")
    res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
    res1.dt[, outcome := Y]
    res1.dt[, controls := FE]
    res1.dt[, econ := "none"]
    res1.dt[, parametric := "No"]
    res1.dt[, Rsq := summary(res1)$r.squared]
    res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)
    
    ## sum leads
    flog.info("Summing leads...")
    lead.test <- glht(res1, linfct = lead.lp.restr)
    lead.test.est <- coef(summary(lead.test))[[1]]
    lead.test.se <- sqrt(vcov(summary(lead.test)))[[1]]
    lead.test.pval <- 2*(1 - pnorm(abs(lead.test.est/lead.test.se)))
    
    ## sum lags
    flog.info("Summing lags...")
    lag.test <- glht(res1, linfct = lag.lp.restr)
    lag.test.est <- coef(summary(lag.test))[[1]]
    lag.test.se <- sqrt(vcov(summary(lag.test)))[[1]]
    lag.test.pval <- 2*(1 - pnorm(abs(lag.test.est/lag.test.se)))
    
    ## sum all
    flog.info("Summing all...")
    total.test <- glht(res1, linfct = total.lp.restr)
    total.test.est <- coef(summary(total.test))[[1]]
    total.test.se <- sqrt(vcov(summary(total.test)))[[1]]
    total.test.pval <- 2*(1 - pnorm(abs(total.test.est/total.test.se)))
    
    ## linear hypothesis results
    lp.dt <- data.table(
      rn = c("Pre.D.ln_sales_tax", "Post.D.ln_sales_tax", "All.D.ln_sales_tax"),
      Estimate = c(lead.test.est, lag.test.est, total.test.est),
      `Cluster s.e.` = c(lead.test.se, lag.test.se, total.test.se),
      `Pr(>|t|)` = c(lead.test.pval, lag.test.pval, total.test.pval),
      outcome = Y,
      controls = FE,
      econ = "none",
      parametric = "No",
      Rsq = summary(res1)$r.squared,
      adj.Rsq = summary(res1)$adj.r.squared)
    LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)
    
    
    ##### Add the cumulative effect at each lead/lag (relative to -2)
    cumul.lead2.est <- 0
    cumul.lead2.se <- NA
    cumul.lead2.pval <- NA
    
    #cumul.lead2.est is just equal to minus the change between -2 and -1
    cumul.lead3.est <- - coef(summary(res1))[ "F2.D.ln_sales_tax", "Estimate"]
    cumul.lead3.se <- coef(summary(res1))[ "F2.D.ln_sales_tax", "Cluster s.e."]
    cumul.lead3.pval <- coef(summary(res1))[ "F2.D.ln_sales_tax", "Pr(>|t|)"]
    
    ##LEADS
    for(j in 4:25) {
      
      ## Create a name for estimate, se and pval of each lead
      cumul.test.est.name <- paste("cumul.lead", j, ".est", sep = "")
      cumul.test.se.name <- paste("cumul.lead", j, ".se", sep = "")
      cumul.test.pval.name <- paste("cumul.lead", j, ".pval", sep = "")
      
      ## Create the formula to compute cumulative estimate at each lead/lag
      cumul.test.form <- paste0("-", paste(paste0("F", (j-1):2, ".D.ln_sales_tax"), collapse = " - "))
      cumul.test.form <- paste(cumul.test.form, " = 0")
      
      ## Compute estimate and store in variables names
      cumul.test <- glht(res1, linfct = cumul.test.form)
      
      assign(cumul.test.est.name, coef(summary(cumul.test))[[1]])
      assign(cumul.test.se.name, sqrt(vcov(summary(cumul.test)))[[1]])
      assign(cumul.test.pval.name, 2*(1 - pnorm(abs(coef(summary(cumul.test))[[1]]/sqrt(vcov(summary(cumul.test)))[[1]]))))
    }
    
    
    ##LAGS
    ## Lead1
    cumul.lead1.est <- coef(summary(res1))[ "F1.D.ln_sales_tax", "Estimate"]
    cumul.lead1.se <- coef(summary(res1))[ "F1.D.ln_sales_tax", "Cluster s.e."]
    cumul.lead1.pval <- coef(summary(res1))[ "F1.D.ln_sales_tax", "Pr(>|t|)"]
    
    ## On Impact --> Effect = coefficient on D.ln_sales_tax
    cumul.test.form <- "F1.D.ln_sales_tax + D.ln_sales_tax = 0"
    cumul.test <- glht(res1, linfct = cumul.test.form)
    
    cumul.lag0.est <- coef(summary(cumul.test))[[1]]
    cumul.lag0.se <- sqrt(vcov(summary(cumul.test)))[[1]]
    cumul.lag0.pval <- 2*(1 - pnorm(abs(coef(summary(cumul.test))[[1]]/sqrt(vcov(summary(cumul.test)))[[1]])))
    
    
    for(j in 1:24) {
      
      ## Create a name for estimate, se and pval of each lead
      cumul.test.est.name <- paste("cumul.lag", j, ".est", sep = "")
      cumul.test.se.name <- paste("cumul.lag", j, ".se", sep = "")
      cumul.test.pval.name <- paste("cumul.lag", j, ".pval", sep = "")
      
      ## Create the formula to compute cumulative estimate at each lead/lag
      cumul.test.form <- paste("F1.D.ln_sales_tax + D.ln_sales_tax + ", paste(paste0("L", 1:j, ".D.ln_sales_tax"), collapse = " + "), sep = "")
      cumul.test.form <- paste(cumul.test.form, " = 0")
      
      ## Compute estimate and store in variables names
      cumul.test <- glht(res1, linfct = cumul.test.form)
      
      assign(cumul.test.est.name, coef(summary(cumul.test))[[1]])
      assign(cumul.test.se.name, sqrt(vcov(summary(cumul.test)))[[1]])
      assign(cumul.test.pval.name, 2*(1 - pnorm(abs(coef(summary(cumul.test))[[1]]/sqrt(vcov(summary(cumul.test)))[[1]]))))
    }
    
    
    ## linear hypothesis results
    lp.dt <- data.table(
      rn = c("cumul.lead25.D.ln_sales_tax", "cumul.lead24.D.ln_sales_tax", "cumul.lead23.D.ln_sales_tax", "cumul.lead22.D.ln_sales_tax", "cumul.lead21.D.ln_sales_tax", "cumul.lead20.D.ln_sales_tax", "cumul.lead19.D.ln_sales_tax", "cumul.lead18.D.ln_sales_tax", "cumul.lead17.D.ln_sales_tax", "cumul.lead16.D.ln_sales_tax", "cumul.lead15.D.ln_sales_tax", "cumul.lead14.D.ln_sales_tax", "cumul.lead13.D.ln_sales_tax", "cumul.lead12.D.ln_sales_tax", "cumul.lead11.D.ln_sales_tax", "cumul.lead10.D.ln_sales_tax", "cumul.lead9.D.ln_sales_tax", "cumul.lead8.D.ln_sales_tax", "cumul.lead7.D.ln_sales_tax", "cumul.lead6.D.ln_sales_tax", "cumul.lead5.D.ln_sales_tax", "cumul.lead4.D.ln_sales_tax", "cumul.lead3.D.ln_sales_tax", "cumul.lead2.D.ln_sales_tax", "cumul.lead1.D.ln_sales_tax", "cumul.lag0.D.ln_sales_tax", "cumul.lag1.D.ln_sales_tax", "cumul.lag2.D.ln_sales_tax", "cumul.lag3.D.ln_sales_tax", "cumul.lag4.D.ln_sales_tax", "cumul.lag5.D.ln_sales_tax", "cumul.lag6.D.ln_sales_tax", "cumul.lag7.D.ln_sales_tax", "cumul.lag8.D.ln_sales_tax", "cumul.lag9.D.ln_sales_tax", "cumul.lag10.D.ln_sales_tax", "cumul.lag11.D.ln_sales_tax", "cumul.lag12.D.ln_sales_tax", "cumul.lag13.D.ln_sales_tax", "cumul.lag14.D.ln_sales_tax", "cumul.lag15.D.ln_sales_tax", "cumul.lag16.D.ln_sales_tax", "cumul.lag17.D.ln_sales_tax", "cumul.lag18.D.ln_sales_tax", "cumul.lag19.D.ln_sales_tax", "cumul.lag20.D.ln_sales_tax", "cumul.lag21.D.ln_sales_tax", "cumul.lag22.D.ln_sales_tax", "cumul.lag23.D.ln_sales_tax", "cumul.lag24.D.ln_sales_tax"),
      Estimate = c(cumul.lead25.est, cumul.lead24.est, cumul.lead23.est, cumul.lead22.est, cumul.lead21.est, cumul.lead20.est, cumul.lead19.est, cumul.lead18.est, cumul.lead17.est, cumul.lead16.est, cumul.lead15.est, cumul.lead14.est, cumul.lead13.est, cumul.lead12.est, cumul.lead11.est, cumul.lead10.est, cumul.lead9.est, cumul.lead8.est, cumul.lead7.est, cumul.lead6.est, cumul.lead5.est, cumul.lead4.est, cumul.lead3.est, cumul.lead2.est, cumul.lead1.est, cumul.lag0.est, cumul.lag1.est, cumul.lag2.est, cumul.lag3.est, cumul.lag4.est, cumul.lag5.est, cumul.lag6.est, cumul.lag7.est, cumul.lag8.est, cumul.lag9.est, cumul.lag10.est, cumul.lag11.est, cumul.lag12.est, cumul.lag13.est, cumul.lag14.est, cumul.lag15.est, cumul.lag16.est, cumul.lag17.est, cumul.lag18.est, cumul.lag19.est, cumul.lag20.est, cumul.lag21.est, cumul.lag22.est, cumul.lag23.est, cumul.lag24.est),
      `Cluster s.e.` = c(cumul.lead25.est, cumul.lead24.se, cumul.lead23.se, cumul.lead22.se, cumul.lead21.se, cumul.lead20.se, cumul.lead19.se, cumul.lead18.se, cumul.lead17.se, cumul.lead16.se, cumul.lead15.se, cumul.lead14.se, cumul.lead13.se, cumul.lead12.se, cumul.lead11.se, cumul.lead10.se, cumul.lead9.se, cumul.lead8.se, cumul.lead7.se, cumul.lead6.se, cumul.lead5.se, cumul.lead4.se, cumul.lead3.se, cumul.lead2.se, cumul.lead1.se, cumul.lag0.se, cumul.lag1.se, cumul.lag2.se, cumul.lag3.se, cumul.lag4.se, cumul.lag5.se, cumul.lag6.se, cumul.lag7.se, cumul.lag8.se, cumul.lag9.se, cumul.lag10.se, cumul.lag11.se, cumul.lag12.se, cumul.lag13.se, cumul.lag14.se, cumul.lag15.se, cumul.lag16.se, cumul.lag17.se, cumul.lag18.se, cumul.lag19.se, cumul.lag20.se, cumul.lag21.se, cumul.lag22.se, cumul.lag23.se, cumul.lag24.se),
      `Pr(>|t|)` = c(cumul.lead25.pval, cumul.lead24.pval, cumul.lead23.pval, cumul.lead22.pval, cumul.lead21.pval, cumul.lead20.pval, cumul.lead19.pval, cumul.lead18.pval, cumul.lead17.pval, cumul.lead16.pval, cumul.lead15.pval, cumul.lead14.pval, cumul.lead13.pval, cumul.lead12.pval, cumul.lead11.pval, cumul.lead10.pval, cumul.lead9.pval, cumul.lead8.pval, cumul.lead7.pval, cumul.lead6.pval, cumul.lead5.pval, cumul.lead4.pval, cumul.lead3.pval, cumul.lead2.pval, cumul.lead1.pval, cumul.lag0.pval, cumul.lag1.pval, cumul.lag2.pval, cumul.lag3.pval, cumul.lag4.pval, cumul.lag5.pval, cumul.lag6.pval, cumul.lag7.pval, cumul.lag8.pval, cumul.lag9.pval, cumul.lag10.pval, cumul.lag11.pval, cumul.lag12.pval, cumul.lag13.pval, cumul.lag14.pval, cumul.lag15.pval, cumul.lag16.pval, cumul.lag17.pval, cumul.lag18.pval, cumul.lag19.pval, cumul.lag20.pval, cumul.lag21.pval, cumul.lag22.pval, cumul.lag23.pval, cumul.lag24.pval),
      outcome = Y,
      controls = FE,
      econ = "none",
      parametric = "No",
      Rsq = summary(res1)$r.squared,
      adj.Rsq = summary(res1)$adj.r.squared)
    LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)
    
    
  }
}



## summary values --------------------------------------------------------------
LRdiff_res$N_obs <- nrow(all_pi)
LRdiff_res$N_modules <- length(unique(all_pi$product_module_code))
LRdiff_res$N_counties <- uniqueN(all_pi, by = c("fips_state", "fips_county"))
LRdiff_res$N_years <- uniqueN(all_pi, by = c("year")) # should be 6 (we lose one because we difference)
LRdiff_res$N_county_modules <- uniqueN(all_pi, by = c("fips_state", "fips_county",
                                                      "product_module_code"))

fwrite(LRdiff_res, output.results.file)





