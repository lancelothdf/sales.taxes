#' Sales Tax Project
#' Retailer data
#' Estimate Propensity Score of the effect of sales taxes using a binary treatment
#' Estimate yearly: treatment status varies (each year high or low depends on yearly median)
#' Use selection equation specification suggested by Imbens (C_lin = 1 and C_qua = 2.71)
#' Try different "matching" algorithms: remember we are matching at countly level
#' 

library(data.table)
library(lfe)
library(futile.logger)
library(AER)
library(multcomp)
library(splitstackshape)


# Set directory

setwd("/project2/igaarder")

## useful filepaths ------------------------------------------------------------
all_goods_pi_path <- "Data/Nielsen/price_quantity_indices_allitems_2006-2016_notaxinfo.csv"
FE_pindex_path <- "Data/Nielsen/Pindex_FE_yearly_all_years.csv"
output_yearly <- "Data/Nielsen/yearly_nielsen_data.csv"
pre_trend_data_path <- "Data/Nielsen/pre_trend_data_yearly.csv"

covariates.nhgis.path <- "Data/covariates/nhgis_county_clean.csv"
covariates.qcew.path <- "Data/covariates/qcew_clean.csv"
census.regions.path <- "Data/covariates/census_regions.csv"

tax.path <- "Data/county_monthly_tax_rates.csv"

zillow_path <- "Data/covariates/zillow_long_by_county_clean.csv"
zillow_state_path <- "Data/covariates/zillow_long_by_state_clean.csv"
unemp.path <- "Data/covariates/county_monthly_unemp_clean.csv"
border.path <- "Data/border_counties.csv"

## Where to save results
output.results.file <- "Data/LRdiff_results_LRspec_crosssection_binarypscore.csv"
output.path <- "../../home/slacouture/PS"


###### County covariates set up-----------------------------

## Time invariant covariates
list.counties <- data.frame(unique(yearly_data[,c('fips_state','fips_county')]))

#nhgis 2010 
nhgis2010 <- fread(covariates.nhgis.path)
nhgis2010 <- nhgis2010[year == 2010,] ## Keep the 2000 values
nhgis2010 <- nhgis2010[, c("statefp", "countyfp", "pct_pop_over_65", "pct_pop_under_25", "pct_pop_black", "pct_pop_urban", "housing_ownership_share")]
names(nhgis2010) <- c("fips_state", "fips_county", "pct_pop_over_65", "pct_pop_under_25", "pct_pop_black", "pct_pop_urban", "housing_ownership_share")
covariates <- merge(list.counties, nhgis2010, by = c("fips_state", "fips_county"), all.x = T)


#nhgis 2000 (because education and median income variables are missing in 2010)
nhgis2000 <- fread(covariates.nhgis.path)
nhgis2000 <- nhgis2000[year == 2000,] ## Keep the 2000 values
nhgis2000 <- nhgis2000[, c("statefp", "countyfp", "pct_pop_no_college", "pct_pop_bachelors", "median_income")]
names(nhgis2000) <- c("fips_state", "fips_county", "pct_pop_no_college", "pct_pop_bachelors", "median_income")

nhgis2000[, median_income := log(median_income)]
covariates <- merge(covariates, nhgis2000, by = c("fips_state", "fips_county"), all.x = T)

#census regions/divisions
census.regions <- fread(census.regions.path)
census.regions[, Division := Region*10 + Division]
covariates <- merge(covariates, census.regions, by = c("fips_state"), all.x = T)

## Time variant covariates
list.obs <- data.frame(unique(yearly_data[,c('fips_state','fips_county', 'year')]))
covariates <- merge(list.obs, covariates, by = c("fips_county", "fips_state"), all.x = T)

#qcew
qcew <- fread(covariates.qcew.path)
qcew <- qcew[year >= 2008 & year <= 2014,]
qcew <- qcew[, fips_state := as.numeric(substr(area_fips, 1, 2))]
qcew <- qcew[, fips_county := as.numeric(substr(area_fips, 3, 5))]
qcew <- qcew[, ln_mean_wage := log(total_mean_wage)]
qcew <- qcew[, c("fips_state", "fips_county", "year", "ln_mean_wage")]
covariates <- merge(covariates, qcew, by = c("year", "fips_county", "fips_state"), all.x = T)


#Zillow
all_counties <- unique(yearly_data[, .(fips_state, fips_county)])
county_skeleton <- data.table(NULL)
for (X in 2008:2014) {
  for (Y in 1:12) {
    all_counties[, year := X]
    all_counties[, month := Y]
    county_skeleton <- rbind(county_skeleton, all_counties)
  }
}


zillow_dt <- fread(zillow_path)
zillow_dt <- zillow_dt[between(year, 2008, 2014)]
zillow_dt <- zillow_dt[, .(fips_state, fips_county, median_home_price, year, month)]
zillow_dt <- merge(county_skeleton, zillow_dt, all.x = T,
                   by = c("fips_state", "fips_county", "year", "month"))

## prep state-level house prices (for when county-level is missing)
zillow_state_dt <- fread(zillow_state_path)
zillow_state_dt <- zillow_state_dt[between(year, 2008, 2014)]
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
covariates <- merge(covariates, zillow_dt, by = c("year", "fips_county", "fips_state"), all.x = T)


### Unemployment data
unemp.data <- fread(unemp.path)
unemp.data <- unemp.data[, c("fips_state", "fips_county", "year", "month", "rate")]
unemp.data <- unemp.data[, list(unemp = mean(rate)), by = .(year, fips_state, fips_county)]
unemp.data <- unemp.data[year >= 2006 & year <= 2016,]
unemp.data <- unemp.data[, ln_unemp := log(unemp)]
unemp.data <- unemp.data[, c("year", "fips_state", "fips_county", "ln_unemp")]

covariates <- merge(covariates, unemp.data, by = c("year", "fips_county", "fips_state"), all.x = T)

#### tax rates

tax.data <- fread(tax.path)
tax.data <- tax.data[, list(sales_tax = mean(sales_tax, na.rm = T)), by = .(year, fips_state, fips_county)]
tax.data <- tax.data[, ln_sales_tax := log(sales_tax)]

covariates <- merge(covariates, tax.data, by = c("year", "fips_county", "fips_state"), all.x = T)
# Release some space
rm(nhgis2000, nhgis2010, census.regions, qcew, zillow_dt, zillow_state_dt, unemp.data, tax.data)

### Need to load yearly data created before to identify counties that we won't use to drop them
yearly_data <- fread(output_yearly)
usefull.counties[, list(N = .N), by = .(year, fips_state, fips_county)]
covariates <- merge(covariates, usefull.counties, by = c("year", "fips_county", "fips_state"), all.x = T)
covariates <- covariates[!is.na(N),]

## Final data
covariates <- covariates[!is.na(ln_sales_tax) ]


###### Propensity Score set up -----------------------------

# Vector of "must be in" variables
Xb <- c("pct_pop_urban", "housing_ownership_share", "median_income", "pct_pop_no_college",  "pct_pop_bachelors",
        "pct_pop_over_65", "pct_pop_under_25", "pct_pop_black", "ln_unemp", "ln_home_price")

# Vector of potential variables
Xa_pot <- c("food_and_drugstores_empshare", "retail_empshare", "realestate_empshare", "public_admin_empshare", 
            "manufacturing_empshare", "finance_insurance_empshare", "construction_empshare", "retail_mean_wage", 
            "total_mean_wage", "nr_employment", "total_establishments", "retail_establishments", "nr_establishments",
            "total_employment", "retail_employment")

# Vector of outcomes to run cross-sectional design
outcomes <- c("ln_cpricei2", "ln_quantity2", "pct_pop_urban", "housing_ownership_share", "median_income", "pct_pop_no_college", "pct_pop_bachelors", "pct_pop_over_65", "pct_pop_under_25", "pct_pop_black", "ln_unemp", "ln_home_price")


###### Run Estimation ------------------------------------
# Start yearly loop
LRdiff_res <- data.table(NULL)
for (yr in 2008:2014) {
  
  flog.info("Starting year %s", yr)
  # Keep year of interest
  year.covariates <- covariates[ year == yr, ]
  year.data <- yearly_data[ year == yr, ]
  
  # Create binary treatment
  year.covariates <- year.covariates[, high.tax.rate := (ln_sales_tax >= median(ln_sales_tax, na.rm = T)) ]
  year.data <- year.data[, taxable :=ifelse(ln_sales_tax == 1, 0, 1)][, -c("ln_sales_tax")]
  
  ### Selection of covariates. Algorithm suggested by Imbens (2015) -----
  # Basic regression
  RHS <- paste(Xb, collapse  = " + ")
  curr.formula <- paste0("high.tax.rate ~ ", RHS)
  basic.select <- glm(curr.formula, family = binomial(link = "logit"), 
                      data = year.covariates, maxit = 10000)
  curr.likelihood <- basic.select$deviance
  reference <- nrow(year.covariates)
  
  
  # Selection of linear covariates to add (C_lin = 1 and logit)
  C_lin <- 1 # Threshold value
  Xblin <- Xb
  for (X in Xa_pot) {
    
    # Capture n_obs of potential variable
    N <- nrow(year.covariates[!is.na(get(X))])
    ratio.attr <- N/reference 
    # Try only those in which we lose less than 2% of the sample
    if (ratio.attr > 0.98) {
      new.formula <- paste(curr.formula, X, sep = " + ")
      # Run logit
      new.select <- glm(new.formula, family = binomial(link = "logit"), 
                        data = year.covariates, maxit = 10000) 
      # Use if likelihood test is above threshold
      lr.test <- (curr.likelihood - new.select$deviance)
      if (lr.test > C_lin) {
        # Capture
        curr.formula <- new.formula
        curr.likelihood <- new.select$deviance
        new <- paste0(X)
        Xblin = c(Xblin, new)
      }
    }
  }
  
  # Selection of quadratic covariates to add (C_qua = 2.71 and logit)
  C_qua <- 2.71 # Threshold value
  dim <- length(Xblin)
  row <- 0
  col <- 0
  Xfinal <- Xblin
  for (X1 in Xblin) {
    row <- row + 1
    for (X2 in Xb) {
      col <- col + 1
      t <- row + col + 1
      # Function to avoid repetition
      if (t <= dim) {
        # create product
        X <- paste(X1, X2, sep = "_")
        if (X1 == X2) {X <- paste0(X1, "_2")}
        year.covariates <- year.covariates[ , (X) := (get(X1)) * (get(X2))]
        new.formula <- paste(curr.formula, X, sep = " + ")
        new.select <- glm(new.formula, family = binomial(link = "logit"), 
                          data = year.covariates, maxit = 10000)  
        lr.test <- (curr.likelihood - new.select$deviance)
        if (lr.test > C_qua) {
          # Capture
          curr.formula <- new.formula
          curr.likelihood <- new.select$deviance
          Xfinal = c(Xfinal, X)
        } else {
          
          year.covariates <- year.covariates[, (X) := NULL ]
          
        }
        
      }  
      
    }
    
  }
  
  flog.info("Selection equation for year %s is: %s", yr, curr.formula)
  # Run the chosen selection equation
  final.select <- glm(curr.formula, family = binomial(link = "logit"), 
                      data = year.covariates, maxit = 10000)
  
  ### Trim Sample: we choose to trim by "Sufficient Overlap" as in Imbens (2015) -------
  # Following their approach, we use the practical choise of alpha = 0.1 an thus 
  # A = {x in X | 0.1 <= e(x) <= 0.9}
  # Predict
  year.covariates[, pscore:= predict(final.select, year.covariates, type = "response")]
  # trimming
  year.covariates.trim <- year.covariates[pscore >= 0.1 & pscore <= 0.9 & !is.na(pscore)]
  
  #### Now create comparision samples. Use 4 different algorithms ----------- 
  # 1) nearest neighbord, 2) k-nearest, 3) caliper, 4) weighted
  
  flog.info("Running matching algorithms for year %s", yr)
  # Algorithm 1: nearest neighbor (with replacement). All units are matched, both treated and controls
  nn.crosswalk <-data.table(NULL)
  for (i in 1:nrow(year.covariates.trim)) {
    
    # Extract observation info
    obs.i <- year.covariates.trim[i, ]
    # Add Info of pair number
    obs.i <- obs.i[, n_pair := i]
    # Find potential pairs and order by distance to selected observation
    potential.pairs <- year.covariates.trim[high.tax.rate != obs.i[, high.tax.rate], 
                                            ][, distance := abs(pscore - obs.i[, pscore])][order(distance)]
    # Extract closest pair
    pair.i<- potential.pairs[1, ][, -c("distance")]
    pair.i <- pair.i[, n_pair := i]
    # paste to previous selected pairs
    nn.crosswalk <- rbind(nn.crosswalk, obs.i, pair.i)
  }
  
  # Algorithm 2: k-nearest neighbor (with replacement). k=3. All units are matched, both treated and controls
  knn.crosswalk <-data.table(NULL)
  for (i in 1:nrow(year.covariates.trim)) {
    
    # Extract observation info
    obs.i <- year.covariates.trim[i, ]
    # Add Info of pair number
    obs.i <- obs.i[, n_pair := i][, w := 1]
    # Find potential pairs and order by distance to selected observation
    potential.pairs <- year.covariates.trim[high.tax.rate != obs.i[, high.tax.rate],
                                            ][, distance := abs(pscore - obs.i[, pscore])][order(distance)]
    # Extract closest pair
    pair.i<- potential.pairs[1:3, ][, -c("distance")]
    pair.i <- pair.i[, n_pair := i][, w := 1/3]
    # paste to previous selected pairs
    knn.crosswalk <- rbind(knn.crosswalk, obs.i, pair.i)
  }
  
  # Algorithm 3: neighbors in caliper (with replacement). r=0.001. All units are matched, both treated and controls. 
  # Note: If no pairfound, drop
  calip.crosswalk <-data.table(NULL)
  r <- 0.001 # Define caliper ratio
  for (i in 1:nrow(year.covariates.trim)) {
    
    # Extract observation info
    obs.i <- year.covariates.trim[i, ]
    # Add Info of pair number
    obs.i <- obs.i[, n_pair := i][, w := 1]
    # Find potential pairs and order by distance to selected observation
    potential.pairs <- year.covariates.trim[high.tax.rate != obs.i[, high.tax.rate],
                                            ][, distance := abs(pscore - obs.i[, pscore])][order(distance)]
    # Extract closest pair
    pair.i<- potential.pairs[distance < r, ][, -c("distance")]
    # PErform if pair found
    if (nrow(pair.i) > 0) {
      pair.i <- pair.i[, n_pair := i][, w := 1/.N]
      # paste to previous selected pairs
      calip.crosswalk <- rbind(calip.crosswalk, obs.i, pair.i)
    }
  }
  
  # Algorithm 4: weighting estimator. Build weights
  weighted.crosswalk <- year.covariates.trim[, w := ifelse(high.tax.rate == T, 
                                                           sum(high.tax.rate)*sum(high.tax.rate/pscore)/pscore,
                                                           sum(1-high.tax.rate)*sum((1-high.tax.rate)/(1-pscore))/(1-pscore)
  )]
  
  
  ##### Check balance using basic regression tests on covariates (Xfinal) by algorithm -------
  flog.info("Checking balance for year %s", yr)
  test.year <- data.table(NULL)
  for (X in Xfinal) {
    
    # Prior balance
    test.out <- lm(get(X) ~ high.tax.rate, data = year.covariates)
    priortest.dt <- data.table(coef(summary(test.out)))[2,][, -c("t value")]
    setnames(priortest.dt, old = c("Estimate", "Std. Error", "Pr(>|t|)"),
             new = c("prior.est", "prior.std.err", "prior.pval"))
    priortest.dt[, outcome := X]
    # Adjusted nn balance
    nn.test.out <- lm(get(X) ~ high.tax.rate, data = nn.crosswalk)
    nn.test.dt <- data.table(coef(summary(nn.test.out)))[2,][, -c("t value")]
    setnames(nn.test.dt, old = c("Estimate", "Std. Error", "Pr(>|t|)"),
             new = c("nn.est", "nn.std.err", "nn.pval"))
    # Adjusted knn balance
    knn.test.out <- lm(get(X) ~ high.tax.rate, data = knn.crosswalk, weights = w)
    knn.test.dt <- data.table(coef(summary(knn.test.out)))[2,][, -c("t value")]
    setnames(knn.test.dt, old = c("Estimate", "Std. Error", "Pr(>|t|)"),
             new = c("knn.est", "knn.std.err", "knn.pval"))
    # Adjusted caliper balance
    calip.test.out <- lm(get(X) ~ high.tax.rate, data = calip.crosswalk, weights = w)
    calip.test.dt <- data.table(coef(summary(calip.test.out)))[2,][, -c("t value")]
    setnames(calip.test.dt, old = c("Estimate", "Std. Error", "Pr(>|t|)"),
             new = c("calip.est", "calip.std.err", "calip.pval"))
    # Adjusted caliper balance
    weight.test.out <- lm(get(X) ~ high.tax.rate, data = weighted.crosswalk, weights = w)
    weight.test.dt <- data.table(coef(summary(weight.test.out)))[2,][, -c("t value")]
    setnames(weight.test.dt, old = c("Estimate", "Std. Error", "Pr(>|t|)"),
             new = c("weight.est", "weight.std.err", "weight.pval"))
    # Merge all tests
    test.dt <- cbind(priortest.dt, nn.test.dt, knn.test.dt, calip.test.dt, weight.test.dt)
    
    # Append to other outcomes
    test.year <- rbind(test.year, test.dt, fill = T)
  }
  
  # Export yearly test
  test.year.outfile <- paste0(output.path, "/Cov.Test/covariate_balance_", yr, ".csv")
  fwrite(test.year, test.year.outfile)
  
  #### Estimate cross-sectional design for each algorithm -------
  
  #### Algorithm 1: Nearest Neighbord
  nn.crosswalk <- merge(nn.crosswalk, year.data, by = c("fips_state", "fips_county"))
  # Create Interaction term
  nn.crosswalk <- nn.crosswalk[, high.tax.rate_taxable := high.tax.rate*taxable]
  
  for(Y in outcomes) {
    
    formula0 <- as.formula(paste0(
      Y, " ~ high.tax.rate + taxable + high.tax.rate_taxable | module_code | 0 | state_by_module ", sep = ""
    ))
    
    ### Base weights
    res0 <- felm(data = nn.crosswalk,
                 formula = formula0,
                 weights = nn.crosswalk$base.sales)
    
    ## attach results
    flog.info("Writing results...")
    res1.dt <- data.table(coef(summary(res0)), keep.rownames=T)
    res1.dt[, outcome := Y]
    res1.dt[, Rsq := summary(res0)$r.squared]
    res1.dt[, adj.Rsq := summary(res0)$adj.r.squared]
    res1.dt[, specification := "NN"]
    res1.dt[, weight := "base.sales"]
    res1.dt[, year := yr]
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)  ## Write results to a csv file 
    
    
    ### Current weights
    res0 <- felm(data = nn.crosswalk,
                 formula = formula0,
                 weights = nn.crosswalk$curr.sales)
    
    ## attach results
    flog.info("Writing results...")
    res1.dt <- data.table(coef(summary(res0)), keep.rownames=T)
    res1.dt[, outcome := Y]
    res1.dt[, Rsq := summary(res0)$r.squared]
    res1.dt[, adj.Rsq := summary(res0)$adj.r.squared]
    res1.dt[, specification := "NN"]
    res1.dt[, weight := "curr.sales"]
    res1.dt[, year := yr]
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)  ## Write results to a csv file 
    
  }
  
  
  #### Algorithm 2: k-Nearest Neighbord
  knn.crosswalk <- merge(knn.crosswalk, year.data, by = c("fips_state", "fips_county"))
  # Create Interaction term
  knn.crosswalk <- knn.crosswalk[, high.tax.rate_taxable := high.tax.rate*taxable]
  # Create new weights
  knn.crosswalk <- knn.crosswalk[, base.sales := base.sales*w]
  knn.crosswalk <- knn.crosswalk[, curr.sales := curr.sales*w]
  
  for(Y in outcomes) {
    
    
    formula0 <- as.formula(paste0(
      Y, " ~ high.tax.rate + taxable + high.tax.rate_taxable | module_code | 0 | state_by_module ", sep = ""
    ))
    ### Base weights
    res0 <- felm(data = knn.crosswalk,
                 formula = formula0,
                 weights = knn.crosswalk$base.sales)
    
    ## attach results
    flog.info("Writing results...")
    res1.dt <- data.table(coef(summary(res0)), keep.rownames=T)
    res1.dt[, outcome := Y]
    res1.dt[, Rsq := summary(res0)$r.squared]
    res1.dt[, adj.Rsq := summary(res0)$adj.r.squared]
    res1.dt[, specification := "KNN"]
    res1.dt[, weight := "base.sales"]
    res1.dt[, year := yr]
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)  ## Write results to a csv file 
    
    
    ### Current weights
    res0 <- felm(data = knn.crosswalk,
                 formula = formula0,
                 weights = knn.crosswalk$curr.sales)
    
    ## attach results
    flog.info("Writing results...")
    res1.dt <- data.table(coef(summary(res0)), keep.rownames=T)
    res1.dt[, outcome := Y]
    res1.dt[, Rsq := summary(res0)$r.squared]
    res1.dt[, adj.Rsq := summary(res0)$adj.r.squared]
    res1.dt[, specification := "KNN"]
    res1.dt[, weight := "curr.sales"]
    res1.dt[, year := yr]
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)  ## Write results to a csv file 
    
  }
  
  #### Algorithm 3: Caliper
  calip.crosswalk <- merge(calip.crosswalk, year.data, by = c("fips_state", "fips_county"))
  # Create Interaction term
  calip.crosswalk <- calip.crosswalk[, high.tax.rate_taxable := high.tax.rate*taxable]
  # Create new weights
  calip.crosswalk <- calip.crosswalk[, base.sales := base.sales*w]
  calip.crosswalk <- calip.crosswalk[, curr.sales := curr.sales*w]
  
  for(Y in outcomes) {
    
    
    formula0 <- as.formula(paste0(
      Y, " ~ high.tax.rate + taxable + high.tax.rate_taxable | module_code | 0 | state_by_module ", sep = ""
    ))
    ### Base weights
    res0 <- felm(data = calip.crosswalk,
                 formula = formula0,
                 weights = calip.crosswalk$base.sales)
    
    ## attach results
    flog.info("Writing results...")
    res1.dt <- data.table(coef(summary(res0)), keep.rownames=T)
    res1.dt[, outcome := Y]
    res1.dt[, Rsq := summary(res0)$r.squared]
    res1.dt[, adj.Rsq := summary(res0)$adj.r.squared]
    res1.dt[, specification := "Caliper"]
    res1.dt[, weight := "base.sales"]
    res1.dt[, year := yr]
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)  ## Write results to a csv file 
    
    
    ### Current weights
    res0 <- felm(data = calip.crosswalk,
                 formula = formula0,
                 weights = calip.crosswalk$curr.sales)
    
    ## attach results
    flog.info("Writing results...")
    res1.dt <- data.table(coef(summary(res0)), keep.rownames=T)
    res1.dt[, outcome := Y]
    res1.dt[, Rsq := summary(res0)$r.squared]
    res1.dt[, adj.Rsq := summary(res0)$adj.r.squared]
    res1.dt[, specification := "Caliper"]
    res1.dt[, weight := "curr.sales"]
    res1.dt[, year := yr]
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)  ## Write results to a csv file 
    
  }
  
  
  #### Algorithm 4: Weighted estimation
  weighted.crosswalk <- merge(weighted.crosswalk, year.data, by = c("fips_state", "fips_county"))
  # Create Interaction term
  weighted.crosswalk <- weighted.crosswalk[, high.tax.rate_taxable := high.tax.rate*taxable]
  # Create new weights
  weighted.crosswalk <- weighted.crosswalk[, base.sales := base.sales*w]
  weighted.crosswalk <- weighted.crosswalk[, curr.sales := curr.sales*w]
  
  for(Y in outcomes) {
    
    
    formula0 <- as.formula(paste0(
      Y, " ~ high.tax.rate + taxable + high.tax.rate_taxable | module_code | 0 | state_by_module ", sep = ""
    ))
    ### Base weights
    res0 <- felm(data = weighted.crosswalk,
                 formula = formula0,
                 weights = weighted.crosswalk$base.sales)
    
    ## attach results
    flog.info("Writing results...")
    res1.dt <- data.table(coef(summary(res0)), keep.rownames=T)
    res1.dt[, outcome := Y]
    res1.dt[, Rsq := summary(res0)$r.squared]
    res1.dt[, adj.Rsq := summary(res0)$adj.r.squared]
    res1.dt[, specification := "Weighted"]
    res1.dt[, weight := "base.sales"]
    res1.dt[, year := yr]
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)  ## Write results to a csv file 
    
    
    ### Current weights
    res0 <- felm(data = weighted.crosswalk,
                 formula = formula0,
                 weights = weighted.crosswalk$curr.sales)
    
    ## attach results
    flog.info("Writing results...")
    res1.dt <- data.table(coef(summary(res0)), keep.rownames=T)
    res1.dt[, outcome := Y]
    res1.dt[, Rsq := summary(res0)$r.squared]
    res1.dt[, adj.Rsq := summary(res0)$adj.r.squared]
    res1.dt[, specification := "Weighted"]
    res1.dt[, weight := "curr.sales"]
    res1.dt[, year := yr]
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)  ## Write results to a csv file 
    
  }
  
}







