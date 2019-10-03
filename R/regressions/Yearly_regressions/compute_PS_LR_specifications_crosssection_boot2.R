#' Sales Tax Project
#' Retailer data
#' Estimate Propensity Score of the effect of sales taxes using a binary treatment
#' Estimate yearly: treatment status varies (each year high or low depends on yearly median)
#' Use selection equation specification suggested by Imbens (C_lin = 1 and C_qua = 2.71)
#' Try different "matching" algorithms: remember we are matching at countly level
#' Calculate standard errors of interest outcomes by block bootstrap at module x state level
#' We include Region FE and lagged values in some cases instead of current
#' NEW: include some politica observables as potential outcomes

library(data.table)
library(lfe)
library(futile.logger)
library(AER)
library(multcomp)
library(boot)
library(dplyr)

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

tax.path <- "Data/county_monthly_tax_rates_2008_2014.csv"

zillow_path <- "Data/covariates/zillow_long_by_county_clean.csv"
zillow_state_path <- "Data/covariates/zillow_long_by_state_clean.csv"
unemp.path <- "Data/covariates/county_monthly_unemp_clean.csv"
border.path <- "Data/border_counties.csv"
pol.path <- "Data/political_covars.csv"

## Where to save results
output.path <- "../../home/slacouture/PS"

###### County covariates set up. I keep every possible covariate -----------------------------
# Need to load yearly data to identify useful counties
yearly_data <- fread(output_yearly)
## Time invariant covariates
list.counties <- data.table(unique(yearly_data[,c('fips_state','fips_county')]))

#nhgis 2010 
nhgis2010 <- fread(covariates.nhgis.path)
nhgis2010 <- nhgis2010[year == 2010,] ## Keep the 2010 values
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
## Transform regions into dummies for selection equation
for (reg in as.integer(unique(covariates[!is.na(Region), c('Region')])[["Region"]])) {
  name <-paste0("reg_", reg)
  covariates[, (name) := ifelse(Region == reg, 1 ,0)]
}

## Time variant covariates
list.obs <- data.frame(unique(yearly_data[,c('fips_state','fips_county', 'year')]))
covariates <- merge(list.obs, covariates, by = c("fips_county", "fips_state"), all.x = T)

#qcew
qcew <- fread(covariates.qcew.path)
qcew <- qcew[year >= 2008 & year <= 2014,]
qcew <- qcew[, fips_state := as.numeric(substr(area_fips, 1, 2))]
qcew <- qcew[, fips_county := as.numeric(substr(area_fips, 3, 5))]
qcew <- qcew[, ln_mean_wage := log(total_mean_wage)]
qcew <- qcew[, ln_mean_retail_wage := log(retail_mean_wage)]
qcew <- qcew[, -c("total_mean_wage", "retail_mean_wage" )]
covariates <- merge(covariates, qcew, by = c("year", "fips_county", "fips_state"), all.x = T)


#Zillow
all_counties <- unique(yearly_data[, .(fips_state, fips_county)])
county_skeleton <- data.table(NULL)
for (X in 2007:2014) {
  for (Y in 1:12) {
    all_counties[, year := X]
    all_counties[, month := Y]
    county_skeleton <- rbind(county_skeleton, all_counties)
  }
}


zillow_dt <- fread(zillow_path)
zillow_dt <- zillow_dt[between(year, 2007, 2014)]
zillow_dt <- zillow_dt[, .(fips_state, fips_county, median_home_price, year, month)]
zillow_dt <- merge(county_skeleton, zillow_dt, all.x = T,
                   by = c("fips_state", "fips_county", "year", "month"))

## prep state-level house prices (for when county-level is missing)
zillow_state_dt <- fread(zillow_state_path)
zillow_state_dt <- zillow_state_dt[between(year, 2007, 2014)]
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
## Create Lagged value and keep interest years
zillow_dt <- zillow_dt[order(fips_state, fips_county, year),] ##Sort on store by year (in ascending order)
zillow_dt[, L.ln_home_price := shift(ln_home_price, n=1, type="lag"),
          by = .(fips_state, fips_county)]
zillow_dt <- zillow_dt[between(year, 2008, 2014)]

covariates <- merge(covariates, zillow_dt, by = c("year", "fips_county", "fips_state"), all.x = T)


### Unemployment data
unemp.data <- fread(unemp.path)
unemp.data <- unemp.data[, c("fips_state", "fips_county", "year", "month", "rate")]
unemp.data <- unemp.data[, list(unemp = mean(rate)), by = .(year, fips_state, fips_county)]
unemp.data <- unemp.data[year >= 2006 & year <= 2016,]
unemp.data <- unemp.data[, ln_unemp := log(unemp)]
## Create Lagged value and keep interest years
unemp.data <- unemp.data[order(fips_state, fips_county, year),] ##Sort on store by year (in ascending order)
unemp.data[, L.ln_unemp := shift(ln_unemp, n=1, type="lag"),
          by = .(fips_state, fips_county)]
unemp.data <- unemp.data[, -c("unemp")]

covariates <- merge(covariates, unemp.data, by = c("year", "fips_county", "fips_state"), all.x = T)

### Political Covars
pol.data <- fread(pol.path)
pol.data <- pol.data[, ln_mw := log(mw)]
pol.data <- pol.data[, fips_county := fips_county - fips_state*1000]
covariates <- merge(covariates, pol.data, by = c("year", "fips_county", "fips_state"), all.x = T)


#### tax rates

tax.data <- fread(tax.path)
tax.data <- tax.data[, list(sales_tax = mean(sales_tax, na.rm = T)), by = .(year, fips_state, fips_county)]
tax.data <- tax.data[, ln_sales_tax := log1p(sales_tax)]

covariates <- merge(covariates, tax.data, by = c("year", "fips_county", "fips_state"), all.x = T)

covariates <- as.data.table(covariates)
yearly_data <- as.data.table(yearly_data)
# Got to drop some variables in yearly data to perform well
yearly_data <- yearly_data[, -c("n", "yr", "sales_tax")]
# Create Share of quantities
yearly_data <- yearly_data[, ln_share_sales := log(sales/sum(sales)), 
                           by = .(store_code_uc, fips_county, fips_state, year)]


###### Propensity Score set up -----------------------------

# Vector of "must be in" variables
Xb <- c("L.ln_unemp", "L.ln_home_price", "reg_2", "reg_3", "reg_4")

# Vector of potential variables
Xa_pot <- c("pct_pop_urban", "housing_ownership_share", "median_income", "pct_pop_no_college", "pct_pop_bachelors",
            "pct_pop_over_65", "pct_pop_under_25", "pct_pop_black", "ln_mean_wage")
# I include republican characteristics
Xa_pot2 <- c("pct_pop_urban", "housing_ownership_share", "median_income", "pct_pop_no_college", "pct_pop_bachelors",
            "pct_pop_over_65", "pct_pop_under_25", "pct_pop_black", "ln_mean_wage", "mrpres_totalvotes",
            "mrpres_rep_share", "gov_n_years", "gov_rep", "rep_legist_part_cont", "ln_mw")

# Vector of all variables
X_all <- c(Xb, Xa_pot)


# Vector of outcomes to run cross-sectional design. Not gonna run on covariates: already balancing on them at county level
 
r.outcomes <- c("ln_cpricei2", "ln_quantity2", "ln_share_sales")
tax.rates <- c("ln_sales_tax", "ln_statutory_sales_tax")
outcomes <- c(r.outcomes, tax.rates)

# Create Interest variables and set up data

covariates <- covariates[!is.na(ln_sales_tax), ]
covariates <- covariates[, high.tax.rate := (ln_sales_tax >= median(ln_sales_tax)), by = .(year) ]
setnames(covariates, old = "ln_sales_tax", new = "ln_statutory_sales_tax")
yearly_data <- yearly_data[year >= 2008 & year <= 2014, ]

############## Create a function that performs the PS matching as desired -----------------

psmatch.taxrate <- function(actual.data, covariate.data, algor = "NN", weights, 
                            must.covar, oth.covars, treatment, main.outcomes, 
                            tau, implicit = T, covar.test = F, boot.run = T) {
  
  #' actual.data contains the estimation data (retailer data)
  #' covariate.data contains the covariates at the county level that are used to match
  #' algor indicates the algorithm to run and can take the following values:
  #'     NN (the default): nearest neighbor
  #'     KNN: three nearest neighbors
  #'     calip: all neighbors within a ratius of 0.001
  #'     weighted: estimates the weighted propensity score regression
  #'     NoMatch: estimates without matching
  #' weights indicates the name of the variable that is used to weight (TODO: work this out)
  #' must.covar are the variables that must be included in ps estimation
  #' oth.covars the other potential covariates that can be used
  #' treatment indicates the name of the "treatment" variable
  #' main.outcomes indicates the vector of outcomes to run the estimation
  #' tau indicate the vector of variables that are tax rates to retrieve the interest parameters
  #' implicit tells the function to expor the "impliced reduced form estimates"
  #' covar.test tells the program to run the covariate test only
  #' boot run tells the function that is going to be used for bootstrapping (thus exporting only the interest vector)
  
  # Identify counties for which we will match
  list.counties <- data.table(unique(actual.data[,c('fips_state','fips_county')]))
  # Keep covariate data we are interested in for each iteration
  covariate.data <- merge(covariate.data, list.counties, by = c('fips_state','fips_county'))
  
  # Identify years
  list.years <- unique(actual.data[, c('year')])[["year"]]
  
  # Identify outcomes to run regressions (main.outcomes + tau)
  outcomes <- c(main.outcomes, tau)
  
  # Make sure median has been defined in data (treatment)
  # Make sure observations without sales_tax rates have been dropped
  # Make sure "ln_sales_tax" is not in covariate.data but only in actual.data
  # Make sure data is already restricted to interest sample
  
  # Create output elements
  LRdiff_res <- data.table(NULL)
  test.total <- data.table(NULL)
  
  # Start yearly estimations
  flog.info("Starting Matching algorithm")
  for (yr in list.years) {
    
    # Keep year of interest
    year.data <- actual.data[ year == yr,]
    year.covariates <- covariate.data[ year == yr, ]
    # Create taxability
    year.data <- year.data[, taxable :=ifelse(ln_sales_tax == 0, FALSE, TRUE)]

    ### Selection of covariates. Algorithm suggested by Imbens (2015) -----
    # Basic regression
    RHS <- paste(must.covar, collapse  = " + ")
    curr.formula <- paste0(treatment, " ~ ", RHS)
    basic.select <- glm(curr.formula, family = binomial(link = "logit"), 
                        data = year.covariates, maxit = 10000)
    curr.likelihood <- basic.select$deviance
    reference <- nrow(year.covariates)
    
    # Selection of linear covariates to add (C_lin = 1 and logit)
    C_lin <- 1 # Threshold value
    Xblin <- must.covar
    for (X in oth.covars) {
      
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
    
    # Run the chosen selection equation
    final.select <- glm(curr.formula, family = binomial(link = "logit"), 
                        data = year.covariates, maxit = 10000)
    
    ### Trim Sample: we choose to trim by "Sufficient Overlap" as in Imbens (2015) -------
    # Following their approach, we use the practical choise of alpha = 0.1 an thus 
    # A = {x in X | 0.1 <= e(x) <= 0.9}
    # Predict and dropping sales tax rates (not used any more and want to use the effective tax rate)
    year.covariates[, pscore:= predict(final.select, year.covariates, type = "response")]
    # trimming 
    year.covariates.trim <- year.covariates[pscore >= 0.1 & pscore <= 0.9 & !is.na(pscore)]
    
    #### Now create comparision samples and estimate. Use 4 different algorithms ----------- 
    # 1) nearest neighbord, 2) k-nearest, 3) caliper, 4) weighted
    ## To be productive: Program will not compute covariates test
    
    flog.info("Algorithm calculation %s", yr)
    # Algorithm 1: nearest neighbor (with replacement). All units are matched, both treated and controls
    if (algor == "NN") {
      
      ## Comparison group
      w.crosswalk <-data.table(NULL)
      for (i in 1:nrow(year.covariates.trim)) {
        
        # Extract observation info
        obs.i <- year.covariates.trim[i, ]
        # Add Info of pair number
        obs.i <- obs.i[, n_pair := i][, w := 1]
        # Find potential pairs and order by distance to selected observation
        potential.pairs <- year.covariates.trim[get(treatment) != obs.i[, get(treatment)], 
                                                ][, distance := abs(pscore - obs.i[, pscore])][distance < 0.1][order(distance)]
        # Extract closest pair
        pair.i<- potential.pairs[1, ][, -c("distance")]
        pair.i <- pair.i[, n_pair := i][, w := 1]
        # paste to previous selected pairs
        w.crosswalk <- rbind(w.crosswalk, obs.i, pair.i)
      } 
      
      ## Prepare Estimation
      crosswalk <- merge(year.data, w.crosswalk, by = c("fips_state", "fips_county", "year"), allow.cartesian=TRUE)
      crosswalk <- data.table(crosswalk)
      # Create Interaction term
      crosswalk <- crosswalk[, interaction := (get(treatment))*taxable]
      # Make sure there are no 0 weights
      crosswalk <- crosswalk[!is.na(c(get(weights)))]
      
    }

    
    # Algorithm 2: k-nearest neighbor (with replacement). k=3. All units are matched, both treated and controls
    if (algor == "KNN") {
      w.crosswalk <-data.table(NULL)
      for (i in 1:nrow(year.covariates.trim)) {
        
        # Extract observation info
        obs.i <- year.covariates.trim[i, ]
        # Add Info of pair number
        obs.i <- obs.i[, n_pair := i][, w := 1]
        # Find potential pairs and order by distance to selected observation
        potential.pairs <- year.covariates.trim[get(treatment) != obs.i[, get(treatment)],
                                                ][, distance := abs(pscore - obs.i[, pscore])][distance < 0.1][order(distance)]
        # Extract closest pair
        pair.i<- potential.pairs[1:3, ][, -c("distance")]
        pair.i <- pair.i[, n_pair := i][, w := 1/3]
        # paste to previous selected pairs
        w.crosswalk <- rbind(w.crosswalk, obs.i, pair.i)
      }
      crosswalk <- merge(year.data, w.crosswalk, by = c("fips_state", "fips_county", "year"), allow.cartesian=TRUE)
      # Create Interaction term
      crosswalk <- crosswalk[, interaction := get(treatment)*taxable]
      # Create new weights
      crosswalk <- crosswalk[, (weights) := (get(weights))*w]
      # Make sure there are no 0 weights
      crosswalk <- crosswalk[!is.na(c(get(weights)))]
    } 
    
    # Algorithm 3: neighbors in caliper (with replacement). r=0.001. All units are matched, both treated and controls. 
    # Note: If no pairfound, drop
    if (algor == "calip") {
      w.crosswalk <-data.table(NULL)
      r <- 0.001 # Define caliper ratio
      for (i in 1:nrow(year.covariates.trim)) {
        
        # Extract observation info
        obs.i <- year.covariates.trim[i, ]
        # Add Info of pair number
        obs.i <- obs.i[, n_pair := i][, w := 1]
        # Find potential pairs and choose by distance to selected observation
        pair.i <- year.covariates.trim[get(treatment) != obs.i[, get(treatment)],
                                                ][, distance := abs(pscore - obs.i[, pscore])][distance < r][, -c("distance")]
        # Perform if pair found
        if (nrow(pair.i) > 0) {
          pair.i <- pair.i[, n_pair := i][, w := 1/.N]
          # paste to previous selected pairs
          w.crosswalk <- rbind(w.crosswalk, obs.i, pair.i)
        }
      }
      crosswalk <- merge(year.data, w.crosswalk, by = c("fips_state", "fips_county", "year"), allow.cartesian=TRUE)
      # Create Interaction term
      crosswalk <- crosswalk[, interaction := get(treatment)*taxable]
      # Create new weights
      crosswalk <- crosswalk[, (weights) := (get(weights))*w]
      # Make sure there are no 0 weights
      crosswalk <- crosswalk[!is.na(c(get(weights)))]
    }
    # Algorithm 4: weighting estimator. Build weights
    if (algor == "weighted") { 
      w.crosswalk <- year.covariates.trim[, w := ifelse(get(treatment) == T, 
                                                             sum(get(treatment))*sum(get(treatment)/pscore)/pscore,
                                                             sum(1-get(treatment))*sum((1-get(treatment))/(1-pscore))/(1-pscore)
      )]
      crosswalk <- merge(year.data, w.crosswalk, by = c("fips_state", "fips_county", "year"))
      # Create Interaction term
      crosswalk <- crosswalk[, interaction := get(treatment)*taxable]
      # Create new weights
      crosswalk <- crosswalk[, (weights) := (get(weights))*w]
      # Make sure there are no 0 weights
      crosswalk <- crosswalk[!is.na(c(get(weights)))]
    }
    
    # Algorithm 5: No matching
    if (algor == "NoMatch") {
      
      # Merge data
      crosswalk <- merge(year.data, year.covariates, by = c("fips_state", "fips_county", "year"))
      crosswalk <- data.table(crosswalk)
      # Create Interaction term
      crosswalk <- crosswalk[, interaction := get(treatment)*taxable]
      # Make sure there are no 0 weights
      crosswalk <- crosswalk[!is.na(c(get(weights)))]
    }
    
    if (covar.test) {
      Xall <- c(must.covar, oth.covars, Xfinal)
      ##### Check balance using basic regression tests on all covariates (first all, then chosen: this allow to identify chosen) -------
      test.year <- data.table(NULL)
      ## Merge covariates to original data to calculate prior
      
      for (X in Xall) {
        
        # Rowname
        outcome <-data.table(X)
        setnames(outcome, old = c("X"), new = c("outcome"))
        # Prior balance
        test.out <- lm(get(X) ~ high.tax.rate, data = year.covariates)
        priortest.dt <- data.table(coef(summary(test.out)))[2,][, -c("t value")]
        setnames(priortest.dt, old = c("Estimate", "Std. Error", "Pr(>|t|)"),
                 new = c("prior.est", "prior.std.err", "prior.pval"))
        # Adjusted balance
        test.out <- lm(get(X) ~ high.tax.rate, data = w.crosswalk, weights = w)
        af.test.dt <- data.table(coef(summary(test.out)))[2,][, -c("t value")]
        setnames(af.test.dt, old = c("Estimate", "Std. Error", "Pr(>|t|)"),
                 new = c("new.est", "new.std.err", "new.pval"))
       
        # Merge all tests
        test.dt <- cbind(outcome, priortest.dt, af.test.dt)
        
        # Append to other outcomes
        test.year <- rbind(test.year, test.dt, fill = T)
      }
      # Append to previous year
      test.year$year <- yr
      test.total <- rbind(test.total, test.year, fill = T)
      
    } else {
      
      #### Estimate cross-sectional design  -------
      flog.info("Estimation %s", yr)
      for(Y in outcomes) {
        
        formula0 <- as.formula(paste0(
          Y, " ~ ",  treatment, " + taxable + interaction | product_module_code "
        ))
        ### Base weights
        res0 <- felm(data = crosswalk,
                     formula = formula0,
                     weights = crosswalk[, get(weights)])
        
        ## attach results
        res1.dt <- data.table(coef(summary(res0)), keep.rownames=T)
        res1.dt[, outcome := Y]
        res1.dt[, year := yr]
        LRdiff_res <- rbind(LRdiff_res, res1.dt)
        
      }
      
      
    }
    
  }
  ## After all estimations, create the return output
  if (!covar.test) {
    
    flog.info("Computing interest coefficients")
    # Identify interest estimates
    c1 <- LRdiff_res[rn == "taxableTRUE", ][, -c("Std. Error", "t value", "Pr(>|t|)")]
    c2 <- LRdiff_res[rn == "taxableTRUE" | rn == "interaction",][, list(Estimate = sum(Estimate)), 
                                                                           by = .(outcome, year) ][, rn := "taxableTRUE + high.tax.rate_taxable"]
    c3 <- LRdiff_res[rn == "taxableTRUE" | rn == "interaction",][, w := ifelse(rn == "taxableTRUE", 2/3 , 1/3)][
                                                                            , list(Estimate = weighted.mean(Estimate, w = w)), 
                                                                           by = .(outcome, year) ][, rn := "(2*taxableTRUE + high.tax.rate_taxable)/2"]
    
    c4 <- LRdiff_res[rn == paste0(treatment, "TRUE") | rn == "interaction",][, list(Estimate = sum(Estimate)), 
                                                                             by = .(outcome, year) ][, rn := "high.tax.rateTRUE + high.tax.rate_taxable"]
    c5 <- LRdiff_res[rn == paste0(treatment, "TRUE"), ][, -c("Std. Error", "t value", "Pr(>|t|)")]
    c6 <- LRdiff_res[rn == "interaction", ][, -c("Std. Error", "t value", "Pr(>|t|)")]
    ### Paste and compute estimates across years
    PS_res <- rbind(c1, c2, c3, c4, c5, c6)
    
    c7 <- PS_res[, list(Estimate = mean(Estimate)), 
                 by = .(rn, outcome) ]
  
    # Append
    PS_res <- rbind(PS_res, c7, fill = T)
    # Keep interest order
    PS_res <- PS_res[order(year, outcome),]
    if (boot.run) {export <- PS_res[order(year, outcome),][["Estimate"]]} else {export <- PS_res[order(year, outcome),]}
    
    ## Compute other Interest estimates: implied 
    if (implicit) {
      implied.coefs <- data.table(NULL)
      for (yr in unique(PS_res[, c('year')])[["year"]]) {
        
        for (Y in main.outcomes) {
         
          for (tax in tau) {
            
            for (coef in unique(PS_res[, c('rn')])[["rn"]]) {
              
              numerator <- PS_res[ outcome == Y & year == yr & rn == coef][["Estimate"]]
              denominator <- PS_res[ outcome == tax & year == yr & rn == coef][["Estimate"]]
    
              Estimate <- numerator/denominator
              
              iter.data <- data.table(Estimate)
              iter.data[, year := yr][, rn := coef][, outcome := paste0(Y, "_", tax)]
              
              implied.coefs <- rbind(implied.coefs, iter.data)
            }
          }
        }
      }
      # compute average
      av <- implied.coefs[, list(Estimate = mean(Estimate)), by = .(rn, outcome)]
      implied.coefs <- rbind(implied.coefs, av, fill = T)
      if (boot.run) {export <- implied.coefs[order(year, outcome),][["Estimate"]]} else {export <- implied.coefs[order(year, outcome),]}
    }
  
    # Return a vector of estimates
    return(export)
  
  } else {
  # Return covar.test
    return(test.total)
  }
  
}

## Try function and export
t <- psmatch.taxrate(actual.data = yearly_data,
                     covariate.data = covariates,
                     algor = "weighted",
                     weights = "base.sales",
                     must.covar = Xb,
                     oth.covars = Xa_pot,
                     treatment = "high.tax.rate",
                     main.outcomes = r.outcomes,
                     tau = tax.rates,
                     covar.test = T)
fwrite(t, "../../home/slacouture/PS/trynew_covartest.csv")

t <- psmatch.taxrate(actual.data = yearly_data,
                covariate.data = covariates,
                algor = "weighted",
                weights = "base.sales",
                must.covar = Xb,
                oth.covars = Xa_pot,
                treatment = "high.tax.rate",
                main.outcomes = r.outcomes,
                tau = tax.rates,
                boot.run = F)
fwrite(t, "../../home/slacouture/PS/trynew.csv")

t <- psmatch.taxrate(actual.data = yearly_data,
                     covariate.data = covariates,
                     algor = "weighted",
                     weights = "base.sales",
                     must.covar = Xb,
                     oth.covars = Xa_pot,
                     treatment = "high.tax.rate",
                     main.outcomes = r.outcomes,
                     tau = tax.rates,
                     boot.run = F,
                     implicit = F)
fwrite(t, "../../home/slacouture/PS/trynew_direct.csv")


## Try function and export using political covars
t <- psmatch.taxrate(actual.data = yearly_data,
                     covariate.data = covariates,
                     algor = "weighted",
                     weights = "base.sales",
                     must.covar = Xb,
                     oth.covars = Xa_pot2,
                     treatment = "high.tax.rate",
                     main.outcomes = r.outcomes,
                     tau = tax.rates,
                     covar.test = T)
fwrite(t, "../../home/slacouture/PS/trynew_pol_covartest.csv")

t <- psmatch.taxrate(actual.data = yearly_data,
                     covariate.data = covariates,
                     algor = "weighted",
                     weights = "base.sales",
                     must.covar = Xb,
                     oth.covars = Xa_pot2,
                     treatment = "high.tax.rate",
                     main.outcomes = r.outcomes,
                     tau = tax.rates,
                     boot.run = F)
fwrite(t, "../../home/slacouture/PS/trynew_pol.csv")

t <- psmatch.taxrate(actual.data = yearly_data,
                     covariate.data = covariates,
                     algor = "weighted",
                     weights = "base.sales",
                     must.covar = Xb,
                     oth.covars = Xa_pot2,
                     treatment = "high.tax.rate",
                     main.outcomes = r.outcomes,
                     tau = tax.rates,
                     boot.run = F,
                     implicit = F)
fwrite(t, "../../home/slacouture/PS/trynew_pol_direct.csv")

############# Run bootstrap: Calip using base.sales -----------------
# 
# block.boot <- function(x, i) {
#   bootdata <- merge(data.table(state_by_module=x[i]), yearly_data, by = "state_by_module", allow.cartesian = T)
#   rep_count <<- rep_count + 1
#   flog.info("Iteration %s", rep_count)
#   psmatch.taxrate(actual.data = bootdata,
#                   covariate.data = covariates,
#                   algor = "calip",
#                   weights = "base.sales",
#                   must.covar = Xb,
#                   oth.covars = Xa_pot,
#                   treatment = "high.tax.rate",
#                   Y = r.outcomes,
#                   tau = tax.rates)
# }
# 
# ### Run essay bootstrap
# 
# # Define level of block bootstrap
# state_by_module_ids <- unique(yearly_data$state_by_module)
# # Improve
# # Run bootstrap
# rep_count = 0
# b0 <- boot(state_by_module_ids, block.boot, 150)
# 
# # Export: observed and distribution
# t <- data.table(b0$t0)
# mat.t <- data.table(b0$t)
# fwrite(t, "../../home/slacouture/PS/C_base_t_150.csv")
# fwrite(mat.t, "../../home/slacouture/PS/C_base_mat.t_150.csv")


# ############## Run bootstrap: Weighted using base.sales -----------------

block.boot <- function(x, i) {
  bootdata <- merge(data.table(state_by_module=x[i]), yearly_data, by = "state_by_module", allow.cartesian = T)
  rep_count <<- rep_count + 1
  flog.info("Iteration %s", rep_count)
  psmatch.taxrate(actual.data = bootdata,
                  covariate.data = covariates,
                  algor = "weighted",
                  weights = "base.sales",
                  must.covar = Xb,
                  oth.covars = Xa_pot,
                  treatment = "high.tax.rate",
                  main.outcomes = r.outcomes,
                  tau = tax.rates)
}

### Run essay bootstrap

# Define level of block bootstrap
state_by_module_ids <- unique(yearly_data$state_by_module)
# Improve
# Run bootstrap
rep_count = 0
b0 <- boot(state_by_module_ids, block.boot, 100)

# Export: observed and distribution
t <- data.table(b0$t0)
mat.t <- data.table(b0$t)
fwrite(t, "../../home/slacouture/PS/W_base_tnew_100.csv")
fwrite(mat.t, "../../home/slacouture/PS/W_base_mat.tnew_100.csv")
