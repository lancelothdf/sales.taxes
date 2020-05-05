## Sales Taxes
# Should food be taxed? Use Consumer data to get information about the distributional effect of tax rates
# Open already cleaned household data and estimate progressivity of sales taxes in each state

library(data.table)
library(futile.logger)


setwd("/project2/igaarder/Data")

# useful paths
hh.panel.clean <- "Nielsen/Household_panel/cleaning/consumer_panel_bytaxability_year_2006-2016.csv"
path.data.figures <- "/home/slacouture/NLP/HH_Food/"
income_w <- "welfare_weights_by_income_bins.csv"

## Open Data
hh_pi <- fread(hh.panel.clean)

# Fix bins for years
weights <- fread(income_w)
al.weights <- data.table(NULL)
for (year in 2006:2014) {
  
  if (year < 2010) {
    yr.data <- weights[ Bin != "$100,000 +"]
    
  } else {
    yr.data <- weights[ household_income <= 27 & Bin != "$100,000 - $124,999"]
  }
  yr.data[, panel_year := year]
  al.weights <- rbind(al.weights, yr.data)
  
}
rm(yr.data, weights)

# merge welfare weigths calculated separate by Lance 
head(hh_pi)
hh_pi <- merge(hh_pi, al.weights, by = c("household_income", "panel_year"))


## Now for household panel lets collapse across households and years at state level 
hh_pi <- hh_pi[, .(expenditures_exempt = sum(expenditures_exempt),
                   expenditures_taxable = sum(expenditures_taxable),
                   expenditures_reduced = sum(expenditures_reduced),
                   expenditures_nonfood	= sum(expenditures_nonfood),
                   expenditures_food = sum(expenditures_food),
                   hh_expenditures = sum(hh_expenditures),
                   projection_factor = sum(projection_factor)),
               by = .(fips_state, household_income, mean)]

## Now calculate the share of total expenditure in each type 

# Calculate logs and shares
vars <- c("expenditures_exempt", "expenditures_taxable", "expenditures_reduced", 
          "expenditures_food", "expenditures_nonfood")
for (var in vars) {
  
  hh_pi[, paste0("sh_", var) := get(var)/hh_expenditures ]
    
}

## Run regressions spending share by g on average income in each bin
regs <- data.table(NULL)
for (state in unique(hh_pi$fips_state)) {
  
  flog.info("Running state %s", state)
  
  ## Identify data
  data <- hh_pi[fips_state == state]
  for (var in vars) {
    
    # Create weights on regression
    data[, reg_weight := get(var)*projection_factor]
    
    # Produce formula
    formula1 <- as.formula(paste0(
      "sh_", var, " ~ mean "
    ))
    
    # Run Linear Regression
    res1 <- lm(formula1, data, weights = data$reg_weight )
    
    # Save results
    res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
    res1.dt[, outcome := Y]
    res1.dt[, fips_state := state]
    
    # Attach
    regs <- rbind(regs, res1.dt)
  }
}

## Export file 
fwrite(regs, paste0(path.data.figures, "slope_coefs_state.csv"))



