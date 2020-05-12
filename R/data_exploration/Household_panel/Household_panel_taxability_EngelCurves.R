## Sales Taxes
# Should food be taxed? Use Consumer data to get information about the distributional effect of tax rates
# Open already cleaned household data to present some descriptive plots on how purchases change by income/expenditure (Engel curves)
# Use all panel and use taxability as of 2014-12.

library(data.table)
library(futile.logger)


setwd("/project2/igaarder/Data")

# useful paths
hh.panel.clean <- "Nielsen/Household_panel/cleaning/consumer_panel_bytaxability_year_2006-2016.csv"
path.data.figures <- "/home/slacouture/NLP/HH_Food/"
income_w <- "welfare_weights_by_income_bins.csv"

## Open Data
all_pi <- fread(hh.panel.clean)

# Create joint tax-exempt and reduced and create unknown to check
all_pi[, expenditures_exred := expenditures_exempt + expenditures_reduced]
all_pi[, expenditures_untax := hh_expenditures - expenditures_exred - expenditures_taxable]

# Calculate logs and shares
vars <- c("expenditures_exempt", "expenditures_taxable", "expenditures_reduced", "expenditures_exred", 
          "expenditures_food", "expenditures_nonfood", "hh_expenditures")
for (var in vars) {
  
  # Create log
  all_pi[, paste0("ln_", var) := log(1 + get(var))] ## Add 1 so we don't lose obs
  
  # Create share (if not total)
  if (var != "hh_expenditures") {
    
    all_pi[, paste0("sh_", var) := get(var)/hh_expenditures ]
    
  }
}

## Compute log consumption bins. I'll do 100 bins
bins <- 80
max.exp <- max(all_pi$ln_hh_expenditures)
min.exp <- min(all_pi$ln_hh_expenditures)
all_pi[, bin := floor(bins*(ln_hh_expenditures - min.exp)/(max.exp-min.exp))]
all_pi[, ln_hh_expenditures_bin := 1.5*bin*(max.exp-min.exp)/bins + min.exp]

## Fix Income bins
all_pi[, household_income := ifelse(household_income > 27, 27, household_income)]

# Identify households were everything is tax_exempt or taxable
all_pi[, s1 := sd(expenditures_reduced), by = .(fips_state)]
all_pi[, s2 := sd(expenditures_taxable), by = .(fips_state)]
all_pi[, cons_taxability := ifelse((s1 == 0 | s2 == 0), 1, 0)]

# Cretae separate data where Exclude states where everything is taxable or exempt
all_pi.nocons <- all_pi[cons_taxability == 0]

### Graphs 1 and 2: Binscatter (log exempt/reduced and log taxable | log food or log nonfood) on log total ----


# Collapse (use weights!)
binscatter.1 <- all_pi.nocons[, c(lapply(.SD, weighted.mean, w = projection_factor), .N, sum(projection_factor)),
                              by = .(ln_hh_expenditures_bin), .SDcols = c("ln_expenditures_taxable", "ln_expenditures_exred")]
binscatter.2 <- all_pi[, c(lapply(.SD, weighted.mean, w = projection_factor), .N, sum(projection_factor)),
                       by = .(ln_hh_expenditures_bin), .SDcols = c("ln_expenditures_food", "ln_expenditures_nonfood")]
# Merge
binscatter <- merge(binscatter.1, binscatter.2, by = "ln_hh_expenditures_bin")

# Export
fwrite(binscatter, paste0(path.data.figures, "Binscatters_80.csv"))

rm(binscatter, binscatter.1, binscatter.2)


### Graphs 3 to 6: income bins of logs as 1 and 2 and of shares

# Collapse
income.bins.1 <- all_pi.nocons[, c(lapply(.SD, weighted.mean, w = projection_factor), .N, sum(projection_factor)),
                               by = .(household_income), 
                               .SDcols = c("ln_expenditures_taxable", "ln_expenditures_exred", "sh_expenditures_taxable", "sh_expenditures_reduced", "sh_expenditures_exempt")]
income.bins.2 <- all_pi[, c(lapply(.SD, weighted.mean, w = projection_factor), .N, sum(projection_factor)),
                        by = .(household_income), 
                       .SDcols = c("ln_expenditures_food", "ln_expenditures_nonfood", "sh_expenditures_food", "sh_expenditures_nonfood")]

# Merge
income.bins <- merge(income.bins.1, income.bins.2, by = "household_income")

# Export
fwrite(income.bins, paste0(path.data.figures, "Bins_income.csv"))
rm(income.bins, income.bins.1, income.bins.2)


## New Graphs for May 11

## Re-Open Data
all_pi <- fread(hh.panel.clean)
al.weights <- fread(income_w)

## Fix Income bins: collapse all
all_pi[, household_income := ifelse(household_income > 27, 27, household_income)]
# Pool exempt and reduced
all_pi[, expenditures_exred := expenditures_exempt + expenditures_reduced]

# merge welfare weigths calculated separate by Lance 
all_pi <- merge(all_pi, al.weights, by = "household_income")

vars <- c("expenditures_exempt", "expenditures_taxable", "expenditures_reduced", "expenditures_exred",
          "expenditures_food", "expenditures_nonfood")


## Now for household panel lets collapse across years and households for graphs (separate data since the other will be used) 
# First, make cases = 0 by constuction as NA: those were consumption total is 0 at state
mean_pi <- all_pi
for (var in vars) {
  
  mean_pi[, paste0("sum_", var) := sum(get(var)), by = .(fips_state)]
  mean_pi[paste0("sum_", var) == 0, (var) := NA ]
  
}
head(mean_pi[is.na(expenditures_taxable)])

# Now collapse
mean_pi <- mean_pi[, c(lapply(.SD, weighted.mean, w = projection_factor, na.rm = T)),
                    by = .(mean, meanlog, meanperc), 
                    .SDcols = vars]
fwrite(mean_pi, paste0(path.data.figures, "av_expend_income.csv"))



reg <- c("mean", "meanlog", "meanperc")

## Run regression of the slope
regs <- data.table(NULL)
for (state in unique(all_pi$fips_state)) {
  
  flog.info("Running state %s", state)
  
  ## Identify data
  data <- all_pi[fips_state == state]
  for (var in vars) {
    
    for (x in reg) {
      
      # Produce formula
      formula1 <- as.formula(paste0(
        var, " ~", x
      ))
      
      # Run Linear Regression
      res1 <- lm(formula1, data, weights = data$projection_factor )
      
      # Save results
      res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
      res1.dt[, outcome := var]
      res1.dt[, income := x]
      res1.dt[, fips_state := state]
      
      # Attach
      regs <- rbind(regs, res1.dt, fill = T)
      # Export file 
      fwrite(regs, paste0(path.data.figures, "beta_expend_state.csv"))
      
    }
  }
}





