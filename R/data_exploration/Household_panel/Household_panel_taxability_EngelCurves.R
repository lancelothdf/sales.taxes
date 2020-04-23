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
bins <- 100
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
binscatter.1 <- all_pi.nocons[, lapply(.SD, weighted.mean, w = projection_factor), 
                       by = .(ln_hh_expenditures_bin), .SDcols = c("ln_expenditures_taxable", "ln_expenditures_exred")]
binscatter.2 <- all_pi[, lapply(.SD, weighted.mean, w = projection_factor), 
                         by = .(ln_hh_expenditures_bin), .SDcols = c("ln_expenditures_food", "ln_expenditures_nonfood")]
# Merge
binscatter <- merge(binscatter.1, binscatter.2, by = "ln_hh_expenditures_bin")

# Export
fwrite(binscatter, paste0(path.data.figures, "Binscatters_100.csv"))

rm(binscatter, binscatter.1, binscatter.2)


### Graphs 3 to 6: income bins of logs as 1 and 2 and of shares

# Collapse
income.bins.1 <- all_pi.nocons[, lapply(.SD, weighted.mean, w = projection_factor), 
                               by = .(household_income), 
                               .SDcols = c("ln_expenditures_taxable", "ln_expenditures_exred", "sh_expenditures_taxable", "sh_expenditures_exred")]
income.bins.2 <- all_pi[, lapply(.SD, weighted.mean, w = projection_factor), 
                       by = .(ln_hh_expenditures_bin), 
                       .SDcols = c("ln_expenditures_food", "ln_expenditures_nonfood", "sh_expenditures_food", "sh_expenditures_nonfood")]

# Merge
income.bins <- merge(income.bins.1, income.bins.2, by = "ln_hh_expenditures_bin")

# Export
fwrite(income.bins, paste0(path.data.figures, "Bins_income.csv"))
rm(income.bins, income.bins.1, income.bins.2)







