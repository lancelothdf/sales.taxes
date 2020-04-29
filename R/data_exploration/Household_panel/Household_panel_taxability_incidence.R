## Sales Taxes
# Should food be taxed? Use Consumer data to get information about the distributional effect of tax rates
# Open already cleaned household data to present calculate incidence parameters of re-distributional effects under sifferent assumptions

library(data.table)
library(futile.logger)


setwd("/project2/igaarder/Data")

# useful paths
data.semester <- "Nielsen/semester_nielsen_data.csv"
hh.panel.clean <- "Nielsen/Household_panel/cleaning/consumer_panel_income_module_state_year_2006-2016.csv"
path.data.figures <- "/home/slacouture/NLP/HH_Food/"
income_w <- "welfare_weights_by_income_bins.csv"

## Open Data
hh_pi <- fread(hh.panel.clean)
ret_pi <- fread(data.semester)

## Calculate weigths of modules from retailer data (use year 2014) by state (so sum across stores and semester)
ret_pi <- ret_pi[year == 2014, .(sales_m_retailer = sum(sales)), by = .(product_module_code, fips_state)]

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


## Now for household panel lets collapse across years: use projection factors here on the welfare weights
hh_pi <- hh_pi[, .(total_expenditures = sum(total_expenditures),
                   hh_expenditures = sum(hh_expenditures),
                   projection_factor = sum(projection_factor),
                   eta_02 = weighted.mean(eta_02, w = projection_factor),
                   eta_05 = weighted.mean(eta_05, w = projection_factor),
                   eta_10 = weighted.mean(eta_10, w = projection_factor),
                   eta_20 = weighted.mean(eta_20, w = projection_factor)),
               by = .(fips_state, household_income, panel_year, product_module_code, taxability)]
# Fix taxability to drop unidentified taxability
hh_pi <- hh_pi[!is.na(taxability)]


## Compute distribution per module x state under different etas 
module.data <- hh_pi[, .(incidence_m_02 = (eta_02)*projection_factor*total_expenditures/sum(total_expenditures*projection_factor),
                         incidence_m_05 = (eta_05)*projection_factor*total_expenditures/sum(total_expenditures*projection_factor),
                         incidence_m_10 = (eta_10)*projection_factor*total_expenditures/sum(total_expenditures*projection_factor),
                         incidence_m_20 = (eta_20)*projection_factor*total_expenditures/sum(total_expenditures*projection_factor),
                         sales_m_consumer = sum(total_expenditures*projection_factor)), 
                  by = .(fips_state, product_module_code, taxability)]

# merge module sales weights from retailer
module.data <- merge(module.data, ret_pi, by = c("product_module_code", "fips_state"), all.x = T, alow.cartesian = T )

# Export
fwrite(module.data, paste0(path.data.figures, "calculate_incidence_module.csv"))







