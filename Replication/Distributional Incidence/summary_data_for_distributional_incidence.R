##### Wesley Janson and Santiago Lacouture
#' Sales Taxes
#' Replication File. Updated on 11/02/2022
#' Cleaning portion of replication: producing summary data stes binning data.
#' 


library(data.table)


setwd("/project2/igaarder")
rm(list = ls())


## Open data filepaths ----------------------------------------------
all_pi <- fread("Data/Replication/all_pi.csv")
taxability <- fread("Data/taxability_state_panel.csv")


# collapse taxability to the semester
taxability[, taxability := ifelse(taxability == 2, 3, taxability)]
taxability[, semester := ceiling(month/6)]
taxability <- taxability[, .(taxability = mean(taxability),
                             reduced_rate = mean(reduced_rate, na.rm = T),
                             Food = mean(FoodNonfood)), 
                         by = .(product_module_code, semester, year, fips_state)]
taxability[, taxability := ifelse(!is.nan(reduced_rate), 2, taxability)]

# Drop items whose taxability status within the period (gets rid of 39 observations - their taxability happens to be between 0 and 1)
taxability <- taxability[taxability == 0 | taxability == 1 | taxability == 2 | taxability == 3,]


## Merge all_pi to taxability and Food status
all_pi <- merge(all_pi, taxability, by = c("year", "semester", "fips_state", "product_module_code"), all.x = T)

### Final binned data sets for welfare extrapolations ---------

## Collapse to binned price for Welfare extrapolation (needed to make it computationally feasible)
# Generate rounded price
all_pi_p <- copy(all_pi)
all_pi_p <- all_pi_p[, p_m := round(dm.ln_cpricei2, 3)]

# collapse for every price x state on taxable goods 
all_pi_p<- all_pi_p[ln_sales_tax > 0, .(tau = weighted.mean(ln_sales_tax, w = base.sales),
                                        eta_m = sum(base.sales)), by = .(fips_state, p_m, taxability, Food)]
## Collapse to binned tax 
# Generate rounded tax
all_pi_t <- copy(all_pi)
all_pi_t[, tau := round(ln_sales_tax, 3)]

# collapse for every price x state on taxable goods 
all_pi_t<- all_pi_t[ln_sales_tax > 0, .(p_m = weighted.mean(dm.ln_cpricei2, w = base.sales),
                                        eta_m = sum(base.sales)), by = .(fips_state, tau, taxability, Food)]


fwrite(all_pi_p, "Data/Replication/extraction_state_binned_price_distributional.csv", showProgress = T)
fwrite(all_pi_t, "Data/Replication/extraction_state_binned_tax_distributional.csv", showProgress = T)

