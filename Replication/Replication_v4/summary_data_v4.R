##### Wesley Janson and Santiago Lacouture
#' Sales Taxes
#' Replication File. Updated on 11/02/2022
#' Cleaning portion of replication: producing summary data stes binning data.
#' 


library(data.table)


setwd("/project/igaarder")
rm(list = ls())

## Open data filepaths ----------------------------------------------
all_pi <- fread("Data/Replication_v4/all_pi.csv")


### Final binned data sets for welfare extrapolations ---------


## Collapse to binned price for Welfare extrapolation (needed to make it computationally feasible)
# Generate rounded price
all_pi_p <- copy(all_pi)
all_pi_p <- all_pi_p[, p_m := round(dm.ln_cpricei2, 3)]

# collapse for every price x state on taxable goods 
all_pi_p<- all_pi_p[ln_sales_tax > 0, .(tau = weighted.mean(ln_sales_tax, w = base.sales),
                                        eta_m = sum(base.sales)), by = .(fips_state, p_m)]
## Collapse to binned tax 
# Generate rounded tax
all_pi_t <- copy(all_pi)
all_pi_t[, tau := round(ln_sales_tax, 3)]

# collapse for every price x state on taxable goods 
all_pi_t<- all_pi_t[ln_sales_tax > 0, .(p_m = weighted.mean(dm.ln_cpricei2, w = base.sales),
                                        eta_m = sum(base.sales)), by = .(fips_state, tau)]


fwrite(all_pi_p, "Data/Replication_v4/extraction_state_binned_price.csv", showProgress = T)
fwrite(all_pi_t, "Data/Replication_v4/extraction_state_binned_tax.csv", showProgress = T)

