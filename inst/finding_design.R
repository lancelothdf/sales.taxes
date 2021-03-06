#' Execute code for finding design (building up)

library(sales.taxes)
library(data.table)
library(bit64)

# We start with a simple case: one county, one product

maricopa_milk <- fread("C:/Users/John Bonney/Desktop/Magne_projects/sales_taxes/output/server/maricopa_milk.csv")
maricopa_tp <- fread("C:/Users/John Bonney/Desktop/Magne_projects/sales_taxes/output/server/maricopa_tp.csv")

county_monthly_tax <- fread("C:/Users/John Bonney/Dropbox/Sales tax/Data/county_monthly_tax_rates.csv")
county_monthly_tax <- county_monthly_tax[, .(fips_state, fips_county, year, month, sales_tax)]

exemptions_path <- "C:/Users/John Bonney/Dropbox/Sales tax/Data/modules_exemptions_long.csv"

# milk (not taxable)
mm_results <- one_cty_one_prod(product_data = maricopa_milk,
                               fips_state = 4,
                               fips_county = 13,
                               product_module_code = 3625,
                               product_name = "milk",
                               month_of_reform = 6,
                               year_of_reform = 2010,
                               county_monthly_tax_data = county_monthly_tax,
                               module_exemptions_path = exemptions_path)

mm_results$price_graph
mm_results$sales_graph
mm_results$quantity_graph

# toilet paper (taxable)
mt_results <- one_cty_one_prod(product_data = maricopa_tp,
                               fips_state = 4,
                               fips_county = 13,
                               product_module_code = 7260,
                               product_name = "toilet tissue",
                               month_of_reform = 6,
                               year_of_reform = 2010,
                               county_monthly_tax_data = county_monthly_tax,
                               rm_month_effects = T,
                               module_exemptions_path = exemptions_path)

mt_results$price_graph
mt_results$sales_graph
mt_results$quantity_graph
# What we are seeing is a lot of seasonality, with some interesting things
# happening around the event time.

# break it down to UPC level
#' Note:
#' 947582855818 is private label (most quantity sold)
#' 927582830919 is private label
#' 947582885950 is private label
#' 3700006443 is Charmin
#' 4200096310 is Quilted Northern
#' 4200096310 is Charmin
#' 3040077377 is Angel Soft
#' 5400010183 is Scott (most stores)

tp_det <- fread('C:/Users/John Bonney/Desktop/Magne_projects/sales_taxes/output/server/maricopa_tp_det.csv')
tp_det[, price := old_price]
mt_results_det <- one_cty_one_prod(product_data = tp_det,
                                   fips_state = 4,
                                   fips_county = 13,
                                   upc = 947582885950,  # c(5400010060, 5400042120) c(947582855818,927582830919,947582885950)
                                   product_name = "store brand toilet tissue",
                                   month_of_reform = 6,
                                   year_of_reform = 2010,
                                   county_monthly_tax_data = county_monthly_tax,
                                   rm_month_effects = F,
                                   module_exemptions_path = exemptions_path,
                                   balance_panel = T)
