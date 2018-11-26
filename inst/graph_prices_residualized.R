#' Maintained by: John Bonney
#'
#' Graphs:
#' Plot residualized log of normalized prices by calendar time
#'

rm(list=ls())
wd <- "/project2/igaarder"
setwd(wd)

library(sales.taxes)
library(readstata13)
library(data.table)
library(zoo)
library(ggplot2)

product_by_county_prices <- fread("Data/Nielsen/product_by_county_prices.csv")

# product_by_county_prices <- fread("Data/Nielsen/product_by_county_prices.csv")
county_pop <- fread("Data/county_population.csv")
product_by_county_prices <- merge(product_by_county_prices,
                                  county_pop,
                                  by = c("fips_state", "fips_county"))
################################
### basic linear time trend ###
###############################

### COMPREHENSIVE DEFINITION ###
price_application(product_by_county_prices,
                  treatment_data_path = "Data/tr_groups_comprehensive.csv",
                  time = "calendar",
                  weighting_var = "cty_base_sales",
                  pretax_var = "mld_price.wtd",
                  posttax_var = "mld_price_w_tax.wtd",
                  resid_type = "A",
                  w_tax = T,
                  fig_outfile = "Graphs/log_price_trends_compr_posttax_A.png")

### RESTRICTIVE DEFINITION ###
price_application(product_by_county_prices,
                  treatment_data_path = "Data/tr_groups_restrictive.csv",
                  time = "calendar",
                  weighting_var = "cty_base_sales",
                  pretax_var = "mld_price.wtd",
                  posttax_var = "mld_price_w_tax.wtd",
                  resid_type = "A",
                  w_tax = T,
                  fig_outfile = "Graphs/log_price_trends_restr_posttax_A.png")

#####################################
### group-level linear time trend ###
####################################

### COMPREHENSIVE DEFINITION ###
price_application(product_by_county_prices,
                  treatment_data_path = "Data/tr_groups_comprehensive.csv",
                  time = "calendar",
                  weighting_var = "cty_base_sales",
                  pretax_var = "mld_price.wtd",
                  posttax_var = "mld_price_w_tax.wtd",
                  resid_type = "B",
                  w_tax = T,
                  fig_outfile = "Graphs/log_price_trends_compr_posttax_B.png")

### RESTRICTIVE DEFINITION ###
price_application(product_by_county_prices,
                  treatment_data_path = "Data/tr_groups_restrictive.csv",
                  time = "calendar",
                  weighting_var = "cty_base_sales",
                  pretax_var = "mld_price.wtd",
                  posttax_var = "mld_price_w_tax.wtd",
                  resid_type = "B",
                  w_tax = T,
                  fig_outfile = "Graphs/log_price_trends_restr_posttax_B.png")

###########################################
### linear time trend and month effects ###
###########################################

### COMPREHENSIVE DEFINITION ###
price_application(product_by_county_prices,
                  treatment_data_path = "Data/tr_groups_comprehensive.csv",
                  time = "calendar",
                  weighting_var = "cty_base_sales",
                  pretax_var = "mld_price.wtd",
                  posttax_var = "mld_price_w_tax.wtd",
                  resid_type = "C",
                  w_tax = T,
                  fig_outfile = "Graphs/log_price_trends_compr_posttax_C.png")

### RESTRICTIVE DEFINITION ###
price_application(product_by_county_prices,
                  treatment_data_path = "Data/tr_groups_restrictive.csv",
                  time = "calendar",
                  weighting_var = "cty_base_sales",
                  pretax_var = "mld_price.wtd",
                  posttax_var = "mld_price_w_tax.wtd",
                  resid_type = "C",
                  w_tax = T,
                  fig_outfile = "Graphs/log_price_trends_restr_posttax_C.png")

##################################################################
### linear time trend and month effects on product-group level ###
##################################################################

### COMPREHENSIVE DEFINITION ###
price_application(product_by_county_prices,
                  treatment_data_path = "Data/tr_groups_comprehensive.csv",
                  time = "calendar",
                  weighting_var = "cty_base_sales",
                  pretax_var = "mld_price.wtd",
                  posttax_var = "mld_price_w_tax.wtd",
                  resid_type = "D",
                  w_tax = T,
                  fig_outfile = "Graphs/log_price_trends_compr_posttax_D.png")

### RESTRICTIVE DEFINITION ###
price_application(product_by_county_prices,
                  treatment_data_path = "Data/tr_groups_restrictive.csv",
                  time = "calendar",
                  weighting_var = "cty_base_sales",
                  pretax_var = "mld_price.wtd",
                  posttax_var = "mld_price_w_tax.wtd",
                  resid_type = "D",
                  w_tax = T,
                  fig_outfile = "Graphs/log_price_trends_restr_posttax_D.png")


#############################
### calendar time effects ###
#############################

### COMPREHENSIVE DEFINITION ###
price_application(product_by_county_prices,
                  treatment_data_path = "Data/tr_groups_comprehensive.csv",
                  time = "calendar",
                  weighting_var = "cty_base_sales",
                  pretax_var = "mld_price.wtd",
                  posttax_var = "mld_price_w_tax.wtd",
                  resid_type = "E",
                  w_tax = T,
                  fig_outfile = "Graphs/log_price_trends_compr_posttax_E.png")

### RESTRICTIVE DEFINITION ###
price_application(product_by_county_prices,
                  treatment_data_path = "Data/tr_groups_restrictive.csv",
                  time = "calendar",
                  weighting_var = "cty_base_sales",
                  pretax_var = "mld_price.wtd",
                  posttax_var = "mld_price_w_tax.wtd",
                  resid_type = "E",
                  w_tax = T,
                  fig_outfile = "Graphs/log_price_trends_restr_posttax_E.png")
