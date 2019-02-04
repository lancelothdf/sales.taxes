#' Author: John Bonney
#'
#' This file creates event-study plots based on the tax-inclusive price indices.
#'
#' TODO:
#'  * Add pseudo-control group
#'  * Do same thing using Brad and David's package?

# devtools::install_github("setzler/eventStudy/eventStudy")

# library(eventStudy)
library(data.table)
library(sales.taxes)
library(ggplot2)

setwd("/project2/igaarder")
pi_data_path <- "Data/Nielsen/price_quantity_indices_food.csv"
tr_data_path <- "Data/event_study_tr_groups_comprehensive.csv"

## load in price index data (variables: "store_code_uc", "quarter", "year",
##                              "product_group_code", "product_module_code",
##                              "pricei", "quantityi", "cpricei", "fips_state",
##                              "fips_county")

pi_data <- fread(pi_data_path)
pi_data <- pi_data[year %in% 2008:2014] # years for which we have tax rates

## merge on sales weights ------------------------------------------------------
sales_weights <- fread("Data/sales_quarterly_2006-2016.csv")
sales_weights <- sales_weights[year == 2008 & quarter == 1]
sales_weights <- sales_weights[, .(store_code_uc, product_module_code, sales)]

pi_data <- merge(pi_data, sales_weights,
                 by = c("store_code_uc", "product_module_code"))
rm(sales_weights)
gc()

## balance on store-level ------------------------------------------------------
pi_data <- balance_panel_data(pi_data, time_vars = c("quarter", "year"),
                              panel_unit = "store_code_uc", n_periods = 28)

## attach event times ----------------------------------------------------------
pi_data <- merge_treatment(original_data = pi_data,
                           treatment_data_path = tr_data_path,
                           merge_by = c("fips_county", "fips_state"))

## define time to event --------------------------------------------------------
pi_data[, ref_quarter := ceiling(ref_month / 4)]
pi_data[, tt_event := as.integer(4 * year + quarter -
                                   (4 * ref_year + ref_quarter))]

## normalize price indices based on time to event ------------------------------
pi_data <- normalize_price(price_data = pi_data,
                           time_type = "event",
                           base_time = -2,
                           price_var = "cpricei",
                           new_price_var = "normalized.cpricei")

# *** NOTE: currently, we do not residualize at all. ***

## limit data to two year window around reform ---------------------------------
pi_data <- pi_data[tt_event >= -1 * 4 & tt_event <= 4]
pi_data <- pi_data[!is.na(cpricei)]

## aggregate by treatment group ------------------------------------------------
pi_collapsed <- pi_data[,
  list(mean_pi = weighted.mean(x = log(normalized.cpricei), w = sales),
       n_counties = uniqueN(1000 * fips_state + fips_county),
       n_stores = uniqueN(store_code_uc)),
   by = c("tr_group", "tt_event")
  ]

pi_collapsed <- add_tr_count(collapsed_data = pi_collapsed,
                             tr_group_name = "tr_group",
                             count_col_name = "n_counties")

fwrite(pi_collapsed, "Data/pi_sw_es.csv")

## plot and export result ------------------------------------------------------
pi_plot <- ggplot(data = pi_collapsed,
                  mapping = aes(x = tt_event, y = mean_pi, color = tr_count)) +
  labs(x = "tt_event",
       y = "Price index",
       color = "Sales tax change",
       note = expression(paste("Weighted by sales in 2008 Q1. ", Y==0, " in ", T==-2))) +
  geom_line() +
  theme_bw()

ggsave("Graphs/pi_sw_es.png")
