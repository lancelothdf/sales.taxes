#' let's start plotting the pre-trends and keep looking for the experimental
#' design (do the event study graphs - maybe using Brad and David's package -
#' and apply the Hansen-Shapiro paper).

# devtools::install_github("setzler/eventStudy/eventStudy")

library(eventStudy)
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

# *** NOTE: currently, we do not weight at all when aggregating. ***

## aggregate by treatment group ------------------------------------------------
pi_collapsed <- pi_data[, list(mean_pi = mean(normalized.cpricei),
                               n_counties = uniqueN(1000 * fips_state +
                                                      fips_county),
                               n_stores = uniqueN(store_code_uc)),
                               by = c("tr_group", "tt_event")]

pi_collapsed <- add_tr_count(collapsed_data = pi_collapsed,
                             tr_group_name = "tr_group",
                             count_col_name = "n_counties")

fwrite(pi_collapsed, "Data/pi_raw_es.csv")

## plot and export result ------------------------------------------------------
pi_plot <- ggplot(data = pi_collapsed,
                  mapping = aes(x = tt_event, y = mean_pi, color = tr_count)) +
  labs(x = "tt_event", y = "Price index", color = "Sales tax change") +
  geom_line() +
  theme_bw()

ggsave("Graphs/pi_raw_es.png")
