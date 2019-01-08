#' Find out which products exhibit the most seasonality.
#'

rm(list=ls())
wd <- "/project2/igaarder"
setwd(wd)

library(sales.taxes)
library(readstata13)
library(data.table)
library(zoo)
library(ggplot2)

## This is already a balanced panel (inst/graph_sales_quarterly_allgoods.R)
sales_data <- fread("Data/Nielsen/allyears_module_store_quarterly.csv")

## normalize sales variable
sales_data <- make_fixed_weights(panel_data = sales_data,
                                 weight_time = list(year = 2008, quarter = 1),
                                 weight_var = "sales",
                                 panel_unit_vars = c("fips_state", "fips_county", "store_code_uc", "product_module_code"))
sales_data[, ln_total_sales := log(sales / sales.weight)]
sales_data[, linear_quarter := (year - 2008) * 4 + quarter]

################################################################################
##################### First, seasonality by product ############################
################################################################################

# sales_panel <- copy(sales_data)
#
# ## remove linear time trend
# group_codes <- unique(sales_panel$product_module_code)
# grouped_input_data <- NULL
#
# for (code in group_codes){
#   group_data <- sales_panel[product_module_code == code]
#   group_data <- remove_time_trends(input_data = group_data,
#                                    outcome_var = "ln_total_sales",
#                                    month_or_quarter = "quarter",
#                                    year_var = "year",
#                                    month_dummies = FALSE,
#                                    calendar_time = FALSE,
#                                    product_group_trend = FALSE,
#                                    weight_var = NULL)
#   grouped_input_data <- rbind(grouped_input_data, group_data)
# }
#
# sales_panel <- grouped_input_data
#
# sales_panel[, season_effect := mean(ln_total_sales_residual),
#             by = .(product_module_code, quarter)]
#
# ## calculate season-product effects
# sales_collapsed <- sales_panel[, list(max_season = max(season_effect),
#                                       min_season = min(season_effect),
#                                       max_season_type = max(as.integer(season_effect == max(season_effect)) * quarter),
#                                       min_season_type = max(as.integer(season_effect == min(season_effect)) * quarter)),
#                                by = product_module_code]
#
# sales_collapsed[, seasonality_range := max_season - min_season]
# setorder(sales_collapsed, -seasonality_range)
# print(head(sales_collapsed, 10))
#
# fwrite(sales_collapsed, "Data/product_seasonality.csv")

################################################################################
####################### Now, seasonality by county #############################
################################################################################

# sales_panel <- copy(sales_data)
# sales_panel[, fips := 1000 * fips_state + fips_county]
#
# ## remove linear time trend
# group_fips <- unique(sales_panel$fips)
# grouped_input_data <- NULL
#
# for (code in group_fips){
#   group_data <- sales_panel[fips == code]
#   group_data <- remove_time_trends(input_data = group_data,
#                                    outcome_var = "ln_total_sales",
#                                    month_or_quarter = "quarter",
#                                    year_var = "year",
#                                    month_dummies = FALSE,
#                                    calendar_time = FALSE,
#                                    product_group_trend = FALSE,
#                                    weight_var = NULL)
#   grouped_input_data <- rbind(grouped_input_data, group_data)
# }
#
# sales_panel <- grouped_input_data
#
# sales_panel[, season_effect := mean(ln_total_sales_residual),
#             by = .(fips, quarter)]
#
# ## calculate season-product effects
# sales_collapsed <- sales_panel[, list(max_season = max(season_effect),
#                                       min_season = min(season_effect),
#                                       max_season_type = max(as.integer(season_effect == max(season_effect)) * quarter),
#                                       min_season_type = max(as.integer(season_effect == min(season_effect)) * quarter)),
#                                by = fips]
#
# sales_collapsed[, seasonality_range := max_season - min_season]
# setorder(sales_collapsed, -seasonality_range)
# print(head(sales_collapsed, 10))
#
# fwrite(sales_collapsed, "Data/county_seasonality.csv")

################################################################################
##################### Now Product X County seasonality #########################
################################################################################

sales_panel <- copy(sales_data)
sales_panel[, fips := 1000 * fips_state + fips_county]
sales_panel[, fips_prod := 10000 * fips + product_module_code]

sales_panel[, season_effect := mean(ln_total_sales),
            by = .(fips, product_module_code, quarter)]

## calculate season-product effects
sales_collapsed <- sales_panel[, list(max_season = max(season_effect),
                                      min_season = min(season_effect),
                                      max_season_type = max(as.integer(season_effect == max(season_effect)) * quarter),
                                      min_season_type = max(as.integer(season_effect == min(season_effect)) * quarter)),
                               by = .(fips, product_module_code)]

sales_collapsed[, seasonality_range := max_season - min_season]
setorder(sales_collapsed, -seasonality_range)
print(head(sales_collapsed, 10))

fwrite(sales_collapsed, "Data/county_product_seasonality.csv")
