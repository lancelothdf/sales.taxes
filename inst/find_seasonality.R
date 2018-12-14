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

sales_panel <- fread("Data/Nielsen/allyears_module_store_quarterly.csv")

## normalize sales variable
sales_panel <- make_fixed_weights(panel_data = sales_panel,
                                  weight_time = list(year = 2008, quarter = 1),
                                  weight_var = "sales",
                                  panel_unit_vars = c("fips_state", "fips_county", "store_code_uc", "product_module_code"))
sales_panel[, ln_total_sales := log(sales / sales.weight)]
sales_panel[, linear_quarter := (year - 2008) * 4 + quarter]

## remove linear time trend
group_codes <- unique(sales_panel$product_module_code)
grouped_input_data <- NULL

for (code in group_codes){
  group_data <- sales_panel[product_module_code == code]
  group_data <- remove_time_trends(input_data = group_data,
                                   outcome_var = "ln_total_sales",
                                   month_or_quarter = "quarter",
                                   year_var = "year",
                                   month_dummies = FALSE,
                                   calendar_time = FALSE,
                                   product_group_trend = FALSE,
                                   weight_var = NULL)
  grouped_input_data <- rbind(grouped_input_data, group_data)
}

sales_panel <- grouped_input_data

sales_panel[, season_effect := mean(ln_total_sales_residual),
            by = .(product_module_code, quarter)]

## calculate season-product effects
sales_collapsed <- sales_panel[, list(max_season = max(season_effect),
                                      min_season = min(season_effect),
                                      max_season_type = max(as.integer(season_effect == max(season_effect)) * quarter),
                                      min_season_type = max(as.integer(season_effect == min(season_effect)) * quarter)),
                               by = product_module_code]

sales_collapsed[, seasonality_range := max_season - min_season]
setorder(sales_collapsed, -seasonality_range)
print(head(sales_collapsed, 10))

fwrite(sales_collapsed, "Data/product_seasonality.csv")
