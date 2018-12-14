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

sales_panel <- make_fixed_weights(panel_data = sales_panel,
                                  weight_time = list(year = 2008, quarter = 1),
                                  weight_var = "sales",
                                  panel_unit_vars = c("fips_state", "fips_county", "store_code_uc", "product_module_code"))
sales_panel[, ln_total_sales := log(sales / sales.weight)]
sales_panel[, linear_quarter := (year - 2008) * 4 + quarter]

## remove linear time trend
sales_panel <- residualize_outcome(input_data = sales_panel,
                                   outcome_var = ln_total_sales,
                                   discrete_vars = NULL,
                                   continuous_vars = linear_quarter,
                                   weight_var = NULL)

sales_panel[, season_effect := mean(ln_total_sales_residual),
            by = .(product_module_code, quarter)]

## calculate season-product effects
sales_collapsed <- sales_panel[, list(max_season = max(season_effect),
                                      min_season = min(season_effect)),
                               by = product_module_code]

sales_collapsed[, seasonality_range := max_season - min_season]
setorder(sales_collapsed, -seasonality_range)
print(head(sales_collapsed, 10))

fwrite(sales_collapsed, "Data/product_seasonality.csv")
