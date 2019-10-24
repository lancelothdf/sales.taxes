#' Explore Relevant support
#' Sales Taxes Project
#' Define the relevant support
#' Check differences between yearly and semester data
#' 


library(data.table)
library(futile.logger)
library(lfe)
library(multcomp)
library(ggplot2)


setwd("/project2/igaarder")


## input filepaths -----------------------------------------------
#' This data is the same as all_goods_pi_path, except it has 2015-2016 data as well.
data.semester <- "Data/Nielsen/semester_nielsen_data.csv"


## output filepaths ----------------------------------------------
output.path <- "../../home/slacouture/NLP"

### Set up Semester Data ---------------------------------
all_pi <- fread(data.semester)

# First lagged value (initial)
all_pi[, L.ln_sales_tax := ln_sales_tax - D.ln_sales_tax]

## Defining the interest region. 
# Baseline: Plot historgram (weighted) of tax rates
graphout <- paste0(output.path,"/full_hist.png")
hist <- ggplot(data=all_pi, aes(ln_sales_tax, weight = base.sales)) + 
  geom_histogram(aes(y=..count../sum(..count..))) +    
  theme_bw(base_size = 24) +
  labs(x = "Sales Tax", y = "Fraction", color = NULL) +
  ggsave(graphout)

# Option 1: "Keeping" observations where there are changes contemporaneous
all_pi[, ln_sales_tax_r := ifelse(D.ln_sales_tax == 0, NA, ln_sales_tax)]
graphout <- paste0(output.path,"/pos_changes_hist.png")
hist <- ggplot(data=all_pi, aes(ln_sales_tax_r, weight = base.sales)) + 
  geom_histogram(aes(y=..count../sum(..count..))) +    
  theme_bw(base_size = 24) +
  labs(x = "Sales Tax", y = "Fraction", color = NULL) +
  ggsave(graphout)
# Distribution report
report <- data.table(NULL)
percentiles <- c(1, 5, 10, 20, 25, 40, 60, 75, 80, 90, 95, 99)
percentiles <- percentiles/100
report <- data.table(quantile(all_pi$ln_sales_tax_r, probs = percentiles, na.rm = T, weight=all_pi$base.sales), 
                     percentiles)

report.out <- paste0(output.path,"/quantiles_pos_change.csv")
fwrite(report, report.out)

# Option 2: "Keeping" observations where there are changes lagged
all_pi[, L.ln_sales_tax_r := ifelse(D.ln_sales_tax == 0, NA, L.ln_sales_tax)]
graphout <- paste0(output.path,"/pos_changes_lag_hist.png")
hist <- ggplot(data=all_pi, aes(L.ln_sales_tax_r, weight = base.sales)) + 
  geom_histogram(aes(y=..count../sum(..count..))) +    
  theme_bw(base_size = 24) +
  labs(x = "Sales Tax", y = "Fraction", color = NULL) +
  ggsave(graphout)
# Distribution report
report <- data.table(NULL)
percentiles <- c(1, 5, 10, 20, 25, 40, 60, 75, 80, 90, 95, 99)
percentiles <- percentiles/100
report <- data.table(quantile(all_pi$L.ln_sales_tax_r, probs = percentiles, na.rm = T, weight=all_pi$base.sales), 
                     percentiles)

report.out <- paste0(output.path,"/quantiles_pos_change_lag.csv")
fwrite(report, report.out)

# Option 2: "Keeping" observations where there are changes lagged
all_pi[, L.ln_sales_tax_r := ifelse(D.ln_sales_tax != 0, NA, L.ln_sales_tax)]
graphout <- paste0(output.path,"/no_changes_lag_hist.png")
hist <- ggplot(data=all_pi, aes(L.ln_sales_tax_r, weight = base.sales)) + 
  geom_histogram(aes(y=..count../sum(..count..))) +    
  theme_bw(base_size = 24) +
  labs(x = "Sales Tax", y = "Fraction", color = NULL) +
  ggsave(graphout)
# Distribution report
report <- data.table(NULL)
percentiles <- c(1, 5, 10, 20, 25, 40, 60, 75, 80, 90, 95, 99)
percentiles <- percentiles/100
report <- data.table(quantile(all_pi$L.ln_sales_tax_r, probs = percentiles, na.rm = T, weight=all_pi$base.sales), 
                     percentiles)

report.out <- paste0(output.path,"/quantiles_no_change_lag.csv")
fwrite(report, report.out)



## Now in the yearly data ------ 

data.year <- "Data/Nielsen/yearly_nielsen_data.csv"


### Set up Year Data ---------------------------------
all_pi <- fread(data.year)

## merge region a division
geo_dt <- structure(list(
  fips_state = c(1L, 2L, 4L, 5L, 6L, 8L, 9L, 10L, 12L, 13L, 15L, 16L, 17L, 18L,
                 19L, 20L, 21L, 22L, 23L, 24L, 25L, 26L, 27L, 28L, 29L, 30L,
                 31L, 32L, 33L, 34L, 35L, 36L, 37L, 38L, 39L, 40L, 41L, 42L,
                 44L, 45L, 46L, 47L, 48L, 49L, 50L, 51L, 53L, 54L, 55L, 56L),
  region = c(3L, 4L, 4L, 3L, 4L, 4L, 1L, 3L, 3L, 3L, 4L, 4L, 2L, 2L, 2L, 2L, 3L,
             3L, 1L, 3L, 1L, 2L, 2L, 3L, 2L, 4L, 2L, 4L, 1L, 1L, 4L, 1L, 3L, 2L,
             2L, 3L, 4L, 1L, 1L, 3L, 2L, 3L, 3L, 4L, 1L, 3L, 4L, 3L, 2L, 4L),
  division = c(6L, 9L, 8L,  7L, 9L, 8L, 1L, 5L, 5L, 5L, 9L, 8L, 3L, 3L, 4L, 4L,
               6L, 7L, 1L, 5L, 1L, 3L, 4L, 6L, 4L, 8L, 4L, 8L, 1L, 2L, 8L, 2L,
               5L, 4L, 3L,  7L, 9L, 2L, 1L, 5L, 4L, 6L, 7L, 8L, 1L, 5L, 9L, 5L, 3L, 8L)),
  class = "data.frame", row.names = c(NA, -50L))
setDT(geo_dt)
all_pi <- merge(all_pi, geo_dt, by = "fips_state")


# take first differences of outcomes and treatment
all_pi <- all_pi[order(store_code_uc, product_module_code, year),] ##Sort on store by year (in ascending order)


all_pi[, D.ln_cpricei2 := ln_cpricei2 - shift(ln_cpricei2, n=1, type="lag"),
       by = .(store_code_uc, product_module_code)]

all_pi[, D.ln_quantity3 := ln_quantity3 - shift(ln_quantity3, n=1, type="lag"),
       by = .(store_code_uc, product_module_code)]

all_pi[, D.ln_sales_tax := ln_sales_tax - shift(ln_sales_tax, n=1, type="lag"),
       by = .(store_code_uc, product_module_code)]

# Make group FE
all_pi[, store_by_module := .GRP, by = .(store_code_uc, product_module_code)]
all_pi[, region_by_module_by_time := .GRP, by = .(region, product_module_code, year)]
all_pi[, division_by_module_by_time := .GRP, by = .(division, product_module_code, year)]
all_pi[, module_by_state := .GRP, by = .(product_module_code, fips_state)]

# Keep relevant years
all_pi <- all_pi[between(year, 2008, 2014)]

# First lagged value (initial)
all_pi[, L.ln_sales_tax := ln_sales_tax - D.ln_sales_tax]

## Defining the interest region. 
# Baseline: Plot historgram (weighted) of tax rates
graphout <- paste0(output.path,"/full_hist_y.png")
hist <- ggplot(data=all_pi, aes(ln_sales_tax, weight = base.sales)) + 
  geom_histogram(aes(y=..count../sum(..count..))) +    
  theme_bw(base_size = 24) +
  labs(x = "Sales Tax", y = "Fraction", color = NULL) +
  ggsave(graphout)

# Option 1: "Keeping" observations where there are changes contemporaneous
all_pi[, ln_sales_tax_r := ifelse(D.ln_sales_tax == 0, NA, ln_sales_tax)]
graphout <- paste0(output.path,"/pos_changes_hist_y.png")
hist <- ggplot(data=all_pi, aes(ln_sales_tax_r, weight = base.sales)) + 
  geom_histogram(aes(y=..count../sum(..count..))) +    
  theme_bw(base_size = 24) +
  labs(x = "Sales Tax", y = "Fraction", color = NULL) +
  ggsave(graphout)
# Distribution report
report <- data.table(NULL)
percentiles <- c(1, 5, 10, 20, 25, 40, 60, 75, 80, 90, 95, 99)
percentiles <- percentiles/100
report <- data.table(quantile(all_pi$ln_sales_tax_r, probs = percentiles, na.rm = T, weight=all_pi$base.sales), 
                     percentiles)

report.out <- paste0(output.path,"/quantiles_pos_change_y.csv")
fwrite(report, report.out)

# Option 2: "Keeping" observations where there are changes lagged
all_pi[, L.ln_sales_tax_r := ifelse(D.ln_sales_tax == 0, NA, L.ln_sales_tax)]
graphout <- paste0(output.path,"/pos_changes_lag_hist_y.png")
hist <- ggplot(data=all_pi, aes(L.ln_sales_tax_r, weight = base.sales)) + 
  geom_histogram(aes(y=..count../sum(..count..))) +    
  theme_bw(base_size = 24) +
  labs(x = "Sales Tax", y = "Fraction", color = NULL) +
  ggsave(graphout)
# Distribution report
report <- data.table(NULL)
percentiles <- c(1, 5, 10, 20, 25, 40, 60, 75, 80, 90, 95, 99)
percentiles <- percentiles/100
report <- data.table(quantile(all_pi$L.ln_sales_tax_r, probs = percentiles, na.rm = T, weight=all_pi$base.sales), 
                     percentiles)

report.out <- paste0(output.path,"/quantiles_pos_change_lag_y.csv")
fwrite(report, report.out)