#' Explore Relevant support
#' Sales Taxes Project
#' Define the relevant support based on 
#' 
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
report <- data.table(quantile(all_pi$ln_sales_tax_r, probs = percentiles, na.rm = T, weight=all_pi$base.sales), 
                     percentiles)

report.out <- paste0(output.path,"/quantiles_pos_change_lag.csv")
fwrite(report, report.out)


