#' Sales Taxes Project
#' This code defines and plots the common support

library(data.table)
library(futile.logger)
library(lfe)
library(multcomp)
library(ggplot2)


setwd("/project2/igaarder")


## input filepaths -----------------------------------------------
#' This data is the same as all_goods_pi_path, except it has 2015-2016 data as well.
data.semester <- "Data/Nielsen/semester_nielsen_data.csv"
data.year <- "Data/Nielsen/yearly_nielsen_data.csv"


## output filepaths ----------------------------------------------
output.path <- "../../home/slacouture/NLP"


### Set up Semester Data ---------------------------------
all_pi <- fread(data.semester)
all_pi[, w.ln_sales_tax := ln_sales_tax - mean(ln_sales_tax), by = .(store_by_module)]
all_pi[, w.ln_cpricei2 := ln_cpricei2 - mean(ln_cpricei2), by = .(store_by_module)]
all_pi[, w.ln_quantity3 := ln_quantity3 - mean(ln_quantity3), by = .(store_by_module)]

# Create lagged value (initial)
all_pi[, L.ln_sales_tax := ln_sales_tax - D.ln_sales_tax]
# need to demean to compare appropiately
all_pi[, module_by_time := .GRP, by = .(product_module_code, year)]
all_pi[, L.ln_cpricei2 := ln_cpricei2 - D.ln_cpricei2]
all_pi[, dm.L.ln_cpricei2 := L.ln_cpricei2 - mean(L.ln_cpricei2, na.rm = T), by = module_by_time]

# Defining common support
control <- all_pi[D.ln_sales_tax == 0,]
treated <- all_pi[D.ln_sales_tax != 0,]

# Tax rate
pct1.control <- quantile(control$L.ln_sales_tax, probs = 0.01, na.rm = T, weight=all_pi$base.sales)
pct1.treated <- quantile(treated$L.ln_sales_tax, probs = 0.01, na.rm = T, weight=all_pi$base.sales)

pct99.control <- quantile(control$L.ln_sales_tax, probs = 0.99, na.rm = T, weight=all_pi$base.sales)
pct99treated <- quantile(treated$L.ln_sales_tax, probs = 0.99, na.rm = T, weight=all_pi$base.sales)

all_pi[, cs_tax := ifelse(L.ln_sales_tax > max(pct1.treated, pct1.control) & 
                            L.ln_sales_tax < min(pct99treated, pct99.control), 1, 0)]
# Make sure missings are 0s
all_pi[, cs_tax := ifelse(is.na(L.ln_sales_tax), 0, cs_tax)]

# Define groups for histogram
all_pi[, treatment := ifelse(D.ln_sales_tax != 0, 1, 0)]


## Export histograms:

## Full sample
graphout <- paste0(output.path,"/lag_tax_group.png")
hist <- ggplot(data=subset(all_pi), aes(L.ln_sales_tax, fill = treatment, weight = base.sales)) + 
  geom_histogram(alpha = 0.3, aes(y=..count../sum(..count..))) +    
  theme_bw(base_size = 24) +
  labs(x = "Sales Tax", y = "Fraction", color = NULL)
ggsave(graphout)

## Common support
graphout <- paste0(output.path,"/lag_tax_group_support.png")
hist <- ggplot(data=subset(all_pi,cs_tax == 1), aes(L.ln_sales_tax, fill = treatment, weight = base.sales)) + 
  geom_histogram(alpha = 0.3, aes(y=..count../sum(..count..))) +    
  theme_bw(base_size = 24) +
  labs(x = "Sales Tax", y = "Fraction", color = NULL)
ggsave(graphout)


# Price
pct1.control <- quantile(control$dm.L.ln_cpricei2, probs = 0.01, na.rm = T, weight=all_pi$base.sales)
pct1.treated <- quantile(treated$dm.L.ln_cpricei2, probs = 0.01, na.rm = T, weight=all_pi$base.sales)

pct99.control <- quantile(control$dm.L.ln_cpricei2, probs = 0.99, na.rm = T, weight=all_pi$base.sales)
pct99treated <- quantile(treated$dm.L.ln_cpricei2, probs = 0.99, na.rm = T, weight=all_pi$base.sales)

all_pi[, cs_price := ifelse(dm.L.ln_cpricei2 > max(pct1.treated, pct1.control) & 
                              dm.L.ln_cpricei2 < min(pct99treated, pct99.control), 1, 0)]
# Make sure missings are 0s
all_pi[, cs_price := ifelse(is.na(dm.L.ln_cpricei2), 0, cs_price)]


## Export histograms:

## Full sample
graphout <- paste0(output.path,"/lag_price_group.png")
hist <- ggplot(data=all_pi, aes(dm.L.ln_cpricei2, fill = treatment, weight = base.sales)) + 
  geom_histogram(alpha = 0.3, aes(y=..count../sum(..count..))) +    
  theme_bw(base_size = 24) +
  scale_x_continuous(limits = c(-0.5,0.5), breaks = seq(-0.5, 0.5, 0.1)) +
  labs(x = "Demeaned Price", y = "Fraction", color = NULL)
ggsave(graphout)

## Common support
graphout <- paste0(output.path,"/lag_price_grupo_support.png")
hist <- ggplot(data=subset(all_pi,cs_price == 1), aes(dm.L.ln_cpricei2, fill = treatment,  weight = base.sales)) + 
  geom_histogram(alpha = 0.3, aes(y=..count../sum(..count..))) +    
  theme_bw(base_size = 24) +
  labs(x = "Demeaned Price", y = "Fraction", color = NULL)
ggsave(graphout)

