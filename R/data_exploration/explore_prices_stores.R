#' Sales Taxes Project
#' This code explores the distribution of prices across stores
#' Take a sales weighted average of the moduleXstoreXsemester prices at the storeXsemester level 


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
output.path <- "../../home/slacouture/NLP/store_prices"

### Set up Semester Data ---------------------------------
all_pi <- fread(data.semester)
all_pi[, w.ln_sales_tax := ln_sales_tax - mean(ln_sales_tax), by = .(store_by_module)]
all_pi[, w.ln_cpricei2 := ln_cpricei2 - mean(ln_cpricei2), by = .(store_by_module)]
all_pi[, w.ln_quantity3 := ln_quantity3 - mean(ln_quantity3), by = .(store_by_module)]

# Need to demean
all_pi[, module_by_time := .GRP, by = .(product_module_code, semester, year)]
all_pi[, L.ln_cpricei2 := ln_cpricei2 - D.ln_cpricei2]
all_pi[, dm.L.ln_cpricei2 := L.ln_cpricei2 - mean(L.ln_cpricei2, na.rm = T), by = module_by_time]
all_pi[, dm.ln_cpricei2 := ln_cpricei2 - mean(ln_cpricei2, na.rm = T), by = module_by_time]
all_pi[, dm.ln_quantity3 := ln_quantity3 - mean(ln_quantity3, na.rm = T), by = module_by_time]


# Defining common support
control <- all_pi[D.ln_sales_tax == 0,]
treated <- all_pi[D.ln_sales_tax != 0,]

# Price 
pct1.control <- quantile(control$dm.L.ln_cpricei2, probs = 0.01, na.rm = T, weight=control$base.sales)
pct1.treated <- quantile(treated$dm.L.ln_cpricei2, probs = 0.01, na.rm = T, weight=treated$base.sales)

pct99.control <- quantile(control$dm.L.ln_cpricei2, probs = 0.99, na.rm = T, weight=control$base.sales)
pct99treated <- quantile(treated$dm.L.ln_cpricei2, probs = 0.99, na.rm = T, weight=treated$base.sales)

all_pi[, cs_price := ifelse(dm.L.ln_cpricei2 > max(pct1.treated, pct1.control) & 
                              dm.L.ln_cpricei2 < min(pct99treated, pct99.control), 1, 0)]
# Make sure missings are 0s
all_pi[, cs_price := ifelse(is.na(dm.L.ln_cpricei2), 0, cs_price)]

## Keep within the common support
all_pi <- all_pi[cs_price == 1,]


### Built the store level prices data -------------

## 1. Compute sales across modules within store and time
all_pi[, sales := exp(ln_cpricei2)*exp(ln_quantity3)]

## 2. collapse prices at the store x time level using sales as weights
head(all_pi)
all_pi[, .(ln_cpricei2 = mean(ln_cpricei2, weight = sales),
          dm.ln_cpricei2 = mean(dm.ln_cpricei2, weight = sales),
          base.sales = mean(base.sales, weight = sales)), 
       by = .(store_code, semester, year)]

## 3. Create demeaned prices at store
all_pi[, dm.s.ln_cpricei2 := ln_cpricei2 - mean(ln_cpricei2, na.rm = T), by = .(semester, year)]


## 4. Export distribution plots
graphout <- paste0(output.path,"/prices.png")
hist <- ggplot(all_pi, aes(ln_cpricei2, weight = base.sales)) + 
  geom_histogram(aes(y=..count../sum(..count..)), position="identity") +    
  theme_classic(base_size = 24) +
  labs(x = "Store (log) prices", y = "Fraction", color = NULL) 
ggsave(graphout)

graphout <- paste0(output.path,"/dm.prices.png")
hist <- ggplot(all_pi, aes(dm.ln_cpricei2, weight = base.sales)) + 
  geom_histogram(aes(y=..count../sum(..count..)), position="identity") +    
  theme_classic(base_size = 24) +
  labs(x = "Store (demeaned log) prices", y = "Fraction", color = NULL) 
ggsave(graphout)

graphout <- paste0(output.path,"/dm.s.prices.png")
hist <- ggplot(all_pi, aes(dm.s.ln_cpricei2, weight = base.sales)) + 
  geom_histogram(aes(y=..count../sum(..count..)), position="identity") +    
  theme_classic(base_size = 24) +
  labs(x = "(Demeaned) Store (log) prices", y = "Fraction", color = NULL) 
ggsave(graphout)

## 5. Export distributions by key percentiles
vars <- c("ln_cpricei2", "dm.ln_cpricei2", "dm.s.ln_cpricei2")
percentiles <- c(1, 2, 5, 10, 90, 95, 98, 99)
for (var in vars) {
  distr <- data.table(NULL)
  for (pc in percentiles) {
    
    value <- quantile(all_pi[[get(var)]], probs = pc/100, na.rm = T, weight=all_pi$base.sales)
    distr <- rbind(distr, data.table(value, pc))
    
  }
  fwrite(paste0(output.path, "/distr_", var, ".csv"))
}


