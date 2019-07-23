## Sales taxes Project. Household Panel
# Exploring Data Set
# This code runs basic descriptive statistics to main outcomes and interest variables
# Author: Santiago Lacouture


library(data.table)
library(lfe)
library(futile.logger)
library(AER)
library(multcomp)
library(psych)
library(ggplot2)

setwd("/project2/igaarder/Data/Nielsen/Household_panel")

## Filepaths
# Exporting results to my folder in the server

purchases.full <- fread("cleaning/consumer_panel_2006-2016_ids.csv")

## Basic Descriptive Statistics -------
# by module 
descriptives <- describe(purchases.full[, .(ln_sales_tax, ln_cpricei, ln_quantity, ln_share_expend,
                                           total_expenditures, share_expend, ln_share_expend_100)])
desc.est  <- data.table(descriptives, keep.rownames=T)
des.est.out <- desc.est

# by store
purchases.store <- purchases.full[, list(sum_total_exp_store = sum(total_expenditures), modules_by_store = .N),
                                    by = .(household_code, year, quarter, store_code_uc)]
descriptives <- describe(purchases.store[, .(sum_total_exp_store, modules_by_store)])
desc.est  <- data.table(descriptives, keep.rownames=T)
des.est.out <- rbind(des.est.out, desc.est, fill = T)

# by quarter
purchases.quarter <- purchases.store[, list(sum_total_exp_quarter = sum(sum_total_exp_store), stores = .N),
                                  by = .(household_code, year, quarter)]
descriptives <- describe(purchases.quarter[, .(sum_total_exp_quarter, stores)])
desc.est  <- data.table(descriptives, keep.rownames=T)
des.est.out <- rbind(des.est.out, desc.est, fill = T)


fwrite(des.est.out, "../../../../../home/slacouture/HMS/Basic_Descriptives.csv")

## Graphing Descriptives: Oneway ---------------

# Histograms
ggplot(purchases.full[total_expenditures < 900], aes(x=total_expenditures, y = ..density..), na.rm = T) +
  geom_histogram(binwidth = 10)
ggsave("../../../../../home/slacouture/HMS/histogram_expenditures.png")

ggplot(purchases.full[ln_cpricei < 5 & ln_cpricei > -5], aes(x=ln_cpricei, y = ..density..), na.rm = T) +
  geom_histogram(binwidth = 0.25)
ggsave("../../../../../home/slacouture/HMS/histogram_ln_cpricei.png")

ggplot(purchases.full, aes(x=ln_sales_tax, y = ..density..), na.rm = T) +
  geom_histogram()
ggsave("../../../../../home/slacouture/HMS/histogram_ln_sales_tax.png")

ggplot(purchases.full[ln_cpricei < 7 & ln_quantity > -4], aes(x=ln_quantity, y = ..density..), na.rm = T) +
  geom_histogram(binwidth = 0.25)
ggsave("../../../../../home/slacouture/HMS/histogram_ln_quantity.png")

ggplot(purchases.full[share_expend < 0.3], aes(x=share_expend, y = ..density..), na.rm = T) +
  geom_histogram(binwidth = 0.025)
ggsave("../../../../../home/slacouture/HMS/histogram_share_expend.png")

ggplot(purchases.full, aes(x=ln_share_expend, y = ..density..), na.rm = T) +
  geom_histogram()
ggsave("../../../../../home/slacouture/HMS/histogram_ln_share_expend.png")

ggplot(purchases.store[sum_total_exp_store < 3000], aes(x=sum_total_exp_store, y = ..density..), na.rm = T) +
  geom_histogram(binwidth = 30)
ggsave("../../../../../home/slacouture/HMS/histogram_sum_total_exp_store.png")

ggplot(purchases.store[modules_by_store < 150], aes(x=modules_by_store, y = ..density..), na.rm = T) +
  geom_histogram()
ggsave("../../../../../home/slacouture/HMS/histogram_modules_by_store.png")

ggplot(purchases.quarter[sum_total_exp_store < 5000], aes(x=sum_total_exp_quarter, y = ..density..), na.rm = T) +
  geom_histogram(binwidth = 50)
ggsave("../../../../../home/slacouture/HMS/histogram_sum_total_exp_quarter.png")

ggplot(purchases.quarter[stores < 20], aes(x=stores, y = ..density..), na.rm = T) +
  geom_histogram(binwidth = 1)
ggsave("../../../../../home/slacouture/HMS/histogram_stores.png")



## Constraining Data set for estimations ------------ 
# Drop "magnet" purchases: 
purchases.full[, sum(is.na(projection_factor))]
# 2016154 obs
purchases.nomagnet <- purchases.full[!is.na(projection_factor)]

# Drop purchases without sales tax data
purchases.full[, sum(is.na(sales_tax))]
# More than 70%: 181247984 obs
purchases.retail <- purchases.nomagnet[!is.na(sales_tax)]


## Repeat statistics on "Estimation Sample"
# by module 
descriptives <- describe(purchases.retail[, .(ln_sales_tax, ln_cpricei, ln_quantity, ln_share_expend,
                                            total_expenditures, share_expend, ln_share_expend_100)])
desc.est  <- data.table(descriptives, keep.rownames=T)
des.est.out <- desc.est

# by store
purchases.store <- purchases.retail[, list(sum_total_exp_store = sum(total_expenditures), modules_by_store = .N),
                                  by = .(household_code, year, quarter, store_code_uc)]
descriptives <- describe(purchases.store[, .(sum_total_exp_store, modules_by_store)])
desc.est  <- data.table(descriptives, keep.rownames=T)
des.est.out <- rbind(des.est.out, desc.est, fill = T)

# by quarter
purchases.quarter <- purchases.store[, list(sum_total_exp_quarter = sum(sum_total_exp_store), stores = .N),
                                     by = .(household_code, year, quarter)]
descriptives <- describe(purchases.quarter[, .(sum_total_exp_quarter, stores)])
desc.est  <- data.table(descriptives, keep.rownames=T)
des.est.out <- rbind(des.est.out, desc.est, fill = T)


fwrite(des.est.out, "../../../../../home/slacouture/HMS/Basic_Descriptives_esample.csv")

## Repeat Graphing Descriptives: Oneway ---------------

# Histograms
ggplot(purchases.retail[total_expenditures < 900], aes(x=total_expenditures, y = ..density..), na.rm = T) +
  geom_histogram(binwidth = 10)
ggsave("../../../../../home/slacouture/HMS/histogram_expenditures_esample.png")

ggplot(purchases.retail[ln_cpricei < 5 & ln_cpricei > -5], aes(x=ln_cpricei, y = ..density..), na.rm = T) +
  geom_histogram(binwidth = 0.25)
ggsave("../../../../../home/slacouture/HMS/histogram_ln_cpricei_esample.png")

ggplot(purchases.retail, aes(x=ln_sales_tax, y = ..density..), na.rm = T) +
  geom_histogram()
ggsave("../../../../../home/slacouture/HMS/histogram_ln_sales_tax_esample.png")

ggplot(purchases.retail[ln_cpricei < 7 & ln_quantity > -4], aes(x=ln_quantity, y = ..density..), na.rm = T) +
  geom_histogram(binwidth = 0.25)
ggsave("../../../../../home/slacouture/HMS/histogram_ln_quantity_esample.png")

ggplot(purchases.retail[share_expend < 0.3], aes(x=share_expend, y = ..density..), na.rm = T) +
  geom_histogram(binwidth = 0.025)
ggsave("../../../../../home/slacouture/HMS/histogram_share_expend_esample.png")

ggplot(purchases.retail, aes(x=ln_share_expend, y = ..density..), na.rm = T) +
  geom_histogram()
ggsave("../../../../../home/slacouture/HMS/histogram_ln_share_expend_esample.png")

ggplot(purchases.store[sum_total_exp_store < 3000], aes(x=sum_total_exp_store, y = ..density..), na.rm = T) +
  geom_histogram(binwidth = 30)
ggsave("../../../../../home/slacouture/HMS/histogram_sum_total_exp_store_esample.png")

ggplot(purchases.store[modules_by_store < 150], aes(x=modules_by_store, y = ..density..), na.rm = T) +
  geom_histogram()
ggsave("../../../../../home/slacouture/HMS/histogram_modules_by_store_esample.png")

ggplot(purchases.quarter[sum_total_exp_store < 5000], aes(x=sum_total_exp_quarter, y = ..density..), na.rm = T) +
  geom_histogram(binwidth = 50)
ggsave("../../../../../home/slacouture/HMS/histogram_sum_total_exp_quarter_esample.png")

ggplot(purchases.quarter[stores < 20], aes(x=stores, y = ..density..), na.rm = T) +
  geom_histogram(binwidth = 1)
ggsave("../../../../../home/slacouture/HMS/histogram_stores_esample.png")





