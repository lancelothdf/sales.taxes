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
des.est.out <- rbind(des.est.out, desc.est)

# by quarter
purchases.quarter <- purchases.store[, list(sum_total_exp_quarter = sum(sum_total_exp_store), stores = .N),
                                  by = .(household_code, year, quarter)]
descriptives <- describe(purchases.quarter[, .(sum_total_exp_quarter, stores)])
desc.est  <- data.table(descriptives, keep.rownames=T, fill = T)
des.est.out <- rbind(des.est.out, desc.est)


fwrite(des.est.out, "../../../../../home/slacouture/HMS/Basic_Descriptives.csv")

## Graphing Descriptives: Oneway ---------------

# Histograms
ggplot(purchases.full, aes(x=total_expenditures), na.rm = T) +
  geom_histogram()
ggsave("../../../../../home/slacouture/HMS/histogram_expenditures.png")

ggplot(purchases.full, aes(x=ln_cpricei), na.rm = T) +
  geom_histogram()
ggsave("../../../../../home/slacouture/HMS/histogram_ln_cpricei.png")

ggplot(purchases.full, aes(x=ln_sales_tax), na.rm = T) +
  geom_histogram()
ggsave("../../../../../home/slacouture/HMS/histogram_ln_sales_tax.png")

ggplot(purchases.full, aes(x=ln_quantity), na.rm = T) +
  geom_histogram()
ggsave("../../../../../home/slacouture/HMS/histogram_ln_quantity.png")

ggplot(purchases.full, aes(x=share_expend), na.rm = T) +
  geom_histogram()
ggsave("../../../../../home/slacouture/HMS/histogram_share_expend.png")

ggplot(purchases.full, aes(x=ln_share_expend), na.rm = T) +
  geom_histogram()
ggsave("../../../../../home/slacouture/HMS/histogram_ln_share_expend.png")

ggplot(purchases.store, aes(x=sum_total_exp_store), na.rm = T) +
  geom_histogram()
ggsave("../../../../../home/slacouture/HMS/histogram_sum_total_exp_store.png")

ggplot(purchases.store, aes(x=modules_by_store), na.rm = T) +
  geom_histogram()
ggsave("../../../../../home/slacouture/HMS/histogram_modules_by_store.png")

ggplot(purchases.quarter, aes(x=sum_total_exp_quarter), na.rm = T) +
  geom_histogram()
ggsave("../../../../../home/slacouture/HMS/histogram_sum_total_exp_quarter.png")

ggplot(purchases.quarter, aes(x=stores), na.rm = T) +
  geom_histogram()
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
