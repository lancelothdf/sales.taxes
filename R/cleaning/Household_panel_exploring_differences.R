## Sales taxes Project. Household Panel
# Compairing sample means: retail data vs. non-retail data 
# Author: Santiago Lacouture


library(data.table)
library(lfe)
library(futile.logger)
library(AER)
library(multcomp)
library(psych)
library(ggplot2)

setwd("/project2/igaarder/Data/Nielsen/Household_panel")

## Open Data
purchases.full <- fread("cleaning/consumer_panel_2006-2016_ids.csv")


## Run mean differences between subsamples
# first by hhold
purchases.full <- purchases.full[, esample := (!is.na(sales_tax) & !is.na(projection_factor))]

differences <- "Full sample"
differences <- purchases.full[esample == 1, mean_1 = mean(total_expenditures)]
differences[, mean_0 := purchases.full[esample == 0, mean_0 = mean(total_expenditures)]]
samples.diff <- t.test(purchases.full$share_expend ~ purchases.full$esample)
differences[, estimate := samples.diff$estimate]
differences[, p.value := samples.diff$p.value]

for (yr in 2008:2014) {
  differences.yr <- yr
  differences.yr[, mean_1 := purchases.full[esample == 1 & year == yr, mean_1 = mean(total_expenditures)]]
  differences.yr[, mean_0 := purchases.full[esample == 0 & year == yr, mean_0 = mean(total_expenditures)]]
  purchases.yr <- purchases.full[year == yr]
  samples.diff <- t.test(purchases.yr$share_expend ~ purchases.yr$esample)
  differences.yr[, estimate := samples.diff$estimate]
  differences.yr[, p.value := samples.diff$p.value]
  differences <- rbind(differences, differences.yr)
}
fwrite(LRdiff_res, "../../../../../home/slacouture/HMS/Difference_samples.csv")
