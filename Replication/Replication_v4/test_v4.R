##### Santiago Lacouture
#' Sales Taxes
#' Replication File. Updated on 04/20/2023
#' Step 10a: Repeat estimation for point identification but with demand under imperfect salience
#' This version includes bootstrapping

library(data.table)
library(futile.logger)
library(lfe)
library(Matrix)
library(zoo)
library(stringr)

setwd("/project/igaarder")
rm(list = ls())

## input filepath ----------------------------------------------
all_pi <- fread("Data/Replication_v4/all_pi.csv")
# restrict to relevant sample
all_pi <- all_pi[non_imp_tax_strong == 1,]

## output filepath ----------------------------------------------
iv.output.salience.results.file <- "Data/Replication_v4/Demand_iv_sat_initial_price_semester_salience.csv"
theta.output.salience.results.file <- "Data/Replication_v4/Demand_theta_sat_initial_price_semester_salience.csv"


print(head(all_pi))

all_pi <- all_pi[fips_state == 1 & fips_county == 3,]

fwrite(all_pi, "/project/igaarder/Data/Replication_v4/test_data.csv")