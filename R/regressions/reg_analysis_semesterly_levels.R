#' Author: John Bonney
#'
#' Run the equivalent specification (with 4 leads and 4 lags) in levels,
#' using semesterly data.

library(data.table)
library(futile.logger)
library(lfe)
library(multcomp)

setwd("/project2/igaarder")

# Some helpful files:
  - Data/Nielsen/semester_nielsen_data.csv # --> Price and quantity data aggregated by semester
