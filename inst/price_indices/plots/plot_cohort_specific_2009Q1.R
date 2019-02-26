#' Author: John Bonney
#'
#' Plot the cohort-specific trends (across tax-change cohorts) for the 2009 Q1
#' group (data obtained from cohort_specific_2009Q1.R)

setwd("C:/Users/John Bonney/Desktop/Magne_projects/sales_taxes")

library(tidyverse)
library(zoo)

data_path <- "output/server/pi_data/pi_2009Q1_cohorts.csv"
outfile_figpath <- "reports/figs/pi_2009Q1_cohorts.png"

dt <- fread(data_path)
