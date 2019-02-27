#' Author: John Bonney
#'
#' Plot the cohort-specific trends (across tax-change cohorts) for the 2009 Q1
#' group (data obtained from cohort_specific_2009Q1.R)

setwd("C:/Users/John Bonney/Desktop/Magne_projects/sales_taxes")

library(tidyverse)
library(zoo)

data_path <- "output/server/pi_data/pi_2009Q1_cohorts.csv"
outfile_figpath <- "reports/figs/pi_2009Q1_cohorts.png"

dt <- read.csv(data_path)

dt <- dt %>% select(year, quarter, cohort, cpricei) %>%
  filter(!is.na(year))

dt$t <- as.yearqtr(paste0(dt$year, " Q", dt$quarter))

ggplot(dt, mapping = aes(x = t, y = cpricei, color = cohort)) + geom_line()

