#' Author: John Bonney
#'
#' Plot price index pre-trends

library(data.table)
library(ggplot2)
library(zoo)

setwd("C:/Users/John Bonney/Desktop/Magne_projects/sales_taxes/output/server/pi_data")

ct.all <- fread("pi_all_calendar.csv")
ct.all$year_qtr <- as.yearqtr(paste(
  as.integer(ct.all$year), as.integer(ct.all$quarter)
), "%Y %q")

all.calendar.plot <- ggplot(data = ct.all, mapping = aes(x = year_qtr,
                                                         y = mean.cpricei,
                                                                   color = tr_count)) +
  labs(x = "Quarter", y = "Mean normalized ln(index)", color = "Sales tax change",
       caption = "Weighted by sales in 2008 Q1.") +
  scale_x_yearqtr(format = "%Y Q%q") +
  geom_line() +
  theme_bw()
all.calendar.plot
