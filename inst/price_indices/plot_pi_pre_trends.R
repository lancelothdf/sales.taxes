#' Author: John Bonney
#'
#' Plot price index pre-trends

library(data.table)
library(ggplot2)
library(zoo)

setwd("C:/Users/John Bonney/Desktop/Magne_projects/sales_taxes/output/server/pi_data")

## define theme I like ---------------------------------------------------------

myTheme <- theme_bw() +  theme(
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_line(size = 0.1, colour = 'grey'),
  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
  strip.background = element_rect(colour = "white", fill = "white"),
  plot.title = element_text(hjust = 0.5, size = 14), panel.spacing = unit(2, "lines"),
  legend.margin = margin(t = -.2, r = 0, b = -.2, l = 0, unit = "cm"),
  axis.text.x = element_text(margin = unit(rep(0.3, 4), "cm")),
  axis.text.y = element_text(margin = unit(rep(0.3, 4), "cm")),
  axis.text.y.right = element_text(margin = unit(rep(0.3, 4), "cm"))
)

## all goods, calendar time ----------------------------------------------------

ct.all <- fread("pi_all_calendar.csv")
ct.all$year_qtr <- as.yearqtr(paste(
  as.integer(ct.all$year), as.integer(ct.all$quarter)
), "%Y %q")

all.calendar.plot <- ggplot(data = ct.all, mapping = aes(x = year_qtr,
                                                         y = mean.cpricei,
                                                         color = tr_count)) +
  labs(x = "Quarter", y = "Mean normalized ln(price index)", color = NULL,
       caption = "Weighted by sales in 2008 Q1. Sales tax changes are any changes occuring between 2009 and 2013.") +
  ggtitle("Price index by sales tax change (all goods)") +
  scale_x_yearqtr(format = "%Y Q%q", expand = c(0.01, 0.01)) +
  scale_y_continuous(breaks = seq(0, 0.13, .04), expand = c(0.005, 0.005)) +
  myTheme +
  theme(legend.position = c(0.8, 0.2), axis.ticks.length = unit(-0.15, "cm"))
all.calendar.plot

## taxable goods, calendar time ------------------------------------------------

ct.taxable <- fread("pi_taxable_calendar.csv")
ct.taxable$year_qtr <- as.yearqtr(paste(
  as.integer(ct.taxable$year), as.integer(ct.taxable$quarter)
), "%Y %q")

## all goods, event time -------------------------------------------------------

## taxable goods, event time ---------------------------------------------------

## tax exempt goods, event time ------------------------------------------------



