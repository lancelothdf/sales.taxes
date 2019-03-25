#' Author: John Bonney
#'
#' Plot the cohort-specific trends (across tax-change cohorts) for the 2009 Q1
#' group (data obtained from cohort_specific_2009Q1.R)

setwd("C:/Users/John Bonney/Desktop/Magne_projects/sales_taxes/output/server")

library(tidyverse)
library(zoo)
library(grid)

et.all.nt <- fread("pi_data/pi_allgoods_es_V2.csv")
et.taxable.nt <- fread("pi_data/pi_taxable_es_V2.csv")
et.taxable.nt[, total_sales := NULL]
et.taxexempt.nt <- fread("pi_data/pi_taxexempt_es_V2.csv")

et.all.ft <- fread("pi_data/pi_allgoods_es_time_of_event.csv")
et.taxable.ft <- fread("pi_data/pi_taxable_es_time_of_event.csv")
et.taxexempt.ft <- fread("pi_data/pi_taxexempt_es_time_of_event.csv")

et.all.ftu <- fread("pi_data/pi_allgoods_es_time_of_event_unrestricted.csv")
et.taxable.ftu <- fread("pi_data/pi_taxable_es_time_of_event_unrestricted.csv")
et.taxexempt.ftu <- fread("pi_data/pi_taxexempt_es_time_of_event_unrestricted.csv")

## construct dataset that is just the means from -8 to 4, for
## a) treated, b) never treated, c) future treated (unrestricted), d) future treated

treated <- et.taxable.nt[tr_group == "Ever increase"]
control.nt <- et.taxable.nt[tr_group == "No change (ever increase)"]
control.ft <- et.taxable.ft[tr_group == "Future change (ever increase)"]
control.ftu <- et.taxable.ftu[tr_group == "Future change (ever increase)"]
control.ftu[, tr_group := "Future change - unrestricted (ever increase)"]

plot.dt <- rbind(treated,
                 control.nt,
                 control.ft,
                 control.ftu)

## define theme I like ---------------------------------------------------------

myTheme <- theme_bw() +  theme(
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_line(size = 0.1, colour = 'grey'),
  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
  strip.background = element_rect(colour = "white", fill = "white"),
  plot.title = element_text(hjust = 0.5, size = 14), panel.spacing = unit(2, "lines"),
  legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"),
  axis.text.x = element_text(margin = unit(rep(0.3, 4), "cm")),
  axis.text.y = element_text(margin = unit(rep(0.3, 4), "cm")),
  axis.text.y.right = element_text(margin = unit(rep(0.3, 4), "cm"))
)


outfile_figpath <- "pi_figs/pretty/pi_all_cohorts_ei_pooled_extended_taxable.png"

ggplot(plot.dt, mapping = aes(x = tt_event, y = mean_pi, color = tr_group)) +
  geom_line(size = .7) +
  geom_point(size = .8) +
  geom_vline(xintercept = 0, color = "red", linetype = "22", alpha = .5) +
  scale_x_continuous(expand = c(.01, -.05)) +
  labs(x = "Quarter", y = "Normalized Log Price Index", color = NULL) +
  myTheme +
  theme(
    legend.position = c(0.22, 0.85),
    legend.margin = margin(t=-.2, r=-1, b=-.2, l=0, unit="cm")
    )
ggsave(outfile_figpath, height = 120, width = 180, units = "mm")


