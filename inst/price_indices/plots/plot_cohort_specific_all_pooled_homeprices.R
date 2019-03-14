#' Author: John Bonney
#'
#' Plot the pooled cohort-specific trends for quantity.

setwd("C:/Users/John Bonney/Desktop/Magne_projects/sales_taxes")

library(tidyverse)
library(zoo)
library(grid)
library(data.table)

element_grob.element_custom <- function(element, ...)  {

  segmentsGrob(c(1,0,0),
               c(0,0,1),
               c(0,0,0),
               c(0,1,1), gp=gpar(lwd=2))
}
## silly wrapper to fool ggplot2
border_custom <- function(...){
  structure(
    list(...), # this ... information is not used
    class = c("element_custom","element_blank", "element") # inheritance test workaround
  )

}

data_path <- "output/server/pi_data/homeprice_all_cohorts_pooled_extended2.csv"
outfile_figpath <- "reports/figs/homeprice_all_cohorts_pooled_extended2.png"

dt <- read.csv(data_path)

dt$t <- as.yearmon(paste0(dt$year, "-", dt$month))
dt$ref_t <- as.yearmon(paste0(dt$ref_year, "-", dt$ref_month))
dt$tt_ev <- round((dt$t - dt$ref_t) * 12)

dt <- as.data.table(dt)
dt.agg <- dt[between(tt_ev, -36, 12)]
dt.agg <- dt.agg[, homeprice := homeprice - homeprice[tt_ev == -6], by = .(group, ref_t)]
dt.agg <- dt.agg[, list(homeprice.agg = weighted.mean(homeprice, cohort_size)),
                 by = .(group, tt_ev)]

dt.test <- dt[between(tt_ev, -36, 12) & ref_t <= 2013.5]
dt.test <- dt.test[, homeprice := homeprice - homeprice[tt_ev == -6], by = .(group, ref_t)]
dt.test <- dt.test[, list(homeprice.agg = weighted.mean(homeprice, cohort_size)),
                   by = .(group, tt_ev)]
dt.test[group == "Future restricted", group := "Future (over one year)"]

ggplot(dt.test, mapping = aes(x = tt_ev, y = homeprice.agg, color = group)) +
  geom_line(size = .7) +
  geom_point(size = .8) +
  geom_vline(xintercept = 0, color = "red", linetype = "22", alpha = .5) +
  theme_bw() +
  scale_x_continuous(expand = c(.01, -.05)) +
  labs(x = "Month", y = "Normalized Home Price", color = "Cohort") +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(size = 0.1, colour = 'grey'),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(colour="white", fill="white"),
    panel.spacing = unit(2, "lines"),
    panel.border = border_custom(),
    legend.position = c(0.2, 0.2),
    axis.ticks.length=unit(-0.15, "cm"),
    legend.margin = margin(t=-.2, r=0, b=-.2, l=0, unit="cm"),
    axis.text.x = element_text(margin=unit(rep(0.3, 4), "cm")),
    axis.text.y = element_text(margin=unit(rep(0.3, 4), "cm"))
  )

ggsave(outfile_figpath, height = 120, width = 180, units = "mm")

for (pp in sort(unique(dt$ref_t))) {
  plot.dt <- dt %>%
    filter(ref_t == pp, between(tt_ev, -36, 12)) %>%
    group_by(group) %>%
    mutate(homeprice = homeprice - homeprice[tt_ev == -6])

  myplot <- ggplot(plot.dt, mapping = aes(x = t, y = homeprice, color = group)) +
    geom_line(size = .7) +
    geom_point(size = .8) +
    geom_vline(xintercept = pp, color = "red", linetype = 1, alpha = .5) +
    theme_bw() +
    scale_x_yearmon(format = "%Y %m", expand = c(.01, -.05)) +
    labs(x = "Month", y = "Normalized Price Index", color = "Cohort") +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(size = 0.1, colour = 'grey'),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(colour="white", fill="white"),
      panel.spacing = unit(2, "lines"),
      panel.border = border_custom(),
      legend.position = c(0.2, 0.8),
      axis.ticks.length=unit(-0.15, "cm"),
      legend.margin = margin(t=-.2, r=0, b=-.2, l=0, unit="cm"),
      axis.text.x = element_text(margin=unit(rep(0.3, 4), "cm")),
      axis.text.y = element_text(margin=unit(rep(0.3, 4), "cm"))
    )
  print(myplot)
  print(pp)
  readline(prompt="Press [enter] to continue")
}
# 2009 Q4
# 2012 Q2
# 2012 Q3
# 2012 Q4

ggplot(dt %>% filter(ref_t == 2010), mapping = aes(x = t, y = cpricei, color = group)) +
  geom_line(size = .7) +
  geom_point(size = .8) +
  geom_vline(xintercept = 2010, color = "red", linetype = 1, alpha = .5) +
  theme_bw() +
  scale_x_yearqtr(format = "%Y Q%q", limits = c(2008, 2014), expand = c(.01, -.05)) +
  labs(x = "Quarter", y = "Normalized Price Index", color = "Cohort") +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(size = 0.1, colour = 'grey'),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(colour="white", fill="white"),
    panel.spacing = unit(2, "lines"),
    panel.border = border_custom(),
    legend.position = c(0.68, 0.27),
    axis.ticks.length=unit(-0.15, "cm"),
    legend.margin = margin(t=-.2, r=0, b=-.2, l=0, unit="cm"),
    axis.text.x = element_text(margin=unit(rep(0.3, 4), "cm")),
    axis.text.y = element_text(margin=unit(rep(0.3, 4), "cm"))
  )

