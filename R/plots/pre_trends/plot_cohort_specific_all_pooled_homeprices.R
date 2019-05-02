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

data_path <- "output/server/pi_data/ever_increase/homeprice_all_cohorts_ei_pooled.csv"
outfile_figpath <- "reports/figs/homeprice_all_cohorts_ei_pooled.png"
outfile_figpath2 <- "reports/figs/homeprice_all_cohorts_ei_pooled_normalized.png"

dt <- read.csv(data_path)

ggplot(dt, mapping = aes(x = tt_event, y = mean_homeprice, color = tr_group)) +
  geom_line(size = .7) +
  geom_point(size = .8) +
  geom_vline(xintercept = 0, color = "red", linetype = "22", alpha = .5) +
  theme_bw(base_size = 14) +
  scale_x_continuous(expand = c(.01, -.05), breaks = seq(-24, 12, 6)) +
  scale_y_continuous(limits = c(11.8, 12.42)) +
  labs(x = "Month", y = "Normalized Log Home Price", color = NULL) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(size = 0.1, colour = 'grey'),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(colour="white", fill="white"),
    panel.spacing = unit(2, "lines"),
    panel.border = border_custom(),
    legend.position = c(0.3, 0.16),
    axis.ticks.length=unit(-0.15, "cm"),
    legend.margin = margin(t=-.4, r=0, b=0, l=0, unit="cm"),
    axis.text.x = element_text(margin=unit(rep(0.3, 4), "cm")),
    axis.text.y = element_text(margin=unit(rep(0.3, 4), "cm"))
  )
ggsave(outfile_figpath, height = 120, width = 180, units = "mm")

dt.norm <- dt %>%
  group_by(tr_group) %>%
  mutate(mean_homeprice = mean_homeprice - mean_homeprice[tt_event == -6])

ggplot(dt.norm, mapping = aes(x = tt_event, y = mean_homeprice, color = tr_group)) +
  geom_line(size = .7) +
  geom_point(size = .8) +
  geom_vline(xintercept = 0, color = "red", linetype = "22", alpha = .5) +
  theme_bw(base_size = 14) +
  scale_x_continuous(expand = c(.01, -.05), breaks = seq(-24, 12, 6)) +
  labs(x = "Month", y = "Normalized Log Home Price", color = NULL) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(size = 0.1, colour = 'grey'),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(colour="white", fill="white"),
    panel.spacing = unit(2, "lines"),
    panel.border = border_custom(),
    legend.position = c(0.29, 0.16),
    axis.ticks.length=unit(-0.15, "cm"),
    legend.margin = margin(t=-1, r=0, b=-.2, l=0, unit="cm"),
    axis.text.x = element_text(margin=unit(rep(0.3, 4), "cm")),
    axis.text.y = element_text(margin=unit(rep(0.3, 4), "cm"))
  )
ggsave(outfile_figpath2, height = 120, width = 180, units = "mm")
