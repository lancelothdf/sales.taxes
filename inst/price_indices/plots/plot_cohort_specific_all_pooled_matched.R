#' Author: John Bonney
#'
#' Plot the cohort-specific trends (across tax-change cohorts) for the 2009 Q1
#' group (data obtained from cohort_specific_2009Q1.R)

setwd("C:/Users/John Bonney/Desktop/Magne_projects/sales_taxes")

library(tidyverse)
library(zoo)
library(grid)

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

data_path <- "output/server/pi_data/ever_increase/pi_all_cohorts_ei_pooled_taxable_matched.csv"
outfile_figpath <- "reports/figs/pi_all_cohorts_ei_pooled_taxable_matched.png"

dt <- read.csv(data_path)

ggplot(dt, mapping = aes(x = tt_event, y = mean_pi, color = tr_group)) +
  geom_line(size = .7) +
  geom_point(size = .8) +
  geom_vline(xintercept = 0, color = "red", linetype = "22", alpha = .5) +
  theme_bw() +
  scale_x_continuous(expand = c(.01, -.05)) +
  labs(x = "Quarter", y = "Normalized Log Price Index", color = NULL) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(size = 0.1, colour = 'grey'),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(colour="white", fill="white"),
    panel.spacing = unit(2, "lines"),
    panel.border = border_custom(),
    axis.ticks.length=unit(-0.15, "cm"),
    legend.position = c(0.22, 0.85),
    legend.margin = margin(t=-.2, r=-1, b=-.2, l=0, unit="cm"),
    axis.text.x = element_text(margin=unit(rep(0.3, 4), "cm")),
    axis.text.y = element_text(margin=unit(rep(0.3, 4), "cm"))
  )

ggsave(outfile_figpath, height = 120, width = 180, units = "mm")
