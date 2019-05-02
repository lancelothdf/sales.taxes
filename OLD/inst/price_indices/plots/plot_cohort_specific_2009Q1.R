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

data_path <- "output/server/pi_data/pi_2009Q1_cohorts.csv"
outfile_figpath <- "reports/figs/pi_2009Q1_cohorts.png"

dt <- read.csv(data_path)

dt <- dt %>% select(year, quarter, cohort, cpricei, cohort_size) %>%
  filter(!is.na(year)) %>% filter(!is.na(cpricei))

dt$t <- as.yearqtr(paste0(dt$year, " Q", dt$quarter))

ggplot(dt, mapping = aes(x = t, y = cpricei, color = cohort)) +
  geom_line(size = .7) +
  geom_point(size = .8) +
  geom_vline(xintercept = 2009, color = "red", linetype = 1, alpha = .5) +
  geom_vline(xintercept = 2010, color = "lightgrey", linetype = 2) +
  geom_vline(xintercept = 2011, color = "lightgrey", linetype = 2) +
  geom_vline(xintercept = 2013, color = "lightgrey", linetype = 2) +
  geom_vline(xintercept = 2014, color = "lightgrey", linetype = 2) +
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

ggsave(outfile_figpath, height = 120, width = 180, units = "mm")

