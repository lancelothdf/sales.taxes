#' Author: John Bonney
#'
#' Plot price index pre-trends

library(data.table)
library(ggplot2)
library(zoo)

setwd("C:/Users/John Bonney/Desktop/Magne_projects/sales_taxes/output/server")

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

## all goods, calendar time ----------------------------------------------------

ct.all <- fread("pi_data/pi_all_calendar.csv")
ct.all$year_qtr <- as.yearqtr(paste(
  as.integer(ct.all$year), as.integer(ct.all$quarter)
), "%Y %q")
ct.all[, tr_count := gsub("=", " = ", tr_count)]

all.calendar.plot <- ggplot(data = ct.all, mapping = aes(x = year_qtr,
                                                         y = mean.cpricei,
                                                         color = tr_count)) +
  geom_line(size = 1) +
  labs(x = "Quarter", y = expression(paste("Normalized ln(", italic("price index"), ")")), color = NULL,
       caption = expression(paste(italic("Note: "), "Weighted by sales in 2008 Q1. ",
                                  "Sales tax changes are any changes occuring between 2009 and 2013."))) +
  ggtitle("Price index by sales tax change (all goods)") +
  scale_x_yearqtr(format = "%Y Q%q", expand = c(0.01, 0.01)) +
  scale_y_continuous(breaks = seq(0, 0.13, .04), expand = c(0.005, 0.005)) +
  myTheme +
  theme(legend.position = c(0.8, 0.2), axis.ticks.length = unit(-0.15, "cm"))
all.calendar.plot

ggsave("pi_figs/pretty/pi_all_calendar.png", height = 120, width = 180, units = "mm")

## taxable goods, calendar time ------------------------------------------------

ct.taxable <- fread("pi_data/taxable_pi_collapsed.csv")
ct.taxable$year_qtr <- as.yearqtr(paste(
  as.integer(ct.taxable$year), as.integer(ct.taxable$quarter)
), "%Y %q")
ct.taxable[, tr_count := gsub("=", " = ", tr_count)]

taxable.calendar.plot <- ggplot(data = ct.taxable,
                                aes(x = year_qtr, y = mean.cpricei, color = tr_count)) +
  geom_line(size = 1) +
  labs(x = "Quarter", y = expression(paste("Normalized ln(", italic("price index"), ")")), color = NULL,
       caption = expression(paste(italic("Note: "), "Weighted by sales in 2008 Q1. ",
                                  "Sales tax changes are any changes occuring between 2009 and 2013."))) +
  ggtitle("Price index by sales tax change (taxable goods)") +
  scale_x_yearqtr(format = "%Y Q%q", expand = c(0.01, 0.01)) +
  scale_y_continuous(breaks = seq(0, 0.13, .04), expand = c(0.005, 0.005)) +
  myTheme +
  theme(legend.position = c(0.8, 0.2), axis.ticks.length = unit(-0.15, "cm"))
taxable.calendar.plot

ggsave("pi_figs/pretty/pi_taxable_calendar.png", height = 120, width = 180, units = "mm")

## all goods, event time -------------------------------------------------------
et.all <- fread("pi_data/pi_allgoods_es.csv")

plot_breaks <- c("ed", "nc.ed", "ei", "nc.ei", "io", "nc.io")
plot_labels <- c("Ever decrease", "No change (Ever decrease)", "Ever increase",
                 "No change (Ever increase)", "Increase only", "No change (Increase only)")

et.all.abbr <- data.table(tr_group = plot_labels, tr_abbr = plot_breaks)
et.all <- et.all[et.all.abbr, on = "tr_group"]

for (i in 1:length(plot_labels)) {
  if (!grepl("\\(", plot_labels[i])) {
    new_lab <- paste0(
      plot_labels[i], " (n = ",
      mean(et.all[tr_group == plot_labels[i], .(n_counties)][[1]]), ")"
      )
    plot_labels[i] <- new_lab
  }
}

all.event.plot <- ggplot(et.all, aes(x = tt_event, y = mean_pi,
                                     color = tr_abbr, linetype = tr_abbr)) +
  geom_line(size = 1) +
  labs(x = "Quarters from event time",
       y = expression(paste("Normalized ln(", italic("price index"), ")")),
       color = NULL,
       caption = expression(paste(
         italic("Note: "),"Weighted by sales in 2008 Q1. ",
         "Sales tax changes are any changes occuring between 2009 and 2013."))) +
  ggtitle("Price index by sales tax change (all goods)") +
  scale_y_continuous(breaks = seq(-.03, 0.03, .015), expand = c(0.005, 0.005)) +
  scale_color_manual(name = NULL, breaks = plot_breaks, labels = plot_labels,
                     values = c("#F8766D", "#00BA38", "#619CFF",
                                "#F8766D", "#00BA38", "#619CFF")) +
  scale_linetype_manual(name = NULL, breaks = plot_breaks, labels = plot_labels,
                        values = c("solid", "solid", "solid",
                                   "11", "11", "11")) +
  geom_vline(xintercept = 0, color = "maroon", size = 0.8, alpha = 0.5, linetype = "dashed") +
  myTheme +
  theme(legend.position = c(0.8, 0.2), axis.ticks.length = unit(-0.15, "cm"))
all.event.plot

ggsave("pi_figs/pretty/pi_all_event.png",
       height = 120*1.2, width = 180*1.2, units = "mm")


## taxable goods, event time ---------------------------------------------------

## tax exempt goods, event time ------------------------------------------------



