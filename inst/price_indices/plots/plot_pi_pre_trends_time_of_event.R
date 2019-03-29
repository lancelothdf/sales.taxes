#' Author: John Bonney
#'
#' Plot **Laspeyres** price index pre-trends

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

################################################################################
########################### plots by event time ################################
################################################################################

## all goods, event time -------------------------------------------------------
et.all <- fread("pi_data/pi_allgoods_es_time_of_event.csv")

plot_breaks <- c("ed", "nc.ed", "ei", "nc.ei", "io", "nc.io")
plot_labels <- c("Ever decrease", "Future change (ever decrease)", "Ever increase",
                 "Future change (ever increase)", "Increase only", "Future change (increase only)")

et.abbr <- data.table(tr_group = plot_labels, tr_abbr = plot_breaks)
et.all <- et.all[et.abbr, on = "tr_group"]

## add informative labels (until the add_tr_count is fixed?)
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
         "Sales tax changes are any changes occurring between 2009 and 2013."))) +
  ggtitle("Price index by sales tax change (all goods)") +
  scale_y_continuous(breaks = seq(-.045, 0.03, .015), expand = c(0.005, 0.005)) +
  scale_x_continuous(breaks = seq(-8, 4, 2), expand = c(0.005, 0.005)) +
  scale_color_manual(name = NULL, breaks = plot_breaks, labels = plot_labels,
                     values = c("#F8766D", "#00BA38", "#619CFF",
                                "#F8766D", "#00BA38", "#619CFF")) +
  scale_linetype_manual(name = NULL, breaks = plot_breaks, labels = plot_labels,
                        values = c("solid", "solid", "solid",
                                   "11", "11", "11")) +
  geom_vline(xintercept = 0, color = "maroon", size = 0.8, alpha = 0.5, linetype = "dashed") +
  myTheme +
  theme(legend.position = c(0.2, 0.8), axis.ticks.length = unit(-0.15, "cm"))
all.event.plot

ggsave("pi_figs/pretty/pi_all_event_time_of_event.png",
       height = 120 * 1.2, width = 180 * 1.2, units = "mm")


## taxable goods, event time ---------------------------------------------------
et.taxable <- fread("pi_data/pi_taxable_es_time_of_event.csv")

plot_breaks <- c("ed", "nc.ed", "ei", "nc.ei", "io", "nc.io")
plot_labels <- c("Ever decrease", "Future change (ever decrease)", "Ever increase",
                 "Future change (ever increase)", "Increase only", "Future change (increase only)")

et.abbr <- data.table(tr_group = plot_labels, tr_abbr = plot_breaks)
et.taxable <- et.taxable[et.abbr, on = "tr_group"]

## add informative labels (until the add_tr_count is fixed?)
for (i in 1:length(plot_labels)) {
  if (!grepl("\\(", plot_labels[i])) {
    new_lab <- paste0(
      plot_labels[i], " (n = ",
      mean(et.taxable[tr_group == plot_labels[i], .(n_counties)][[1]]), ")"
    )
    plot_labels[i] <- new_lab
  }
}

taxable.event.plot <- ggplot(et.taxable, aes(x = tt_event, y = mean_pi,
                                             color = tr_abbr, linetype = tr_abbr)) +
  geom_line(size = 1) +
  labs(x = "Quarters from event time",
       y = expression(paste("Normalized ln(", italic("price index"), ")")),
       color = NULL,
       caption = expression(paste(
         italic("Note: "),"Weighted by sales in 2008 Q1. ",
         "Sales tax changes are any changes occurring between 2009 and 2013."))) +
  ggtitle("Price index by sales tax change (taxable goods)") +
  scale_y_continuous(breaks = seq(-.045, 0.03, .015), expand = c(0.005, 0.005)) +
  scale_x_continuous(breaks = seq(-8, 4, 2), expand = c(0.005, 0.005)) +
  scale_color_manual(name = NULL, breaks = plot_breaks, labels = plot_labels,
                     values = c("#F8766D", "#00BA38", "#619CFF",
                                "#F8766D", "#00BA38", "#619CFF")) +
  scale_linetype_manual(name = NULL, breaks = plot_breaks, labels = plot_labels,
                        values = c("solid", "solid", "solid",
                                   "11", "11", "11")) +
  geom_vline(xintercept = 0, color = "maroon", size = 0.8, alpha = 0.5, linetype = "dashed") +
  myTheme +
  theme(legend.position = c(0.2, 0.8), axis.ticks.length = unit(-0.15, "cm"))
taxable.event.plot

ggsave("pi_figs/pretty/pi_taxable_event_time_of_event.png",
       height = 120 * 1.2, width = 180 * 1.2, units = "mm")

## tax exempt goods, event time ------------------------------------------------
et.taxexempt <- fread("pi_data/pi_taxexempt_es_time_of_event.csv")

plot_breaks <- c("ed", "nc.ed", "ei", "nc.ei", "io", "nc.io")
plot_labels <- c("Ever decrease", "Future change (ever decrease)", "Ever increase",
                 "Future change (ever increase)", "Increase only", "Future change (increase only)")

et.abbr <- data.table(tr_group = plot_labels, tr_abbr = plot_breaks)
et.taxexempt <- et.taxexempt[et.abbr, on = "tr_group"]

## add informative labels (until the add_tr_count is fixed?)
for (i in 1:length(plot_labels)) {
  if (!grepl("\\(", plot_labels[i])) {
    new_lab <- paste0(
      plot_labels[i], " (n = ",
      mean(et.taxexempt[tr_group == plot_labels[i], .(n_counties)][[1]]), ")"
    )
    plot_labels[i] <- new_lab
  }
}

taxexempt.event.plot <- ggplot(et.taxexempt, aes(x = tt_event, y = mean_pi,
                                             color = tr_abbr, linetype = tr_abbr)) +
  geom_line(size = 1) +
  labs(x = "Quarters from event time",
       y = expression(paste("Normalized ln(", italic("price index"), ")")),
       color = NULL,
       caption = expression(paste(
         italic("Note: "),"Weighted by sales in 2008 Q1. ",
         "Sales tax changes are any changes occurring between 2009 and 2013."))) +
  ggtitle("Price index by sales tax change (tax-exempt goods)") +
  scale_y_continuous(breaks = seq(-.05, 0.05, .025), expand = c(0.005, 0.005)) +
  scale_x_continuous(breaks = seq(-8, 4, 2), expand = c(0.005, 0.005)) +
  scale_color_manual(name = NULL, breaks = plot_breaks, labels = plot_labels,
                     values = c("#F8766D", "#00BA38", "#619CFF",
                                "#F8766D", "#00BA38", "#619CFF")) +
  scale_linetype_manual(name = NULL, breaks = plot_breaks, labels = plot_labels,
                        values = c("solid", "solid", "solid",
                                   "11", "11", "11")) +
  geom_vline(xintercept = 0, color = "maroon", size = 0.8, alpha = 0.5, linetype = "dashed") +
  myTheme +
  theme(legend.position = c(0.2, 0.8), axis.ticks.length = unit(-0.15, "cm"))
taxexempt.event.plot

ggsave("pi_figs/pretty/pi_taxexempt_event_time_of_event.png",
       height = 120 * 1.2, width = 180 * 1.2, units = "mm")

################################################################################
##################### event time plots by taxability ###########################
################################################################################

## taxable vs. non-taxable, ever decrease --------------------------------------
et.taxable[, taxable := "Taxable"]
et.taxexempt[, taxable := "Tax-exempt"]
et.tax <- rbind(et.taxable, et.taxexempt)
et.tax[, taxable := ifelse(grepl("\\(", tr_group),
                           paste(taxable, "(future change)"),
                           taxable)]
et.tax[, normalized_tax := mean_tax - mean_tax[tt_event == -2], .(tr_group, taxable)]
# I remove tax rates for events before -4, since we only have tax rates from -4 to 4
# for all counties (<-4 is too early to observe for all)
et.tax[, normalized_tax := ifelse(tt_event < -4, NA, normalized_tax)]
## we only want to plot -4 to 4
et.tax <- et.tax[between(tt_event, -4, 4)]

et.decrease <- et.tax[tr_group %in% c("Ever decrease",
                                      "Future change (ever decrease)")]
tax.decrease <- et.decrease[taxable == "Taxable", .(tr_group, normalized_tax,
                                                    tt_event)]
futchange.decrease <- et.decrease[taxable == "Taxable (future change)",
                                 .(tt_event, mean_pi)]
tax.decrease <- merge(tax.decrease, futchange.decrease)
tax.decrease[, mean_pi := normalized_tax + mean_pi]
tax.decrease[, taxable := "future change mean + log tax"]

et.decrease <- rbind(et.decrease, tax.decrease, fill = T)

plot_breaks <- c("Taxable", "Taxable (future change)",
                 "Tax-exempt", "Tax-exempt (future change)",
                 "future change mean + log tax")

plot_labs <- c("Taxable (ever decrease)", "Taxable (future change)",
               "Tax-exempt (ever decrease)", "Tax-exempt (future change)",
               "Taxable (future change) + ln(1 + tax rate)")

decrease.event.plot <- ggplot(et.decrease, aes(x = tt_event, y = mean_pi,
                                               color = taxable, linetype = taxable)) +
  geom_line(size = 1) +
  labs(x = "Quarters from event time",
       y = expression(paste("Normalized ln(", italic("price index"), ")")),
       color = NULL,
       caption = expression(paste(
         italic("Note: "),"Weighted by sales in 2008 Q1. ",
         "Sales tax changes are any changes occurring between 2009 and 2013."))) +
  ggtitle("Price index by taxability") +
  scale_y_continuous(breaks = seq(-.03, 0.045, .015), expand = c(0.005, 0.005)) +
  scale_x_continuous(breaks = seq(-4, 4, 1), expand = c(0.005, 0.005)) +
  scale_color_manual(name = NULL, breaks = plot_breaks, labels = plot_labs,
                     values = c("grey", "#F8766D", "#F8766D", "#00BFC4", "#00BFC4")) +
  scale_linetype_manual(name = NULL, breaks = plot_breaks, labels = plot_labs,
                        values = c("dotdash", "solid", "11", "solid", "11")) +
  geom_vline(xintercept = 0, color = "maroon", size = 0.8, alpha = 0.5, linetype = "dashed") +
  myTheme +
  theme(legend.position = c(0.2, 0.8), axis.ticks.length = unit(-0.15, "cm"))
decrease.event.plot

ggsave("pi_figs/pretty/pi_everdecrease_event_time_of_event.png",
       height = 120 * 1.2, width = 180 * 1.2, units = "mm")

## taxable vs. non-taxable, ever increase --------------------------------------
et.increase <- et.tax[tr_group %in% c("Ever increase",
                                      "Future change (ever increase)")]
tax.increase <- et.increase[taxable == "Taxable", .(tr_group, normalized_tax,
                                                    tt_event)]
nochange.increase <- et.increase[taxable == "Taxable (future change)",
                                 .(tt_event, mean_pi)]
tax.increase <- merge(tax.increase, nochange.increase)
tax.increase[, mean_pi := normalized_tax + mean_pi]
tax.increase[, taxable := "future change mean + log tax"]

et.increase <- rbind(et.increase, tax.increase, fill = T)

plot_breaks <- c("Taxable", "Taxable (future change)",
                 "Tax-exempt", "Tax-exempt (future change)",
                 "future change mean + log tax")

plot_labs <- c("Taxable (ever increase)", "Taxable (future change)",
               "Tax-exempt (ever increase)", "Tax-exempt (future change)",
               "Taxable (future change) + ln(1 + tax rate)")

increase.event.plot <- ggplot(et.increase, aes(x = tt_event, y = mean_pi,
                                               color = taxable, linetype = taxable)) +
  geom_line(size = 1) +
  labs(x = "Quarters from event time",
       y = expression(paste("Normalized ln(", italic("price index"), ")")),
       color = NULL,
       caption = expression(paste(
         italic("Note: "),"Weighted by sales in 2008 Q1. ",
         "Sales tax changes are any changes occurring between 2009 and 2013."))) +
  ggtitle("Price index by taxability") +
  scale_y_continuous(breaks = seq(-.02, 0.02, .01), expand = c(0.005, 0.005), limits = c(-.025, .025)) +
  scale_x_continuous(breaks = seq(-4, 4, 1), expand = c(0.005, 0.005)) +
  scale_color_manual(name = NULL, breaks = plot_breaks, labels = plot_labs,
                     values = c("grey", "#F8766D", "#F8766D", "#00BFC4", "#00BFC4")) +
  scale_linetype_manual(name = NULL, breaks = plot_breaks, labels = plot_labs,
                        values = c("dotdash", "solid", "11", "solid", "11")) +
  geom_vline(xintercept = 0, color = "maroon", size = 0.8, alpha = 0.5, linetype = "dashed") +
  myTheme +
  theme(legend.position = c(0.2, 0.8), axis.ticks.length = unit(-0.15, "cm"))
increase.event.plot

ggsave("pi_figs/pretty/pi_everincrease_event_time_of_event.png",
       height = 120 * 1.2, width = 180 * 1.2, units = "mm")

## taxable only (ever increase)
increase.event.plot <- ggplot(et.increase %>% filter(taxable %in% c("Taxable", "Taxable (future change)", "future change mean + log tax")),
                              aes(x = tt_event, y = mean_pi,
                                               color = taxable, linetype = taxable)) +
  geom_line(size = 1) +
  labs(x = "Quarters from event time",
       y = expression(paste("Normalized ln(", italic("price index"), ")")),
       color = NULL,
       caption = expression(paste(
         italic("Note: "),"Weighted by sales in 2008 Q1. ",
         "Sales tax changes are any changes occurring between 2009 and 2013."))) +
  # ggtitle("Price index by taxability") +
  scale_y_continuous(breaks = seq(-.02, 0.02, .01), expand = c(0, 0), limits = c(-.022, .022)) +
  scale_x_continuous(breaks = seq(-4, 4, 1), expand = c(0.005, 0.005)) +
  scale_color_manual(name = NULL, breaks = plot_breaks[c(1, 2, 5)], labels = plot_labs[c(1, 2, 5)],
                     values = c("grey", "#F8766D", "#F8766D", "#00BFC4", "#00BFC4")[c(1, 4, 5)]) +
  scale_linetype_manual(name = NULL, breaks = plot_breaks, labels = plot_labs,
                        values = c("dotdash", "solid", "11", "solid", "11")[c(1, 4, 5)]) +
  geom_vline(xintercept = 0, color = "maroon", size = 0.8, alpha = 0.5, linetype = "dashed") +
  myTheme +
  theme(legend.position = c(0.24, 0.84), axis.ticks.length = unit(-0.15, "cm"))
increase.event.plot

ggsave("pi_figs/pretty/pi_everincrease_taxable_time_of_event.png",
       height = 120, width = 180, units = "mm")

## tax-exempt only (ever increase)
increase.event.plot <- ggplot(et.increase %>%
                                filter(taxable %in% c("Tax-exempt", "Tax-exempt (future change)")),
                              aes(x = tt_event, y = mean_pi,
                                               color = taxable, linetype = taxable)) +
  geom_line(size = 1) +
  labs(x = "Quarters from event time",
       y = expression(paste("Normalized ln(", italic("price index"), ")")),
       color = NULL,
       caption = expression(paste(
         italic("Note: "),"Weighted by sales in 2008 Q1. ",
         "Sales tax changes are any changes occurring between 2009 and 2013."))) +
  # ggtitle("Price index by taxability") +
  scale_y_continuous(breaks = seq(-.02, 0.02, .01), expand = c(0, 0), limits = c(-.022, .022)) +
  scale_x_continuous(breaks = seq(-4, 4, 1), expand = c(0.005, 0.005)) +
  scale_color_manual(name = NULL, breaks = plot_breaks[3:4], labels = plot_labs[3:4],
                     values = c("grey", "#F8766D", "#F8766D", "#00BFC4", "#00BFC4")[2:3]) +
  scale_linetype_manual(name = NULL, breaks = plot_breaks, labels = plot_labs,
                        values = c("dotdash", "solid", "11", "solid", "11")[2:3]) +
  geom_vline(xintercept = 0, color = "maroon", size = 0.8, alpha = 0.5, linetype = "dashed") +
  myTheme +
  theme(legend.position = c(0.18, 0.87), axis.ticks.length = unit(-0.15, "cm"))
increase.event.plot

ggsave("pi_figs/pretty/pi_everincrease_taxexempt_time_of_event.png",
       height = 120, width = 180, units = "mm")

## taxable vs. non-taxable, increase only --------------------------------------
et.increase.only <- et.tax[tr_group %in% c("Increase only",
                                           "Future change (increase only)")]

tax.increase.only <- et.increase.only[taxable == "Taxable", .(tr_group, normalized_tax,
                                                    tt_event)]
nochange.increase.only <- et.increase.only[taxable == "Taxable (future change)",
                                           .(tt_event, mean_pi)]
tax.increase.only <- merge(tax.increase.only, nochange.increase.only)
tax.increase.only[, mean_pi := normalized_tax + mean_pi]
tax.increase.only[, taxable := "future change mean + log tax"]
et.increase.only <- rbind(et.increase.only, tax.increase.only, fill = T)

plot_breaks <- c("Taxable", "Taxable (future change)",
                 "Tax-exempt", "Tax-exempt (future change)", "future change mean + log tax")

plot_labs <- c("Taxable (increase only)", "Taxable (future change)",
               "Tax-exempt (increase only)", "Tax-exempt (future change)", "Taxable (future change) + ln(1 + tax rate)")

increase.only.plot <- ggplot(et.increase.only, aes(x = tt_event, y = mean_pi,
                                               color = taxable, linetype = taxable)) +
  geom_line(size = 1) +
  labs(x = "Quarters from event time",
       y = expression(paste("Normalized ln(", italic("price index"), ")")),
       color = NULL,
       caption = expression(paste(
         italic("Note: "),"Weighted by sales in 2008 Q1. ",
         "Sales tax changes are any changes occurring between 2009 and 2013."))) +
  ggtitle("Price index by taxability") +
  scale_y_continuous(limits = c(-0.07, 0.033), breaks = seq(-.06, 0.045, .03), expand = c(0.005, 0.005)) +
  scale_x_continuous(breaks = seq(-8, 4, 2), expand = c(0.005, 0.005)) +
  scale_color_manual(name = NULL, breaks = plot_breaks, labels = plot_labs,
                     values = c("grey", "#F8766D", "#F8766D", "#00BFC4", "#00BFC4")) +
  scale_linetype_manual(name = NULL, breaks = plot_breaks, labels = plot_labs,
                        values = c("dotdash", "solid", "11", "solid", "11")) +
  geom_vline(xintercept = 0, color = "maroon", size = 0.8, alpha = 0.5, linetype = "dashed") +
  myTheme +
  theme(legend.position = c(0.2, 0.8), axis.ticks.length = unit(-0.15, "cm"))
increase.only.plot

ggsave("pi_figs/pretty/pi_increaseonly_event_time_of_event.png",
       height = 120 * 1.2, width = 180 * 1.2, units = "mm")
