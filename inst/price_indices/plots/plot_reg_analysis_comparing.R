#' Author: John Bonney
#'
#' Plot regression output: taxable goods only, ever increase, comparing
#'     results across time-of-event and event-no-event specifications.

library(data.table)
library(ggplot2)
library(zoo)

setwd("C:/Users/John Bonney/Desktop/Magne_projects/sales_taxes/output/server")

res.ene <- fread("pi_data/regression_output/pi_ei_regression_res.csv")
res.ene[, control.type := "ene"]
res.toe <- fread("pi_data/regression_output/pi_ei_regression_res_time_of_event.csv")
res.toe[, control.type := "toe"]
res.all <- rbind(res.ene, res.toe)


## limit to events between -4 and 4 for now
res.all <- res.all[between(tt_event, -4, 4)]

res.cpricei <- res.all[outcome == "cpricei"]
res.tax <- res.all[outcome == "sales_tax", .(estimate, tt_event, ref_year, ref_quarter, product_module_code, control.type)]
setnames(res.tax, "estimate", "sales_tax_estimate")

res.cpricei <- merge(res.cpricei, res.tax,
                     by = c("tt_event", "ref_year", "ref_quarter", "product_module_code", "control.type"))
res.cpricei[, pt.estimate := estimate / sales_tax_estimate]
res.cpricei[sales_tax_estimate == 0, pt.estimate := NA]

agg.estimates <- res.cpricei[, list(agg.pi = weighted.mean(estimate, total_sales),
                                    agg.st = weighted.mean(sales_tax_estimate, total_sales),
                                    agg.sales = sum(total_sales)),
                             by = .(tt_event, control.type)]
agg.estimates[, agg.pt := agg.pi / agg.st]
agg.estimates <- rbind(agg.estimates,
                       data.table(tt_event = rep(-2, 2), agg.pt = rep(0, 2),
                                  agg.st = rep(0, 2), agg.pi = rep(0, 2),
                                  control.type = c("ene", "toe")),
                       fill = T)

ggplot(data = agg.estimates, mapping = aes(x = tt_event, y = agg.pi, color = control.type)) +
  geom_point(size = 3, alpha = .5) +
  geom_line(linetype = "55") +
  geom_point(aes(x = -2, y = 0), size = 3, color = "black") +
  theme_bw(base_size = 16) +
  labs(x = "Event time (quarters)", y = "Estimate", color = NULL) +
  geom_hline(yintercept = 0, color = "red", linetype = "55", alpha = .8) +
  scale_y_continuous(limits = c(-.005, .01)) +
  scale_color_discrete(breaks = c("ene", "toe"),
                       labels = c("Never treated control", "Future treated control")) +
  theme(legend.position = c(.2, .83),
        legend.margin = margin(t=-.2, r=0, b=-.2, l=0, unit="cm"))
ggsave("pi_figs/reg_results/cpricei_estimates_compared.png",
       height = 120, width = 180, units = "mm")

ggplot(data = agg.estimates, mapping = aes(x = tt_event, y = agg.st, color = control.type)) +
  geom_point(size = 3, alpha = .5) +
  geom_line(linetype = "55") +
  geom_point(aes(x = -2, y = 0), size = 3, color = "black") +
  theme_bw(base_size = 16) +
  labs(x = "Event time (quarters)", y = "Estimate", color = NULL) +
  geom_hline(yintercept = 0, color = "red", linetype = "55", alpha = .8) +
  scale_y_continuous(limits = c(-.0015, .01)) +
  scale_color_discrete(breaks = c("ene", "toe"),
                       labels = c("Never treated control", "Future treated control")) +
  theme(legend.position = c(.2, .83),
        legend.margin = margin(t=-.2, r=0, b=-.2, l=0, unit="cm"))
ggsave("pi_figs/reg_results/tax_estimates_compared.png",
       height = 120, width = 180, units = "mm")

ggplot(data = agg.estimates[between(tt_event, 0, 4)], mapping = aes(x = tt_event, y = agg.pt, color = control.type)) +
  geom_point(size = 3, alpha = .5) +
  geom_line(linetype = "55") +
  theme_bw(base_size = 16) +
  scale_y_continuous(breaks = seq(0, 1, .25)) +
  labs(x = "Event time (quarters)", y = "Estimate", color = NULL) +
  geom_hline(yintercept = 0, color = "red", linetype = "55", alpha = .8) +
  scale_color_discrete(breaks = c("ene", "toe"),
                       labels = c("Never treated control", "Future treated control")) +
  theme(legend.position = c(.2, .2),
        legend.margin = margin(t=-.2, r=0, b=-.2, l=0, unit="cm"))
ggsave("pi_figs/reg_results/passthrough_estimates_compared.png",
       height = 120, width = 180, units = "mm")

