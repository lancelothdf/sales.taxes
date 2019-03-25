#' Author: John Bonney
#'
#' Plot regression output: taxable goods only, ever increase, control group is
#'   the group of never treated counties.

library(data.table)
library(ggplot2)
library(zoo)

setwd("C:/Users/John Bonney/Desktop/Magne_projects/sales_taxes/output/server")

res.all <- fread("pi_data/regression_output/pi_ei_regression_res_time_of_event.csv")
# res.all <- fread("pi_data/regression_output/pi_ei_regression_res_taxexempt.csv")

## we want to create two plots: one plotting ATTs of the pass-through, one
## plotting the distribution of treatment effects at t=0 (and maybe t=4)?

## limit to events between -4 and 4 for now
res.all <- res.all[between(tt_event, -4, 4)]

res.cpricei <- res.all[outcome == "cpricei"]
res.tax <- res.all[outcome == "sales_tax", .(estimate, tt_event, ref_year, ref_quarter, product_module_code)]
setnames(res.tax, "estimate", "sales_tax_estimate")

res.cpricei <- merge(res.cpricei, res.tax,
                     by = c("tt_event", "ref_year", "ref_quarter", "product_module_code"))
res.cpricei[, pt.estimate := estimate / sales_tax_estimate]
res.cpricei[sales_tax_estimate == 0, pt.estimate := NA]

agg.estimates <- res.cpricei[, list(agg.pi = weighted.mean(estimate, total_sales),
                                    agg.st = weighted.mean(sales_tax_estimate, total_sales),
                                    agg.sales = sum(total_sales)),
                             by = .(tt_event)]
agg.estimates[, agg.pt := agg.pi / agg.st]

# agg.estimates <- res.cpricei[, list(agg.st = weighted.mean(sales_tax_estimate, total_sales),
#                                     agg.pi = weighted.mean(estimate, total_sales),
#                                     agg.sales = sum(total_sales)),
#                              by = .(product_module_code, tt_event)]
# agg.estimates[, agg.pt := agg.pi / agg.st]
# agg.estimates <- agg.estimates[, list(agg.st = weighted.mean(agg.st, agg.sales),
#                                       agg.pi = weighted.mean(agg.pi, agg.sales),
#                                       agg.pt = weighted.mean(agg.pt, agg.sales)),
#                                by = tt_event]
agg.estimates <- rbind(agg.estimates,
                       data.table(tt_event = -2, agg.pt = 0, agg.st = 0, agg.pi = 0), fill = T)


ggplot(data = agg.estimates, mapping = aes(x = tt_event, y = agg.pi)) +
  geom_point(size = 3, alpha = .5) +
  geom_line(linetype = "55") +
  geom_point(aes(x = -2, y = 0), size = 3, color = "black") +
  theme_bw(base_size = 16) +
  labs(x = "Event time (quarters)", y = "Estimate", color = NULL) +
  geom_hline(yintercept = 0, color = "red", linetype = "55", alpha = .8) +
  scale_y_continuous(limits = c(-.005, .01))
ggsave("pi_figs/reg_results/cpricei_estimates_time_of_event.png",
       height = 120, width = 180, units = "mm")

ggplot(data = agg.estimates, mapping = aes(x = tt_event, y = agg.st)) +
  geom_point(size = 3, alpha = .5) +
  geom_line(linetype = "55") +
  geom_point(aes(x = -2, y = 0), size = 3, color = "black") +
  theme_bw(base_size = 16) +
  labs(x = "Event time (quarters)", y = "Estimate", color = NULL) +
  geom_hline(yintercept = 0, color = "red", linetype = "55", alpha = .8) +
  scale_y_continuous(limits = c(-.0015, .01))
ggsave("pi_figs/reg_results/tax_estimates_time_of_event.png",
       height = 120, width = 180, units = "mm")

ggplot(data = agg.estimates[between(tt_event, 0, 4)], mapping = aes(x = tt_event, y = agg.pt)) +
  geom_point(size = 3, alpha = .5) +
  geom_line(linetype = "55") +
  theme_bw(base_size = 16) +
  scale_y_continuous(breaks = seq(0, 1, .25), limits = c(0, 1)) +
  labs(x = "Event time (quarters)", y = "Estimate", color = NULL) +
  geom_hline(yintercept = 0, color = "red", linetype = "55", alpha = .8)
ggsave("pi_figs/reg_results/passthrough_estimates_time_of_event.png",
       height = 120, width = 180, units = "mm")
