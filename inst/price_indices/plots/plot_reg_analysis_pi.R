#' Author: John Bonney
#'
#' Plot regression output: taxable goods only, ever increase, control group is
#'   the group of never treated counties.

library(data.table)
library(ggplot2)
library(zoo)

setwd("C:/Users/John Bonney/Desktop/Magne_projects/sales_taxes/output/server")

res.all <- fread("pi_data/regression_output/pi_ei_regression_res.csv")
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

## nontaxable
ggplot(data = agg.estimates, mapping = aes(x = tt_event, y = agg.pi)) +
  geom_point(size = 3, alpha = .5) +
  geom_line(linetype = "55") +
  geom_point(aes(x = -2, y = 0), size = 3, color = "black") +
  theme_bw(base_size = 16) +
  labs(x = "Event time (quarters)", y = "Estimate", color = NULL) +
  geom_hline(yintercept = 0, color = "red", linetype = "55", alpha = .8) +
  scale_y_continuous(breaks = seq(-.0075, .0025, .0025))
ggsave("pi_figs/reg_results/cpricei_estimates_taxexempt.png",
       height = 120, width = 180, units = "mm")

ggplot(data = agg.estimates, mapping = aes(x = tt_event, y = agg.pi)) +
  geom_point(size = 3, alpha = .5) +
  geom_line(linetype = "55") +
  geom_point(aes(x = -2, y = 0), size = 3, color = "black") +
  theme_bw(base_size = 16) +
  labs(x = "Event time (quarters)", y = "Estimate", color = NULL) +
  geom_hline(yintercept = 0, color = "red", linetype = "55", alpha = .8) +
  scale_y_continuous(limits = c(-.0015, .01))
ggsave("pi_figs/reg_results/cpricei_estimates.png",
       height = 120, width = 180, units = "mm")

ggplot(data = res.cpricei[tt_event == 0 & between(estimate, -.1, .1)], aes(x = estimate)) +
  geom_histogram(binwidth = .001) +
  geom_vline(xintercept = 0, color = "red") +
  theme_bw() +
  labs(x = "Estimate at onset time (tax increase on price index)",
       y = "Number of cohort-by-product estimates")
ggsave("pi_figs/reg_results/pi_estimates_t0_hist.png",
       height = 120, width = 180, units = "mm")

ggplot(data = res.cpricei[tt_event == 0 & between(estimate, -.1, .1)], aes(x = estimate, weight = (total_sales / sum(total_sales)))) +
  geom_density() +
  geom_vline(xintercept = 0, color = "red") +
  theme_bw() +
  labs(x = "Estimate at onset time (tax increase on price index)",
       y = "Density of cohort-by-product estimates")
ggsave("pi_figs/reg_results/pi_estimates_t0_density.png",
       height = 120, width = 180, units = "mm")

ggplot(data = res.cpricei[tt_event %in% c(0,4) & between(estimate, -.2, .2)],
       aes(x = estimate, fill = factor(tt_event), weight = (total_sales / 10000))) +
  geom_histogram(binwidth = .001, alpha = .8) +
  geom_vline(xintercept = 0, color = "red") +
  theme_bw() +
  labs(x = "Estimate (tax increase on price index)",
       y = "Number of cohort-by-product estimates", fill = NULL) +
  scale_fill_discrete(breaks = c("0", "4"),
                      labels = c("At time of tax change", "One year after tax change")) +
  theme(legend.position = c(.8, .8))
ggsave("pi_figs/reg_results/pi_estimates_comparing_hist_wtd.png",
       height = 120, width = 180, units = "mm")

ggplot(data = res.cpricei[tt_event == 4 & between(estimate, -.2, .2)], aes(x = estimate)) +
  geom_histogram(binwidth = .001) +
  geom_vline(xintercept = 0, color = "red") +
  theme_bw() +
  labs(x = "Estimate one year after onset (tax increase on price index)",
       y = "Number of cohort-by-product estimates")
ggsave("pi_figs/reg_results/pi_estimates_t4_hist.png",
       height = 120, width = 180, units = "mm")

ggplot(data = agg.estimates, mapping = aes(x = tt_event, y = agg.st)) +
  geom_point(size = 3, alpha = .5) +
  geom_line(linetype = "55") +
  geom_point(aes(x = -2, y = 0), size = 3, color = "black") +
  theme_bw(base_size = 16) +
  labs(x = "Event time (quarters)", y = "Estimate", color = NULL) +
  geom_hline(yintercept = 0, color = "red", linetype = "55", alpha = .8) +
  scale_y_continuous(limits = c(-.0015, .01))
ggsave("pi_figs/reg_results/tax_estimates.png",
       height = 120, width = 180, units = "mm")

ggplot(data = res.cpricei[tt_event == 0 & between(sales_tax_estimate, -.5, .5)], aes(x = sales_tax_estimate)) +
  geom_histogram(binwidth = .001)

ggplot(data = agg.estimates[between(tt_event, 0, 4)], mapping = aes(x = tt_event, y = agg.pt)) +
  geom_point(size = 3, alpha = .5) +
  geom_line(linetype = "55") +
  theme_bw(base_size = 16) +
  labs(x = "Event time (quarters)", y = "Estimate", color = NULL) +
  geom_hline(yintercept = 0, color = "red", linetype = "55", alpha = .8)
ggsave("pi_figs/reg_results/passthrough_estimates2.png",
       height = 120, width = 180, units = "mm")

ggplot(res.compare, aes(x = ref_event_time, y = estimate, color = req)) +
  geom_point(size = 3, alpha = .5) +
  geom_line(linetype = "55") +
  geom_point(aes(x = omitted_event_time, y = 0), size = 3, color = "black") +
  scale_color_discrete(breaks = c(description1, description2, description3, description4),
                       labels = c(label1, label2, label3, label4)) +
  theme_bw(base_size = 16) +
  labs(x = "Event Time", y = "Estimate", color = NULL) +
  geom_hline(yintercept = 0, color = "red", linetype = "55", alpha = .8)
