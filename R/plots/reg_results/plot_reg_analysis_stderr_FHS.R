library(data.table)
library(ggplot2)
library(zoo)

setwd("C:/Users/John Bonney/Desktop/Magne_projects/sales_taxes/output/server")

## ever increase ---------------

## first, event-no-event

res.ei <- fread("pi_data/regression_output/with_stderr/Passthrough_estimates_stderr_ei_combined_FHS_homeprice.csv")

res.ei[, aggregated := rn == "post-treatment"]
res.ei[, rn := as.integer(gsub("catt", "", rn))]
setnames(res.ei, "rn", "tt_event")
res.ei[, tt_event := as.double(tt_event)]
res.ei[aggregated == T, tt_event := 5.5] # assign aggregate estimates tt_event = 5.5

res.ei.combined <- rbind(res.ei[, .(estimates, tt_event, outcome, aggregated)],
                         data.table(tt_event = rep(-2:-1, each=2),
                                    estimates = rep(0, 4),
                                    outcome = rep(c("cpricei", "sales_tax"), 2),
                                    aggregated = rep(F, 4)))

ggplot(data = res.ei.combined[outcome == "cpricei"],
       mapping = aes(x = tt_event, y = estimates, color = aggregated)) +
  geom_point(size = 2, alpha = .5) +
  geom_errorbar(data = res.ei[outcome == "cpricei"],
                aes(ymax = estimates + 1.96 * std.errors,
                    ymin = estimates - 1.96 * std.errors),
                width = .8) +
  geom_line(data = res.ei.combined[outcome == "cpricei" & aggregated == F],
            linetype = "55") +
  scale_color_manual(breaks = c(TRUE, FALSE), values = c("black", "firebrick")) +
  geom_point(aes(x = -2, y = 0), size = 2, color = "black") + # omitted (collinearity)
  geom_point(aes(x = -1, y = 0), size = 2, color = "black") + # omitted (used as instrument)
  theme_bw(base_size = 16) +
  scale_x_continuous(breaks = c(seq(-4, 4, 2), 5.5),
                     labels = c(as.character(seq(-4, 4, 2)), "Aggregate")) +
  labs(x = "Event time (quarters)", y = "Estimate", color = NULL) +
  geom_hline(yintercept = 0, color = "red", linetype = "55", alpha = .8) +
  scale_y_continuous(limits = c(-.005, .01), breaks = seq(-.005, .01, .0025)) +
  theme(legend.position = "none")
ggsave("pi_figs/reg_results/cpricei_estimates_ei_FHS_houseprice.png",
       height = 120, width = 180, units = "mm")

ggplot(data = res.ei.combined[outcome == "sales_tax"],
       mapping = aes(x = tt_event, y = estimates, color = aggregated)) +
  geom_point(size = 2, alpha = .5) +
  geom_errorbar(data = res.ei[outcome == "sales_tax"],
                aes(ymax = estimates + 1.96 * std.errors,
                    ymin = estimates - 1.96 * std.errors),
                width = .8) +
  geom_line(data = res.ei.combined[outcome == "sales_tax" & aggregated == F],
            linetype = "55") +
  scale_color_manual(breaks = c(TRUE, FALSE), values = c("black", "firebrick")) +
  geom_point(aes(x = -2, y = 0), size = 2, color = "black") +
  geom_point(aes(x = -2, y = 0), size = 2, color = "black") +
  theme_bw(base_size = 16) +
  scale_x_continuous(breaks = c(seq(-4, 4, 2), 5.5),
                     labels = c(as.character(seq(-4, 4, 2)), "Aggregate")) +
  labs(x = "Event time (quarters)", y = "Estimate", color = NULL) +
  geom_hline(yintercept = 0, color = "red", linetype = "55", alpha = .8) +
  scale_y_continuous(limits = c(-.004, .01), breaks = seq(-.005, .01, .0025)) +
  theme(legend.position = "none")
ggsave("pi_figs/reg_results/tax_estimates_ei_FHS_houseprice.png",
       height = 120, width = 180, units = "mm")

ggplot(data = res.ei.combined[outcome == "passthrough_1"],
       mapping = aes(x = tt_event, y = estimates, color = aggregated)) +
  geom_point(size = 2, alpha = .5) +
  geom_errorbar(data = res.ei[outcome == "passthrough_1"],
                aes(ymax = estimates + 1.96 * std.errors,
                    ymin = estimates - 1.96 * std.errors),
                width = .8) +
  geom_line(data = res.ei.combined[outcome == "passthrough_1" & aggregated == F],
            linetype = "55") +
  scale_color_manual(breaks = c(TRUE, FALSE), values = c("black", "firebrick")) +
  theme_bw(base_size = 16) +
  scale_x_continuous(breaks = c(seq(0, 4, 2), 5.5),
                     labels = c(as.character(seq(0, 4, 2)), "Aggregate")) +
  labs(x = "Time since reform (quarters)", y = "Estimate", color = NULL) +
  geom_hline(yintercept = 0, color = "red", linetype = "55", alpha = .8) +
  theme(legend.position = "none")
ggsave("pi_figs/reg_results/passthrough1_estimates_ei_FHS_houseprice.png",
       height = 120, width = 180, units = "mm")

## looking at sales
sales.ei <- fread("pi_data/regression_output/with_stderr/Sales_estimates_stderr_ei_FHS_homeprice.csv")
sales.ei <- sales.ei[outcome == "sales"]
sales.ei[, aggregated := rn == "post-treatment"]
sales.ei[, rn := as.integer(gsub("catt", "", rn))]
setnames(sales.ei, "rn", "tt_event")
sales.ei[, tt_event := as.double(tt_event)]
sales.ei[aggregated == T, tt_event := 5.5] # assign aggregate estimates tt_event = 5.5

sales.ei.combined <- rbind(sales.ei[, .(estimates, tt_event, outcome, aggregated)],
                         data.table(tt_event = -2:-1,
                                    estimates = 0,
                                    outcome = "sales",
                                    aggregated = F))

ggplot(data = sales.ei.combined,
       mapping = aes(x = tt_event, y = estimates, color = aggregated)) +
  geom_point(size = 2, alpha = .5) +
  geom_errorbar(data = sales.ei,
                aes(ymax = estimates + 1.96 * std.errors,
                    ymin = estimates - 1.96 * std.errors),
                width = .8) +
  geom_line(data = sales.ei.combined[aggregated == F],
            linetype = "55") +
  scale_color_manual(breaks = c(TRUE, FALSE), values = c("black", "firebrick")) +
  geom_point(aes(x = -2, y = 0), size = 2, color = "black") +
  geom_point(aes(x = -1, y = 0), size = 2, color = "black") +
  theme_bw(base_size = 16) +
  scale_x_continuous(breaks = c(seq(-4, 4, 2), 5.5),
                     labels = c(as.character(seq(-4, 4, 2)), "Aggregate")) +
  labs(x = "Event time (quarters)", y = "Estimate", color = NULL) +
  geom_hline(yintercept = 0, color = "red", linetype = "55", alpha = .8) +
  # scale_y_continuous(limits = c(-.004, .01), breaks = seq(-.005, .01, .0025)) +
  theme(legend.position = "none")
ggsave("pi_figs/reg_results/sales_estimates_FHS_houseprice.png",
       height = 120, width = 180, units = "mm")


### The following hasn't been adapted for the FHS method yet
### Looking at decreases ------------
res.ed <- fread("pi_data/regression_output/with_stderr/Passthrough_estimates_stderr_ed_combined.csv")

res.ed[, aggregated := rn == "post-treatment"]
# res.ed <- res.ed[aggregated == F]
res.ed[, rn := as.double(gsub("catt", "", rn))]
setnames(res.ed, "rn", "tt_event")
res.ed[aggregated == T, tt_event := 5.5]

# TODO: we want to remove aggregated from these "ever decrease" plots

res.ed.combined <- rbind(res.ed[, .(estimates, tt_event, outcome, aggregated)],
                         data.table(tt_event = rep(-2, 2),
                                    estimates = rep(0, 2),
                                    outcome = c("cpricei", "sales_tax"),
                                    aggregated = rep(F, 2)))

ggplot(data = res.ed.combined[outcome == "cpricei"],
       mapping = aes(x = tt_event, y = estimates, color = aggregated)) +
  geom_point(size = 2, alpha = .5) +
  geom_errorbar(data = res.ed[outcome == "cpricei"],
                aes(ymax = estimates + 1.96 * std.errors,
                    ymin = estimates - 1.96 * std.errors),
                width = .8) +
  geom_line(data = res.ed.combined[outcome == "cpricei" & aggregated == F],
            linetype = "55") +
  scale_color_manual(breaks = c(TRUE, FALSE), values = c("black", "firebrick")) +
  geom_point(aes(x = -2, y = 0), size = 2, color = "black") +
  theme_bw(base_size = 16) +
  scale_x_continuous(breaks = c(seq(-4, 4, 2), 5.5),
                     labels = c(as.character(seq(-4, 4, 2)), "Aggregate")) +
  labs(x = "Event time (quarters)", y = "Estimate", color = NULL) +
  geom_hline(yintercept = 0, color = "red", linetype = "55", alpha = .8) +
  scale_y_continuous(limits = c(-.013, .005), breaks = seq(-.010, .005, .005)) +
  theme(legend.position = "none")
ggsave("pi_figs/reg_results/cpricei_estimates_ed.png",
       height = 120, width = 180, units = "mm")

ggplot(data = res.ed.combined[outcome == "sales_tax"],
       mapping = aes(x = tt_event, y = estimates, color = aggregated)) +
  geom_point(size = 2, alpha = .5) +
  geom_errorbar(data = res.ed[outcome == "sales_tax"],
                aes(ymax = estimates + 1.96 * std.errors,
                    ymin = estimates - 1.96 * std.errors),
                width = .8) +
  geom_line(data = res.ed.combined[outcome == "sales_tax" & aggregated == F],
            linetype = "55") +
  scale_color_manual(breaks = c(TRUE, FALSE), values = c("black", "firebrick")) +
  geom_point(aes(x = -2, y = 0), size = 2, color = "black") +
  theme_bw(base_size = 16) +
  scale_x_continuous(breaks = c(seq(-4, 4, 2), 5.5),
                     labels = c(as.character(seq(-4, 4, 2)), "Aggregate")) +
  labs(x = "Event time (quarters)", y = "Estimate", color = NULL) +
  geom_hline(yintercept = 0, color = "red", linetype = "55", alpha = .8) +
  scale_y_continuous(limits = c(-.013, .005), breaks = seq(-.010, .005, .005)) +
  theme(legend.position = "none")
ggsave("pi_figs/reg_results/tax_estimates_ed.png",
       height = 120, width = 180, units = "mm")

ggplot(data = res.ed.combined[outcome == "passthrough_1"],
       mapping = aes(x = tt_event, y = estimates, color = aggregated)) +
  geom_point(size = 2, alpha = .5) +
  geom_errorbar(data = res.ed[outcome == "passthrough_1"],
                aes(ymax = estimates + 1.96 * std.errors,
                    ymin = estimates - 1.96 * std.errors),
                width = .8) +
  geom_line(data = res.ed.combined[outcome == "passthrough_1" & aggregated == F],
            linetype = "55") +
  scale_color_manual(breaks = c(TRUE, FALSE), values = c("black", "firebrick")) +
  theme_bw(base_size = 16) +
  scale_x_continuous(breaks = c(seq(0, 4, 2), 5.5),
                     labels = c(as.character(seq(0, 4, 2)), "Aggregate")) +
  labs(x = "Time since reform (quarters)", y = "Estimate", color = NULL) +
  geom_hline(yintercept = 0, color = "red", linetype = "55", alpha = .8) +
  theme(legend.position = "none")
ggsave("pi_figs/reg_results/passthrough1_estimates_ed.png",
       height = 120, width = 180, units = "mm")

## looking at sales
sales.ed <- fread("pi_data/regression_output/with_stderr/Sales_estimates_stderr_ed_combined.csv")
sales.ed <- sales.ed[outcome == "sales"]
sales.ed[, aggregated := rn == "post-treatment"]
sales.ed[, rn := as.integer(gsub("catt", "", rn))]
setnames(sales.ed, "rn", "tt_event")
sales.ed[, tt_event := as.double(tt_event)]
sales.ed[aggregated == T, tt_event := 5.5] # assign aggregate estimates tt_event = 5.5

sales.ed.combined <- rbind(sales.ed[, .(estimates, tt_event, outcome, aggregated)],
                           data.table(tt_event = -2,
                                      estimates = 0,
                                      outcome = "sales",
                                      aggregated = F))

ggplot(data = sales.ed.combined,
       mapping = aes(x = tt_event, y = estimates, color = aggregated)) +
  geom_point(size = 2, alpha = .5) +
  geom_errorbar(data = sales.ed,
                aes(ymax = estimates + 1.96 * std.errors,
                    ymin = estimates - 1.96 * std.errors),
                width = .8) +
  geom_line(data = sales.ed.combined[aggregated == F],
            linetype = "55") +
  scale_color_manual(breaks = c(TRUE, FALSE), values = c("black", "firebrick")) +
  geom_point(aes(x = -2, y = 0), size = 2, color = "black") +
  theme_bw(base_size = 16) +
  scale_x_continuous(breaks = c(seq(-4, 4, 2), 5.5),
                     labels = c(as.character(seq(-4, 4, 2)), "Aggregate")) +
  labs(x = "Event time (quarters)", y = "Estimate", color = NULL) +
  geom_hline(yintercept = 0, color = "red", linetype = "55", alpha = .8) +
  # scale_y_continuous(limits = c(-.004, .01), breaks = seq(-.005, .01, .0025)) +
  theme(legend.position = "none")
ggsave("pi_figs/reg_results/sales_estimates_ed.png",
       height = 120, width = 180, units = "mm")

# TODO: compare passthrough for ever increase, ever decrease

res.ed.combined
res.ei.combined <- res.ei.combined[aggregated == F & cont.type == "Never treated control",
                                   .(estimates, tt_event, outcome)]

res.ed
res.ei <- res.ei[aggregated == F & cont.type == "Never treated control",
                 .(estimates, std.errors, tt_event, outcome)]

res.ed.combined[, reform := "Tax decrease"]
res.ei.combined[, reform := "Tax increase"]
res.ed[, reform := "Tax decrease"]
res.ei[, reform := "Tax increase"]

res.ed[, aggregated := NULL]

res.combined <- rbind(res.ei.combined, res.ed.combined)
res <- rbind(res.ei, res.ed)

res.combined[, jitter := .GRP, by = reform]
res.combined[, jitter_tt_event := tt_event + (jitter - jitter_center) / jitter_scale]
res.combined[tt_event == -2, jitter_tt_event := -2]

res[, jitter := .GRP, by = reform]
res[, jitter_tt_event := tt_event + (jitter - jitter_center) / jitter_scale]
res[tt_event == -2, jitter_tt_event := -2]

ggplot(data = res.combined[outcome == "passthrough_2"],
       mapping = aes(x = jitter_tt_event, y = estimates, color = reform)) +
  geom_point(size = 2, alpha = .5) +
  geom_errorbar(data = res[outcome == "passthrough_2"],
                aes(ymax = estimates + 1.96 * std.errors,
                    ymin = estimates - 1.96 * std.errors),
                width = .5) +
  geom_line(data = res.combined[outcome == "passthrough_2"],
            linetype = "55") +
  # scale_color_manual(breaks = c(TRUE, FALSE), values = c("black", "firebrick")) +
  theme_bw(base_size = 16) +
  scale_x_continuous(breaks = c(seq(0, 4, 2), 5.5),
                     labels = c(as.character(seq(0, 4, 2)), "Aggregate")) +
  scale_y_continuous(limits = c(-.4, 2)) +
  labs(x = "Time since reform (quarters)", y = "Estimate", color = NULL) +
  geom_hline(yintercept = 0, color = "red", linetype = "55", alpha = .8) +
  theme(legend.position = c(0.2, 0.12),
        legend.margin = margin(t = -.2, r = 0, b = 0, l = 0, unit = "cm"))
ggsave("pi_figs/reg_results/passthrough_estimates_compared_decreaseincrease.png",
       height = 120, width = 180, units = "mm")
