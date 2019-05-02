library(data.table)
library(ggplot2)
library(zoo)

setwd("C:/Users/John Bonney/Desktop/Magne_projects/sales_taxes/output/server")

## ever increase ---------------

## first, event-no-event

res.ei <- fread("pi_data/regression_output/with_stderr/Passthrough_estimates_stderr_ei_monthly_combined.csv")

res.ei[, aggregated := rn == "post-treatment"]
res.ei[, rn := as.integer(gsub("catt", "", rn))]
setnames(res.ei, "rn", "tt_event")
res.ei[, tt_event := as.double(tt_event)]
res.ei[aggregated == T, tt_event := 14.5] # assign aggregate estimates tt_event = 5.5

res.ei.combined <- rbind(res.ei[, .(estimates, tt_event, outcome, aggregated)],
                         data.table(tt_event = rep(-6, 2),
                                    estimates = rep(0, 2),
                                    outcome = c("cpricei", "sales_tax"),
                                    aggregated = rep(F, 2)))

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
  geom_point(aes(x = -6, y = 0), size = 2, color = "black") +
  theme_bw(base_size = 16) +
  scale_x_continuous(breaks = c(seq(-12, 12, 3), 14.5),
                     labels = c(as.character(seq(-12, 12, 3)), "Aggregate")) +
  labs(x = "Event time (quarters)", y = "Estimate", color = NULL) +
  geom_hline(yintercept = 0, color = "red", linetype = "55", alpha = .8) +
  scale_y_continuous(limits = c(-.004, .01), breaks = seq(-.005, .01, .0025)) +
  theme(legend.position = "none")
ggsave("pi_figs/reg_results/cpricei_estimates_monthly.png",
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
  geom_point(aes(x = -6, y = 0), size = 2, color = "black") +
  theme_bw(base_size = 16) +
  scale_x_continuous(breaks = c(seq(-12, 12, 3), 14.5),
                     labels = c(as.character(seq(-12, 12, 3)), "Aggregate")) +
  labs(x = "Event time (quarters)", y = "Estimate", color = NULL) +
  geom_hline(yintercept = 0, color = "red", linetype = "55", alpha = .8) +
  scale_y_continuous(limits = c(-.004, .01), breaks = seq(-.005, .01, .0025)) +
  theme(legend.position = "none")
ggsave("pi_figs/reg_results/tax_estimates_monthly.png",
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
  scale_x_continuous(breaks = c(seq(0, 12, 3), 14.5),
                     labels = c(as.character(seq(0, 12, 3)), "Aggregate")) +
  labs(x = "Time since reform (quarters)", y = "Estimate", color = NULL) +
  geom_hline(yintercept = 0, color = "red", linetype = "55", alpha = .8) +
  theme(legend.position = "none")
ggsave("pi_figs/reg_results/passthrough1_estimates_monthly.png",
       height = 120, width = 180, units = "mm")
