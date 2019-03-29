library(data.table)
library(ggplot2)
library(zoo)

setwd("C:/Users/John Bonney/Desktop/Magne_projects/sales_taxes/output/server")

## ever increase ---------------

## first, event-no-event

res.ei <- fread("pi_data/regression_output/with_stderr/Passthrough_estimates_stderr_ei_nevertreated.csv")

res.ei[, aggregated := rn == "post-treatment"]
res.ei[, rn := as.integer(gsub("catt", "", rn))]
setnames(res.ei, "rn", "tt_event")
res.ei[, tt_event := as.double(tt_event)]
res.ei[aggregated == T, tt_event := 5.5] # assign aggregate estimates tt_event = 5.5

res.ei.combined <- rbind(res.ei[, .(estimates, tt_event, outcome, aggregated)],
                         data.table(tt_event = rep(-2, 2),
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
  geom_point(aes(x = -2, y = 0), size = 2, color = "black") +
  theme_bw(base_size = 16) +
  scale_x_continuous(breaks = c(seq(-4, 4, 2), 5.5),
                     labels = c(as.character(seq(-4, 4, 2)), "Aggregate")) +
  labs(x = "Event time (quarters)", y = "Estimate", color = NULL) +
  geom_hline(yintercept = 0, color = "red", linetype = "55", alpha = .8) +
  scale_y_continuous(limits = c(-.004, .01), breaks = seq(-.005, .01, .0025)) +
  theme(legend.position = "none")
ggsave("pi_figs/reg_results/cpricei_estimates.png",
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
  theme_bw(base_size = 16) +
  scale_x_continuous(breaks = c(seq(-4, 4, 2), 5.5),
                     labels = c(as.character(seq(-4, 4, 2)), "Aggregate")) +
  labs(x = "Event time (quarters)", y = "Estimate", color = NULL) +
  geom_hline(yintercept = 0, color = "red", linetype = "55", alpha = .8) +
  scale_y_continuous(limits = c(-.004, .01), breaks = seq(-.005, .01, .0025)) +
  theme(legend.position = "none")
ggsave("pi_figs/reg_results/tax_estimates.png",
       height = 120, width = 180, units = "mm")

ggplot(data = res.ei.combined[outcome == "passthrough_2"],
       mapping = aes(x = tt_event, y = estimates, color = aggregated)) +
  geom_point(size = 2, alpha = .5) +
  geom_errorbar(data = res.ei[outcome == "passthrough_2"],
                aes(ymax = estimates + 1.96 * std.errors,
                    ymin = estimates - 1.96 * std.errors),
                width = .8) +
  geom_line(data = res.ei.combined[outcome == "passthrough_2" & aggregated == F],
            linetype = "55") +
  scale_color_manual(breaks = c(TRUE, FALSE), values = c("black", "firebrick")) +
  theme_bw(base_size = 16) +
  scale_x_continuous(breaks = c(seq(0, 4, 2), 5.5),
                     labels = c(as.character(seq(0, 4, 2)), "Aggregate")) +
  labs(x = "Time since reform (quarters)", y = "Estimate", color = NULL) +
  geom_hline(yintercept = 0, color = "red", linetype = "55", alpha = .8) +
  theme(legend.position = "none")
ggsave("pi_figs/reg_results/passthrough_estimates.png",
       height = 120, width = 180, units = "mm")

## comparing to time-of-event
res.ei.toe <- fread("pi_data/regression_output/with_stderr/Passthrough_estimates_stderr_ei_timeofevent.csv")
res.ei.toe[, aggregated := rn == "post-treatment"]
res.ei.toe[, rn := as.integer(gsub("catt", "", rn))]
setnames(res.ei.toe, "rn", "tt_event")
res.ei.toe[, tt_event := as.double(tt_event)]
res.ei.toe[aggregated == T, tt_event := 5.5] # assign aggregate estimates tt_event = 5.5

res.ei.toe.combined <- rbind(res.ei.toe[, .(estimates, tt_event, outcome, aggregated)],
                         data.table(tt_event = rep(-2, 2),
                                    estimates = rep(0, 2),
                                    outcome = c("cpricei", "sales_tax"),
                                    aggregated = rep(F, 2)))
res.ei.toe.combined[, cont.type := "Future treated control"]
res.ei.combined[, cont.type := "Never treated control"]
res.ei.master <- rbind(res.ei.combined,
                       res.ei.toe.combined)

res.ei.toe[, cont.type := "Future treated control"]
res.ei[, cont.type := "Never treated control"]
res.mini.master <- rbind(res.ei,
                         res.ei.toe)

res.mini.master[, jitter := .GRP, by = cont.type]
jitter_center <- res.mini.master[, median(unique(jitter), na.rm = T)]
jitter_scale <- res.mini.master[,length(unique(jitter))] * 4
res.mini.master[, jitter_tt_event := tt_event + (jitter - jitter_center) / jitter_scale]
res.mini.master[tt_event == -2, jitter_tt_event := -2]

res.ei.master[, jitter := .GRP, by = cont.type]
res.ei.master[, jitter_tt_event := tt_event + (jitter - jitter_center) / jitter_scale]
res.ei.master[tt_event == -2, jitter_tt_event := -2]

# TODO: need to jitter these correctly

ggplot(data = res.ei.master[outcome == "cpricei"],
       mapping = aes(x = jitter_tt_event, y = estimates, color = cont.type)) +
  geom_point(size = 2, alpha = .5) +
  geom_errorbar(data = res.mini.master[outcome == "cpricei"],
                aes(ymax = estimates + 1.96 * std.errors,
                    ymin = estimates - 1.96 * std.errors),
                width = .5) +
  geom_line(data = res.ei.master[outcome == "cpricei" & aggregated == F],
            linetype = "55") +
  # scale_color_manual(breaks = c(TRUE, FALSE), values = c("black", "firebrick")) +
  geom_point(aes(x = -2, y = 0), size = 2, color = "black") +
  theme_bw(base_size = 16) +
  scale_x_continuous(breaks = c(seq(-4, 4, 2), 5.5),
                     labels = c(as.character(seq(-4, 4, 2)), "Aggregate")) +
  labs(x = "Event time (quarters)", y = "Estimate", color = NULL) +
  geom_hline(yintercept = 0, color = "red", linetype = "55", alpha = .8) +
  scale_y_continuous(limits = c(-.004, .01), breaks = seq(-.005, .01, .0025)) +
  theme(legend.position = c(0.22, 0.85))
ggsave("pi_figs/reg_results/cpricei_estimates_compared.png",
       height = 120, width = 180, units = "mm")

ggplot(data = res.ei.master[outcome == "sales_tax"],
       mapping = aes(x = jitter_tt_event, y = estimates, color = cont.type)) +
  geom_point(size = 2, alpha = .5) +
  geom_errorbar(data = res.mini.master[outcome == "sales_tax"],
                aes(ymax = estimates + 1.96 * std.errors,
                    ymin = estimates - 1.96 * std.errors),
                width = .5) +
  geom_line(data = res.ei.master[outcome == "sales_tax" & aggregated == F],
            linetype = "55") +
  # scale_color_manual(breaks = c(TRUE, FALSE), values = c("black", "firebrick")) +
  geom_point(aes(x = -2, y = 0), size = 2, color = "black") +
  theme_bw(base_size = 16) +
  scale_x_continuous(breaks = c(seq(-4, 4, 2), 5.5),
                     labels = c(as.character(seq(-4, 4, 2)), "Aggregate")) +
  labs(x = "Event time (quarters)", y = "Estimate", color = NULL) +
  geom_hline(yintercept = 0, color = "red", linetype = "55", alpha = .8) +
  scale_y_continuous(limits = c(-.004, .01), breaks = seq(-.005, .01, .0025)) +
  theme(legend.position = c(0.22, 0.85))
ggsave("pi_figs/reg_results/tax_estimates_compared.png",
       height = 120, width = 180, units = "mm")

ggplot(data = res.ei.master[outcome == "passthrough_2"],
       mapping = aes(x = jitter_tt_event, y = estimates, color = cont.type)) +
  geom_point(size = 2, alpha = .5) +
  geom_errorbar(data = res.mini.master[outcome == "passthrough_2"],
                aes(ymax = estimates + 1.96 * std.errors,
                    ymin = estimates - 1.96 * std.errors),
                width = .5) +
  geom_line(data = res.ei.master[outcome == "passthrough_2" & aggregated == F],
            linetype = "55") +
  # scale_color_manual(breaks = c(TRUE, FALSE), values = c("black", "firebrick")) +
  theme_bw(base_size = 16) +
  scale_x_continuous(breaks = c(seq(0, 4, 2), 5.5),
                     labels = c(as.character(seq(0, 4, 2)), "Aggregate")) +
  scale_y_continuous(limits = c(-.3, 1.4)) +
  labs(x = "Time since reform (quarters)", y = "Estimate", color = NULL) +
  geom_hline(yintercept = 0, color = "red", linetype = "55", alpha = .8) +
  theme(legend.position = c(0.2, 0.12),
        legend.margin = margin(t = -.2, r = 0, b = 0, l = 0, unit = "cm"))
ggsave("pi_figs/reg_results/passthrough_estimates_compared.png",
       height = 120, width = 180, units = "mm")


### Looking at decreases ------------
res.ed <- fread("pi_data/regression_output/with_stderr/Passthrough_estimates_stderr_ed_nevertreated.csv")

res.ed[, aggregated := rn == "post-treatment"]
res.ed <- res.ed[aggregated == F]
res.ed[, rn := as.integer(gsub("catt", "", rn))]
setnames(res.ed, "rn", "tt_event")

# TODO: we want to remove aggregated from these "ever decrease" plots

res.ed.combined <- rbind(res.ed[, .(estimates, tt_event, outcome)],
                         data.table(tt_event = rep(-2, 2),
                                    estimates = rep(0, 2),
                                    outcome = c("cpricei", "sales_tax")))

ggplot(data = res.ed.combined[outcome == "cpricei"],
       mapping = aes(x = tt_event, y = estimates)) +
  geom_point(size = 2, alpha = .5) +
  geom_errorbar(data = res.ed[outcome == "cpricei"],
                aes(ymax = estimates + 1.96 * std.errors,
                    ymin = estimates - 1.96 * std.errors),
                width = .8) +
  geom_line(data = res.ed.combined[outcome == "cpricei"],
            linetype = "55") +
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
       mapping = aes(x = tt_event, y = estimates)) +
  geom_point(size = 2, alpha = .5) +
  geom_errorbar(data = res.ed[outcome == "sales_tax"],
                aes(ymax = estimates + 1.96 * std.errors,
                    ymin = estimates - 1.96 * std.errors),
                width = .8) +
  geom_line(data = res.ed.combined[outcome == "sales_tax"],
            linetype = "55") +
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

ggplot(data = res.ed.combined[outcome == "passthrough_2"],
       mapping = aes(x = tt_event, y = estimates)) +
  geom_point(size = 2, alpha = .5) +
  geom_errorbar(data = res.ed[outcome == "passthrough_2"],
                aes(ymax = estimates + 1.96 * std.errors,
                    ymin = estimates - 1.96 * std.errors),
                width = .8) +
  geom_line(data = res.ed.combined[outcome == "passthrough_2"],
            linetype = "55") +
  theme_bw(base_size = 16) +
  scale_x_continuous(breaks = c(seq(0, 4, 2), 5.5),
                     labels = c(as.character(seq(0, 4, 2)), "Aggregate")) +
  labs(x = "Time since reform (quarters)", y = "Estimate", color = NULL) +
  geom_hline(yintercept = 0, color = "red", linetype = "55", alpha = .8) +
  theme(legend.position = "none")
ggsave("pi_figs/reg_results/passthrough_estimates_ed.png",
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
