library(data.table)
library(ggplot2)
library(zoo)

setwd("C:/Users/John Bonney/Desktop/Magne_projects/sales_taxes/output/server")

## setup -------------------
res.all <- fread("pi_data/semesterly_pi_output_pooled.csv")
res.all <- res.all[rn %in% paste0(c("L0", "L1", "L2", "L3", "L4", "F1", "F3", "F4", "Pre", "Post", "All"), ".ln_sales_tax")]
# res.all <- res.all[controls %in% c("module_by_time", "region_by_module_by_time")]

## back out the lead/lag
res.all$tt_event <- as.double(stringr::str_match(res.all$rn, "[0-9]"))
res.all[is.na(tt_event), tt_event := ifelse(grepl("Pre", rn), -6,
                                     ifelse(grepl("Post", rn), 6,
                                     ifelse(grepl("All", rn), 8.5, 0)))]
res.all[, tt_event := ifelse(grepl("F", rn), -1 * tt_event, tt_event)]
res.all[, aggregated := tt_event %in% c(-6, 6, 8.5)]

setnames(res.all, old = c("Estimate", "Cluster s.e."), new = c("estimate", "se"))


## cpricei plots ---------------------

## module-by-time FE
res.cpricei <- res.all[outcome == "ln_cpricei" & controls == "module_by_time"]
res.cpricei <- rbind(res.cpricei, data.table(tt_event = -2, estimate = 0, aggregated = F), fill = T)
ggplot(data = res.cpricei, mapping = aes(x = tt_event, y = estimate, color = aggregated)) +
  geom_point(size = 2, alpha = .5) +
  geom_errorbar(data = res.cpricei[tt_event != -2],
                aes(ymax = estimate + 1.96 * se,
                    ymin = estimate - 1.96 * se),
                width = .6) +
  geom_line(data = res.cpricei[aggregated == F],
            linetype = "55") +
  scale_color_manual(breaks = c(TRUE, FALSE), values = c("black", "firebrick")) +
  theme_bw(base_size = 16) +
  scale_x_continuous(breaks = c(-6, seq(-4, 4, 1), 6, 8.5),
                     labels = c("Tot. pre", seq(-24, 24, 6), "Tot. post", "Total effect")) +
  labs(x = "Event time (months)", y = "Estimate", color = NULL) +
  geom_hline(yintercept = 0, color = "red", linetype = "55", alpha = .8) +
  # scale_y_continuous(limits = c(-0.5, 0.5), breaks = seq(-0.5, 0.5, .25)) +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())
ggsave("pi_figs/reg_results/pooled/cpricei_module_by_time.png",
       height = 120, width = 180, units = "mm")

## module-by-time-by-region
res.cpricei <- res.all[outcome == "D.ln_cpricei" & controls == "region_by_module_by_time"]
ggplot(data = res.cpricei, mapping = aes(x = tt_event, y = estimate, color = aggregated)) +
  geom_point(size = 2, alpha = .5) +
  geom_errorbar(data = res.cpricei,
                aes(ymax = estimate + 1.96 * se,
                    ymin = estimate - 1.96 * se),
                width = .6) +
  geom_line(data = res.cpricei[aggregated == F],
            linetype = "55") +
  scale_color_manual(breaks = c(TRUE, FALSE), values = c("black", "firebrick")) +
  theme_bw(base_size = 16) +
  scale_x_continuous(breaks = c(-8, seq(-6, 6, 1), 8, 10.5),
                     labels = c("Tot. pre", seq(-6, 6, 1), "Tot. post", "Total effect")) +
  labs(x = "Event time (quarters)", y = "Estimate", color = NULL) +
  geom_hline(yintercept = 0, color = "red", linetype = "55", alpha = .8) +
  scale_y_continuous(limits = c(-0.5, 1.25), breaks = seq(-0.5, 1.25, .25)) +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())
ggsave("pi_figs/reg_results/lags/cpricei_region_by_module_by_time.png",
       height = 120, width = 180, units = "mm")

## cpricei2 plots ---------------------

## module-by-time FE
res.cpricei <- res.all[outcome == "D.ln_cpricei2" & controls == "module_by_time"]
ggplot(data = res.cpricei, mapping = aes(x = tt_event, y = estimate, color = aggregated)) +
  geom_point(size = 2, alpha = .5) +
  geom_errorbar(data = res.cpricei,
                aes(ymax = estimate + 1.96 * se,
                    ymin = estimate - 1.96 * se),
                width = .6) +
  geom_line(data = res.cpricei[aggregated == F],
            linetype = "55") +
  scale_color_manual(breaks = c(TRUE, FALSE), values = c("black", "firebrick")) +
  theme_bw(base_size = 16) +
  scale_x_continuous(breaks = c(-8, seq(-6, 6, 1), 8, 10.5),
                     labels = c("Tot. pre", seq(-6, 6, 1), "Tot. post", "Total effect")) +
  labs(x = "Event time (quarters)", y = "Estimate", color = NULL) +
  geom_hline(yintercept = 0, color = "red", linetype = "55", alpha = .8) +
  scale_y_continuous(limits = c(-0.5, 1.3), breaks = seq(-0.5, 1.3, .25)) +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())
ggsave("pi_figs/reg_results/lags/cpricei2_module_by_time.png",
       height = 120, width = 180, units = "mm")

## module-by-time-by-region
res.cpricei <- res.all[outcome == "D.ln_cpricei2" & controls == "region_by_module_by_time"]
ggplot(data = res.cpricei, mapping = aes(x = tt_event, y = estimate, color = aggregated)) +
  geom_point(size = 2, alpha = .5) +
  geom_errorbar(data = res.cpricei,
                aes(ymax = estimate + 1.96 * se,
                    ymin = estimate - 1.96 * se),
                width = .6) +
  geom_line(data = res.cpricei[aggregated == F],
            linetype = "55") +
  scale_color_manual(breaks = c(TRUE, FALSE), values = c("black", "firebrick")) +
  theme_bw(base_size = 16) +
  scale_x_continuous(breaks = c(-8, seq(-6, 6, 1), 8, 10.5),
                     labels = c("Tot. pre", seq(-6, 6, 1), "Tot. post", "Total effect")) +
  labs(x = "Event time (quarters)", y = "Estimate", color = NULL) +
  geom_hline(yintercept = 0, color = "red", linetype = "55", alpha = .8) +
  scale_y_continuous(limits = c(-0.5, 1.3), breaks = seq(-0.5, 1.3, .25)) +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())
ggsave("pi_figs/reg_results/lags/cpricei2_region_by_module_by_time.png",
       height = 120, width = 180, units = "mm")

## quantity plots ---------------------

## module-by-time FE
res.quantity <- res.all[outcome == "D.ln_quantity" & controls == "module_by_time"]
ggplot(data = res.quantity, mapping = aes(x = tt_event, y = estimate, color = aggregated)) +
  geom_point(size = 2, alpha = .5) +
  geom_errorbar(data = res.quantity,
                aes(ymax = estimate + 1.96 * se,
                    ymin = estimate - 1.96 * se),
                width = .6) +
  geom_line(data = res.quantity[aggregated == F],
            linetype = "55") +
  scale_color_manual(breaks = c(TRUE, FALSE), values = c("black", "firebrick")) +
  theme_bw(base_size = 16) +
  scale_x_continuous(breaks = c(-8, seq(-6, 6, 1), 8, 10.5),
                     labels = c("Tot. pre", seq(-6, 6, 1), "Tot. post", "Total effect")) +
  labs(x = "Event time (quarters)", y = "Estimate", color = NULL) +
  geom_hline(yintercept = 0, color = "red", linetype = "55", alpha = .8) +
  scale_y_continuous(limits = c(-2, 1.25), breaks = seq(-2, 1.25, .5)) +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())
ggsave("pi_figs/reg_results/lags/quantity_module_by_time.png",
       height = 120, width = 180, units = "mm")

## module-by-time-by-region
res.quantity <- res.all[outcome == "D.ln_quantity" & controls == "region_by_module_by_time"]
ggplot(data = res.quantity, mapping = aes(x = tt_event, y = estimate, color = aggregated)) +
  geom_point(size = 2, alpha = .5) +
  geom_errorbar(data = res.quantity,
                aes(ymax = estimate + 1.96 * se,
                    ymin = estimate - 1.96 * se),
                width = .6) +
  geom_line(data = res.quantity[aggregated == F],
            linetype = "55") +
  scale_color_manual(breaks = c(TRUE, FALSE), values = c("black", "firebrick")) +
  theme_bw(base_size = 16) +
  scale_x_continuous(breaks = c(-8, seq(-6, 6, 1), 8, 10.5),
                     labels = c("Tot. pre", seq(-6, 6, 1), "Tot. post", "Total effect")) +
  labs(x = "Event time (quarters)", y = "Estimate", color = NULL) +
  geom_hline(yintercept = 0, color = "red", linetype = "55", alpha = .8) +
  scale_y_continuous(limits = c(-2, 1.25), breaks = seq(-2, 1.25, .5)) +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())
ggsave("pi_figs/reg_results/lags/quantity_region_by_module_by_time.png",
       height = 120, width = 180, units = "mm")

## quantity2 plots ----------------

## module-by-time FE
res.quantity <- res.all[outcome == "D.ln_quantity2" & controls == "module_by_time"]
ggplot(data = res.quantity, mapping = aes(x = tt_event, y = estimate, color = aggregated)) +
  geom_point(size = 2, alpha = .5) +
  geom_errorbar(data = res.quantity,
                aes(ymax = estimate + 1.96 * se,
                    ymin = estimate - 1.96 * se),
                width = .6) +
  geom_line(data = res.quantity[aggregated == F],
            linetype = "55") +
  scale_color_manual(breaks = c(TRUE, FALSE), values = c("black", "firebrick")) +
  theme_bw(base_size = 16) +
  scale_x_continuous(breaks = c(-8, seq(-6, 6, 1), 8, 10.5),
                     labels = c("Tot. pre", seq(-6, 6, 1), "Tot. post", "Total effect")) +
  labs(x = "Event time (quarters)", y = "Estimate", color = NULL) +
  geom_hline(yintercept = 0, color = "red", linetype = "55", alpha = .8) +
  scale_y_continuous(limits = c(-2, 1.25), breaks = seq(-2, 1.25, .5)) +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())
ggsave("pi_figs/reg_results/lags/quantity2_module_by_time.png",
       height = 120, width = 180, units = "mm")

## module-by-time-by-region
res.quantity <- res.all[outcome == "D.ln_quantity2" & controls == "region_by_module_by_time"]
ggplot(data = res.quantity, mapping = aes(x = tt_event, y = estimate, color = aggregated)) +
  geom_point(size = 2, alpha = .5) +
  geom_errorbar(data = res.quantity,
                aes(ymax = estimate + 1.96 * se,
                    ymin = estimate - 1.96 * se),
                width = .6) +
  geom_line(data = res.quantity[aggregated == F],
            linetype = "55") +
  scale_color_manual(breaks = c(TRUE, FALSE), values = c("black", "firebrick")) +
  theme_bw(base_size = 16) +
  scale_x_continuous(breaks = c(-8, seq(-6, 6, 1), 8, 10.5),
                     labels = c("Tot. pre", seq(-6, 6, 1), "Tot. post", "Total effect")) +
  labs(x = "Event time (quarters)", y = "Estimate", color = NULL) +
  geom_hline(yintercept = 0, color = "red", linetype = "55", alpha = .8) +
  scale_y_continuous(limits = c(-2, 1.25), breaks = seq(-2, 1.25, .5)) +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())
ggsave("pi_figs/reg_results/lags/quantity2_region_by_module_by_time.png",
       height = 120, width = 180, units = "mm")
