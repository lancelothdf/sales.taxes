library(data.table)
library(ggplot2)
library(zoo)

setwd("C:/Users/John Bonney/Desktop/Magne_projects/sales_taxes/output/server")


res.all <- fread("pi_data/quarterly_pi_output_quarterFEs.csv")
res.all <- res.all[controls %in% c("module_by_time", "region_by_module_by_time")]

## back out the lead/lag
res.all$tt_event <- as.integer(stringr::str_match(res.all$rn, "[0-9]"))
res.all[is.na(tt_event), tt_event := 0]
res.all[, tt_event := ifelse(grepl("F", rn), -1 * tt_event, tt_event)]


res.all[lead_lag == "avg. pre"]$tt_event <- -3.5
res.all[lead_lag == "avg. post"]$tt_event <- 3.5
res.all[lead_lag == "avg. total effect"]$tt_event <- 5
res.all$tt_event <- as.numeric(res.all$tt_event)
res.all[, aggregated := lead_lag %in% c("avg. pre", "avg. post", "avg. total effect") ]



#### Start with cpricei
res.cpricei <- res.all[controls == "module_by_time" & outcome == "D.ln_quantity2"]

ggplot(data = res.cpricei,
       mapping = aes(x = tt_event, y = Estimate)) +
  geom_point(size = 2, alpha = .5) +
  geom_errorbar(data = res.cpricei,
                aes(ymax = Estimate + 1.96 * `Cluster s.e.`,
                    ymin = Estimate - 1.96 * `Cluster s.e.`),
                width = .8) +
  geom_line(data = res.cpricei[aggregated == F],
            linetype = "55") +
  scale_color_manual(breaks = c(TRUE, FALSE), values = c("black", "firebrick")) +
  #geom_point(aes(x = -2, y = 0), size = 2, color = "black") +
  theme_bw(base_size = 16) +
  scale_x_continuous(breaks = c(-3.5, seq(-2, 2, 1), 3.5, 5),
                     labels = c("Tot. pre", seq(-2, 2, 1), "Tot. post", "Total effect")) +
  labs(x = "Event time (years)", y = "Estimate", color = NULL) +
  geom_hline(yintercept = 0, color = "red", linetype = "55", alpha = .8) +
  scale_y_continuous(limits = c(-0.5, 1.25), breaks = seq(-0.5, 1.25, .25)) +
  theme(legend.position = "none")
ggsave("Regressions/diff_diff_cpricei_regXmodXyear.png", height = 120, width = 180, units = "mm")



#### Start with cpricei2
res.cpricei2 <- res.all[outcome %in% c("d_lcpricei2")]
res.cpricei2 <- res.cpricei[order(tt_event),]


ggplot(data = res.cpricei2,
       mapping = aes(x = tt_event, y = estimate, color = aggregated)) +
  geom_point(size = 2, alpha = .5) +
  geom_errorbar(data = res.cpricei2,
                aes(ymax = estimate + 1.96 * se,
                    ymin = estimate - 1.96 * se),
                width = .8) +
  geom_line(data = res.cpricei2[aggregated == F],
            linetype = "55") +
  scale_color_manual(breaks = c(TRUE, FALSE), values = c("black", "firebrick")) +
  #geom_point(aes(x = -2, y = 0), size = 2, color = "black") +
  theme_bw(base_size = 16) +
  scale_x_continuous(breaks = c(-3.5, seq(-2, 2, 1), 3.5, 5),
                     labels = c("Tot. pre", seq(-2, 2, 1), "Tot. post", "Total effect")) +
  labs(x = "Event time (years)", y = "Estimate", color = NULL) +
  geom_hline(yintercept = 0, color = "red", linetype = "55", alpha = .8) +
  scale_y_continuous(limits = c(-0.5, 1.25), breaks = seq(-0.5, 1.25, .25)) +
  theme(legend.position = "none")
ggsave("Regressions/diff_diff_cpricei2_regXmodXyear.png", height = 120, width = 180, units = "mm")


#### quantity
res.quantity <- res.all[outcome %in% c("d_lquantity")]
res.quantity <- res.quantity[order(tt_event),]


ggplot(data = res.quantity,
       mapping = aes(x = tt_event, y = estimate, color = aggregated)) +
  geom_point(size = 2, alpha = .5) +
  geom_errorbar(data = res.quantity,
                aes(ymax = estimate + 1.96 * se,
                    ymin = estimate - 1.96 * se),
                width = .8) +
  geom_line(data = res.quantity[aggregated == F],
            linetype = "55") +
  scale_color_manual(breaks = c(TRUE, FALSE), values = c("black", "firebrick")) +
  #geom_point(aes(x = -2, y = 0), size = 2, color = "black") +
  theme_bw(base_size = 16) +
  scale_x_continuous(breaks = c(-3.5, seq(-2, 2, 1), 3.5, 5),
                     labels = c("Tot. pre", seq(-2, 2, 1), "Tot. post", "Total effect")) +
  labs(x = "Event time (years)", y = "Estimate", color = NULL) +
  geom_hline(yintercept = 0, color = "red", linetype = "55", alpha = .8) +
  scale_y_continuous(limits = c(-1.5, 1), breaks = seq(-1.5, 1, .25)) +
  theme(legend.position = "none")
ggsave("Regressions/diff_diff_quantity_regXmodXyear.png", height = 120, width = 180, units = "mm")



#### Start with cpricei2
res.quantity2 <- res.all[outcome %in% c("d_lquantity2")]
res.quantity2 <- res.quantity2[order(tt_event),]


ggplot(data = res.quantity2,
       mapping = aes(x = tt_event, y = estimate, color = aggregated)) +
  geom_point(size = 2, alpha = .5) +
  geom_errorbar(data = res.quantity2,
                aes(ymax = estimate + 1.96 * se,
                    ymin = estimate - 1.96 * se),
                width = .8) +
  geom_line(data = res.quantity2[aggregated == F],
            linetype = "55") +
  scale_color_manual(breaks = c(TRUE, FALSE), values = c("black", "firebrick")) +
  #geom_point(aes(x = -2, y = 0), size = 2, color = "black") +
  theme_bw(base_size = 16) +
  scale_x_continuous(breaks = c(-3.5, seq(-2, 2, 1), 3.5, 5),
                     labels = c("Tot. pre", seq(-2, 2, 1), "Tot. post", "Total effect")) +
  labs(x = "Event time (years)", y = "Estimate", color = NULL) +
  geom_hline(yintercept = 0, color = "red", linetype = "55", alpha = .8) +
  scale_y_continuous(limits = c(-1.5, 1), breaks = seq(-1.5, 1, .25)) +
  theme(legend.position = "none")
ggsave("Regressions/diff_diff_quantity2_regXmodXyear.png", height = 120, width = 180, units = "mm")
