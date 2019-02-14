#' Author: John Bonney
#'
#' Plot Lance's price indices, comparing to CPI

setwd("C:/Users/John Bonney/Desktop/Magne_projects/sales_taxes/data")

library(tidyverse)
library(zoo)

### Import county-index averages
pi_county_avg <- read.csv("C:/Users/John Bonney/Desktop/Magne_projects/sales_taxes/output/server/national_pi_popweights.csv")
pi_county_avg$t <- with(pi_county_avg,
                            as.yearqtr(paste0(year, " Q", quarter)))

pi_county_avg <- pi_county_avg %>%
  select(year, quarter, chained_food_cpi = national_index, t) %>%
  mutate(index = "county_avg")

base_index <- pi_county_avg %>%
  filter(year == 2006, quarter == 4) %>%
  select(chained_food_cpi) %>% pull()
pi_county_avg <- pi_county_avg %>%
  mutate(chained_food_cpi = chained_food_cpi / base_index)

### Import national index
pi_national <- read.csv("C:/Users/John Bonney/Desktop/Magne_projects/sales_taxes/output/server/national_pi2.csv")
pi_national$t <- with(pi_national,
                            as.yearqtr(paste0(year, " Q", quarter)))

pi_national <- pi_national %>%
  select(year, quarter, chained_food_cpi = natl_index, t) %>%
  mutate(index = "national_index")

base_index <- pi_national %>%
  filter(year == 2006, quarter == 4) %>%
  select(chained_food_cpi) %>% pull()
pi_national <- pi_national %>%
  mutate(chained_food_cpi = chained_food_cpi / base_index)

cpi_data <- read.csv("CPI/national_monthly_food_beverage_chained_cpi.csv") %>%
  as.tibble() %>% filter(between(year, 2006, 2016)) %>%
  mutate(quarter = as.integer(ceiling(month / 3))) %>%
  group_by(year, quarter) %>%
  summarize(chained_food_cpi = mean(chained_food_cpi))
cpi_data$t <- with(cpi_data, as.yearqtr(paste0(year, " Q", quarter)))

base_cpi <- cpi_data %>% ungroup() %>%
  filter(year == 2006, quarter == 4) %>%
  select(chained_food_cpi) %>% pull()
cpi_data <- cpi_data %>%
  filter(year > 2006 | quarter == 4) %>%
  mutate(chained_food_cpi = chained_food_cpi / base_cpi,
         index = "cpi")

cpi_data <- base::rbind.data.frame(ungroup(cpi_data),
                                   pi_county_avg,
                                   pi_national)

pi_first_set <- cpi_data

ggplot(mapping = aes(x = t, y = chained_food_cpi, linetype = index, color = index)) +
  geom_line(data = cpi_data, aes(group = index), size = 1) +
  scale_x_yearqtr(format = "%Y Q%q", expand = c(0.01, 0.01), n = 6) +
  scale_y_continuous(breaks = seq(0, 1.25, .05), expand = c(0.005, 0.005)) +
  theme_bw() +
  labs(y = expression(bold("Price Index (Normalized 2006 Q4 = 1.00)")), x = expression(bold("Year-Quarter"))) +
  scale_linetype_manual(name = NULL, breaks = c("cpi", "county_avg", "national_index"),
                        labels = c("Food & Beverage Chained CPI", "Retail Scanner Index (county average)",
                                   "Retail Scanner Index (national index)"),
                        values = c("3344", "solid", "4433")) +
  scale_color_manual(name = NULL, breaks = c("cpi", "county_avg", "national_index"),
                     labels = c("Food & Beverage Chained CPI", "Retail Scanner Index (county average)",
                                "Retail Scanner Index (national index)"),
                     values = c("#F8766D", "black", "#00BFC4")) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(size = 0.1, colour = 'grey'),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    strip.background = element_rect(colour="white", fill="white"),
    plot.title = element_text(hjust = 0.5, size = 11), panel.spacing = unit(2, "lines"),
    legend.position = c(0.7, 0.31), axis.ticks.length=unit(-0.15, "cm"),
    legend.margin = margin(t=-.2, r=0, b=-.2, l=0, unit="cm"),
    axis.text.x = element_text(margin=unit(rep(0.3, 4), "cm")),
    axis.text.y = element_text(margin=unit(rep(0.3, 4), "cm")),
    axis.text.y.right = element_text(margin=unit(rep(0.3, 4), "cm"))
  )

ggsave(
  "C:/Users/John Bonney/Desktop/Magne_projects/sales_taxes/reports/figs/comparing2.png",
  height = 120, width = 180, units = "mm"
)

##########################################
##### simple sales-weighted average ######
##########################################

### Import sales-weighted avg
sw_avg <- read.csv("C:/Users/John Bonney/Desktop/Magne_projects/sales_taxes/output/server/simple_sales_weighted_pi.csv")
sw_avg$t <- with(sw_avg, as.yearqtr(paste0(year, " Q", quarter)))

sw_avg <- sw_avg %>%
  select(year, quarter, chained_food_cpi = natl_avg, t) %>%
  mutate(index = "sales_avg")

base_index <- sw_avg %>%
  filter(year == 2006, quarter == 4) %>%
  select(chained_food_cpi) %>% pull()
sw_avg <- sw_avg %>% mutate(chained_food_cpi = chained_food_cpi / base_index)

## Import sales-weighted avg (balanced on store-module level)

sw_avg_b <- read.csv("C:/Users/John Bonney/Desktop/Magne_projects/sales_taxes/output/server/simple_sales_weighted_pi_modbalanced.csv")
sw_avg_b$t <- with(sw_avg_b, as.yearqtr(paste0(year, " Q", quarter)))

sw_avg_b <- sw_avg_b %>%
  select(year, quarter, chained_food_cpi = natl_avg, t) %>%
  mutate(index = "sales_avg_balanced")

base_index <- sw_avg_b %>%
  filter(year == 2006, quarter == 4) %>%
  select(chained_food_cpi) %>% pull()
sw_avg_b <- sw_avg_b %>% mutate(chained_food_cpi = chained_food_cpi / base_index)

pi_combined <- rbind(pi_first_set, sw_avg, sw_avg_b)

ggplot(mapping = aes(x = t, y = chained_food_cpi, linetype = index, color = index)) +
  geom_line(data = pi_combined, aes(group = index), size = 1) +
  scale_x_yearqtr(format = "%Y Q%q", expand = c(0.01, 0.01), n = 6) +
  scale_y_continuous(breaks = seq(0, 1.4, .1), expand = c(0.005, 0.005)) +
  theme_bw() +
  labs(y = expression(bold("Price Index (Normalized 2006 Q4 = 1.00)")), x = expression(bold("Year-Quarter"))) +
  scale_linetype_manual(name = NULL, breaks = c("cpi", "county_avg", "national_index", "sales_avg", "sales_avg_balanced"),
                        labels = c("Food & Beverage Chained CPI", "Retail Scanner Index (county average)",
                                   "Retail Scanner Index (national index)", "Sales-weighted Index (store-level balanced)",
                                   "Sales-weighted Index (store-module-level balanced)"),
                        values = c("3344", "solid", "4433", "11", "11")) +
  scale_color_manual(name = NULL, breaks = c("cpi", "county_avg", "national_index", "sales_avg", "sales_avg_balanced"),
                     labels = c("Food & Beverage Chained CPI", "Retail Scanner Index (county average)",
                                "Retail Scanner Index (national index)", "Sales-weighted Index (store-level balanced)",
                                "Sales-weighted Index (store-module-level balanced)"),
                     values = c("#F8766D", "black", "#7CAE00", "#00BFC4", "#C77CFF")) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(size = 0.1, colour = 'grey'),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    strip.background = element_rect(colour="white", fill="white"),
    plot.title = element_text(hjust = 0.5, size = 11), panel.spacing = unit(2, "lines"),
    legend.position = c(0.7, 0.2), axis.ticks.length=unit(-0.15, "cm"),
    legend.margin = margin(t=-.2, r=0, b=-.2, l=0, unit="cm"),
    axis.text.x = element_text(margin=unit(rep(0.3, 4), "cm")),
    axis.text.y = element_text(margin=unit(rep(0.3, 4), "cm")),
    axis.text.y.right = element_text(margin=unit(rep(0.3, 4), "cm"))
  )

ggsave(
  "C:/Users/John Bonney/Desktop/Magne_projects/sales_taxes/reports/figs/comparing_all2.png",
  height = 120, width = 180, units = "mm"
)
