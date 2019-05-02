#' Author: John Bonney
#'
#' Plot Lance's price indices, comparing to CPI

setwd("C:/Users/John Bonney/Desktop/Magne_projects/sales_taxes/data")

library(tidyverse)
library(zoo)

### Import store-level balanced data
pi_store_balanced <- read.csv("C:/Users/John Bonney/Desktop/Magne_projects/sales_taxes/output/server/national_pi_store_balanced.csv")
pi_store_balanced$t <- with(pi_store_balanced,
                            as.yearqtr(paste0(year, " Q", quarter)))

pi_store_balanced <- pi_store_balanced %>%
  select(year, quarter, chained_food_cpi = national_index, t) %>%
  mutate(index = "retail_scanner")

base_index <- pi_store_balanced %>%
  filter(year == 2006, quarter == 4) %>%
  select(chained_food_cpi) %>% pull()
pi_store_balanced <- pi_store_balanced %>%
  mutate(chained_food_cpi = chained_food_cpi / base_index)

### Import store-module level balanced data
pi_module_balanced <- read.csv("C:/Users/John Bonney/Desktop/Magne_projects/sales_taxes/output/server/national_pi_store_module_balanced.csv")
pi_module_balanced$t <- with(pi_module_balanced,
                            as.yearqtr(paste0(year, " Q", quarter)))

pi_module_balanced <- pi_module_balanced %>%
  select(year, quarter, chained_food_cpi = national_index, t) %>%
  mutate(index = "retail_scanner_module")

base_index <- pi_module_balanced %>%
  filter(year == 2006, quarter == 4) %>%
  select(chained_food_cpi) %>% pull()
pi_module_balanced <- pi_module_balanced %>%
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
                                   pi_store_balanced,
                                   pi_module_balanced)

ggplot(mapping = aes(x = t, y = chained_food_cpi, linetype = index, color = index)) +
  geom_line(data = cpi_data, aes(group = index), size = 1) +
  scale_x_yearqtr(format = "%Y Q%q", expand = c(0.01, 0.01), n = 6) +
  scale_y_continuous(breaks = seq(0, 1.25, .05), expand = c(0.005, 0.005)) +
  theme_bw() +
  labs(y = expression(bold("Price Index (Normalized 2006 Q4 = 1.00)")), x = expression(bold("Year-Quarter"))) +
  scale_linetype_manual(name = NULL, breaks = c("cpi", "retail_scanner", "retail_scanner_module"),
                        labels = c("Food & Beverage Chained CPI", "Retail Scanner Index (store-balanced)",
                                   "Retail Scanner Index (store-module-balanced)"),
                        values = c("solid", "3344", "4433")) +
  scale_color_manual(name = NULL, breaks = c("cpi", "retail_scanner", "retail_scanner_module"),
                     labels = c("Food & Beverage Chained CPI", "Retail Scanner Index (store-balanced)",
                                "Retail Scanner Index (store-module-balanced)"),
                     values = c("black", "#F8766D", "#00BFC4")) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(size = 0.1, colour = 'grey'),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    strip.background = element_rect(colour="white", fill="white"),
    plot.title = element_text(hjust = 0.5, size = 11), panel.spacing = unit(2, "lines"),
    legend.position = c(0.7, 0.32), axis.ticks.length=unit(-0.15, "cm"),
    legend.margin = margin(t=-.2, r=0, b=-.2, l=0, unit="cm"),
    axis.text.x = element_text(margin=unit(rep(0.3, 4), "cm")),
    axis.text.y = element_text(margin=unit(rep(0.3, 4), "cm")),
    axis.text.y.right = element_text(margin=unit(rep(0.3, 4), "cm"))
  )

ggsave(
  "C:/Users/John Bonney/Desktop/Magne_projects/sales_taxes/reports/figs/comparing.png",
  height = 120, width = 180, units = "mm"
)
