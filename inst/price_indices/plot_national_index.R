#' Author: John Bonney
#'
#' Plot Lance's price indices, comparing to CPI

setwd("C:/Users/John Bonney/Desktop/Magne_projects/sales_taxes/data")

library(tidyverse)
library(zoo)

# TODO: replace fake data with real data
fake_data <- expand.grid(year = 2006:2016, quarter = 1:4) %>% as.tibble() %>%
  mutate(chained_food_cpi = 1 + (year - 2006) / 36  + quarter * runif(44, 0, .01),
         index = "retail_scanner") %>% filter(year > 2006 | quarter == 4)
fake_data$t <- with(fake_data, as.yearqtr(paste0(year, " Q", quarter)))

base_index <- fake_data %>%
  filter(year == 2006, quarter == 4) %>%
  select(chained_food_cpi) %>% pull()
fake_data <- fake_data %>% mutate(chained_food_cpi = chained_food_cpi / base_index)
# TODO: aggregate CPI to quarters?

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

cpi_data <- base::rbind.data.frame(ungroup(cpi_data), fake_data)

ggplot(mapping = aes(x = t, y = chained_food_cpi, linetype = index, color = index)) +
  geom_line(data = cpi_data, size = 1) +
  geom_line(data = fake_data, size = 1) +
  scale_x_yearqtr(format = "%Y Q%q", expand = c(0.01, 0.01), n = 6) +
  scale_y_continuous(breaks = seq(0, 1.25, .05), expand = c(0.005, 0.005)) +
  theme_bw() +
  labs(y = expression(bold("Price Index (Normalized 2006 Q4 = 1.00)")), x = expression(bold("Year-Quarter"))) +
  scale_linetype_manual(name = NULL, breaks = c("cpi", "retail_scanner"),
                        labels = c("Consumer Price Index (CPI)", "Retail Scanner Index"),
                        values = c("solid", "31")) +
  scale_color_manual(name = NULL, breaks = c("cpi", "retail_scanner"),
                     labels = c("Consumer Price Index (CPI)", "Retail Scanner Index"),
                     values = c("black", "red")) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(size = 0.1, colour = 'grey'),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    strip.background = element_rect(colour="white", fill="white"),
    plot.title = element_text(hjust = 0.5, size = 11), panel.spacing = unit(2, "lines"),
    legend.position = c(0.8, 0.28), axis.ticks.length=unit(-0.15, "cm"),
    axis.text.x = element_text(margin=unit(rep(0.3, 4), "cm")),
    axis.text.y = element_text(margin=unit(rep(0.3, 4), "cm")),
    axis.text.y.right = element_text(margin=unit(rep(0.3, 4), "cm"))
  )
