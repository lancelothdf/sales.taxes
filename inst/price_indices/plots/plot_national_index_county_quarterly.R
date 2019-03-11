#' Author: John Bonney
#'
#' Plot Lance's monthly (geometric and Laspeyres) price indices, comparing to CPI

setwd("C:/Users/John Bonney/Desktop/Magne_projects/sales_taxes")

library(tidyverse)
library(zoo)

lance.pi_path <- "output/server/national_pi_county_quarterly_balanced.csv"
cpi_path <- "data/CPI/national_monthly_food_beverage_chained_cpi.csv"
outfile_figpath <- "reports/figs/comparing_county_quarterly_balanced.png"

### Import and clean geometric price indices -----------------------------------
pi_county <- read.csv(lance.pi_path) %>% select(-contains("ratio"))
pi_county$t <- with(pi_county, as.yearqtr(paste0(year, " Q", quarter)))

pi_all_balanced <- pi_county %>%
  gather(index, chained_food_cpi,
         national.cpricei.unbal:national.geocpricei.bal)

### Import and clean CPI data  -------------------------------------------------
cpi_data <- read.csv("data/CPI/national_monthly_food_beverage_chained_cpi.csv") %>%
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

cpi_data <- base::rbind.data.frame(ungroup(cpi_data), pi_all_balanced)

## the graph looks identical whether or not we balance on the store or store-produce level.
cpi_data <- cpi_data %>% filter(!grepl("unbal", index))
#cpi_data <- cpi_data %>% filter(!index %in% c("national.geocpricei.bal", "national.cpricei.bal"))

### compare the two indices ----------------------------------------------------
ggplot(mapping = aes(x = t, y = chained_food_cpi, linetype = index, color = index)) +
  geom_line(data = cpi_data, aes(group = index), size = 1) +
  scale_x_yearmon(expand = c(0.01, 0.01), n = 6) +
  scale_y_continuous(breaks = seq(0, 1.25, .05), expand = c(0.005, 0.005)) +
  theme_bw() +
  labs(y = expression(bold("Price Index (Normalized Dec 2006 = 1.00)")), x = expression(bold("Month"))) +
  scale_linetype_manual(name = NULL, breaks = c("cpi", "national.cpricei.bal", "national.geocpricei.bal"),
                        labels = c("Food & Beverage Chained CPI",
                                   "Retail Scanner Index (Geometric)",
                                   "Retail Scanner Index (Laspeyres)"),
                        values = c("solid", "33", "33")) +
  scale_color_manual(name = NULL, breaks = c("cpi", "national.geocpricei.bal", "national.cpricei.bal"),
                     labels = c("Food & Beverage Chained CPI",
                                "Retail Scanner Index (Geometric)",
                                "Retail Scanner Index (Laspeyres)"),
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

ggsave(outfile_figpath, height = 120, width = 180, units = "mm")
