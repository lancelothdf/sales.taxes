#' Author: John Bonney
#'
#' Plot Lance's price indices for Ingvil, comparing to CPI

setwd("C:/Users/John Bonney/Desktop/Magne_projects/sales_taxes")

library(tidyverse)
library(zoo)

cpi_path <- "data/CPI/national_monthly_food_beverage_chained_cpi.csv"
outfile_figpath <- "reports/figs/comparing_laspeyres_master.png"
county.quarterly_path <- "output/server/national_pi_county_quarterly_balanced.csv"
store.monthly_path <- "output/server/national_pi_monthly.csv"
store.quarterly_path <- "output/server/national_pi_store_module_balanced.csv"
store.quarterly.incl_nonfood_path <- "output/server/national_pi_quarterly_incl_nonfood.csv"


## CountyXQuarterly index: /project2/igaarder/Data/Nielsen/Quarterly_county_balanced_price_quantity_indices_food.dta
pi_county.quarterly <- read.csv(county.quarterly_path) %>%
  select(year, quarter, chained_food_cpi = national.cpricei.bal) %>%
  mutate(index = "County Quarterly",
         month = (quarter - 1) * 3 + 2) %>%
  select(-quarter)
pi_county.quarterly$t <- with(pi_county.quarterly, as.yearmon(paste0(year, "-", month)))


## StoreXMonthly index: /project2/igaarder/Data/Nielsen/Monthly_price_quantity_indices_food.dta
pi_store.monthly <- read.csv(store.monthly_path) %>%
  select(year, month, chained_food_cpi = national.cpricei.storeprodbal) %>%
  mutate(index = "Store Monthly")
pi_store.monthly$t <- with(pi_store.monthly,
                            as.yearmon(paste0(year, "-", month)))

## StoreXQuarterly index: /project2/igaarder/Data/Nielsen/Price_quantity_indices_food.dta
pi_store.quarterly <- read.csv(store.quarterly_path) %>%
  select(year, quarter, chained_food_cpi = national_index) %>%
  mutate(index = "Store Quarterly",
         month = (quarter - 1) * 3 + 2) %>%
  select(-quarter)
pi_store.quarterly$t <- with(pi_store.quarterly,
                             as.yearmon(paste0(year, "-", month)))

## Food and Beverage CPI

cpi_data <- read.csv(cpi_path) %>% select(-X) %>% filter(between(year, 2006, 2016))
cpi_data$t <- with(cpi_data, as.yearmon(paste0(year, "-", month)))

base_cpi <- cpi_data %>% ungroup() %>%
  filter(year == 2006, month == 12) %>%
  select(chained_food_cpi) %>% pull()
cpi_data <- cpi_data %>%
  filter(year > 2006 | month == 12) %>%
  mutate(chained_food_cpi = chained_food_cpi / base_cpi,
         index = "cpi")

## Join them all together
cpi_data <- rbind(cpi_data,
                  pi_county.quarterly,
                  pi_store.monthly,
                  pi_store.quarterly)

### compare the indices --------------------------------------------------------
ggplot(cpi_data,
       mapping = aes(x = t, y = chained_food_cpi, linetype = index, color = index)) +
  geom_line(aes(group = index), size = 1) +
  scale_x_yearmon(expand = c(0.01, 0.01), n = 6) +
  scale_y_continuous(breaks = seq(0, 1.25, .05), expand = c(0.005, 0.005)) +
  theme_bw() +
  labs(y = expression(bold("Price Index (Normalized Dec 2006 = 1.00)")), x = expression(bold("Month"))) +
  scale_linetype_manual(name = NULL, breaks = c("cpi", "Store Quarterly", "Store Monthly", "County Quarterly"),
                        labels = c("Food & Beverage Chained CPI",
                                   "Retail Scanner Index (store-level, quarterly)",
                                   "Retail Scanner Index (store-level, monthly)",
                                   "Retail Scanner Index (county-level, quarterly)"),
                        values = c("33", "solid", "33", "33")) +
  scale_color_manual(name = NULL, breaks = c("cpi", "Store Quarterly", "Store Monthly", "County Quarterly"),
                     labels = c("Food & Beverage Chained CPI",
                                "Retail Scanner Index (store-level, quarterly)",
                                "Retail Scanner Index (store-level, monthly)",
                                "Retail Scanner Index (county-level, quarterly)"),
                     values = c("#619CFF", "black", "#F8766D", "#00BA38")) +
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


############# including nonfood

## StoreXQuarterly index with nonfood
pi_store.incl_nonfood <- read.csv(store.quarterly.incl_nonfood_path) %>%
  select(year, quarter, chained_food_cpi = national_index) %>%
  mutate(index = "Store Quarterly (w nonfood)",
         month = (quarter - 1) * 3 + 2) %>%
  select(-quarter) %>%
  filter(year <= 2013)
pi_store.incl_nonfood$t <- with(pi_store.incl_nonfood,
                             as.yearmon(paste0(year, "-", month)))

## Food and Beverage CPI

cpi_data <- read.csv(cpi_path) %>% select(-X) %>% filter(between(year, 2006, 2016))
cpi_data$t <- with(cpi_data, as.yearmon(paste0(year, "-", month)))

base_cpi <- cpi_data %>% ungroup() %>%
  filter(year == 2006, month == 12) %>%
  select(chained_food_cpi) %>% pull()
cpi_data <- cpi_data %>%
  filter(year <= 2013, year > 2006 | month == 12) %>%
  mutate(chained_food_cpi = chained_food_cpi / base_cpi,
         index = "cpi")

cpi_data2 <- rbind(cpi_data, pi_store.incl_nonfood)

### compare the indices --------------------------------------------------------
ggplot(cpi_data2,
       mapping = aes(x = t, y = chained_food_cpi, linetype = index, color = index)) +
  geom_line(aes(group = index), size = 1) +
  scale_x_yearmon(expand = c(0.01, 0.01), n = 6) +
  scale_y_continuous(breaks = seq(0, 1.25, .05), expand = c(0.005, 0.005)) +
  theme_bw() +
  labs(y = expression(bold("Price Index (Normalized Dec 2006 = 1.00)")), x = expression(bold("Month"))) +
  scale_linetype_manual(name = NULL, breaks = c("cpi", "Store Quarterly (w nonfood)"),
                        labels = c("Food & Beverage Chained CPI",
                                   "Retail Scanner Index (quarterly, including nonfood)"),
                        values = c("solid", "33")) +
  scale_color_manual(name = NULL, breaks = c("cpi", "Store Quarterly (w nonfood)"),
                     labels = c("Food & Beverage Chained CPI",
                                "Retail Scanner Index (quarterly, including nonfood)"),
                     values = c("black", "#F8766D")) +
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

ggsave("reports/figs/comparing_pi_nonfood.png", height = 120, width = 180, units = "mm")
