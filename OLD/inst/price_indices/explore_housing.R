#' Author: John Bonney
#'
#' Explore covariates in an event-study fashion.

library(data.table)
library(sales.taxes)
library(tidyverse)
library(zoo)

setwd("C:/Users/John Bonney/Desktop/Magne_projects/sales_taxes/output")

main_dir <- "C:/Users/John Bonney/Desktop/Magne_projects/sales_taxes/data/"
zillow_path <- paste0(main_dir, "zillow/zillow_long_by_county_clean.csv")

county_weights <- fread("C:/Users/John Bonney/Dropbox/Sales tax/Data/county_population.csv")
tr_groups_path <- "C:/Users/John Bonney/Desktop/Magne_projects/sales_taxes/output/tr_groups_comprehensive.csv"
tr_events_path <- "C:/Users/John Bonney/Desktop/Magne_projects/sales_taxes/output/event_study_tr_groups_comprehensive.csv"

# Load in treatment ------------------------------------------------------------
tr_groups <- fread(tr_groups_path)
tr_events <- fread(tr_events_path)

# Load county x time covariates ------------------------------------------------
zillow_data <- fread(zillow_path)

## Collapse data (calendar time) ===============================================
# Merge on treatment -----------------------------------------------------------

zillow_data <- zillow_data[year %in% 2008:2014 & !is.na(median_home_price)]
zillow_data[, median_home_price := log(median_home_price)]
zillow_data[, normalized.home_price := median_home_price -
              median_home_price[as.integer(year) == 2008 & as.integer(month) == 1], .(fips_state, fips_county)]

zillow_data <- merge(zillow_data, county_weights, by = c("fips_county", "fips_state"))
zillow_data <- zillow_data[!is.na(population) & !is.na(normalized.home_price)]
zillow_data[, county_ID := fips_state * 1000 + fips_county]
zillow_data <- balance_panel_data(zillow_data, time_vars = c("month", "year"),
                                  panel_unit = "county_ID", n_periods = 84)

zillow_data <- merge(zillow_data, tr_groups,
                     by = c("fips_state", "fips_county"),
                     all = TRUE,
                     allow.cartesian = TRUE)
zillow_data <- zillow_data[!is.na(tr_group) & !is.na(normalized.home_price)]

## aggregate across treatment groups -------------------------------------------
zillow_collapsed <- zillow_data[, list(
  mean.homeprice = weighted.mean(x = normalized.home_price, w = population),
  n_counties = uniqueN(county_ID)
), by = c("tr_group", "year", "month")]
zillow_collapsed <- zillow_collapsed[!is.na(year)]

zillow_collapsed <- add_tr_count(collapsed_data = zillow_collapsed,
                                 tr_group_name = "tr_group",
                                 count_col_name = "n_counties")
print(zillow_collapsed)
fwrite(zillow_collapsed, "C:/Users/John Bonney/Desktop/Magne_projects/sales_taxes/data/zillow_ct.csv")

# Event time -------------------------------------------------------------------

zillow_data <- fread(zillow_path)
zillow_data <- zillow_data[year %in% 2008:2014]
zillow_data[, median_home_price := log(median_home_price)]
zillow_data <- merge(zillow_data, county_weights, by = c("fips_county", "fips_state"))

zillow_data <- zillow_data[!is.na(population) & !is.na(median_home_price)]

## balance panel ---------------------------------------------------------------
zillow_data[, county_ID := fips_state * 1000 + fips_county]
zillow_data <- balance_panel_data(zillow_data, time_vars = c("month", "year"),
                                  panel_unit = "county_ID", n_periods = 84)

zillow_original <- copy(zillow_data)

## merge treatment, attach event times -----------------------------------------
setnames(tr_events, "V1", "event_ID", skip_absent = TRUE)
zillow_data <- merge(zillow_data, tr_events,
                     by = c("fips_state", "fips_county"),
                     all = TRUE,
                     allow.cartesian = TRUE)
zillow_data <- zillow_data[!is.na(median_home_price) & !is.na(ref_year)]

## define time to event --------------------------------------------------------
zillow_data[, tt_event := 12 * as.integer(year) + as.integer(round(month)) -
                                    (12 * ref_year + ref_month)]

## limit data to two year window around reform ---------------------------------
zillow_data <- zillow_data[tt_event >= -12 & tt_event <= 12]

## add pseudo-control group ----------------------------------------------------

### create unique dataset of never treated counties
control_counties <- fread(tr_groups_path)
control_counties <- control_counties[tr_group == "No change"]
control_counties <- unique(control_counties[, .(fips_county, fips_state)])
control_dt <- merge(zillow_original, control_counties,
                    by = c("fips_state", "fips_county"))

rm(zillow_original)
gc()

### take the mean for each time period for each product module code
control_dt <- control_dt[,
                         list(control.houseprice = weighted.mean(median_home_price, w = population)),
                         by = .(month, year)
                         ]

matched_control_data <- merge(zillow_data, control_dt, by = c("month", "year"))
matched_control_data <- matched_control_data[, .(control.houseprice, tt_event, event_ID,
                                                 fips_state, fips_county, population,
                                                 tr_group, ref_year, ref_month)]

setnames(matched_control_data,
         old = c("control.houseprice"),
         new = c("median_home_price"))
matched_control_data[, tr_group := paste0("No change (", tolower(tr_group), ")")]
zillow_data <- rbind(zillow_data, matched_control_data, fill = T)

## normalize price indices based on time to event ------------------------------
zillow_data[, normalized.houseprice := median_home_price - median_home_price[tt_event == -2],
         by = .(fips_state, fips_county, ref_year, ref_month,
              tr_group, event_ID)]

# note that this is the log difference (log was calculated earlier)

## aggregate by treatment group ------------------------------------------------
zillow_es_collapsed <- zillow_data[,
                              list(mean_price = weighted.mean(normalized.houseprice, w = population),
                                   n_counties = uniqueN(1000 * fips_state + fips_county)),
                              by = c("tr_group", "tt_event")
                              ]

zillow_es_collapsed <- add_tr_count(collapsed_data = zillow_es_collapsed,
                                    tr_group_name = "tr_group",
                                    count_col_name = "n_counties")

fwrite(zillow_es_collapsed, "C:/Users/John Bonney/Desktop/Magne_projects/sales_taxes/data/zillow_es.csv")

## Plot results ================================================================

## define theme I like ---------------------------------------------------------

myTheme <- theme_bw() +  theme(
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_line(size = 0.1, colour = 'grey'),
  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
  strip.background = element_rect(colour = "white", fill = "white"),
  plot.title = element_text(hjust = 0.5, size = 14), panel.spacing = unit(2, "lines"),
  legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"),
  axis.text.x = element_text(margin = unit(rep(0.3, 4), "cm")),
  axis.text.y = element_text(margin = unit(rep(0.3, 4), "cm")),
  axis.text.y.right = element_text(margin = unit(rep(0.3, 4), "cm"))
)

## house prices ----------------------------------------------------------------

zillow_collapsed$year_mon <- as.yearmon(as.integer(zillow_collapsed$year) +
                                          as.integer(zillow_collapsed$month) / 12)
zillow_collapsed[, tr_count := gsub("=", " = ", tr_count)]

zillow.calendar.plot <- ggplot(data = zillow_collapsed, mapping = aes(x = year_mon,
                                                         y = mean.homeprice,
                                                         color = tr_count)) +
  geom_line(size = 1) +
  labs(x = "Month", y = expression(paste("Normalized ln(", italic("median home price"), ")")), color = NULL,
       caption = expression(paste(italic("Note: "), "Weighted by county population in 2000. ",
                                  "Sales tax changes are any changes occurring between 2009 and 2013."))) +
  ggtitle("Median home price by sales tax change") +
  scale_x_yearmon(format = "%Y-%m", expand = c(0.01, 0.01)) +
  scale_y_continuous(expand = c(0.005, 0.005)) +
  myTheme +
  theme(legend.position = c(0.6, 0.8), axis.ticks.length = unit(-0.15, "cm"))
zillow.calendar.plot

ggsave("server/pi_figs/pretty/covariates/houseprices_ct.png",
       height = 120 * 1.2, width = 180 * 1.2, units = "mm")

## Event time plot -------------------------------------------------------------

plot_breaks <- c("ed", "nc.ed", "ei", "nc.ei", "io", "nc.io")
plot_labels <- c("Ever decrease", "No change (ever decrease)", "Ever increase",
                 "No change (ever increase)", "Increase only", "No change (increase only)")

et.abbr <- data.table(tr_group = plot_labels, tr_abbr = plot_breaks)
zillow_es_collapsed <- zillow_es_collapsed[et.abbr, on = "tr_group"]

## add informative labels (until the add_tr_count is fixed?)
# TODO: fix these labels, also figure out why the count is changing...
for (i in 1:length(plot_labels)) {
  if (!grepl("\\(", plot_labels[i])) {
    new_lab <- paste0(
      plot_labels[i], " (n = ",
      mean(zillow_es_collapsed[tr_group == plot_labels[i], .(n_counties)][[1]]), ")"
    )
    plot_labels[i] <- new_lab
  }
}

zillow.event.plot <- ggplot(zillow_es_collapsed, aes(x = tt_event, y = mean_price,
                                        color = tr_abbr, linetype = tr_abbr)) +
  geom_line(size = 1) +
  labs(x = "Months from event time",
       y = expression(paste("Normalized ln(", italic("median home price"), ")")),
       color = NULL,
       caption = paste(
         "Note: Weighted by county population in 2000. ",
         "Sales tax changes are any changes occurring between 2009 and 2013.")) +
  ggtitle("Median home price by sales tax change") +
  scale_y_continuous(expand = c(0.005, 0.005)) +
  scale_color_manual(name = NULL, breaks = plot_breaks, labels = plot_labels,
                     values = c("#F8766D", "#00BA38", "#619CFF",
                                "#F8766D", "#00BA38", "#619CFF")) +
  scale_linetype_manual(name = NULL, breaks = plot_breaks, labels = plot_labels,
                        values = c("solid", "solid", "solid",
                                   "11", "11", "11")) +
  geom_vline(xintercept = 0, color = "maroon", size = 0.8, alpha = 0.5, linetype = "dashed") +
  myTheme +
  theme(legend.position = c(0.5, 0.8), axis.ticks.length = unit(-0.15, "cm"))
zillow.event.plot

ggsave("server/pi_figs/pretty/covariates/houseprices_es.png",
       height = 120 * 1.2, width = 180 * 1.2, units = "mm")
