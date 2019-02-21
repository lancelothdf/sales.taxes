#' Author: John Bonney
#'
#' Explore covariates in an event-study fashion.

library(data.table)
library(sales.taxes)
library(tidyverse)
library(zoo)

rm(list=ls())
setwd("C:/Users/John Bonney/Desktop/Magne_projects/sales_taxes/output")

county_weights <- fread("C:/Users/John Bonney/Dropbox/Sales tax/Data/county_population.csv")
tr_groups_path <- "tr_groups_comprehensive.csv"
tr_events_path <- "event_study_tr_groups_comprehensive.csv"

main_dir <- "C:/Users/John Bonney/Desktop/Magne_projects/sales_taxes/data/"
unemp_path <- paste0(main_dir, "bls_cty_unemp/county_monthly_unemployment.csv")
new.unemp.path <- paste0(main_dir, "bls_cty_unemp/county_monthly_unemp_clean.csv")

# Load in treatment ------------------------------------------------------------
tr_groups <- fread(tr_groups_path)
tr_events <- fread(tr_events_path)

# Load county x time covariates ------------------------------------------------

## read in unemployment data
unemp_data <- unique(fread(unemp_path))
unemp_data <- unemp_data[State != "State"]
setnames(unemp_data, old = names(unemp_data),
         new = tolower(names(unemp_data)))
setnames(unemp_data, old = c("state", "county"), new = c("state_name", "county_name"))

## fix the duplicates issue (the first in the group is the correct one)
## Note: 9% are duplicated (years 2010 and above). Something with scraping.
unemp_data[, unit_ID := .I]
unemp_data[, duplicate_group := .GRP, by = .(year, month, state_name, county_name)]
unemp_data[, first_ID := min(unit_ID), by = duplicate_group]
unemp_data <- unemp_data[unit_ID == first_ID]
unemp_data[, c("unit_ID", "duplicate_group", "first_ID") := NULL]

unemp_data[, county_name := tolower(gsub("Saint", "St.", county_name))]
unemp_data <- unemp_data[state_name != "Alaska"]
unemp_data[state_name == "Illinois" & county_name == "la salle county", county_name := "lasalle county"]
unemp_data[county_name == "queen annes county", county_name := "queen anne's county"]
unemp_data[county_name == "st. marys county", county_name := "st. mary's county"]
unemp_data[county_name == "st.e genevieve county", county_name := "ste. genevieve county"]
unemp_data[county_name == "debaca county", county_name := "de baca county"]
unemp_data[county_name == "dona ana county", county_name := "doða ana county"]
unemp_data[county_name == "mc kean county", county_name := "mckean county"]

counties <- fread("C:/Users/John Bonney/Desktop/Magne_projects/sales_taxes/data/county_fips_master.csv") %>%
  select(fips, county_name, state_name, state_abbr)
counties[, county_name := tolower(county_name)]

unemp_data <- merge(unemp_data, counties)
unemp_data[, fips_state := floor(fips / 1000)]
unemp_data[, fips_county := fips - fips_state * 1000]

unemp_data[, rate := as.numeric(rate)]
unemp_data[, year := as.integer(year)]
unemp_data[, month := match(month, month.name)]

fwrite(unemp_data, new.unemp.path)

## Unemployment data ===========================================================
# Merge on treatment -----------------------------------------------------------
unemp_data <- fread(new.unemp.path)
setkey(unemp_data, fips_state, fips_county, year, month)

## create rolling average to smooth out seasonality
ravg.val <- 5
if (ravg.val != 1){
  unemp_data[, ravg.rate := rollmean(rate, k = ravg.val, align = "right", na.pad = T),
             by = .(fips_state, fips_county)]
  unemp_data[, rate := ravg.rate]
  unemp_data <- unemp_data[!is.na(rate)]
}

unemp_data <- unemp_data[year %in% 2008:2014 & !is.na(rate)]
unemp_data[, log.rate := log(rate)]
unemp_data[, normalized.rate := log.rate -
              log.rate[as.integer(year) == 2008 & as.integer(month) == 1], .(fips_state, fips_county)]

unemp_data <- merge(unemp_data, county_weights, by = c("fips_county", "fips_state"))
unemp_data <- unemp_data[!is.na(population) & !is.na(normalized.rate)]
unemp_data[, county_ID := fips_state * 1000 + fips_county]
unemp_data <- balance_panel_data(unemp_data, time_vars = c("month", "year"),
                                  panel_unit = "county_ID", n_periods = 84)

unemp_data <- merge(unemp_data, tr_groups,
                     by = c("fips_state", "fips_county"),
                     all = TRUE,
                     allow.cartesian = TRUE)
unemp_data <- unemp_data[!is.na(tr_group)]

## aggregate across treatment groups -------------------------------------------
unemp_collapsed <- unemp_data[, list(
  mean.rate = weighted.mean(x = normalized.rate, w = population),
  n_counties = uniqueN(county_ID)
), by = c("tr_group", "year", "month")]
unemp_collapsed <- unemp_collapsed[!is.na(year)]

unemp_collapsed <- add_tr_count(collapsed_data = unemp_collapsed,
                                 tr_group_name = "tr_group",
                                 count_col_name = "n_counties")
print(unemp_collapsed[])
fwrite(unemp_collapsed, "C:/Users/John Bonney/Desktop/Magne_projects/sales_taxes/data/unemp_ct_rollmean5.csv")

# Event time -------------------------------------------------------------------

unemp_data <- fread(new.unemp.path)

## create rolling average to smooth out seasonality
ravg.val <- 1
if (ravg.val != 1){
  unemp_data[, ravg.rate := rollmean(rate, k = ravg.val, align = "right", na.pad = T),
             by = .(fips_state, fips_county)]
  unemp_data[, rate := ravg.rate]
  unemp_data <- unemp_data[!is.na(rate)]
}

unemp_data <- unemp_data[year %in% 2008:2014]
unemp_data[, rate := log(rate)]
unemp_data <- merge(unemp_data, county_weights, by = c("fips_county", "fips_state"))

unemp_data <- unemp_data[!is.na(population) & !is.na(rate)]

## balance panel ---------------------------------------------------------------
unemp_data[, county_ID := fips_state * 1000 + fips_county]
unemp_data <- balance_panel_data(unemp_data, time_vars = c("month", "year"),
                                  panel_unit = "county_ID", n_periods = 84)

unemp_original <- copy(unemp_data)

## merge treatment, attach event times -----------------------------------------
unemp_data <- merge(unemp_data, tr_events,
                    by = c("fips_state", "fips_county"),
                    all = TRUE,
                    allow.cartesian = TRUE)
unemp_data <- unemp_data[!is.na(rate) & !is.na(ref_year)]

setnames(unemp_data, "V1", "event_ID")

## define time to event --------------------------------------------------------
unemp_data[, tt_event := as.integer(12 * year     + month -
                                   (12 * ref_year + ref_month))]

## limit data to two year window around reform ---------------------------------
unemp_data <- unemp_data[tt_event >= -12 & tt_event <= 12]

## add pseudo-control group ----------------------------------------------------

### create unique dataset of never treated counties
control_counties <- fread(tr_groups_path)
control_counties <- control_counties[tr_group == "No change"]
control_counties <- unique(control_counties[, .(fips_county, fips_state)])
control_dt <- merge(unemp_original, control_counties,
                    by = c("fips_state", "fips_county"))

rm(unemp_original)
gc()

### take the mean for each time period for each product module code
control_dt <- control_dt[,
                         list(control.rate = weighted.mean(rate, w = population)),
                         by = .(month, year)
                         ]

matched_control_data <- merge(unemp_data, control_dt, by = c("month", "year"))
matched_control_data <- matched_control_data[, .(control.rate, tt_event, event_ID,
                                                 fips_state, fips_county, population,
                                                 tr_group, ref_year, ref_month)]

setnames(matched_control_data,
         old = c("control.rate"),
         new = c("rate"))
matched_control_data[, tr_group := paste0("No change (", tolower(tr_group), ")")]
unemp_data <- rbind(unemp_data, matched_control_data, fill = T)

## normalize price indices based on time to event ------------------------------
unemp_data[, normalized.rate := rate - rate[tt_event == -2],
         by = .(fips_state, fips_county, ref_year, ref_month,
                tr_group, event_ID)]

# note that this is the log difference (log was calculated earlier)

## aggregate by treatment group ------------------------------------------------
unemp_es_collapsed <- unemp_data[,
                              list(mean_rate = weighted.mean(normalized.rate, w = population),
                                   n_counties = uniqueN(1000 * fips_state + fips_county)),
                              by = c("tr_group", "tt_event")
                              ]

unemp_es_collapsed <- add_tr_count(collapsed_data = unemp_es_collapsed,
                                    tr_group_name = "tr_group",
                                    count_col_name = "n_counties")

fwrite(unemp_es_collapsed, "C:/Users/John Bonney/Desktop/Magne_projects/sales_taxes/data/unemp_es_rollmean5.csv")

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

## calendar time ---------------------------------------------------------------

unemp_collapsed$year_mon <- as.yearmon(as.integer(unemp_collapsed$year) +
                                          as.integer(unemp_collapsed$month) / 12)
unemp_collapsed[, tr_count := gsub("=", " = ", tr_count)]

unemp.calendar.plot <- ggplot(data = unemp_collapsed, mapping = aes(x = year_mon,
                                                         y = mean.rate,
                                                         color = tr_count)) +
  geom_line(size = 1) +
  labs(x = "Month", y = expression(paste("Normalized ln(", italic("unemployment rate"), ")")), color = NULL,
       caption = expression(paste(italic("Note: "), "Weighted by county population in 2000. ",
                                  "Sales tax changes are any changes occurring between 2009 and 2013."))) +
  ggtitle("Unemployment (seasonally adjusted) by sales tax change") +
  scale_x_yearmon(format = "%Y-%m", expand = c(0.01, 0.01)) +
  scale_y_continuous(expand = c(0.005, 0.005)) +
  myTheme +
  theme(legend.position = c(0.4, 0.3), axis.ticks.length = unit(-0.15, "cm"))
unemp.calendar.plot
ggsave("server/pi_figs/pretty/covariates/unemp_ct_rollavg5.png",
       height = 120 * 1.2, width = 180 * 1.2, units = "mm")

## event time plot -------------------------------------------------------------

plot_breaks <- c("ed", "nc.ed", "ei", "nc.ei", "io", "nc.io")
plot_labels <- c("Ever decrease", "No change (ever decrease)", "Ever increase",
                 "No change (ever increase)", "Increase only", "No change (increase only)")

et.abbr <- data.table(tr_group = plot_labels, tr_abbr = plot_breaks)
unemp_es_collapsed <- unemp_es_collapsed[et.abbr, on = "tr_group"]

## add informative labels (until the add_tr_count is fixed?)
for (i in 1:length(plot_labels)) {
  if (!grepl("\\(", plot_labels[i])) {
    new_lab <- paste0(
      plot_labels[i], " (n = ",
      mean(unemp_es_collapsed[tr_group == plot_labels[i], .(n_counties)][[1]]), ")"
    )
    plot_labels[i] <- new_lab
  }
}

unemp.event.plot <- ggplot(unemp_es_collapsed, aes(x = tt_event, y = mean_rate,
                                        color = tr_abbr, linetype = tr_abbr)) +
  geom_line(size = 1) +
  labs(x = "Months from event time",
       y = expression(paste("Normalized ln(", italic("unemployment rate"), ")")),
       color = NULL,
       caption = paste(
         "Note: Weighted by county population in 2000. ",
         "Sales tax changes are any changes occurring between 2009 and 2013.")) +
  ggtitle("Unemployment by sales tax change") +
  scale_y_continuous(expand = c(0.005, 0.005)) +
  scale_color_manual(name = NULL, breaks = plot_breaks, labels = plot_labels,
                     values = c("#F8766D", "#00BA38", "#619CFF",
                                "#F8766D", "#00BA38", "#619CFF")) +
  scale_linetype_manual(name = NULL, breaks = plot_breaks, labels = plot_labels,
                        values = c("solid", "solid", "solid",
                                   "11", "11", "11")) +
  geom_vline(xintercept = 0, color = "maroon", size = 0.8, alpha = 0.5, linetype = "dashed") +
  myTheme +
  theme(legend.position = c(0.65, 0.2), axis.ticks.length = unit(-0.15, "cm"))
unemp.event.plot

ggsave("server/pi_figs/pretty/covariates/unemp_es.png",
       height = 120 * 1.2, width = 180 * 1.2, units = "mm")
