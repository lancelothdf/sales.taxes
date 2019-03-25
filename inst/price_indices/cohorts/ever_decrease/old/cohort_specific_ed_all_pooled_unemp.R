#' Author: John Bonney
#'
#' Now we should "pool" across cohorts for both a diff-in-diff and a pure event
#' study design and see which one looks better.  Those would be similar graphs
#' to the ones you have been doing recently.  We revert back to defining cohorts
#' by quarters.

#' 3b)  Plot mean price index (tax inclusive) for all counties that increased
#' their taxes at some point (ever increase group).  Then build two alternative
#' control groups: A) same as we have been doing - use an average of counties
#' that never change their taxes "matching" calendar time and product.  B) for
#' each "treated cohort" build the control group by taking all counties from the
#' ever increase group that have not yet increased their taxes (but will in the
#' future) "matching" on calendar time and product - average over the different
#' control groups for each treated cohortXproduct.

library(tidyverse)
library(data.table)
library(readstata13)
library(sales.taxes)
library(zoo)

setwd("/project2/igaarder")
prep.new.data <- F

# check function
g <- function(dt) {
  print(head(dt))
}

change_of_interest <- "Ever decrease"
output_filepath <- "Data/unemprate_all_cohorts_ed_pooled_extended.csv"

## useful filepaths ------------------------------------------------------------
eventstudy_tr_path <- "Data/event_study_tr_groups_comprehensive_w2014.csv"
tr_groups_path <- "Data/tr_groups_comprehensive_w2014.csv"
sales_data_path <- "Data/sales_monthly_2006-2016.csv"
tax_rates_path <- "Data/county_monthly_tax_rates.csv"
quarterly_tax_path <- "Data/quarterly_tax_rates.csv"
module_exemptions_path <- "Data/modules_exemptions_long.csv"
all_goods_pi_path <- "Data/Nielsen/price_quantity_indices_allitems_2006-2016_notaxinfo.csv"
taxable_pi_path <- "Data/Nielsen/price_quantity_indices_taxableitems.csv"
expanded.reforms.path <- "Data/tax_reforms_all_incl2014.csv"

unemp_path <- "Data/covariates/county_monthly_unemp_clean.csv"

county_weights <- fread("Data/county_population.csv")
unemp_dt <- fread(unemp_path)
unemp_dt <- unemp_dt[between(year, 2006, 2014)]

###  attach sales --------------------------------------------------------------
sales_data <- fread(sales_data_path)
sales_data <- sales_data[, .(store_code_uc, product_module_code, fips_county,
                             fips_state, month, year, sales)]
sales_data <- sales_data[year <= 2014]
sales_data <- sales_data[, list(sales = sum(sales)),
                         by = .(fips_county, fips_state, month, year)]

unemp_dt <- merge(unemp_dt, sales_data,
                   by = c("fips_county", "fips_state", "month", "year"))
rm(sales_data)
gc()

## prep the data ---------------------------------------------------------------

## normalize
unemp_dt[, normalized.rate := rate] # just for consistency in notation -- we don't actually normalized unemp
unemp_dt[, base.sales := sales[year == 2008 & month == 1],
       by = .(fips_county, fips_state)]
unemp_dt <- unemp_dt[!is.na(normalized.rate) & !is.na(base.sales)]

## balance on county level
keep_counties <- unemp_dt[, list(n = .N), by = .(fips_state, fips_county)]
keep_counties <- keep_counties[n == (2014 - 2005) * 12]

setkey(unemp_dt, fips_county, fips_state)
setkey(keep_counties, fips_county, fips_state)

unemp_dt <- unemp_dt[keep_counties]

## prep treatment events -------------------------------------------------------
tr.events <- fread(eventstudy_tr_path)
tr.events <- tr.events[tr_group == change_of_interest]
tr.events[, treatment_month := 12 * ref_year + ref_month]
## we are only interested in the first event
tr.events[, min.event := (treatment_month == min(treatment_month)),
          by = .(fips_state, fips_county)]
tr.events <- tr.events[min.event == T]
tr.events[, n_events := .N, by = .(fips_state, fips_county)]
tr.events <- tr.events[, .(fips_county, fips_state, ref_year, ref_month, n_events)]
# exclude 2012 Q4, 2013 Q1, 2013 Q2 reforms
tr.events <- tr.events[!(ref_year == 2012 & ref_month >= 10) & !(ref_year == 2013 & ref_month %in% 1:6)]
g(tr.events)

setnames(tr.events, "ref_year", "treatment_year")
setkey(tr.events, fips_county, fips_state)

cohort_sizes <- tr.events[, list(cohort_size = .N), by = .(treatment_year, ref_month)]

# for later use
tr.groups <- fread(tr_groups_path)
never.treated.master <- tr.groups[tr_group == "No change"]
never.treated.master <- never.treated.master[, .(fips_state, fips_county)]
setkey(never.treated.master, fips_state, fips_county)

## iterate over all quarters and years -----------------------------------------
master_res <- data.table(NULL)
pool_cohort_weights <- data.table(NULL)

for (ref.year in 2009:2013) {
  for (ref.month in 1:12) {
    if ((ref.year == 2012 & ref.month %in% 10:12) | (ref.year == 2013 & ref.month %in% 1:6)) {
      print(paste("Skipping", ref.year, "m", ref.month))
      next
    }
    print(paste("Year:", ref.year, "; Month:", ref.month))
    ## identify treated cohort -------------------------------------------------
    tr.events.09m1 <- tr.events[treatment_year == ref.year & ref_month == ref.month]
    if (nrow(tr.events.09m1) == 0) {
      print("No events, skipping")
      next
    }
    setkey(tr.events.09m1, fips_county, fips_state)

    unemp.09m1 <- unemp_dt[tr.events.09m1]
    g(unemp.09m1)

    ## get sum of sales in treated counties
    pool_cohort_weights <- rbind(
      pool_cohort_weights,
      data.table(ref_year = ref.year, ref_month = ref.month,
                 ## sum of all base sales weights in the cohort at time of treatment
                 cohort_sales = sum(unemp.09m1[year == ref.year & month == ref.month]$base.sales))
    )

    ## prepare control data --------------------------------------------------------

    ## identify never treated counties
    never.treated <- unemp_dt[never.treated.master]
    never.treated[, group := "No change"]
    g(never.treated)

    ## identify not-yet-treated (but future treated) counties
    unemp_ss <- unemp_dt[tr.events[treatment_year > ref.year |
                                 (treatment_year == ref.year &
                                    ref_month > ref.month)]]
    unemp_ss[, treatment_month := 12 * treatment_year + ref_month]
    unemp_ss[, calendar_month := 12 * year + month]
    unemp_ss[, min_treat_month := min(treatment_month), by = .(fips_state, fips_county)]

    unemp_ss_yearplus <- unemp_ss[min_treat_month > (12 * ref.year + ref.month + 12) &
                                    min_treat_month > calendar_month]
    future_restr_grp <- T
    if (nrow(unemp_ss_yearplus) == 0) {
      future_restr_grp <- F
    } else {
      unemp_ss_yearplus[, group := "Future restricted"]
    }

    unemp_ss <- unemp_ss[min_treat_month > calendar_month]
    unemp_ss[, group := "Future"]

    g(unemp_ss)
    g(unemp_ss_yearplus)

    ## combine never treated + later cohorts
    unemp_ss <- rbind(unemp_ss, unemp_ss_yearplus, never.treated, fill = T)
    g(unemp_ss)
    rm(never.treated, unemp_ss_yearplus)

    # ss_pi[, event.weight := ifelse(is.na(n_events), 1, 1 / n_events)]

    ## collapse to group x time level ------------------------------------------
    unemp_ss.collapsed <- unemp_ss[, list(
      control.rate = weighted.mean(normalized.rate, w = base.sales)
    ), by = .(year, month, group)]
    rm(unemp_ss)
    g(unemp_ss.collapsed)

    ## rearrange for simple merging of groups onto 2009 m1 cohort
    unemp_ss.collapsed <- tidyr::spread(unemp_ss.collapsed, group, control.rate)
    g(unemp_ss.collapsed)

    ## merge onto the treated cohort by product
    unemp.09m1 <- merge(unemp.09m1, unemp_ss.collapsed,
                             by = c("year", "month"))
    rm(unemp_ss.collapsed)

    ## aggregate over calendar time ------------------------------------------------
    g(unemp.09m1)

    if (future_restr_grp) {
      unemp.09m1.collapsed <- unemp.09m1[, list(
        mean.rate = weighted.mean(normalized.rate, w = base.sales),
        Future = weighted.mean(Future, w = base.sales),
        `Future restricted` = weighted.mean(`Future restricted`, w = base.sales),
        `No change` = weighted.mean(`No change`, w = base.sales)
      ), by = .(year, month)]
    } else {
      unemp.09m1.collapsed <- unemp.09m1[, list(
        mean.rate = weighted.mean(normalized.rate, w = base.sales),
        Future = weighted.mean(Future, w = base.sales),
        `No change` = weighted.mean(`No change`, w = base.sales)
      ), by = .(year, month)]
      unemp.09m1.collapsed[, `Future restricted` := NA]
    }

    g(unemp.09m1.collapsed)

    setnames(unemp.09m1.collapsed, "mean.rate", "Treated")
    unemp.09m1.collapsed <- tidyr::gather(unemp.09m1.collapsed,
                                               key = group, value = rate,
                                               c(Treated, Future,
                                                 `Future restricted`, `No change`))
    g(unemp.09m1.collapsed)
    unemp.09m1.collapsed <- as.data.table(unemp.09m1.collapsed)
    unemp.09m1.collapsed <- unemp.09m1.collapsed[!is.na(rate)]
    unemp.09m1.collapsed[, ref_year := ref.year]
    unemp.09m1.collapsed[, ref_month := ref.month]
    master_res <- rbind(master_res, unemp.09m1.collapsed)

    rm(unemp.09m1.collapsed, unemp.09m1)
  }
}

fwrite(master_res, output_filepath)

setnames(cohort_sizes, "treatment_year", "ref_year")
## merge on cohort size
master_res <- merge(master_res, cohort_sizes, by = c("ref_year", "ref_month"))
master_res <- merge(master_res, pool_cohort_weights, by = c("ref_year", "ref_month"))

fwrite(master_res, output_filepath)
