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
# check function
g <- function(dt) {
  print(head(dt))
}

## useful filepaths ------------------------------------------------------------
eventstudy_tr_path <- "Data/event_study_tr_groups_comprehensive_w2014.csv"
tr_groups_path <- "Data/tr_groups_comprehensive_w2014.csv"
sales_data_path <- "Data/sales_quarterly_2006-2016.csv"
tax_rates_path <- "Data/county_monthly_tax_rates.csv"
quarterly_tax_path <- "Data/quarterly_tax_rates.csv"
module_exemptions_path <- "Data/modules_exemptions_long.csv"
all_goods_pi_path <- "Data/Nielsen/price_quantity_indices_allitems.csv"
taxable_pi_path <- "Data/Nielsen/price_quantity_indices_taxableitems.csv"
expanded.reforms.path <- "Data/tax_reforms_all_incl2014.csv"

## prep the data ---------------------------------------------------------------
all_pi <- fread(all_goods_pi_path)
all_pi <- all_pi[between(year, 2008, 2014)]

## normalize
all_pi[, normalized.cpricei := log(cpricei) - log(cpricei[year == 2008 & quarter == 1]),
       by = .(store_code_uc, product_module_code)]
all_pi[, base.sales := sales[year == 2008 & quarter == 1],
       by = .(store_code_uc, product_module_code)]
all_pi <- all_pi[!is.na(normalized.cpricei) & !is.na(base.sales)]

## balance on store-module level
keep_store_modules <- all_pi[, list(n = .N),
                             by = .(store_code_uc, product_module_code)]
keep_store_modules <- keep_store_modules[n == (2014 - 2007) * 4]

setkey(all_pi, store_code_uc, product_module_code)
setkey(keep_store_modules, store_code_uc, product_module_code)

all_pi <- all_pi[keep_store_modules]
setkey(all_pi, fips_county, fips_state)

## prep treatment events -------------------------------------------------------
tr.events <- fread(eventstudy_tr_path)
tr.events <- tr.events[tr_group == "Ever increase"]
tr.events[, treatment_month := 12 * ref_year + ref_month]
## we are only interested in the first event
tr.events[, min.event := (treatment_month == min(treatment_month)),
          by = .(fips_state, fips_county)]
tr.events <- tr.events[min.event == T]
tr.events[, ref_quarter := ceiling(ref_month / 3)]
tr.events[, n_events := .N, by = .(fips_state, fips_county)]
tr.events <- tr.events[, .(fips_county, fips_state, ref_year, ref_quarter, n_events)]
g(tr.events)

setnames(tr.events, "ref_year", "treatment_year")
setkey(tr.events, fips_county, fips_state)

cohort_sizes <- tr.events[, list(cohort_size = .N), by = .(ref_year, ref_quarter)]

# for later use
tr.groups <- fread(tr_groups_path)
never.treated.master <- tr.groups[tr_group == "No change"]
never.treated.master <- never.treated.master[, .(fips_state, fips_county)]
setkey(never.treated.master, fips_state, fips_county)

## iterate over all quarters and years -----------------------------------------
master_res <- data.table(NULL)

for (ref.year in 2009:2013) {
  for (ref.quarter in 1:4) {
    ## identify treated cohort -------------------------------------------------
    tr.events.09Q1 <- tr.events[treatment_year == ref.year & ref_quarter == ref.quarter]
    setkey(tr.events.09Q1, fips_county, fips_state)

    taxable_pi.09Q1 <- all_pi[sales_tax > 1]
    taxable_pi.09Q1 <- taxable_pi.09Q1[tr.events.09Q1]

    ## prepare control data --------------------------------------------------------

    ## limit to goods that are taxable for the cohort (e.g., 2009 Q1)
    constant.goods.set <- unique(taxable_pi.09Q1[year == ref.year & quarter == ref.quarter]$product_module_code)
    ss_pi <- all_pi[product_module_code %in% constant.goods.set] # keep goods constant
    rm(constant.goods.set)

    ## identify never treated counties
    never.treated <- ss_pi[never.treated.master]
    never.treated[, group := "No change"]

    ## identify not-yet-treated (but future treated) counties
    ss_pi <- ss_pi[tr.events[treatment_year > ref.year |
                                 (treatment_year == ref.year &
                                    ref_quarter > ref.quarter)]]
    ss_pi[, treatment_quarter := 4 * treatment_year + ref_quarter]
    ss_pi[, calendar_quarter := 4 * year + quarter]
    ss_pi[, not_yet_treated := min(treatment_quarter) > calendar_quarter,
           by = c("fips_state", "fips_county")]

    ss_pi <- ss_pi[not_yet_treated == TRUE]
    ss_pi[, not_yet_treated := NULL]
    ss_pi[, group := "Future"]

    ## combine never treated + later cohorts
    ss_pi <- rbind(ss_pi, never.treated, fill = T)
    rm(never.treated)

    # ss_pi[, event.weight := ifelse(is.na(n_events), 1, 1 / n_events)]

    ## collapse to product x group x time level -----------------------------------
    ss_pi.collapsed <- ss_pi[, list(
      control.cpricei = weighted.mean(normalized.cpricei, w = base.sales)
    ), by = .(year, quarter, group, product_module_code)]
    rm(ss_pi)

    ## rearrange for simple merging of groups onto 2009 Q1 cohort
    ss_pi.collapsed <- tidyr::spread(ss_pi.collapsed, group, control.cpricei)

    ## merge onto the treated cohort by product
    taxable_pi.09Q1 <- merge(taxable_pi.09Q1, ss_pi.collapsed,
                             by = c("year", "quarter", "product_module_code"))
    rm(ss_pi.collapsed)

    ## aggregate over calendar time ------------------------------------------------

    taxable_pi.09Q1.collapsed <- taxable_pi.09Q1[, list(
      mean.cpricei = weighted.mean(normalized.cpricei, w = base.sales),
      Future = weighted.mean(Future, w = base.sales),
      `No change` = weighted.mean(`No change`, w = base.sales)
    ), by = .(year, quarter)]
    rm(taxable_pi.09Q1)

    setnames(taxable_pi.09Q1.collapsed, "mean.cpricei", "Treated")
    taxable_pi.09Q1.collapsed <- tidyr::gather(taxable_pi.09Q1.collapsed,
                                               key = group, value = cpricei,
                                               c(Treated, Future, `No change`))

    taxable_pi.09Q1.collapsed[, ref_year := ref.year]
    taxable_pi.09Q1.collapsed[, ref_quarter := ref.quarter]
    master_res <- rbind(master_res, taxable_pi.09Q1.collapsed)

    rm(taxable_pi.09Q1.collapsed)
  }
}

fwrite(master_res, "Data/pi_all_cohorts_pooled.csv")

## merge on cohort size
master_res <- merge(master_res, cohort_sizes, by = c("ref_year", "ref_quarter"))
fwrite(master_res, "Data/pi_all_cohorts_pooled.csv")
