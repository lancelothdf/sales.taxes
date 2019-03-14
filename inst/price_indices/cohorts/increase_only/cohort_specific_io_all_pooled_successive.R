#' Author: John Bonney
#'
#' Now we should "pool" across cohorts for both a diff-in-diff and a pure event
#' study design and see which one looks better.  Those would be similar graphs
#' to the ones you have been doing recently.  We revert back to defining cohorts
#' by quarters.

#' The main difference between this and "cohort_specific_ei_all_pooled.R" is that
#' this file creates control groups that are treated 1, 2, 3, 4 quarters after.

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

change_of_interest <- "Increase only"
output_filepath <- "Data/pi_all_cohorts_io_pooled_successive.csv"

## useful filepaths ------------------------------------------------------------
eventstudy_tr_path <- "Data/event_study_tr_groups_comprehensive_w2014.csv"
tr_groups_path <- "Data/tr_groups_comprehensive_w2014.csv"
sales_data_path <- "Data/sales_quarterly_2006-2016.csv"
tax_rates_path <- "Data/county_monthly_tax_rates.csv"
quarterly_tax_path <- "Data/quarterly_tax_rates.csv"
module_exemptions_path <- "Data/modules_exemptions_long.csv"
all_goods_pi_path <- "Data/Nielsen/price_quantity_indices_allitems_2006-2016_notaxinfo.csv"
taxable_pi_path <- "Data/Nielsen/price_quantity_indices_taxableitems.csv"
expanded.reforms.path <- "Data/tax_reforms_all_incl2014.csv"

## combine the data (addition of 2006 + 2007) ----------------------------------
if (prep.new.data) {
nonfood_pi <- read.dta13("Data/Nielsen/Price_quantity_indices_nonfood.dta")
nonfood_pi <- as.data.table(nonfood_pi)
# fwrite(nonfood_pi, "Data/Nielsen/price_quantity_indices_nonfood.csv")

food_pi <- fread("Data/Nielsen/price_quantity_indices_food.csv")
food_pi[, c("fips_state", "fips_county") := NULL]

all_pi <- rbind(food_pi, nonfood_pi)
all_pi <- all_pi[year <= 2014]
rm(nonfood_pi, food_pi)
gc()

### attach county and state FIPS codes, sales ----------------------------------
sales_data <- fread(sales_data_path)
sales_data <- sales_data[, .(store_code_uc, product_module_code, fips_county,
                             fips_state, quarter, year, sales)]
sales_data <- sales_data[year <= 2014]

all_pi <- merge(all_pi, sales_data, by = c("store_code_uc", "quarter", "year",
                                           "product_module_code" ))
rm(sales_data)
gc()

all.tax <- fread(quarterly_tax_path)
all_pi <- merge(all_pi, all.tax, by = c("store_code_uc", "product_module_code",
                                        "year", "quarter", "product_group_code"),
                all.x = T)
rm(all.tax)

fwrite(all_pi, all_goods_pi_path)
} else {
  all_pi <- fread(all_goods_pi_path)
}

## prep the data ---------------------------------------------------------------

# do `arbitrary` correction for the 2013 Q1 jump in the data
## calculate price index in 2013 Q1 / cpricei in 2012 Q4
all_pi[, correction := pricei[year == 2013 & quarter == 1] / pricei[year == 2012 & quarter == 4],
       by = .(store_code_uc, product_module_code)]
## divide price index after 2013 Q1 (inclusive) by above value
all_pi[year >= 2013, cpricei := cpricei / correction]

## normalize
all_pi[, normalized.cpricei := log(cpricei) - log(cpricei[year == 2006 & quarter == 1]),
       by = .(store_code_uc, product_module_code)]
all_pi[, base.sales := sales[year == 2008 & quarter == 1],
       by = .(store_code_uc, product_module_code)]
all_pi <- all_pi[!is.na(normalized.cpricei) & !is.na(base.sales)]

## balance on store-module level
keep_store_modules <- all_pi[, list(n = .N),
                             by = .(store_code_uc, product_module_code)]
keep_store_modules <- keep_store_modules[n == (2014 - 2005) * 4]

setkey(all_pi, store_code_uc, product_module_code)
setkey(keep_store_modules, store_code_uc, product_module_code)

all_pi <- all_pi[keep_store_modules]
setkey(all_pi, fips_county, fips_state)

## prep treatment events -------------------------------------------------------
tr.events <- fread(eventstudy_tr_path)
tr.events <- tr.events[tr_group == change_of_interest]
tr.events[, treatment_month := 12 * ref_year + ref_month]
## we are only interested in the first event
tr.events[, min.event := (treatment_month == min(treatment_month)),
          by = .(fips_state, fips_county)]
tr.events <- tr.events[min.event == T]
tr.events[, ref_quarter := ceiling(ref_month / 3)]
tr.events[, n_events := .N, by = .(fips_state, fips_county)]
tr.events <- tr.events[, .(fips_county, fips_state, ref_year, ref_quarter, n_events)]
# exclude 2012 Q4, 2013 Q1, 2013 Q2 reforms
tr.events <- tr.events[!(ref_year == 2012 & ref_quarter == 4) & !(ref_year == 2013 & ref_quarter %in% 1:2)]
g(tr.events)

setnames(tr.events, "ref_year", "treatment_year")
setkey(tr.events, fips_county, fips_state)

cohort_sizes <- tr.events[, list(cohort_size = .N), by = .(treatment_year, ref_quarter)]

# for later use
# tr.groups <- fread(tr_groups_path)
# never.treated.master <- tr.groups[tr_group == "No change"]
# never.treated.master <- never.treated.master[, .(fips_state, fips_county)]
# setkey(never.treated.master, fips_state, fips_county)

## iterate over all quarters and years -----------------------------------------
master_res <- data.table(NULL)
pool_cohort_weights <- data.table(NULL)

for (ref.year in 2009:2013) {
  for (ref.quarter in 1:4) {
    if ((ref.year == 2012 & ref.quarter == 4) | (ref.year == 2013 & ref.quarter %in% 1:2)) {
      print(paste("Skipping", ref.year, "Q", ref.quarter))
      next
    }
    print(paste("Year:", ref.year, "; Quarter:", ref.quarter))
    ## identify treated cohort -------------------------------------------------
    tr.events.09Q1 <- tr.events[treatment_year == ref.year & ref_quarter == ref.quarter]
    if (nrow(tr.events.09Q1) == 0) {
      print("No events, skipping")
      next
    }
    setkey(tr.events.09Q1, fips_county, fips_state)

    taxable_pi.09Q1 <- all_pi[sales_tax > 1 | (year < 2008 & is.na(sales_tax))]
    taxable_pi.09Q1 <- taxable_pi.09Q1[tr.events.09Q1]
    g(taxable_pi.09Q1)

    ## get sum of sales in treated counties
    pool_cohort_weights <- rbind(
      pool_cohort_weights,
      data.table(ref_year = ref.year, ref_quarter = ref.quarter,
                 ## sum of all base sales weights in the cohort at time of treatment
                 cohort_sales = sum(taxable_pi.09Q1[year == ref.year & quarter == ref.quarter]$base.sales))
    )

    ## prepare control data --------------------------------------------------------

    ## limit to goods that are taxable for the cohort (e.g., 2009 Q1)
    constant.goods.set <- unique(taxable_pi.09Q1[year == ref.year & quarter == ref.quarter]$product_module_code)
    ss_pi <- all_pi[product_module_code %in% constant.goods.set] # keep goods constant
    taxable_pi.09Q1
    g(ss_pi)

    ## identify future treated counties
    ss_pi <- ss_pi[tr.events[treatment_year > ref.year |
                                 (treatment_year == ref.year &
                                    ref_quarter > ref.quarter)]]
    ss_pi[, treatment_quarter := 4 * treatment_year + ref_quarter]
    ss_pi[, calendar_quarter := 4 * year + quarter]
    ss_pi[, min_treat_quarter := min(treatment_quarter), by = .(fips_state, fips_county)]

    ## subset future treated into four groups: +1 quarter, ..., +4 quarters
    ss_pi_grp1 <- ss_pi[min_treat_quarter == (4 * ref.year + ref.quarter + 1)]
    ss_pi_grp2 <- ss_pi[min_treat_quarter == (4 * ref.year + ref.quarter + 2)]
    ss_pi_grp3 <- ss_pi[min_treat_quarter == (4 * ref.year + ref.quarter + 3)]
    ss_pi_grp4 <- ss_pi[min_treat_quarter == (4 * ref.year + ref.quarter + 4)]

    ss_pi_grp1[, group := "Future restricted (in 1 quarter)"]
    ss_pi_grp2[, group := "Future restricted (in 2 quarters)"]
    ss_pi_grp3[, group := "Future restricted (in 3 quarters)"]
    ss_pi_grp4[, group := "Future restricted (in 4 quarters)"]

    grp1 <- (nrow(ss_pi_grp1) != 0)
    grp2 <- (nrow(ss_pi_grp2) != 0)
    grp3 <- (nrow(ss_pi_grp3) != 0)
    grp4 <- (nrow(ss_pi_grp4) != 0)

    print("Group 1:")
    g(ss_pi_grp1)
    print("Group 2:")
    g(ss_pi_grp2)
    print("Group 3:")
    g(ss_pi_grp3)
    print("Group 4:")
    g(ss_pi_grp4)

    ## combine control cohorts
    ss_pi <- rbind(ss_pi_grp1, ss_pi_grp2, ss_pi_grp3, ss_pi_grp4, fill = T)
    g(ss_pi)
    rm(ss_pi_grp1, ss_pi_grp2, ss_pi_grp3, ss_pi_grp4)

    ## collapse to product x group x time level -----------------------------------
    ss_pi.collapsed <- ss_pi[, list(
      control.cpricei = weighted.mean(normalized.cpricei, w = base.sales)
    ), by = .(year, quarter, group, product_module_code)]
    rm(ss_pi)
    g(ss_pi.collapsed)

    ## rearrange for simple merging of groups onto treated cohort
    ss_pi.collapsed <- tidyr::spread(ss_pi.collapsed, group, control.cpricei)
    g(ss_pi.collapsed)

    ## merge onto the treated cohort by product
    taxable_pi.09Q1 <- taxable_pi.09Q1[product_module_code %in% constant.goods.set]
    taxable_pi.09Q1 <- merge(taxable_pi.09Q1, ss_pi.collapsed,
                             by = c("year", "quarter", "product_module_code"))
    rm(ss_pi.collapsed)

    ## aggregate over calendar time ------------------------------------------------
    g(taxable_pi.09Q1)

    ## need to have all variables declared, even if the group is empty
    if (!grp1) taxable_pi.09Q1[, `Future restricted (in 1 quarter)` := NA]
    if (!grp2) taxable_pi.09Q1[, `Future restricted (in 2 quarters)` := NA]
    if (!grp3) taxable_pi.09Q1[, `Future restricted (in 3 quarters)` := NA]
    if (!grp4) taxable_pi.09Q1[, `Future restricted (in 4 quarters)` := NA]

    taxable_pi.09Q1.collapsed <- taxable_pi.09Q1[, list(
      mean.cpricei = weighted.mean(normalized.cpricei, w = base.sales),
      `Future restricted (in 1 quarter)` = weighted.mean(`Future restricted (in 1 quarter)`, w = base.sales),
      `Future restricted (in 2 quarters)` = weighted.mean(`Future restricted (in 2 quarters)`, w = base.sales),
      `Future restricted (in 3 quarters)` = weighted.mean(`Future restricted (in 3 quarters)`, w = base.sales),
      `Future restricted (in 4 quarters)` = weighted.mean(`Future restricted (in 4 quarters)`, w = base.sales)
    ), by = .(year, quarter)]

    g(taxable_pi.09Q1.collapsed)

    setnames(taxable_pi.09Q1.collapsed, "mean.cpricei", "Treated")
    taxable_pi.09Q1.collapsed <- tidyr::gather(taxable_pi.09Q1.collapsed,
                                               key = group, value = cpricei,
                                               c(Treated,
                                                 `Future restricted (in 1 quarter)`,
                                                 `Future restricted (in 2 quarters)`,
                                                 `Future restricted (in 3 quarters)`,
                                                 `Future restricted (in 4 quarters)`))
    g(taxable_pi.09Q1.collapsed)
    taxable_pi.09Q1.collapsed <- as.data.table(taxable_pi.09Q1.collapsed)
    taxable_pi.09Q1.collapsed <- taxable_pi.09Q1.collapsed[!is.na(cpricei)]
    taxable_pi.09Q1.collapsed[, ref_year := ref.year]
    taxable_pi.09Q1.collapsed[, ref_quarter := ref.quarter]
    master_res <- rbind(master_res, taxable_pi.09Q1.collapsed)

    rm(taxable_pi.09Q1.collapsed, taxable_pi.09Q1)
  }
}

fwrite(master_res, output_filepath)

setnames(cohort_sizes, "treatment_year", "ref_year")
## merge on cohort size
master_res <- merge(master_res, cohort_sizes, by = c("ref_year", "ref_quarter"))
master_res <- merge(master_res, pool_cohort_weights, by = c("ref_year", "ref_quarter"))
fwrite(master_res, output_filepath)
