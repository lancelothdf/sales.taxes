#' Author: John Bonney
#'
#' Now we should "pool" across cohorts for both a diff-in-diff and a pure event
#' study design and see which one looks better.  Those would be similar graphs
#' to the ones you have been doing recently.  We revert back to defining cohorts
#' by quarters.

#' 3a) Plot mean price index (tax inclusive) for counties that increased their
#' taxes in 2009Q1.  Then build two alternative control groups: A) same as we
#' have been doing - use an average of all counties (only include the set of
#' products that are taxable in the treated cohort) that never change their
#' taxes in the same calendar time (so around 2009 Q1).  B) taking all counties
#' from the ever increase group that have not yet increased their taxes (but
#' will in the future - so all cohorts from 2009Q2 - 2014Q4) "matching" on
#' calendar time and product.

library(tidyverse)
library(data.table)
library(readstata13)
library(sales.taxes)
library(zoo)

setwd("/project2/igaarder")
prep_tax_ref <- F

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

## identify 2009 Q1 cohort -----------------------------------------------------
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

setnames(tr.events, "ref_year", "treatment_year")
setkey(tr.events, fips_county, fips_state)

tr.events.09Q1 <- tr.events[treatment_year == 2009 & ref_quarter == 1]
print(nrow(tr.events.09Q1))
print(nrow(unique(tr.events.09Q1, by = c("fips_state", "fips_county"))))

setkey(tr.events.09Q1, fips_county, fips_state)

taxable_pi.09Q1 <- all_pi[sales_tax > 1]
taxable_pi.09Q1 <- taxable_pi.09Q1[tr.events.09Q1]

treatment.info <- taxable_pi.09Q1[, list(n_counties = uniqueN(1000 * fips_state + fips_county)),
                                  by = .(year, quarter)]
treatment.info[, group := "Treated"]

## prepare control data --------------------------------------------------------

## limit to goods that are taxable for the 2009 Q1 cohort
constant.goods.set <- unique(taxable_pi.09Q1[year == 2009 & quarter == 1]$product_module_code)
all_pi <- all_pi[product_module_code %in% constant.goods.set] # keep goods constant

## identify never treated counties
tr.groups <- fread(tr_groups_path)
never.treated <- tr.groups[tr_group == "No change"]
never.treated <- never.treated[, .(fips_state, fips_county)]
setkey(never.treated, fips_state, fips_county)
never.treated <- all_pi[never.treated]
never.treated[, group := "No change"]

## identify not-yet-treated (but future treated) counties
all_pi <- all_pi[tr.events[treatment_year != 2009 | quarter != 1]] # previously had allow = T; shouldn't be necessary in first event case
all_pi[, treatment_quarter := 4 * treatment_year + ref_quarter]
all_pi[, calendar_quarter := 4 * year + quarter]
all_pi[, not_yet_treated := min(treatment_quarter) > calendar_quarter,
       by = c("fips_state", "fips_county")]

## drop counties once they are treated
print(nrow(all_pi)) # total count
print(nrow(all_pi[not_yet_treated == FALSE])) # total dropped observations
print(nrow(unique(all_pi, by = c("store_code_uc", "product_module_code",
                                 "quarter", "year")))) # total storeXproductXtime obs
print(nrow(unique(all_pi, by = c("store_code_uc", "product_module_code",
                                 "quarter", "year"))[not_yet_treated == FALSE])) # total storeXproductXtime obs dropped
print(nrow(unique(all_pi, by = c("fips_county", "fips_state",
                                 "quarter", "year")))) # total countyXtime obs
print(nrow(unique(all_pi, by = c("fips_county", "fips_state",
                                 "quarter", "year"))[not_yet_treated == FALSE])) # total countyXtime obs dropped

all_pi <- all_pi[not_yet_treated == TRUE]
all_pi[, not_yet_treated := NULL]
all_pi[, group := "Future"]

## combine never treated + later cohorts
all_pi <- rbind(all_pi, never.treated, fill = T)
# all_pi[, event.weight := ifelse(is.na(n_events), 1, 1 / n_events)]

## collapse to product x group x time level -----------------------------------
control.info <- all_pi[, list(n_counties := uniqueN(1000 * fips_state + fips_county)),
                       by = .(year, quarter, group)]
all.info <- rbind(control.info, treatment.info)

all_pi.collapsed <- all_pi[, list(
  control.cpricei = weighted.mean(normalized.cpricei, w = base.sales)
), by = .(year, quarter, group, product_module_code)]

## rearrange for simple merging of groups onto 2009 Q1 cohort
all_pi.collapsed <- tidyr::spread(all_pi.collapsed, group, control.cpricei)

## merge onto the 2009 cohort by product
taxable_pi.09Q1 <- merge(taxable_pi.09Q1, all_pi.collapsed,
                         by = c("year", "quarter", "product_module_code"))
# taxable_pi.09Q1[, event.weight := 1 / n_events]

## aggregate over calendar time ------------------------------------------------

taxable_pi.09Q1.collapsed <- taxable_pi.09Q1[, list(
  mean.cpricei = weighted.mean(normalized.cpricei, w = base.sales),
        Future = weighted.mean(Future, w = base.sales),
   `No change` = weighted.mean(`No change`, w = base.sales)
), by = .(year, quarter)]

setnames(taxable_pi.09Q1.collapsed, "mean.cpricei", "Treated")
taxable_pi.09Q1.collapsed <- tidyr::gather(taxable_pi.09Q1.collapsed,
                                           key = group, value = cpricei,
                                           c(Future, `No change`))
taxable_pi.09Q1.collapsed <- merge(taxable_pi.09Q1.collapsed, all.info,
                                   by = c("year", "quarter", "group"))

fwrite(taxable_pi.09Q1.collapsed, "Data/pi_2009Q1_cohorts_pooled.csv")

