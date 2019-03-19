#' Author: John Bonney
#'
#' Idea is to think about a regression specification that relates to the
#'     figures we have been making. We will want to run two regressions,
#'     one corresponding to the event-no-event case and one corresponding
#'     to the time-of-event case.

library(data.table)
library(lfe)
library(futile.logger)

setwd("/project2/igaarder")
change_of_interest <- "Ever increase"

output.results.filepath <- "Data/pi_ei_regression_res.csv"

## useful filepaths ------------------------------------------------------------
all_goods_pi_path <- "Data/Nielsen/price_quantity_indices_allitems_2006-2016_notaxinfo.csv"
eventstudy_tr_path <- "Data/event_study_tr_groups_comprehensive_firstonly_no2012q4_2013q1q2.csv"
tr_groups_path <- "Data/tr_groups_comprehensive_firstonly_no2012q4_2013q1q2.csv"

## Want to run cohort-product specific regressions.
## Data is on store-product-quarter level (right?)

# Start with event-no-event case ===============================================

## prep the data ---------------------------------------------------------------

all_pi <- fread(all_goods_pi_path)
all_pi <- all_pi[year %in% 2006:2014 & !is.na(cpricei)]

# do `arbitrary` correction for the 2013 Q1 jump in the data
## calculate price index in 2013 Q1 / cpricei in 2012 Q4
all_pi[, correction := pricei[year == 2013 & quarter == 1] / pricei[year == 2012 & quarter == 4],
       by = .(store_code_uc, product_module_code)]
## divide price index after 2013 Q1 (inclusive) by above value
all_pi[year >= 2013, cpricei := cpricei / correction]

## take logs
all_pi[, cpricei := log(cpricei)]
all_pi[, sales_tax := log(sales_tax)]

## get sales weights
all_pi[, base.sales := sales[year == 2008 & quarter == 1],
       by = .(store_code_uc, product_module_code)]

all_pi[, sales := NULL]
all_pi <- all_pi[!is.na(base.sales)]

## balance on store-module level
keep_store_modules <- all_pi[, list(n = .N),
                             by = .(store_code_uc, product_module_code)]
keep_store_modules <- keep_store_modules[n == (2014 - 2005) * 4]

setkey(all_pi, store_code_uc, product_module_code)
setkey(keep_store_modules, store_code_uc, product_module_code)

all_pi <- all_pi[keep_store_modules]
setkey(all_pi, fips_county, fips_state)

### create unique dataset of never treated counties ----------------------------
control_counties <- fread(tr_groups_path)
control_counties <- control_counties[tr_group == "No change"]
control_counties <- unique(control_counties[, .(fips_county, fips_state)])
control_dt <- merge(all_pi, control_counties, by = c("fips_state", "fips_county"))
control_dt[, ref_year := Inf]
control_dt[, ref_quarter := Inf]

## merge treatment, attach event times -----------------------------------------
treated_counties <- fread(eventstudy_tr_path)
treated_counties <- treated_counties[tr_group == change_of_interest]
treated_counties[, ref_quarter := ceiling(ref_month / 3)]
treated_counties[, ref_month := NULL]

all_pi <- merge(all_pi, treated_counties, by = c("fips_state", "fips_county"))

val1 <- uniqueN(treated_counties, by = c("fips_state", "fips_county"))
val2 <- uniqueN(all_pi, by = c("fips_state", "fips_county"))
if (val1 != val2) {
  warning(sprintf("val1 (%s) != val2 (%s)", val1, val2))
}

## now we have the treated and untreated groups
print("TREATED")
print(head(all_pi))
print("CONTROL")
print(head(control_dt))

## combine the two groups ------------------------------------------------------
all_pi <- rbind(all_pi, control_dt, fill = T)
all_pi[, county_ID := .GRP, by = .(fips_state, fips_county)]

print("TREATED + CONTROL")
print(head(all_pi))

## loop through all possible treatment yr and qtr ("cohorts") ------------------
cp.all.res <- data.table(NULL)
for (yr in 2009:2013) {
  for (qtr in 1:4) {
    if (nrow(all_pi[ref_year == yr & ref_quarter == qtr]) == 0) {
      next
    }
    # given a cohort, loop through all products
    for (prd in unique(all_pi[ref_year == yr & ref_quarter == qtr]$product_module_code)) {
      flog.info("Estimating for %s Q%s, product %s", yr, qtr, prd)

      # prepare a subset of data -----------------------------------------------

      # limit to the cohort or the untreated, and product prd
      ss_pi <- all_pi[((ref_year == yr & ref_quarter == qtr) |
                         ref_year == Inf) &
                        product_module_code == prd]
      # limit estimation to eight pre-periods and four post-periods
      ss_pi[, tt_event := (year * 4 + quarter) - (yr * 4 + qtr)]
      ss_pi <- ss_pi[between(tt_event, -8, 4)]

      ss_pi[, treated := as.integer(ref_year == yr & ref_quarter == qtr)]

      flog.info("Created subset of data for the selected groups.")
      ## create dummies for event times (except -2)
      start_cols <- copy(colnames(ss_pi))
      for (r in setdiff(-8:4, -2)) {
        var <- sprintf("catt%s", r)
        ss_pi[, (var) := as.integer(treated == 1 & tt_event == r)]
      }
      flog.info("Created mutually exclusive treatment columns.")
      print(head(ss_pi))

      ## rename columns to prevent confusion for felm
      new_cols <- setdiff(colnames(ss_pi), start_cols)
      new_cols_used <- gsub("\\-", "lead", new_cols)
      setnames(ss_pi, new_cols, new_cols_used)

      ## estimate for cpricei =================================================
      felm_formula_input <- paste(new_cols_used, collapse = "+")
      cXp_formula <- as.formula(paste0("cpricei ~ ", felm_formula_input,
                                       " | county_ID + tt_event | 0 | county_ID"))

      res.cp <- felm(data = ss_pi, formula = cXp_formula,
                     weights = ss_pi$base.sales)
      flog.info("Estimated with price index as outcome.")
      print(coef(summary(res.cp)))

      ## *$* Here is where one would extract other information from res.cp *$* ##

      ## clean and save output
      res.cp <- as.data.table(summary(res.cp, robust = T)$coefficients, keep.rownames = T)
      res.cp[, rn := gsub("lead", "-", rn)]

      res.cp[, tt_event := as.integer(NA)]

      for (c in setdiff(-8:4, -2)) {
        res.cp[grepl(sprintf("catt%s", c), rn) & is.na(tt_event), tt_event := as.integer(c)]
      }
      res.cp <- res.cp[!is.na(tt_event)]

      res.cp[, ref_year := yr]
      res.cp[, ref_quarter := qtr]
      res.cp[, product_module_code := prd]
      res.cp[, outcome := "cpricei"]
      setnames(res.cp,
               old = c("Estimate", "Cluster s.e.", "Pr(>|t|)"),
               new = c("estimate", "cluster_se", "pval"))

      flog.info("Attaching output to master data.table.")
      cp.all.res <- rbind(cp.all.res, res.cp)

      ## run for log(1 + tax) as well ==========================================

      drop_cols <- paste0("cattlead", 8:5)
      tax_cols <- setdiff(new_cols_used, drop_cols)

      ss_pi[, c(drop_cols) := NULL]
      ss_pi <- ss_pi[between(tt_event, -4, 4)] # to be consistent across cohorts

      ## create formula
      tax_formula_input <- paste(tax_cols, collapse = "+")
      tax_formula <- as.formula(paste0("sales_tax ~ ", tax_formula_input,
                                       " | county_ID + tt_event | 0 | county_ID"))

      res.tax <- felm(data = ss_pi, formula = tax_formula,
                     weights = ss_pi$base.sales)
      flog.info("Estimated with tax rate as outcome.")
      print(coef(summary(res.tax)))

      ## *$* Here is where one would extract other information from res.cp *$* ##

      res.tax <- as.data.table(summary(res.tax, robust = T)$coefficients, keep.rownames = T)
      res.tax[, rn := gsub("lead", "-", rn)]

      res.tax[, tt_event := as.integer(NA)]

      for (c in setdiff(-4:4, -2)) {
        res.tax[grepl(sprintf("catt%s", c), rn) & is.na(tt_event), tt_event := as.integer(c)]
      }
      res.tax <- res.tax[!is.na(tt_event)]

      res.tax[, ref_year := yr]
      res.tax[, ref_quarter := qtr]
      res.tax[, product_module_code := prd]
      res.tax[, outcome := "sales_tax"]
      setnames(res.tax,
               old = c("Estimate", "Cluster s.e.", "Pr(>|t|)"),
               new = c("estimate", "cluster_se", "pval"))
      flog.info("Attaching output to master data.table.")
      cp.all.res <- rbind(cp.all.res, res.tax)

      rm(ss_pi)
      gc()

    }
    # re-write once a cohort in case it crashes
    fwrite(cp.all.res, output.results.filepath)
  }

}

