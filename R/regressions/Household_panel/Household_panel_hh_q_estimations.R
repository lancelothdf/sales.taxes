## Sales taxes Project. Household Panel
# Running Basic Estimations on the new panel (household aggregate x quarter)
# Author: Santiago Lacouture


library(data.table)
library(lfe)
library(futile.logger)
library(AER)
library(multcomp)
library(psych)
library(ggplot2)

setwd("/project2/igaarder/Data/Nielsen/Household_panel")

## Open Data
purchases.full <- fread("cleaning/consumer_panel_q_hh_2006-2016.csv")

purchases.full$time <- factor(with(purchases.full, interaction(year, month)))


## Constraining Data set for estimations ------------ 
# Drop "magnet" households: 
purchases.full[, sum(is.na(projection_factor))]
# 2016154 obs
purchases.nomagnet <- purchases.full[!is.na(projection_factor)]

# Drop households without sales tax data
purchases.full[, sum(is.na(sales_tax))]
# More than 70%: 181247984 obs
purchases.sample <- purchases.nomagnet[!is.na(sales_tax)]


## creating interest outcomes -------

purchases.sample <- purchases.sample[, ln_expenditure_taxable := log(expenditure_taxable)]
purchases.sample$ln_expenditure_taxable[is.infinite(purchases.sample$ln_expenditure_taxable)] <- NA
purchases.sample <- purchases.sample[, ln_expenditure_non_taxable := log(expenditure_non_taxable)]
purchases.sample$ln_expenditure_non_taxable[is.infinite(purchases.sample$ln_expenditure_non_taxable)] <- NA
purchases.sample <- purchases.sample[, ln_expenditure_unknown := log(expenditure_unknown)]
purchases.sample$ln_expenditure_unknown[is.infinite(purchases.sample$ln_expenditure_unknown)] <- NA

## Shares
# type or taxability
purchases.sample[, share_taxable := expenditure_taxable/sum_total_exp_quarter]
purchases.sample[, share_non_taxable := expenditure_non_taxable/sum_total_exp_quarter]
purchases.sample[, share_unknown := expenditure_unknown/sum_total_exp_quarter]
purchases.sample[, share_same3 := expenditure_same3/sum_total_exp_quarter]
purchases.sample[, share_diff3 := expenditure_diff3/sum_total_exp_quarter]

# type x taxability
purchases.sample[, share_taxable_same3 := expenditures_same3_1/sum_total_exp_quarter]
purchases.sample[, share_taxable_diff3 := expenditures_diff3_1/sum_total_exp_quarter]
purchases.sample[, share_non_taxable_same3 := expenditures_same3_0/sum_total_exp_quarter]
purchases.sample[, share_non_taxable_diff3 := expenditures_diff3_0/sum_total_exp_quarter]
purchases.sample[, share_unknown_same3 := expenditures_same3_2/sum_total_exp_quarter]
purchases.sample[, share_unknown_diff3 := expenditures_diff3_2/sum_total_exp_quarter]

# type x taxability
purchases.sample <- purchases.sample[, ln_expenditure_taxable_same3 := log(expenditures_same3_1)]
purchases.sample$ln_expenditure_taxable_same3[is.infinite(purchases.sample$ln_expenditure_taxable_same3)] <- NA
purchases.sample <- purchases.sample[, ln_expenditure_taxable_diff3 := log(expenditures_diff3_1)]
purchases.sample$ln_expenditure_taxable_diff3[is.infinite(purchases.sample$ln_expenditure_taxable_diff3)] <- NA
purchases.sample <- purchases.sample[, ln_expenditure_non_taxable_same3 := log(expenditures_same3_0)]
purchases.sample$ln_expenditure_non_taxable_same3[is.infinite(purchases.sample$ln_expenditure_non_taxable_same3)] <- NA
purchases.sample <- purchases.sample[, ln_expenditure_non_taxable_diff3 := log(expenditures_diff3_0)]
purchases.sample$ln_expenditure_non_taxable_diff3[is.infinite(purchases.sample$ln_expenditure_non_taxable_diff3)] <- NA
purchases.sample <- purchases.sample[, ln_expenditure_unknown_same3 := log(expenditures_same3_2)]
purchases.sample$ln_expenditure_unknown_same3[is.infinite(purchases.sample$ln_expenditure_unknown_same3)] <- NA
purchases.sample <- purchases.sample[, ln_expenditure_unknown_diff3 := log(expenditures_diff3_2)]
purchases.sample$ln_expenditure_unknown_diff3[is.infinite(purchases.sample$ln_expenditure_unknown_diff3)] <- NA

# shares type or taxability
purchases.sample <- purchases.sample[, ln_share_taxable := log(share_taxable)]
purchases.sample$ln_share_taxable[is.infinite(purchases.sample$ln_share_taxable)] <- NA
purchases.sample <- purchases.sample[, ln_share_non_taxable := log(share_non_taxable)]
purchases.sample$ln_share_non_taxable[is.infinite(purchases.sample$ln_share_non_taxable)] <- NA
purchases.sample <- purchases.sample[, ln_share_unknown := log(share_unknown)]
purchases.sample$ln_share_unknown[is.infinite(purchases.sample$ln_share_unknown)] <- NA
purchases.sample <- purchases.sample[, ln_share_same3 := log(share_same3)]
purchases.sample$ln_share_same3[is.infinite(purchases.sample$ln_share_same3)] <- NA
purchases.sample <- purchases.sample[, ln_share_diff3 := log(share_diff3)]
purchases.sample$ln_share_diff3[is.infinite(purchases.sample$ln_share_diff3)] <- NA

# shares type x taxability
purchases.sample <- purchases.sample[, ln_share_taxable_same3 := log(share_taxable_same3)]
purchases.sample$ln_share_taxable_same3[is.infinite(purchases.sample$ln_share_taxable_same3)] <- NA
purchases.sample <- purchases.sample[, ln_share_taxable_diff3 := log(share_taxable_diff3)]
purchases.sample$ln_share_taxable_diff3[is.infinite(purchases.sample$ln_share_taxable_diff3)] <- NA
purchases.sample <- purchases.sample[, ln_share_non_taxable_same3 := log(share_non_taxable_same3)]
purchases.sample$ln_share_non_taxable_same3[is.infinite(purchases.sample$ln_share_non_taxable_same3)] <- NA
purchases.sample <- purchases.sample[, ln_share_non_taxable_diff3 := log(share_non_taxable_diff3)]
purchases.sample$ln_share_non_taxable_diff3[is.infinite(purchases.sample$ln_share_non_taxable_diff3)] <- NA
purchases.sample <- purchases.sample[, ln_share_unknown_same3 := log(share_unknown_same3)]
purchases.sample$ln_share_unknown_same3[is.infinite(purchases.sample$ln_share_unknown_same3)] <- NA
purchases.sample <- purchases.sample[, ln_share_unknown_diff3 := log(share_unknown_diff3)]
purchases.sample$ln_share_unknown_diff3[is.infinite(purchases.sample$ln_share_unknown_diff3)] <- NA

purchases.sample[, region_by_time := .GRP, by = .(region_code, year, quarter)]
purchases.sample[, time := .GRP, by = .(year, quarter)]


## Estimations: Expenditure on type of module --------

output.results.file <- "../../../../../home/slacouture/HMS/HH_quarter_basic.csv"

outcomes <- c("ln_expenditure_taxable", "ln_expenditure_non_taxable", "ln_expenditure_unknown",
              "ln_expenditure_diff3", "ln_expenditure_same3", "ln_share_taxable",
              "ln_share_non_taxable", "ln_share_unknown", "ln_share_same3", "ln_share_diff3")
outcomes_t <- c("ln_expenditure_taxable_same3", "ln_expenditure_taxable_diff3", 
                "ln_expenditure_non_taxable_same3", "ln_expenditure_non_taxable_diff3",
                "ln_expenditure_unknown_same3", "ln_expenditure_unknown_diff3", 
                "ln_share_taxable_same3", "ln_share_taxable_diff3", 
                "ln_share_non_taxable_same3", "ln_share_non_taxable_diff3",
                "ln_share_unknown_same3", "ln_share_unknown_diff3")

FE_opts <- c("region_by_time", "time")


LRdiff_res <- data.table(NULL)
for (Y in c(outcomes, outcomes_t)) {
  for (FE in FE_opts) {
    formula1 <- as.formula(paste0(
      Y, "~ ln_sales_tax | ", FE, "+ household_code | 0 | household_code"
    ))
    flog.info("Estimating with %s as outcome and %s FE", Y, FE)
    res1 <- felm(formula = formula1, data = purchases.sample,
                 weights = purchases.sample$projection_factor)
    flog.info("Finished estimating with %s as outcomeand %s FE.", Y, FE)
    
    ## attach results
    flog.info("Writing results...")
    res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
    res1.dt[, outcome := Y]
    res1.dt[, spec := FE]
    res1.dt[, Rsq := summary(res1)$r.squared]
    res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
    res1.dt[, N_obs := sum(!is.na(purchases.sample$get(Y)))]
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)

  }
}



## summary values --------------------------------------------------------------
LRdiff_res$N_hholds <- length(unique(purchases.sample$household_code))
LRdiff_res$N_counties <- uniqueN(purchases.sample, by = c("fips_state_code", "fips_county_code"))
LRdiff_res$N_years <- uniqueN(purchases.sample, by = c("year"))

fwrite(LRdiff_res, output.results.file)
