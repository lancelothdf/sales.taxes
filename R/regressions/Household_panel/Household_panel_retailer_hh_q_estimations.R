## Sales taxes Project. Household Panel
# Running Basic Estimations on the new panel (household aggregate x quarter on purchases at retailer data)
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
purchases.full <- fread("cleaning/consumer_panel_retailer_q_hh_2006-2016.csv")

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
purchases.sample[, share_taxable := expenditure_taxable/sum_total_exp]
purchases.sample[, share_non_taxable := expenditure_non_taxable/sum_total_exp]
purchases.sample[, share_unknown := expenditure_unknown/sum_total_exp]

# shares type or taxability
purchases.sample <- purchases.sample[, ln_share_taxable := log(share_taxable)]
purchases.sample$ln_share_taxable[is.infinite(purchases.sample$ln_share_taxable)] <- NA
purchases.sample <- purchases.sample[, ln_share_non_taxable := log(share_non_taxable)]
purchases.sample$ln_share_non_taxable[is.infinite(purchases.sample$ln_share_non_taxable)] <- NA
purchases.sample <- purchases.sample[, ln_share_unknown := log(share_unknown)]
purchases.sample$ln_share_unknown[is.infinite(purchases.sample$ln_share_unknown)] <- NA

purchases.sample[, region_by_time := .GRP, by = .(hh_region_code, year, quarter)]
purchases.sample[, time := .GRP, by = .(year, quarter)]


## Estimations: Expenditure on type of module --------

output.results.file <- "../../../../../home/slacouture/HMS/HH_retailer_quarter_twowayFE.csv"

outcomes <- c("ln_expenditure_taxable", "ln_expenditure_non_taxable", "ln_expenditure_unknown",
              "ln_share_taxable", "ln_share_non_taxable", "ln_share_unknown")

FE_opts <- c("region_by_time", "time")


LRdiff_res <- data.table(NULL)
for (Y in outcomes) {
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
    res1.dt[, N.obs := nrow(purchases.sample[!is.na(get(Y))])]
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)

  }
}


## summary values --------------------------------------------------------------
LRdiff_res$N_hholds <- length(unique(purchases.sample$household_code))
LRdiff_res$N_years <- uniqueN(purchases.sample, by = c("year"))

fwrite(LRdiff_res, output.results.file)
