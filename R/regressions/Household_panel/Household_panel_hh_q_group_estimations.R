## Sales taxes Project. Household Panel
# Running Basic Estimations on the new panel (household x group x quarter on all purchases)
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
purchases.full <- fread("cleaning/consumer_panel_q_hh_group_2006-2016.csv")

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


## Share
purchases.sample[, share_expenditures := expenditures/sum_total_exp_quarter]

## Logarithms
# Expenditures
purchases.sample <- purchases.sample[, ln_expenditures := log(expenditures)]
purchases.sample$ln_expenditures[is.infinite(purchases.sample$ln_expenditures)] <- NA

purchases.sample[, ln_expenditures_taxable := ifelse(taxability == 1, ln_expenditures, NA)]
purchases.sample[, ln_expenditures_non_taxable := ifelse(taxability == 0, ln_expenditures, NA)]
purchases.sample[, ln_expenditures_unknown := ifelse(taxability == 2, ln_expenditures, NA)]

# Share
purchases.sample <- purchases.sample[, ln_share := log(share_expenditures)]
purchases.sample$ln_share[is.infinite(purchases.sample$ln_share)] <- NA

purchases.sample[, ln_share_taxable := ifelse(taxability == 1, ln_share, NA)]
purchases.sample[, ln_share_non_taxable := ifelse(taxability == 0, ln_share, NA)]
purchases.sample[, ln_share_unknown := ifelse(taxability == 2, ln_share, NA)]

# FE
purchases.sample[, region_by_group_by_time := .GRP, by = .(hh_region_code, product_group_code, year, quarter)]
purchases.sample[, group_by_time := .GRP, by = .(product_group_code, year, quarter)]
purchases.sample[, household_by_group := .GRP, by = .(product_group_code, household_code)]


## Estimations: Expenditure on type of module --------

output.results.file <- "../../../../../home/slacouture/HMS/HH_group_quarter_twowayFE.csv"

outcomes <- c("D.ln_expenditures", "D.ln_expenditures_taxable", "D.ln_expenditures_non_taxable", "D.ln_expenditures_unknown",
              "D.ln_share", "D.ln_share_taxable",  "D.ln_share_non_taxable", "D.ln_share_unknown")

FE_opts <- c("region_by_group_by_time", "group_by_time")


LRdiff_res <- data.table(NULL)
for (Y in outcomes) {
  for (FE in FE_opts) {
    formula1 <- as.formula(paste0(
      Y, "~ ln_sales_tax | ", FE, "+ household_by_group | 0 | household_code"
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
    res1.dt[, N_hholds := uniqueN(purchases.sample[!is.na(get(Y))], by = c("household_code"))]
    res1.dt[, N_groups := uniqueN(purchases.sample[!is.na(get(Y))], by = c("product_group_code"))]
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)

  }
}


## summary values --------------------------------------------------------------
LRdiff_res$N_years <- uniqueN(purchases.sample, by = c("year"))

fwrite(LRdiff_res, output.results.file)
