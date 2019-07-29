## Sales taxes Project. Household Panel
# Running Basic Estimations: Controling for "Income"
# Runs basic identification with covariates and without time lags
# Author: Santiago Lacouture


library(data.table)
library(lfe)
library(futile.logger)
library(AER)
library(multcomp)
library(psych)
library(ggplot2)

setwd("/project2/igaarder/Data/Nielsen/Household_panel")

## Filepaths
# Exporting results to my folder in the server
purchases.full <- fread("cleaning/consumer_panel_2006-2016_ids.csv")


## Constraining Data set for estimations ------------ 
# Drop "magnet" purchases: 
purchases.full[, sum(is.na(projection_factor))]
# 2016154 obs
purchases.nomagnet <- purchases.full[!is.na(projection_factor)]

# Drop purchases without sales tax data
purchases.full[, sum(is.na(sales_tax))]
# More than 70%: 181247984 obs
purchases.retail <- purchases.nomagnet[!is.na(sales_tax)]


## Control for income using linear log total expenditure of quarter ----------

purchases.retail <- purchases.retail[, ln_quarter_expenditure := log(sum_total_exp_quarter)]

# Log Share of Expenditure
formula0 <- as.formula(paste0(
  "ln_share_expend ~ ln_sales_tax + ln_quarter_expenditure | module_by_time + household_by_store_by_module"
))

flog.info("Estimating Log Share")
res0 <- felm(data = purchases.retail,
             formula = formula0,
             weights = purchases.retail$projection_factor,
             na.omit)

flog.info("Writing results...")
res0.dt <- data.table(coef(summary(res0)), keep.rownames=T)
res0.dt[, outcome := "ln_share_expend"]
res0.dt[, Rsq := summary(res0)$r.squared]
res0.dt[, adj.Rsq := summary(res0)$adj.r.squared]
res0.dt[, specification := "Income: expenditures"]
res0.dt[, N_obs := sum((!is.na(purchases.retail$ln_share_expend)))]
LRdiff_res <- res0.dt ### Create table LRdiff_res in which we store all results (we start with the results we had just stored in res1.dt)
fwrite(LRdiff_res, "../../../../../home/slacouture/HMS/Basic_Results_Income.csv")

# Share of Expenditure
formula1 <- as.formula(paste0(
  "share_expend ~ ln_sales_tax + ln_quarter_expenditure| module_by_time + household_by_store_by_module"
))

flog.info("Estimating Share")
res1 <- felm(data = purchases.retail,
             formula = formula1,
             weights = purchases.retail$projection_factor,
             na.omit)
flog.info("Writing results...")
res0.dt <- data.table(coef(summary(res1)), keep.rownames=T)
res0.dt[, outcome := "share_expend"]
res0.dt[, Rsq := summary(res1)$r.squared]
res0.dt[, adj.Rsq := summary(res1)$adj.r.squared]
res0.dt[, specification := "Income: expenditures"]
res0.dt[, N_obs := sum((!is.na(purchases.retail$share_expend)))]
LRdiff_res <- rbind(LRdiff_res,res0.dt) ### Append 
fwrite(LRdiff_res, "../../../../../home/slacouture/HMS/Basic_Results_Income.csv")

# Log Price
formula2 <- as.formula(paste0(
  "ln_cpricei ~ ln_sales_tax + ln_quarter_expenditure| module_by_time + household_by_store_by_module"
))
flog.info("Estimating Log Price")
res2 <- felm(data = purchases.retail,
             formula = formula2,
             weights = purchases.retail$projection_factor,
             na.omit)

flog.info("Writing results...")
res0.dt <- data.table(coef(summary(res2)), keep.rownames=T)
res0.dt[, outcome := "ln_cpricei"]
res0.dt[, Rsq := summary(res2)$r.squared]
res0.dt[, adj.Rsq := summary(res2)$adj.r.squared]
res0.dt[, specification := "Income: expenditures"]
res0.dt[, N_obs := sum((!is.na(purchases.retail$ln_cpricei)))]
LRdiff_res <- rbind(LRdiff_res,res0.dt) ### Append 
fwrite(LRdiff_res, "../../../../../home/slacouture/HMS/Basic_Results_Income.csv")

# Log Quantity
formula2 <- as.formula(paste0(
  "ln_quantity ~ ln_sales_tax + ln_quarter_expenditure | module_by_time + household_by_store_by_module"
))
flog.info("Estimating Log Price")
res2 <- felm(data = purchases.retail,
             formula = formula2,
             weights = purchases.retail$projection_factor,
             na.omit)

flog.info("Writing results...")
res0.dt <- data.table(coef(summary(res2)), keep.rownames=T)
res0.dt[, outcome := "ln_quantity"]
res0.dt[, Rsq := summary(res2)$r.squared]
res0.dt[, adj.Rsq := summary(res2)$adj.r.squared]
res0.dt[, specification := "Income: expenditures"]
res0.dt[, N_obs := sum((!is.na(purchases.retail$ln_quantity)))]
LRdiff_res <- rbind(LRdiff_res,res0.dt) ### Append 
fwrite(LRdiff_res, "../../../../../home/slacouture/HMS/Basic_Results_Income.csv")


## Control for income using household income range as continuous based on the middle point ----------

purchases.retail$household_income <- factor(purchases.retail$household_income)
purchases.retail.est <- purchases.retail[!is.na(sales_tax)]

purchases.retail.est$household_income_c[household_income == 3] <- 2500
purchases.retail.est$household_income_c[household_income == 4] <- 6500
purchases.retail.est$household_income_c[household_income == 6] <- 9000
purchases.retail.est$household_income_c[household_income == 8] <- 11000
purchases.retail.est$household_income_c[household_income == 10] <- 13000
purchases.retail.est$household_income_c[household_income == 11] <- 17500
purchases.retail.est$household_income_c[household_income == 13] <- 22500
purchases.retail.est$household_income_c[household_income == 15] <- 27500
purchases.retail.est$household_income_c[household_income == 16] <- 32500
purchases.retail.est$household_income_c[household_income == 17] <- 37500
purchases.retail.est$household_income_c[household_income == 18] <- 42500
purchases.retail.est$household_income_c[household_income == 19] <- 47500
purchases.retail.est$household_income_c[household_income == 21] <- 55000
purchases.retail.est$household_income_c[household_income == 23] <- 65000
purchases.retail.est$household_income_c[household_income == 26] <- 80000
purchases.retail.est$household_income_c[household_income == 27] <- 112500
purchases.retail.est$household_income_c[household_income == 28] <- 137500
purchases.retail.est$household_income_c[household_income == 29] <- 175000
purchases.retail.est$household_income_c[household_income == 30] <- 212500




# Log Share of Expenditure
formula0 <- as.formula(paste0(
  "ln_share_expend ~ ln_sales_tax + household_income_c | module_by_time + household_by_store_by_module"
))

flog.info("Estimating Log Share")
res0 <- felm(data = purchases.retail.est,
             formula = formula0,
             weights = purchases.retail.est$projection_factor,
             na.omit)

flog.info("Writing results...")
res0.dt <- data.table(coef(summary(res0)), keep.rownames=T)
res0.dt[, outcome := "ln_share_expend"]
res0.dt[, Rsq := summary(res0)$r.squared]
res0.dt[, adj.Rsq := summary(res0)$adj.r.squared]
res0.dt[, specification := "Income: continuous"]
res0.dt[, N_obs := sum((!is.na(purchases.retail.est$ln_share_expend)))]
LRdiff_res <- rbind(LRdiff_res,res0.dt) ### Append 
fwrite(LRdiff_res, "../../../../../home/slacouture/HMS/Basic_Results_income.csv")

# Share of Expenditure
formula1 <- as.formula(paste0(
  "share_expend ~ ln_sales_tax + household_income_c | module_by_time + household_by_store_by_module"
))

flog.info("Estimating Share")
res1 <- felm(data = purchases.retail.est,
             formula = formula1,
             weights = purchases.retail.est$projection_factor,
             na.omit)
flog.info("Writing results...")
res0.dt <- data.table(coef(summary(res1)), keep.rownames=T)
res0.dt[, outcome := "share_expend"]
res0.dt[, Rsq := summary(res1)$r.squared]
res0.dt[, adj.Rsq := summary(res1)$adj.r.squared]
res0.dt[, specification := "Income: continuous"]
res0.dt[, N_obs := sum((!is.na(purchases.retail.est$share_expend)))]
LRdiff_res <- rbind(LRdiff_res,res0.dt) ### Append 
fwrite(LRdiff_res, "../../../../../home/slacouture/HMS/Basic_Results_income.csv")

# Log Price
formula2 <- as.formula(paste0(
  "ln_cpricei ~ ln_sales_tax + household_income_c | module_by_time + household_by_store_by_module"
))
flog.info("Estimating Log Price")
res2 <- felm(data = purchases.retail.est,
             formula = formula2,
             weights = purchases.retail.est$projection_factor,
             na.omit)

flog.info("Writing results...")
res0.dt <- data.table(coef(summary(res2)), keep.rownames=T)
res0.dt[, outcome := "ln_cpricei"]
res0.dt[, Rsq := summary(res2)$r.squared]
res0.dt[, adj.Rsq := summary(res2)$adj.r.squared]
res0.dt[, specification := "Income: continuous"]
res0.dt[, N_obs := sum((!is.na(purchases.retail.est$ln_cpricei)))]
LRdiff_res <- rbind(LRdiff_res,res0.dt) ### Append 
fwrite(LRdiff_res, "../../../../../home/slacouture/HMS/Basic_Results_income.csv")

# Log Quantity
formula2 <- as.formula(paste0(
  "ln_quantity ~ ln_sales_tax + household_income_c | module_by_time + household_by_store_by_module"
))
flog.info("Estimating Log Price")
res2 <- felm(data = purchases.retail.est,
             formula = formula2,
             weights = purchases.retail.est$projection_factor,
             na.omit)

flog.info("Writing results...")
res0.dt <- data.table(coef(summary(res2)), keep.rownames=T)
res0.dt[, outcome := "ln_quantity"]
res0.dt[, Rsq := summary(res2)$r.squared]
res0.dt[, adj.Rsq := summary(res2)$adj.r.squared]
res0.dt[, specification := "Income: continuous"]
res0.dt[, N_obs := sum((!is.na(purchases.retail.est$ln_quantity)))]
LRdiff_res <- rbind(LRdiff_res,res0.dt) ### Append 
fwrite(LRdiff_res, "../../../../../home/slacouture/HMS/Basic_Results_income.csv")
## Control for income using household income range as FE ----------

purchases.retail$household_income <- factor(purchases.retail$household_income)
purchases.retail.est <- purchases.retail[!is.na(sales_tax)]


# Log Share of Expenditure
formula0 <- as.formula(paste0(
  "ln_share_expend ~ ln_sales_tax | module_by_time + household_by_store_by_module + household_income"
))

flog.info("Estimating Log Share")
res0 <- felm(data = purchases.retail.est,
             formula = formula0,
             weights = purchases.retail.est$projection_factor,
             na.omit)

flog.info("Writing results...")
res0.dt <- data.table(coef(summary(res0)), keep.rownames=T)
res0.dt[, outcome := "ln_share_expend"]
res0.dt[, Rsq := summary(res0)$r.squared]
res0.dt[, adj.Rsq := summary(res0)$adj.r.squared]
res0.dt[, specification := "Income: FE"]
res0.dt[, N_obs := sum((!is.na(purchases.retail.est$ln_share_expend)))]
LRdiff_res <- rbind(LRdiff_res,res0.dt) ### Append 
fwrite(LRdiff_res, "../../../../../home/slacouture/HMS/Basic_Results_income.csv")

# Share of Expenditure
formula1 <- as.formula(paste0(
  "share_expend ~ ln_sales_tax | module_by_time + household_by_store_by_module + household_income"
))

flog.info("Estimating Share")
res1 <- felm(data = purchases.retail.est,
             formula = formula1,
             weights = purchases.retail.est$projection_factor,
             na.omit)
flog.info("Writing results...")
res0.dt <- data.table(coef(summary(res1)), keep.rownames=T)
res0.dt[, outcome := "share_expend"]
res0.dt[, Rsq := summary(res1)$r.squared]
res0.dt[, adj.Rsq := summary(res1)$adj.r.squared]
res0.dt[, specification := "Income: FE"]
res0.dt[, N_obs := sum((!is.na(purchases.retail.est$share_expend)))]
LRdiff_res <- rbind(LRdiff_res,res0.dt) ### Append 
fwrite(LRdiff_res, "../../../../../home/slacouture/HMS/Basic_Results_income.csv")

# Log Price
formula2 <- as.formula(paste0(
  "ln_cpricei ~ ln_sales_tax | module_by_time + household_by_store_by_module + household_income"
))
flog.info("Estimating Log Price")
res2 <- felm(data = purchases.retail.est,
             formula = formula2,
             weights = purchases.retail.est$projection_factor,
             na.omit)

flog.info("Writing results...")
res0.dt <- data.table(coef(summary(res2)), keep.rownames=T)
res0.dt[, outcome := "ln_cpricei"]
res0.dt[, Rsq := summary(res2)$r.squared]
res0.dt[, adj.Rsq := summary(res2)$adj.r.squared]
res0.dt[, specification := "Income: FE"]
res0.dt[, N_obs := sum((!is.na(purchases.retail.est$ln_cpricei)))]
LRdiff_res <- rbind(LRdiff_res,res0.dt) ### Append 
fwrite(LRdiff_res, "../../../../../home/slacouture/HMS/Basic_Results_income.csv")

# Log Quantity
formula2 <- as.formula(paste0(
  "ln_quantity ~ ln_sales_tax | module_by_time + household_by_store_by_module + household_income"
))
flog.info("Estimating Log Price")
res2 <- felm(data = purchases.retail.est,
             formula = formula2,
             weights = purchases.retail.est$projection_factor,
             na.omit)

flog.info("Writing results...")
res0.dt <- data.table(coef(summary(res2)), keep.rownames=T)
res0.dt[, outcome := "ln_quantity"]
res0.dt[, Rsq := summary(res2)$r.squared]
res0.dt[, adj.Rsq := summary(res2)$adj.r.squared]
res0.dt[, specification := "Income: FE"]
res0.dt[, N_obs := sum((!is.na(purchases.retail.est$ln_quantity)))]
LRdiff_res <- rbind(LRdiff_res,res0.dt) ### Append 
fwrite(LRdiff_res, "../../../../../home/slacouture/HMS/Basic_Results_income.csv")





