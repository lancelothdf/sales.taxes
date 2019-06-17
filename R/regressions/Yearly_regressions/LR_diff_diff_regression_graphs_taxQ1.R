### In this R-file we explore a specification meant to estimate long-run pass-through and sales/quantity response to tax changes

library(data.table)
library(lfe)
library(futile.logger)
library(AER)


setwd("/project2/igaarder")

## useful filepaths ------------------------------------------------------------
all_goods_pi_path <- "Data/Nielsen/price_quantity_indices_allitems_2006-2016_notaxinfo.csv"
FE_pindex_path <- "Data/Nielsen/Pindex_FE_yearly_all_years.csv"
output_yearly <- "Data/Nielsen/yearly_nielsen_data.csv"
pre_trend_data_path <- "Data/Nielsen/pre_trend_data_yearly_taxQ1.csv"
Q1_data_path <- "Data/all_nielsen_data_2006_2016_Q1only.csv"
#taxable_pi_path <- "Data/Nielsen/price_quantity_indices_taxableitems_2006-2016.csv"
#eventstudy_tr_path <- "Data/event_study_tr_groups_comprehensive_firstonly_no2012q4_2013q1q2.csv"
#tr_groups_path <- "Data/tr_groups_comprehensive_firstonly_no2012q4_2013q1q2.csv"


covariates.nhgis.path <- "Data/covariates/nhgis_county_clean.csv"
covariates.qcew.path <- "Data/covariates/qcew_clean.csv"
census.regions.path <- "Data/covariates/census_regions.csv"


# ### Prepare the data
## See LR_diff_diff_regression_sales_FE_specification.R -- the file output_yearly is prepared in that file
yearly_data <- fread(output_yearly)


###############################
Q1_data <- fread(Q1_data_path)
yearly_data <- merge(yearly_data, Q1_data, by = c("fips_state", "fips_county", "store_code_uc", "product_module_code", "year"), all.x = T)
rm(Q1_data)


## Get rid of observations with missing values
yearly_data <- yearly_data[!is.na(base.sales) & !is.na(ln_cpricei) &
                             !is.na(ln_sales_tax) & !is.na(ln_cpricei2) & !is.na(ln_quantity) &
                             !is.na(ln_quantity2) & !is.na(ln_quantity_Q1) & !is.na(ln_quantity2_Q1) &
                             !is.na(ln_cpricei_Q1) & !is.na(ln_cpricei2_Q1)]


## Use Q1 tax rate as tax rate for that year - Idem with price measures
## For quantity - we keep yearly average because of seasonality (but will do robustness check with Q1 quantity)
yearly_data[, ln_sales_tax := ln_sales_tax_Q1]
yearly_data[, ln_cpricei := ln_cpricei_Q1]
yearly_data[, ln_cpricei2 := ln_cpricei2_Q1]
yearly_data <- yearly_data[, -c("ln_cpricei_Q1", "ln_cpricei2_Q1", "ln_sales_tax_Q1")]



################################
## Include some covariates
#List of unique counties in the sales data
list.counties <- data.frame(unique(yearly_data[,c('fips_state','fips_county')]))

covariates.nhgis <- fread(covariates.nhgis.path)
census.regions <- fread(census.regions.path)
census.regions <- merge(list.counties, census.regions, by = c("fips_state"),
                        all.x = T)
census.regions$Division <- census.regions$Region*10 + census.regions$Division


setkey(covariates.nhgis, year)
covariates.nhgis <- covariates.nhgis[list(2000),]

setnames(covariates.nhgis, c('statefp','countyfp'), c('fips_state', 'fips_county'))
covariates.nhgis <- covariates.nhgis[,c('fips_state', 'fips_county', 'pct_pop_urban')] #For now, let's just use "urban"
covariates.nhgis$urban <- as.integer(covariates.nhgis$pct_pop_urban >= 0.5)

covariates.nhgis <- merge(census.regions, covariates.nhgis, by = c("fips_state", "fips_county"),
                          all.x = T)


###
yearly_data <- merge(yearly_data, covariates.nhgis, by = c("fips_state", "fips_county"), all.x = T)

yearly_data[, region_by_time := .GRP, by = .(Region, year)]
yearly_data[, division_by_time := .GRP, by = .(Division, year)]
yearly_data[, urban_by_time := .GRP, by = .(urban, year)]
yearly_data[, regXurban_by_time := .GRP, by = .(Region, urban, year)]
yearly_data[, divXurban_by_time := .GRP, by = .(Division, urban, year)]
yearly_data[, reg_by_module_by_time := .GRP, by = .(Region, product_module_code, year)]
yearly_data[, div_by_module_by_time := .GRP, by = .(Division, product_module_code, year)]
################################



### Create measure of change in log sales tax rate
yearly_data <- yearly_data[order(fips_state, fips_county, store_code_uc, product_module_code, -year),] ##Sort on store by year (year in descending order)

yearly_data$dltax <- -diff(yearly_data$ln_sales_tax) ## Difference log of tax rate
yearly_data$dltax[yearly_data$year <= 2008] <- NA



### First produce graphs to explore pre-trends
list.outcomes <- c("ln_cpricei", "ln_cpricei2", "ln_quantity", "ln_quantity2", "ln_sales_tax", "ln_quantity_Q1", "ln_quantity2_Q1")
for(j in 1:length(list.outcomes)) {

  #
  outcome.j <- list.outcomes[j]

  ## Start with log price index (1)
  diff_data <- yearly_data[, c("year", "fips_state", "fips_county", "store_code_uc", "product_module_code", "dltax", "base.sales")]
  temp <- yearly_data[[outcome.j]]
  diff_data$outcome <- temp

  # Compute residualized price index for different designs
  reg_formula <- as.formula(paste0(outcome.j,
    " ~ 0 | yr | 0 | state_by_module "
  ))

  reg_res <- felm(data = yearly_data,
                              formula = reg_formula,
                              weights = yearly_data$base.sales)

  diff_data$outcome_res0 <- reg_res$residuals

  reg_formula <- as.formula(paste0(outcome.j,
    " ~ 0 | module_by_time | 0 | state_by_module "
  ))

  reg_res <- felm(data = yearly_data,
                  formula = reg_formula,
                  weights = yearly_data$base.sales)

  diff_data$outcome_res1 <- reg_res$residuals

  reg_formula <- as.formula(paste0(outcome.j,
    " ~ 0 | store_by_time | 0 | state_by_module "
  ))

  reg_res <- felm(data = yearly_data,
                  formula = reg_formula,
                  weights = yearly_data$base.sales)

  diff_data$outcome_res2 <- reg_res$residuals


  ##
  reg_formula <- as.formula(paste0(outcome.j,
    " ~ 0 | module_by_time + store_by_time | 0 | state_by_module "
  ))

  reg_res <- felm(data = yearly_data,
                  formula = reg_formula,
                  weights = yearly_data$base.sales)

  diff_data$outcome_res3 <- reg_res$residuals


  ##
  reg_formula <- as.formula(paste0(outcome.j,
                                   " ~ 0 | module_by_time + region_by_time | 0 | state_by_module "
  ))

  reg_res <- felm(data = yearly_data,
                  formula = reg_formula,
                  weights = yearly_data$base.sales)

  diff_data$outcome_res4 <- reg_res$residuals


  ##
  reg_formula <- as.formula(paste0(outcome.j,
                                   " ~ 0 | module_by_time + division_by_time | 0 | state_by_module "
  ))

  reg_res <- felm(data = yearly_data,
                  formula = reg_formula,
                  weights = yearly_data$base.sales)

  diff_data$outcome_res5 <- reg_res$residuals


  ##
  reg_formula <- as.formula(paste0(outcome.j,
                                   " ~ 0 | reg_by_module_by_time | 0 | state_by_module "
  ))

  reg_res <- felm(data = yearly_data,
                  formula = reg_formula,
                  weights = yearly_data$base.sales)

  diff_data$outcome_res6 <- reg_res$residuals


  ##
  reg_formula <- as.formula(paste0(outcome.j,
                                   " ~ 0 | div_by_module_by_time | 0 | state_by_module "
  ))

  reg_res <- felm(data = yearly_data,
                  formula = reg_formula,
                  weights = yearly_data$base.sales)

  diff_data$outcome_res7 <- reg_res$residuals


  # Create means by "treatment" vs "control"
  for (k in 2009:2014) {

    data.year.k <- diff_data[year == k, ]

    # Identify observations that had a tax change in year k
    increase_store_modules <- data.year.k[dltax >= 0.001, c("store_code_uc", "product_module_code")]
    nochange_store_modules <- data.year.k[dltax < 0.001 & dltax > - 0.001, c("store_code_uc", "product_module_code")]
    decrease_store_modules <- data.year.k[dltax <= -0.001, c("store_code_uc", "product_module_code")]

    setkey(increase_store_modules, store_code_uc, product_module_code)
    setkey(nochange_store_modules, store_code_uc, product_module_code)
    setkey(decrease_store_modules, store_code_uc, product_module_code)


        #Keep only data in year k-3 to k+3 (or 2014)
        data.year.j <- diff_data[year >= k-3 & year <= min(k+3, 2016), ]
        setkey(data.year.j, store_code_uc, product_module_code)

        #Divide data in increase/decrease/no change in year k
        increase.data.j <- data.year.j[increase_store_modules]
        nochange.data.j <- data.year.j[nochange_store_modules]
        decrease.data.j <- data.year.j[decrease_store_modules]

        #Take the weighted means
        increase.data.j <- increase.data.j[, list(dltax = weighted.mean(dltax, w = base.sales), outcome = weighted.mean(outcome, w = base.sales), outcome_res0 = weighted.mean(outcome_res0, w = base.sales), outcome_res1 = weighted.mean(outcome_res1, w = base.sales), outcome_res2 = weighted.mean(outcome_res2, w = base.sales), outcome_res3 = weighted.mean(outcome_res3, w=base.sales), outcome_res4 = weighted.mean(outcome_res4, w=base.sales), outcome_res5 = weighted.mean(outcome_res5, w=base.sales), outcome_res6 = weighted.mean(outcome_res6, w=base.sales), outcome_res7 = weighted.mean(outcome_res7, w=base.sales), totsales = sum(base.sales), n = .N), by = .(year)]
        decrease.data.j <- decrease.data.j[, list(dltax = weighted.mean(dltax, w = base.sales), outcome = weighted.mean(outcome, w = base.sales), outcome_res0 = weighted.mean(outcome_res0, w = base.sales), outcome_res1 = weighted.mean(outcome_res1, w = base.sales), outcome_res2 = weighted.mean(outcome_res2, w = base.sales), outcome_res3 = weighted.mean(outcome_res3, w=base.sales), outcome_res4 = weighted.mean(outcome_res4, w=base.sales), outcome_res5 = weighted.mean(outcome_res5, w=base.sales), outcome_res6 = weighted.mean(outcome_res6, w=base.sales), outcome_res7 = weighted.mean(outcome_res7, w=base.sales), totsales = sum(base.sales), n = .N), by = .(year)]
        nochange.data.j <- nochange.data.j[, list(dltax = weighted.mean(dltax, w = base.sales), outcome = weighted.mean(outcome, w = base.sales), outcome_res0 = weighted.mean(outcome_res0, w = base.sales), outcome_res1 = weighted.mean(outcome_res1, w = base.sales), outcome_res2 = weighted.mean(outcome_res2, w = base.sales), outcome_res3 = weighted.mean(outcome_res3, w=base.sales), outcome_res4 = weighted.mean(outcome_res4, w=base.sales), outcome_res5 = weighted.mean(outcome_res5, w=base.sales), outcome_res6 = weighted.mean(outcome_res6, w=base.sales), outcome_res7 = weighted.mean(outcome_res7, w=base.sales), totsales = sum(base.sales), n = .N), by = .(year)]

        #Compute the average change in log(1+tax) in year k for each group
        increase.data.j[, dltax_k_inc := dltax[year == k]]
        decrease.data.j$dltax_k_inc <- increase.data.j$dltax_k_inc
        nochange.data.j$dltax_k_inc <- increase.data.j$dltax_k_inc

        decrease.data.j[, dltax_k_dec := dltax[year == k]]
        increase.data.j$dltax_k_dec <- decrease.data.j$dltax_k_dec
        nochange.data.j$dltax_k_dec <- decrease.data.j$dltax_k_dec

        nochange.data.j[, dltax_k_noc := dltax[year == k]]
        increase.data.j$dltax_k_noc <- nochange.data.j$dltax_k_noc
        decrease.data.j$dltax_k_noc <- nochange.data.j$dltax_k_noc

        #Create cohort, treatment and variable variables
        increase.data.j$cohort <- k
        decrease.data.j$cohort <- k
        nochange.data.j$cohort <- k
        increase.data.j$treatment <- "increase"
        decrease.data.j$treatment <- "decrease"
        nochange.data.j$treatment <- "no change"
        increase.data.j$variable <- outcome.j
        decrease.data.j$variable <- outcome.j
        nochange.data.j$variable <- outcome.j


        if(j == 1 & k == 2009) {
          pre.trend.data <- rbind(increase.data.j, decrease.data.j, nochange.data.j)
        }
        else {
          pre.trend.data <- rbind(pre.trend.data, increase.data.j, decrease.data.j, nochange.data.j)
        }
  }

}

pre.trend.data[, tt_event := year - cohort]
fwrite(pre.trend.data, pre_trend_data_path)


#### Plot the averages across cohorts
pre.trend.data <- fread(pre_trend_data_path)


##Average across cohorts

# First - keep only cohorts for which we have at least two post-periods data points
avg.pre.trend <- pre.trend.data[, list(outcome = mean(outcome), outcome_res0 = mean(outcome_res0), outcome_res1 = mean(outcome_res1), outcome_res2 = mean(outcome_res2), outcome_res3 = mean(outcome_res3), outcome_res4 = mean(outcome_res4), outcome_res5 = mean(outcome_res5), outcome_res6 = mean(outcome_res6), outcome_res7 = mean(outcome_res7), dltax = mean(dltax), dltax_k_inc = mean(dltax_k_inc), dltax_k_dec = mean(dltax_k_dec), dltax_k_noc = mean(dltax_k_noc)), by = .(treatment, variable, tt_event)]
avg.pre.trend <- avg.pre.trend[tt_event <= 2] ## We keep only three post period data points (because otherwise we either lose a cohort or have an unbalanced group)

#Normalize at t = - 1
avg.pre.trend[, outcome_atm1 := outcome[tt_event == -1], by = .(treatment, variable)]
avg.pre.trend[, outcome0_atm1 := outcome_res0[tt_event == -1], by = .(treatment, variable)]
avg.pre.trend[, outcome1_atm1 := outcome_res1[tt_event == -1], by = .(treatment, variable)]
avg.pre.trend[, outcome2_atm1 := outcome_res2[tt_event == -1], by = .(treatment, variable)]
avg.pre.trend[, outcome3_atm1 := outcome_res3[tt_event == -1], by = .(treatment, variable)]
avg.pre.trend[, outcome4_atm1 := outcome_res4[tt_event == -1], by = .(treatment, variable)]
avg.pre.trend[, outcome5_atm1 := outcome_res5[tt_event == -1], by = .(treatment, variable)]
avg.pre.trend[, outcome6_atm1 := outcome_res6[tt_event == -1], by = .(treatment, variable)]
avg.pre.trend[, outcome7_atm1 := outcome_res7[tt_event == -1], by = .(treatment, variable)]
avg.pre.trend[, outcome_norm := outcome - outcome_atm1]
avg.pre.trend[, outcome_norm0 := outcome_res0 - outcome0_atm1]
avg.pre.trend[, outcome_norm1 := outcome_res1 - outcome1_atm1]
avg.pre.trend[, outcome_norm2 := outcome_res2 - outcome2_atm1]
avg.pre.trend[, outcome_norm3 := outcome_res3 - outcome3_atm1]
avg.pre.trend[, outcome_norm4 := outcome_res4 - outcome4_atm1]
avg.pre.trend[, outcome_norm5 := outcome_res5 - outcome5_atm1]
avg.pre.trend[, outcome_norm6 := outcome_res6 - outcome6_atm1]
avg.pre.trend[, outcome_norm7 := outcome_res7 - outcome7_atm1]


########### First: Plot log of sales tax rate (log(1+t))
## ln_sales_tax
pre.trend.increase <- avg.pre.trend[variable == "ln_sales_tax" & treatment == "increase"]
pre.trend.decrease <- avg.pre.trend[variable == "ln_sales_tax" & treatment == "decrease"]
pre.trend.nochange <- avg.pre.trend[variable == "ln_sales_tax" & treatment == "no change"]

### No FE - Not normalized
#setEPS()
#postscript('Graphs/pretrends/ln_cpricei_yearly_reg0.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome], ylim = c(0, 0.08), xlab = "Time to event", ylab = "log of sales tax rate", lty = 3, col = "green", type = "b")
title(main = "Average log of sales tax rate", sub = "Raw means")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome], lty = 3, col = "red", type = "b")
legend(x = -3, y = 0.13, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
#dev.off()

### No FE - normalized at -1
setEPS()
postscript('Graphs/pretrends/ln_sales_tax_yearly_reg0.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_norm], ylim = c(-0.008, 0.008), xlab = "Time to event", ylab = "log of sales tax rate", lty = 3, col = "green", type = "b")
title(main = "Average log of sales tax rate", sub = "Raw means")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_norm], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_norm], lty = 3, col = "red", type = "b")
legend(x = -3, y = - 0.003, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
dev.off()



########### Second - ln_cpricei
## ln_cpricei
pre.trend.increase <- avg.pre.trend[variable == "ln_cpricei" & treatment == "increase"]
pre.trend.decrease <- avg.pre.trend[variable == "ln_cpricei" & treatment == "decrease"]
pre.trend.nochange <- avg.pre.trend[variable == "ln_cpricei" & treatment == "no change"]


### No FE - Not normalized
#setEPS()
#postscript('Graphs/pretrends/ln_cpricei_yearly_reg0.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome], ylim = c(0.04, 0.15), xlab = "Time to event", ylab = "log consumer price index (1)", lty = 3, col = "green", type = "b")
title(main = "Average log consumer price index", sub = "Raw means")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome], lty = 3, col = "red", type = "b")
legend(x = -3, y = 0.13, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
#dev.off()


### No FE - normalized at -1
setEPS()
postscript('Graphs/pretrends/ln_cpricei_yearly_reg0.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_norm], ylim = c(-0.05, 0.05), xlab = "Time to event", ylab = "log consumer price index (1)", lty = 3, col = "green", type = "b")
title(main = "Average log consumer price index", sub = "Raw means")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_norm], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_norm], lty = 3, col = "red", type = "b")
legend(x = -3, y = 0.035, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
dev.off()


### Year FE - not normalized
#setEPS()
#postscript('Graphs/pretrends/ln_cpricei_yearly_reg1.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_res0], ylim = c(-.032, 0.008), xlab = "Time to event", ylab = "log consumer price index (1)", lty = 3, col = "green", type = "b")
title(main = "Average log consumer price index", sub = "After year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_res0], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_res0], lty = 3, col = "red", type = "b")
legend(x = -3, y = 0, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
#dev.off()


### Year FE - normalized at -2
setEPS()
postscript('Graphs/pretrends/ln_cpricei_yearly_reg1.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_norm0], ylim = c(-.022, 0.015), xlab = "Time to event", ylab = "log consumer price index (1)", lty = 3, col = "green", type = "b")
title(main = "Average log consumer price index", sub = "After year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_norm0], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_norm0], lty = 3, col = "red", type = "b")
legend(x = -2.5, y = -0.01, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
dev.off()


### Create counter-factual under full-passthrough
pre.trend.nochange[, full_pass_inc := outcome_norm1 + dltax_k_inc - dltax_k_noc]
pre.trend.nochange[, full_pass_dec := outcome_norm1 + dltax_k_dec - dltax_k_noc]

### Module by Year FE - not normalized
#setEPS()
#postscript('Graphs/pretrends/ln_cpricei_yearly_reg2.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_res1], ylim = c(-.01, 0.006), xlab = "Time to event", ylab = "log consumer price index (1)", lty = 3, col = "green", type = "b")
title(main = "Average log consumer price index", sub = "After module by year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_res1], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_res1], lty = 3, col = "red", type = "b")
legend(x = -3, y = -0.002, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
#dev.off()


### Module by Year FE - normalized at -2
setEPS()
postscript('Graphs/pretrends/ln_cpricei_yearly_reg2.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_norm1], ylim = c(-.008, 0.006), xlab = "Time to event", ylab = "log consumer price index (1)", lty = 3, col = "green", type = "b")
title(main = "Average log consumer price index", sub = "After module by year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_norm1], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_norm1], lty = 3, col = "red", type = "b")
points(pre.trend.nochange[tt_event >= 0, tt_event], pre.trend.nochange[tt_event >= 0, full_pass_inc], lty = 2, col = "grey", type = "b")
points(pre.trend.nochange[tt_event >= 0, tt_event], pre.trend.nochange[tt_event >= 0, full_pass_dec], lty = 2, col = "grey", type = "b")
legend(x = -3, y = -0.003, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0", "Full Passthrough"), lty = c(3,1,3, 2), col = c("green", "black", "red", "grey"))
abline(v=0, lty = 2, col = "grey")
dev.off()


### Store by Year FE - not normalized
#setEPS()
#postscript('Graphs/pretrends/ln_cpricei_yearly_reg3.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_res2], ylim = c(-.015, 0.003), xlab = "Time to event", ylab = "log consumer price index (1)", lty = 3, col = "green", type = "b")
title(main = "Average log consumer price index", sub = "After store by year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_res2], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_res2], lty = 3, col = "red", type = "b")
legend(x = -2, y = 0, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
#dev.off()


### Store by Year FE - normalized at -2
### Create counter-factual under full-passthrough
pre.trend.nochange[, full_pass_inc := outcome_norm2 + dltax_k_inc - dltax_k_noc]
pre.trend.nochange[, full_pass_dec := outcome_norm2 + dltax_k_dec - dltax_k_noc]

setEPS()
postscript('Graphs/pretrends/ln_cpricei_yearly_reg3.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_norm2], ylim = c(-.01, 0.006), xlab = "Time to event", ylab = "log consumer price index (1)", lty = 3, col = "green", type = "b")
title(main = "Average log consumer price index", sub = "After store by year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_norm2], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_norm2], lty = 3, col = "red", type = "b")
points(pre.trend.nochange[tt_event >= 0, tt_event], pre.trend.nochange[tt_event >= 0, full_pass_inc], lty = 2, col = "grey", type = "b")
points(pre.trend.nochange[tt_event >= 0, tt_event], pre.trend.nochange[tt_event >= 0, full_pass_dec], lty = 2, col = "grey", type = "b")
legend(x = -3, y = -0.0035, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0", "Full Passthrough"), lty = c(3,1,3, 2), col = c("green", "black", "red", "grey"))
abline(v=0, lty = 2, col = "grey")
dev.off()


### Store by Year + Module by Year FE - not normalized
#setEPS()
#postscript('Graphs/pretrends/ln_cpricei_yearly_reg4.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_res3], ylim = c(-.0045, 0.006), xlab = "Time to event", ylab = "log consumer price index (1)", lty = 3, col = "green", type = "b")
title(main = "Average log consumer price index", sub = "After store by year + module by year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_res3], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_res3], lty = 3, col = "red", type = "b")
legend(x = -2.5, y = -0.001, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
#dev.off()


### Module by Year + Store by Year FE - normalized at -1
### Create counter-factual under full-passthrough
pre.trend.nochange[, full_pass_inc := outcome_norm3 + dltax_k_inc - dltax_k_noc]
pre.trend.nochange[, full_pass_dec := outcome_norm3 + dltax_k_dec - dltax_k_noc]

setEPS()
postscript('Graphs/pretrends/ln_cpricei_yearly_reg4.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_norm3], ylim = c(-.006, 0.005), xlab = "Time to event", ylab = "log consumer price index (1)", lty = 3, col = "green", type = "b")
title(main = "Average log consumer price index", sub = "After store by year + module by year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_norm3], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_norm3], lty = 3, col = "red", type = "b")
points(pre.trend.nochange[tt_event >= 0, tt_event], pre.trend.nochange[tt_event >= 0, full_pass_inc], lty = 2, col = "grey", type = "b")
points(pre.trend.nochange[tt_event >= 0, tt_event], pre.trend.nochange[tt_event >= 0, full_pass_dec], lty = 2, col = "grey", type = "b")
legend(x = -3, y = -0.0025, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0", "Full Passthrough"), lty = c(3,1,3, 2), col = c("green", "black", "red", "grey"))
abline(v=0, lty = 2, col = "grey")
dev.off()



### Module by Year + Region by Year FE - normalized at -1
### Create counter-factual under full-passthrough
pre.trend.nochange[, full_pass_inc := outcome_norm4 + dltax_k_inc - dltax_k_noc]
pre.trend.nochange[, full_pass_dec := outcome_norm4 + dltax_k_dec - dltax_k_noc]

setEPS()
postscript('Graphs/pretrends/ln_cpricei_yearly_reg5.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_norm4], ylim = c(-.009, 0.006), xlab = "Time to event", ylab = "log consumer price index (1)", lty = 3, col = "green", type = "b")
title(main = "Average log consumer price index", sub = "After module by year + region by year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_norm4], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_norm4], lty = 3, col = "red", type = "b")
points(pre.trend.nochange[tt_event >= 0, tt_event], pre.trend.nochange[tt_event >= 0, full_pass_inc], lty = 2, col = "grey", type = "b")
points(pre.trend.nochange[tt_event >= 0, tt_event], pre.trend.nochange[tt_event >= 0, full_pass_dec], lty = 2, col = "grey", type = "b")
legend(x = -3, y = -0.0035, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0", "Full Passthrough"), lty = c(3,1,3, 2), col = c("green", "black", "red", "grey"))
abline(v=0, lty = 2, col = "grey")
dev.off()


### Module by Year + Division by Year FE - normalized at -1
### Create counter-factual under full-passthrough
pre.trend.nochange[, full_pass_inc := outcome_norm5 + dltax_k_inc - dltax_k_noc]
pre.trend.nochange[, full_pass_dec := outcome_norm5 + dltax_k_dec - dltax_k_noc]

setEPS()
postscript('Graphs/pretrends/ln_cpricei_yearly_reg6.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_norm5], ylim = c(-.008, 0.005), xlab = "Time to event", ylab = "log consumer price index (1)", lty = 3, col = "green", type = "b")
title(main = "Average log consumer price index", sub = "After module by year + division by year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_norm5], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_norm5], lty = 3, col = "red", type = "b")
points(pre.trend.nochange[tt_event >= 0, tt_event], pre.trend.nochange[tt_event >= 0, full_pass_inc], lty = 2, col = "grey", type = "b")
points(pre.trend.nochange[tt_event >= 0, tt_event], pre.trend.nochange[tt_event >= 0, full_pass_dec], lty = 2, col = "grey", type = "b")
legend(x = -3, y = -0.0035, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0", "Full Passthrough"), lty = c(3,1,3, 2), col = c("green", "black", "red", "grey"))
abline(v=0, lty = 2, col = "grey")
dev.off()


### Module by Region by Year FE - normalized at -1
### Create counter-factual under full-passthrough
pre.trend.nochange[, full_pass_inc := outcome_norm6 + dltax_k_inc - dltax_k_noc]
pre.trend.nochange[, full_pass_dec := outcome_norm6 + dltax_k_dec - dltax_k_noc]

setEPS()
postscript('Graphs/pretrends/ln_cpricei_yearly_reg7.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_norm6], ylim = c(-.006, 0.0045), xlab = "Time to event", ylab = "log consumer price index (1)", lty = 3, col = "green", type = "b")
title(main = "Average log consumer price index", sub = "After module by region by year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_norm6], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_norm6], lty = 3, col = "red", type = "b")
points(pre.trend.nochange[tt_event >= 0, tt_event], pre.trend.nochange[tt_event >= 0, full_pass_inc], lty = 2, col = "grey", type = "b")
points(pre.trend.nochange[tt_event >= 0, tt_event], pre.trend.nochange[tt_event >= 0, full_pass_dec], lty = 2, col = "grey", type = "b")
legend(x = -3, y = -0.003, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0", "Full Passthrough"), lty = c(3,1,3, 2), col = c("green", "black", "red", "grey"))
abline(v=0, lty = 2, col = "grey")
dev.off()


### Module by Division by Year FE - normalized at -1
### Create counter-factual under full-passthrough
pre.trend.nochange[, full_pass_inc := outcome_norm7 + dltax_k_inc - dltax_k_noc]
pre.trend.nochange[, full_pass_dec := outcome_norm7 + dltax_k_dec - dltax_k_noc]

setEPS()
postscript('Graphs/pretrends/ln_cpricei_yearly_reg8.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_norm7], ylim = c(-.005, 0.0045), xlab = "Time to event", ylab = "log consumer price index (1)", lty = 3, col = "green", type = "b")
title(main = "Average log consumer price index", sub = "After module by division by year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_norm7], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_norm7], lty = 3, col = "red", type = "b")
points(pre.trend.nochange[tt_event >= 0, tt_event], pre.trend.nochange[tt_event >= 0, full_pass_inc], lty = 2, col = "grey", type = "b")
points(pre.trend.nochange[tt_event >= 0, tt_event], pre.trend.nochange[tt_event >= 0, full_pass_dec], lty = 2, col = "grey", type = "b")
legend(x = -3, y = -0.002, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0", "Full Passthrough"), lty = c(3,1,3, 2), col = c("green", "black", "red", "grey"))
abline(v=0, lty = 2, col = "grey")
dev.off()


########### Second - ln_cpricei2
## ln_cpricei2
pre.trend.increase <- avg.pre.trend[variable == "ln_cpricei2" & treatment == "increase"]
pre.trend.decrease <- avg.pre.trend[variable == "ln_cpricei2" & treatment == "decrease"]
pre.trend.nochange <- avg.pre.trend[variable == "ln_cpricei2" & treatment == "no change"]


### No FE - Not normalized
#setEPS()
#postscript('Graphs/pretrends/ln_cpricei2_yearly_reg0.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome], ylim = c(-1.5,-0.8), xlab = "Time to event", ylab = "log consumer price index (2)", lty = 3, col = "green", type = "b")
title(main = "Average log consumer price index", sub = "Raw means")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome], lty = 3, col = "red", type = "b")
legend(x = -3, y = -1.2, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
#dev.off()


### No FE - normalized at -1
setEPS()
postscript('Graphs/pretrends/ln_cpricei2_yearly_reg0.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_norm], ylim = c(-0.04, 0.03), xlab = "Time to event", ylab = "log consumer price index (2)", lty = 3, col = "green", type = "b")
title(main = "Average log consumer price index", sub = "Raw means")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_norm], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_norm], lty = 3, col = "red", type = "b")
legend(x = -3, y = 0.025, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
dev.off()


### Year FE - not normalized
#setEPS()
#postscript('Graphs/pretrends/ln_cpricei2_yearly_reg1.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_res0], ylim = c(-0.1, 0.45), xlab = "Time to event", ylab = "log consumer price index (2)", lty = 3, col = "green", type = "b")
title(main = "Average log consumer price index", sub = "After year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_res0], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_res0], lty = 3, col = "red", type = "b")
legend(x = -3, y = 0.1, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
#dev.off()


### Year FE - normalized at -2
setEPS()
postscript('Graphs/pretrends/ln_cpricei2_yearly_reg1.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_norm0], ylim = c(-.013, 0.011), xlab = "Time to event", ylab = "log consumer price index (2)", lty = 3, col = "green", type = "b")
title(main = "Average log consumer price index", sub = "After year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_norm0], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_norm0], lty = 3, col = "red", type = "b")
legend(x = -3, y = -0.005, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
dev.off()


### Create counter-factual under full-passthrough
pre.trend.nochange[, full_pass_inc := outcome_norm1 + dltax_k_inc - dltax_k_noc]
pre.trend.nochange[, full_pass_dec := outcome_norm1 + dltax_k_dec - dltax_k_noc]

### Module by Year FE - not normalized
#setEPS()
#postscript('Graphs/pretrends/ln_cpricei2_yearly_reg2.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_res1], ylim = c(-.01, 0.025), xlab = "Time to event", ylab = "log consumer price index (2)", lty = 3, col = "green", type = "b")
title(main = "Average log consumer price index", sub = "After module by year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_res1], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_res1], lty = 3, col = "red", type = "b")
legend(x = -3, y = 0.007, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
#dev.off()


### Module by Year FE - normalized at -2
setEPS()
postscript('Graphs/pretrends/ln_cpricei2_yearly_reg2.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_norm1], ylim = c(-.008, 0.005), xlab = "Time to event", ylab = "log consumer price index (2)", lty = 3, col = "green", type = "b")
title(main = "Average log consumer price index", sub = "After module by year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_norm1], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_norm1], lty = 3, col = "red", type = "b")
points(pre.trend.nochange[tt_event >= 0, tt_event], pre.trend.nochange[tt_event >= 0, full_pass_inc], lty = 2, col = "grey", type = "b")
points(pre.trend.nochange[tt_event >= 0, tt_event], pre.trend.nochange[tt_event >= 0, full_pass_dec], lty = 2, col = "grey", type = "b")
legend(x = -3, y = -0.0025, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0", "Full Passthrough"), lty = c(3,1,3, 2), col = c("green", "black", "red", "grey"))
abline(v=0, lty = 2, col = "grey")
dev.off()


### Store by Year FE - not normalized
#setEPS()
#postscript('Graphs/pretrends/ln_cpricei2_yearly_reg3.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_res2], ylim = c(-.08, 0.2), xlab = "Time to event", ylab = "log consumer price index (2)", lty = 3, col = "green", type = "b")
title(main = "Average log consumer price index", sub = "After store by year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_res2], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_res2], lty = 3, col = "red", type = "b")
legend(x = -3, y = 0.06, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
#dev.off()


### Store by Year FE - normalized at -2
### Create counter-factual under full-passthrough
pre.trend.nochange[, full_pass_inc := outcome_norm2 + dltax_k_inc - dltax_k_noc]
pre.trend.nochange[, full_pass_dec := outcome_norm2 + dltax_k_dec - dltax_k_noc]

setEPS()
postscript('Graphs/pretrends/ln_cpricei2_yearly_reg3.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_norm2], ylim = c(-.009, 0.006), xlab = "Time to event", ylab = "log consumer price index (2)", lty = 3, col = "green", type = "b")
title(main = "Average log consumer price index", sub = "After store by year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_norm2], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_norm2], lty = 3, col = "red", type = "b")
points(pre.trend.nochange[tt_event >= 0, tt_event], pre.trend.nochange[tt_event >= 0, full_pass_inc], lty = 2, col = "grey", type = "b")
points(pre.trend.nochange[tt_event >= 0, tt_event], pre.trend.nochange[tt_event >= 0, full_pass_dec], lty = 2, col = "grey", type = "b")
legend(x = -3, y = -0.004, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0", "Full Passthrough"), lty = c(3,1,3, 2), col = c("green", "black", "red", "grey"))
abline(v=0, lty = 2, col = "grey")
dev.off()


### Store by Year + Module by Year FE - not normalized
#setEPS()
#postscript('Graphs/pretrends/ln_cpricei2_yearly_reg4.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_res3], ylim = c(-.003, 0.015), xlab = "Time to event", ylab = "log consumer price index (2)", lty = 3, col = "green", type = "b")
title(main = "Average log consumer price index", sub = "After store by year + module by year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_res3], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_res3], lty = 3, col = "red", type = "b")
legend(x = -2.5, y = -0.001, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
#dev.off()


### Module by Year + Module by Year FE - normalized at -2
### Create counter-factual under full-passthrough
pre.trend.nochange[, full_pass_inc := outcome_norm3 + dltax_k_inc - dltax_k_noc]
pre.trend.nochange[, full_pass_dec := outcome_norm3 + dltax_k_dec - dltax_k_noc]

setEPS()
postscript('Graphs/pretrends/ln_cpricei2_yearly_reg4.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_norm3], ylim = c(-.006, 0.005), xlab = "Time to event", ylab = "log consumer price index (2)", lty = 3, col = "green", type = "b")
title(main = "Average log consumer price index", sub = "After store by year + module by year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_norm3], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_norm3], lty = 3, col = "red", type = "b")
points(pre.trend.nochange[tt_event >= 0, tt_event], pre.trend.nochange[tt_event >= 0, full_pass_inc], lty = 2, col = "grey", type = "b")
points(pre.trend.nochange[tt_event >= 0, tt_event], pre.trend.nochange[tt_event >= 0, full_pass_dec], lty = 2, col = "grey", type = "b")
legend(x = -3, y = -0.0025, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0", "Full Passthrough"), lty = c(3,1,3, 2), col = c("green", "black", "red", "grey"))
abline(v=0, lty = 2, col = "grey")
dev.off()



### Module by Year FE + Region by Year FE - normalized at -1
### Create counter-factual under full-passthrough
pre.trend.nochange[, full_pass_inc := outcome_norm4 + dltax_k_inc - dltax_k_noc]
pre.trend.nochange[, full_pass_dec := outcome_norm4 + dltax_k_dec - dltax_k_noc]

setEPS()
postscript('Graphs/pretrends/ln_cpricei2_yearly_reg5.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_norm4], ylim = c(-.007, 0.005), xlab = "Time to event", ylab = "log consumer price index (2)", lty = 3, col = "green", type = "b")
title(main = "Average log consumer price index", sub = "After module by year + region by year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_norm4], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_norm4], lty = 3, col = "red", type = "b")
points(pre.trend.nochange[tt_event >= 0, tt_event], pre.trend.nochange[tt_event >= 0, full_pass_inc], lty = 2, col = "grey", type = "b")
points(pre.trend.nochange[tt_event >= 0, tt_event], pre.trend.nochange[tt_event >= 0, full_pass_dec], lty = 2, col = "grey", type = "b")
legend(x = -3, y = -0.003, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0", "Full Passthrough"), lty = c(3,1,3, 2), col = c("green", "black", "red", "grey"))
abline(v=0, lty = 2, col = "grey")
dev.off()


### Module by Year FE + Division by Year FE - normalized at -1
### Create counter-factual under full-passthrough
pre.trend.nochange[, full_pass_inc := outcome_norm5 + dltax_k_inc - dltax_k_noc]
pre.trend.nochange[, full_pass_dec := outcome_norm5 + dltax_k_dec - dltax_k_noc]

setEPS()
postscript('Graphs/pretrends/ln_cpricei2_yearly_reg6.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_norm5], ylim = c(-.007, 0.005), xlab = "Time to event", ylab = "log consumer price index (2)", lty = 3, col = "green", type = "b")
title(main = "Average log consumer price index", sub = "After module by year + division by year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_norm5], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_norm5], lty = 3, col = "red", type = "b")
points(pre.trend.nochange[tt_event >= 0, tt_event], pre.trend.nochange[tt_event >= 0, full_pass_inc], lty = 2, col = "grey", type = "b")
points(pre.trend.nochange[tt_event >= 0, tt_event], pre.trend.nochange[tt_event >= 0, full_pass_dec], lty = 2, col = "grey", type = "b")
legend(x = -3, y = -0.003, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0", "Full Passthrough"), lty = c(3,1,3, 2), col = c("green", "black", "red", "grey"))
abline(v=0, lty = 2, col = "grey")
dev.off()


### Module by Region by Year FE - normalized at -1
### Create counter-factual under full-passthrough
pre.trend.nochange[, full_pass_inc := outcome_norm6 + dltax_k_inc - dltax_k_noc]
pre.trend.nochange[, full_pass_dec := outcome_norm6 + dltax_k_dec - dltax_k_noc]

setEPS()
postscript('Graphs/pretrends/ln_cpricei2_yearly_reg7.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_norm6], ylim = c(-.007, 0.005), xlab = "Time to event", ylab = "log consumer price index (2)", lty = 3, col = "green", type = "b")
title(main = "Average log consumer price index", sub = "After module by region by year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_norm6], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_norm6], lty = 3, col = "red", type = "b")
points(pre.trend.nochange[tt_event >= 0, tt_event], pre.trend.nochange[tt_event >= 0, full_pass_inc], lty = 2, col = "grey", type = "b")
points(pre.trend.nochange[tt_event >= 0, tt_event], pre.trend.nochange[tt_event >= 0, full_pass_dec], lty = 2, col = "grey", type = "b")
legend(x = -3, y = -0.003, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0", "Full Passthrough"), lty = c(3,1,3, 2), col = c("green", "black", "red", "grey"))
abline(v=0, lty = 2, col = "grey")
dev.off()


########### Third - ln_quantity
## ln_quantity
pre.trend.increase <- avg.pre.trend[variable == "ln_quantity" & treatment == "increase"]
pre.trend.decrease <- avg.pre.trend[variable == "ln_quantity" & treatment == "decrease"]
pre.trend.nochange <- avg.pre.trend[variable == "ln_quantity" & treatment == "no change"]


### No FE - Not normalized
#setEPS()
#postscript('Graphs/pretrends/ln_quantity_yearly_reg0.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome], ylim = c(8.35, 8.45), xlab = "Time to event", ylab = "log quantity (1)", lty = 3, col = "green", type = "b")
title(main = "Average log quantity", sub = "Raw means")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome], lty = 3, col = "red", type = "b")
legend(x = -3, y = 8.4, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
#dev.off()


### No FE - normalized at -2
setEPS()
postscript('Graphs/pretrends/ln_quantity_yearly_reg0.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_norm], ylim = c(-0.06, 0.04), xlab = "Time to event", ylab = "log quantity (1)", lty = 3, col = "green", type = "b")
title(main = "Average log quantity", sub = "Raw means")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_norm], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_norm], lty = 3, col = "red", type = "b")
legend(x = -3, y = -0.01, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
dev.off()


### Year FE - not normalized
#setEPS()
#postscript('Graphs/pretrends/ln_quantity_yearly_reg1.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_res0], ylim = c(-0.04, 0.001), xlab = "Time to event", ylab = "log quantity (1)", lty = 3, col = "green", type = "b")
title(main = "Average log quantity", sub = "After year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_res0], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_res0], lty = 3, col = "red", type = "b")
legend(x = -3, y = -0.02, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
#dev.off()


### Year FE - normalized at -1
setEPS()
postscript('Graphs/pretrends/ln_quantity_yearly_reg1.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_norm0], ylim = c(-.025, 0.014), xlab = "Time to event", ylab = "log quantity (1)", lty = 3, col = "green", type = "b")
title(main = "Average log quantity", sub = "After year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_norm0], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_norm0], lty = 3, col = "red", type = "b")
legend(x = -2.5, y = -0.008, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
dev.off()


### Module by Year FE - not normalized
#setEPS()
#postscript('Graphs/pretrends/ln_quantity_yearly_reg2.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_res1], ylim = c(-.05, 0.05), xlab = "Time to event", ylab = "log quantity (1)", lty = 3, col = "green", type = "b")
title(main = "Average log quantity", sub = "After module by year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_res1], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_res1], lty = 3, col = "red", type = "b")
legend(x = -3, y = 0.02, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
#dev.off()


### Module by Year FE - normalized at -2
setEPS()
postscript('Graphs/pretrends/ln_quantity_yearly_reg2.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_norm1], ylim = c(-.005, 0.018), xlab = "Time to event", ylab = "log quantity (1)", lty = 3, col = "green", type = "b")
title(main = "Average log quantity", sub = "After module by year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_norm1], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_norm1], lty = 3, col = "red", type = "b")
legend(x = -3, y = 0.012, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
dev.off()


### Store by Year FE - not normalized
#setEPS()
#postscript('Graphs/pretrends/ln_quantity_yearly_reg3.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_res2], ylim = c(-.02, 0.07), xlab = "Time to event", ylab = "log quantity (1)", lty = 3, col = "green", type = "b")
title(main = "Average log quantity", sub = "After store by year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_res2], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_res2], lty = 3, col = "red", type = "b")
legend(x = -3, y = 0.02, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
#dev.off()


### Store by Year FE - normalized at -2
setEPS()
postscript('Graphs/pretrends/ln_quantity_yearly_reg3.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_norm2], ylim = c(-.018, 0.017), xlab = "Time to event", ylab = "log quantity (1)", lty = 3, col = "green", type = "b")
title(main = "Average log quantity", sub = "After store by year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_norm2], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_norm2], lty = 3, col = "red", type = "b")
legend(x = -3, y = -0.01, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
dev.off()


### Store by Year + Module by Year FE - not normalized
#setEPS()
#postscript('Graphs/pretrends/ln_quantity_yearly_reg4.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_res3], ylim = c(-.023, 0.005), xlab = "Time to event", ylab = "log quantity (1)", lty = 3, col = "green", type = "b")
title(main = "Average log quantity", sub = "After store by year + module by year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_res3], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_res3], lty = 3, col = "red", type = "b")
legend(x = -2.5, y = -0.001, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
#dev.off()


### Module by Year + Module by Year FE - normalized at -1

setEPS()
postscript('Graphs/pretrends/ln_quantity_yearly_reg4.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_norm3], ylim = c(-.003, 0.008), xlab = "Time to event", ylab = "log quantity (1)", lty = 3, col = "green", type = "b")
title(main = "Average log quantity", sub = "After store by year + module by year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_norm3], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_norm3], lty = 3, col = "red", type = "b")
legend(x = -3, y = 0.0055, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
dev.off()


### Module by Year + Region by Year FE - normalized at -1
setEPS()
postscript('Graphs/pretrends/ln_quantity_yearly_reg5.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_norm4], ylim = c(-.005, 0.011), xlab = "Time to event", ylab = "log quantity (1)", lty = 3, col = "green", type = "b")
title(main = "Average log quantity", sub = "After module by year + Region by year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_norm4], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_norm4], lty = 3, col = "red", type = "b")
legend(x = -3, y = 0.008, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
dev.off()


### Module by Year + Division by Year FE - normalized at -1
setEPS()
postscript('Graphs/pretrends/ln_quantity_yearly_reg6.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_norm5], ylim = c(-.005, 0.007), xlab = "Time to event", ylab = "log quantity (1)", lty = 3, col = "green", type = "b")
title(main = "Average log quantity", sub = "After module by year + Division by year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_norm5], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_norm5], lty = 3, col = "red", type = "b")
legend(x = -3, y = 0.0055, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
dev.off()


### Module by Region by Year FE - normalized at -1
setEPS()
postscript('Graphs/pretrends/ln_quantity_yearly_reg7.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_norm6], ylim = c(-.005, 0.008), xlab = "Time to event", ylab = "log quantity (1)", lty = 3, col = "green", type = "b")
title(main = "Average log quantity", sub = "After module by region by year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_norm6], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_norm6], lty = 3, col = "red", type = "b")
legend(x = -3, y = 0.007, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
dev.off()


### Module by Division by Year FE - normalized at -1
setEPS()
postscript('Graphs/pretrends/ln_quantity_yearly_reg8.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_norm7], ylim = c(-.005, 0.008), xlab = "Time to event", ylab = "log quantity (1)", lty = 3, col = "green", type = "b")
title(main = "Average log quantity", sub = "After module by division by year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_norm7], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_norm7], lty = 3, col = "red", type = "b")
legend(x = -3, y = 0.007, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
dev.off()




########### Fourth - ln_quantity (2)
## ln_quantity2
pre.trend.increase <- avg.pre.trend[variable == "ln_quantity2" & treatment == "increase"]
pre.trend.decrease <- avg.pre.trend[variable == "ln_quantity2" & treatment == "decrease"]
pre.trend.nochange <- avg.pre.trend[variable == "ln_quantity2" & treatment == "no change"]


### No FE - Not normalized
#setEPS()
#postscript('Graphs/pretrends/ln_quantity2_yearly_reg0.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome], ylim = c(9.4, 10), xlab = "Time to event", ylab = "log quantity (2)", lty = 3, col = "green", type = "b")
title(main = "Average log quantity", sub = "Raw means")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome], lty = 3, col = "red", type = "b")
legend(x = -2, y = 9.9, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
#dev.off()


### No FE - normalized at -2
setEPS()
postscript('Graphs/pretrends/ln_quantity2_yearly_reg0.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_norm], ylim = c(-0.05, 0.03), xlab = "Time to event", ylab = "log quantity (2)", lty = 3, col = "green", type = "b")
title(main = "Average log quantity", sub = "Raw means")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_norm], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_norm], lty = 3, col = "red", type = "b")
legend(x = -3, y = -0.01, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
dev.off()


### Year FE - not normalized
#setEPS()
#postscript('Graphs/pretrends/ln_quantity2_yearly_reg1.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_res0], ylim = c(-0.5, 0.1), xlab = "Time to event", ylab = "log quantity (2)", lty = 3, col = "green", type = "b")
title(main = "Average log quantity", sub = "After year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_res0], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_res0], lty = 3, col = "red", type = "b")
legend(x = -3, y = 0, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
#dev.off()


### Year FE - normalized at -2
setEPS()
postscript('Graphs/pretrends/ln_quantity2_yearly_reg1.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_norm0], ylim = c(-.03, 0.02), xlab = "Time to event", ylab = "log quantity (2)", lty = 3, col = "green", type = "b")
title(main = "Average log quantity", sub = "After year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_norm0], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_norm0], lty = 3, col = "red", type = "b")
legend(x = -3, y = -0.015, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
dev.off()


### Module by Year FE - not normalized
#setEPS()
#postscript('Graphs/pretrends/ln_quantity2_yearly_reg2.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_res1], ylim = c(-.05, 0.05), xlab = "Time to event", ylab = "log quantity (2)", lty = 3, col = "green", type = "b")
title(main = "Average log quantity", sub = "After module by year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_res1], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_res1], lty = 3, col = "red", type = "b")
legend(x = 1, y = 0.02, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
#dev.off()


### Module by Year FE - normalized at -2
setEPS()
postscript('Graphs/pretrends/ln_quantity2_yearly_reg2.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_norm1], ylim = c(-.005, 0.017), xlab = "Time to event", ylab = "log quantity (2)", lty = 3, col = "green", type = "b")
title(main = "Average log quantity", sub = "After module by year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_norm1], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_norm1], lty = 3, col = "red", type = "b")
legend(x = -3, y = 0.014, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
dev.off()


### Store by Year FE - not normalized
#setEPS()
#postscript('Graphs/pretrends/ln_quantity2_yearly_reg3.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_res2], ylim = c(-.12, 0.018), xlab = "Time to event", ylab = "log quantity (2)", lty = 3, col = "green", type = "b")
title(main = "Average log quantity", sub = "After store by year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_res2], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_res2], lty = 3, col = "red", type = "b")
legend(x = -3, y = 0.02, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
#dev.off()


### Store by Year FE - normalized at -2
setEPS()
postscript('Graphs/pretrends/ln_quantity2_yearly_reg3.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_norm2], ylim = c(-.02, 0.02), xlab = "Time to event", ylab = "log quantity (2)", lty = 3, col = "green", type = "b")
title(main = "Average log quantity", sub = "After store by year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_norm2], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_norm2], lty = 3, col = "red", type = "b")
legend(x = -3, y = -0.01, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
dev.off()


### Store by Year + Module by Year FE - not normalized
#setEPS()
#postscript('Graphs/pretrends/ln_quantity2_yearly_reg4.eps')
#plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_res3], ylim = c(-.023, 0.005), xlab = "Time to event", ylab = "log quantity (2)", lty = 3, col = "green", type = "b")
#title(main = "Average log quantity", sub = "After store by year + module by year FE")
#points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_res3], lty = 1, col = "black", type = "b")
#points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_res3], lty = 3, col = "red", type = "b")
#legend(x = -2.5, y = -0.001, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
#abline(v=0, lty = 2, col = "grey")
#dev.off()


### Module by Year + Store by Year FE - normalized at -1
setEPS()
postscript('Graphs/pretrends/ln_quantity2_yearly_reg4.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_norm3], ylim = c(-.003, 0.006), xlab = "Time to event", ylab = "log quantity (2)", lty = 3, col = "green", type = "b")
title(main = "Average log quantity", sub = "After store by year + module by year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_norm3], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_norm3], lty = 3, col = "red", type = "b")
legend(x = -3, y = 0.004, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
dev.off()


### Module by Year + Region by Year FE - normalized at -1
setEPS()
postscript('Graphs/pretrends/ln_quantity2_yearly_reg5.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_norm4], ylim = c(-.005, 0.01), xlab = "Time to event", ylab = "log quantity (2)", lty = 3, col = "green", type = "b")
title(main = "Average log quantity", sub = "After module by year + region by year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_norm4], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_norm4], lty = 3, col = "red", type = "b")
legend(x = -3, y = 0.008, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
dev.off()


### Module by Year + Division by Year FE - normalized at -1
setEPS()
postscript('Graphs/pretrends/ln_quantity2_yearly_reg6.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_norm5], ylim = c(-.005, 0.01), xlab = "Time to event", ylab = "log quantity (2)", lty = 3, col = "green", type = "b")
title(main = "Average log quantity", sub = "After module by year + division by year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_norm5], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_norm5], lty = 3, col = "red", type = "b")
legend(x = -3, y = 0.008, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
dev.off()



### Module by Region by Year FE - normalized at -1
setEPS()
postscript('Graphs/pretrends/ln_quantity2_yearly_reg7.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_norm6], ylim = c(-.006, 0.01), xlab = "Time to event", ylab = "log quantity (2)", lty = 3, col = "green", type = "b")
title(main = "Average log quantity", sub = "After module by region by year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_norm6], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_norm6], lty = 3, col = "red", type = "b")
legend(x = -3, y = 0.009, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
dev.off()


### Module by Division by Year FE - normalized at -1
setEPS()
postscript('Graphs/pretrends/ln_quantity2_yearly_reg8.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_norm7], ylim = c(-.006, 0.01), xlab = "Time to event", ylab = "log quantity (2)", lty = 3, col = "green", type = "b")
title(main = "Average log quantity", sub = "After module by division by year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_norm7], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_norm7], lty = 3, col = "red", type = "b")
legend(x = -3, y = 0.009, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
dev.off()




#########
#########
#Make some plots with Both price and quantity
## ln_cpricei
pre.trend.price.increase <- avg.pre.trend[variable == "ln_cpricei" & treatment == "increase"]
pre.trend.price.decrease <- avg.pre.trend[variable == "ln_cpricei" & treatment == "decrease"]
pre.trend.price.nochange <- avg.pre.trend[variable == "ln_cpricei" & treatment == "no change"]

### Create counter-factual under full-passthrough
pre.trend.price.nochange[, full_pass_inc := outcome_norm6 + dltax_k_inc - dltax_k_noc]
pre.trend.price.nochange[, full_pass_dec := outcome_norm6 + dltax_k_dec - dltax_k_noc]

## ln_quantity
pre.trend.quantity.increase <- avg.pre.trend[variable == "ln_quantity" & treatment == "increase"]
pre.trend.quantity.decrease <- avg.pre.trend[variable == "ln_quantity" & treatment == "decrease"]
pre.trend.quantity.nochange <- avg.pre.trend[variable == "ln_quantity" & treatment == "no change"]



setEPS()
postscript('Graphs/pretrends/ln_cpricei_quantity_yearly.eps')
plot(pre.trend.price.increase[, tt_event], pre.trend.price.increase[, outcome_norm6], ylim = c(-.008, 0.006), xlab = "Time to event", ylab = "log price & log quantity (1)", lty = 1, col = "green", type = "b")
title(main = "Average log consumer price and quantity", sub = "After module by region by year FE")
points(pre.trend.price.nochange[, tt_event], pre.trend.price.nochange[, outcome_norm6], lty = 1, col = "black", type = "b")
points(pre.trend.price.decrease[, tt_event], pre.trend.price.decrease[, outcome_norm6], lty = 1, col = "red", type = "b")
points(pre.trend.price.nochange[tt_event >= 0, tt_event], pre.trend.price.nochange[tt_event >= 0, full_pass_inc], lty = 2, col = "grey", type = "b")
points(pre.trend.price.nochange[tt_event >= 0, tt_event], pre.trend.price.nochange[tt_event >= 0, full_pass_dec], lty = 2, col = "grey", type = "b")
points(pre.trend.quantity.increase[, tt_event], -pre.trend.quantity.increase[, outcome_norm6], lty = 3, col = "green", type = "b")
points(pre.trend.quantity.nochange[, tt_event], -pre.trend.quantity.nochange[, outcome_norm6], lty = 3, col = "black", type = "b")
points(pre.trend.quantity.decrease[, tt_event], -pre.trend.quantity.decrease[, outcome_norm6], lty = 3, col = "red", type = "b")
legend(x = -3, y = -0.0022, legend = c("Price (increase)", "Price (no change)", "Price (decrease)", "-Quantity (increase)", "-Quantity (no change)", "-Quantity (decrease)"), lty = c(1,1,1,3,3,3), col = c("green", "black", "red", "green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
dev.off()


### Same but for price index (2)
## ln_cpricei
pre.trend.price.increase <- avg.pre.trend[variable == "ln_cpricei2" & treatment == "increase"]
pre.trend.price.decrease <- avg.pre.trend[variable == "ln_cpricei2" & treatment == "decrease"]
pre.trend.price.nochange <- avg.pre.trend[variable == "ln_cpricei2" & treatment == "no change"]

### Create counter-factual under full-passthrough
pre.trend.price.nochange[, full_pass_inc := outcome_norm6 + dltax_k_inc - dltax_k_noc]
pre.trend.price.nochange[, full_pass_dec := outcome_norm6 + dltax_k_dec - dltax_k_noc]

## ln_quantity
pre.trend.quantity.increase <- avg.pre.trend[variable == "ln_quantity2" & treatment == "increase"]
pre.trend.quantity.decrease <- avg.pre.trend[variable == "ln_quantity2" & treatment == "decrease"]
pre.trend.quantity.nochange <- avg.pre.trend[variable == "ln_quantity2" & treatment == "no change"]



setEPS()
postscript('Graphs/pretrends/ln_cpricei2_quantity2_yearly.eps')
plot(pre.trend.price.increase[, tt_event], pre.trend.price.increase[, outcome_norm6], ylim = c(-.008, 0.006), xlab = "Time to event", ylab = "log price & log quantity (2)", lty = 1, col = "green", type = "b")
title(main = "Average consumer price and quantity", sub = "After module by region by year FE")
points(pre.trend.price.nochange[, tt_event], pre.trend.price.nochange[, outcome_norm6], lty = 1, col = "black", type = "b")
points(pre.trend.price.decrease[, tt_event], pre.trend.price.decrease[, outcome_norm6], lty = 1, col = "red", type = "b")
points(pre.trend.price.nochange[tt_event >= 0, tt_event], pre.trend.price.nochange[tt_event >= 0, full_pass_inc], lty = 2, col = "grey", type = "b")
points(pre.trend.price.nochange[tt_event >= 0, tt_event], pre.trend.price.nochange[tt_event >= 0, full_pass_dec], lty = 2, col = "grey", type = "b")
points(pre.trend.quantity.increase[, tt_event], -pre.trend.quantity.increase[, outcome_norm6], lty = 3, col = "green", type = "b")
points(pre.trend.quantity.nochange[, tt_event], -pre.trend.quantity.nochange[, outcome_norm6], lty = 3, col = "black", type = "b")
points(pre.trend.quantity.decrease[, tt_event], -pre.trend.quantity.decrease[, outcome_norm6], lty = 3, col = "red", type = "b")
legend(x = -3, y = -0.0022, legend = c("Price (increase)", "Price (no change)", "Price (decrease)", "-Quantity (increase)", "-Quantity (no change)", "-Quantity (decrease)"), lty = c(1,1,1,3,3,3), col = c("green", "black", "red", "green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
dev.off()

