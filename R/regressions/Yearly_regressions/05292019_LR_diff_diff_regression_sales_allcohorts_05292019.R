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
pre_trend_data_path <- "Data/Nielsen/pre_trend_data_yearly.csv"
#taxable_pi_path <- "Data/Nielsen/price_quantity_indices_taxableitems_2006-2016.csv"
#eventstudy_tr_path <- "Data/event_study_tr_groups_comprehensive_firstonly_no2012q4_2013q1q2.csv"
#tr_groups_path <- "Data/tr_groups_comprehensive_firstonly_no2012q4_2013q1q2.csv"


# ### Prepare the data
## See LR_diff_diff_regression_sales_FE_specification.R -- the file output_yearly is prepared in that file
yearly_data <- fread(output_yearly)


### Difference-in-differences specifications
yearly_data <- yearly_data[order(fips_state, fips_county, store_code_uc, product_module_code, -year),] ##Sort on store by year (year in descending order)

yearly_data$dltax <- -diff(yearly_data$ln_sales_tax) ## Difference log of tax rate
yearly_data$dltax[yearly_data$year <= 2008] <- NA



### First produce graphs to explore pre-trends
list.outcomes <- c("ln_cpricei", "ln_cpricei2", "ln_quantity", "ln_quantity2", "expend_share", "ln_expend_share")
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


  reg_formula <- as.formula(paste0(outcome.j,
    " ~ 0 | module_by_time + store_by_time | 0 | state_by_module "
  ))

  reg_res <- felm(data = yearly_data,
                  formula = reg_formula,
                  weights = yearly_data$base.sales)

  diff_data$outcome_res3 <- reg_res$residuals



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
        data.year.j <- diff_data[year >= k-3 & year <= min(k+3, 2014), ]
        setkey(data.year.j, store_code_uc, product_module_code)

        #Divide data in increase/decrease/no change in year k
        increase.data.j <- data.year.j[increase_store_modules]
        nochange.data.j <- data.year.j[nochange_store_modules]
        decrease.data.j <- data.year.j[decrease_store_modules]

        #Take the weighted means
        increase.data.j <- increase.data.j[, list(dltax = weighted.mean(dltax, w = base.sales), outcome = weighted.mean(outcome, w = base.sales), outcome_res0 = weighted.mean(outcome_res0, w = base.sales), outcome_res1 = weighted.mean(outcome_res1, w = base.sales), outcome_res2 = weighted.mean(outcome_res2, w = base.sales), outcome_res3 = weighted.mean(outcome_res3, w=base.sales), totsales = sum(base.sales), n = .N), by = .(year)]
        decrease.data.j <- decrease.data.j[, list(dltax = weighted.mean(dltax, w = base.sales), outcome = weighted.mean(outcome, w = base.sales), outcome_res0 = weighted.mean(outcome_res0, w = base.sales), outcome_res1 = weighted.mean(outcome_res1, w = base.sales), outcome_res2 = weighted.mean(outcome_res2, w = base.sales), outcome_res3 = weighted.mean(outcome_res3, w=base.sales), totsales = sum(base.sales), n = .N), by = .(year)]
        nochange.data.j <- nochange.data.j[, list(dltax = weighted.mean(dltax, w = base.sales), outcome = weighted.mean(outcome, w = base.sales), outcome_res0 = weighted.mean(outcome_res0, w = base.sales), outcome_res1 = weighted.mean(outcome_res1, w = base.sales), outcome_res2 = weighted.mean(outcome_res2, w = base.sales), outcome_res3 = weighted.mean(outcome_res3, w=base.sales), totsales = sum(base.sales), n = .N), by = .(year)]

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

# First across all cohorts
avg.pre.trend <- pre.trend.data[, list(outcome = mean(outcome), outcome_res0 = mean(outcome_res0), outcome_res1 = mean(outcome_res1), outcome_res2 = mean(outcome_res2), outcome_res3 = mean(outcome_res3), dltax = mean(dltax), dltax_k_inc = mean(dltax_k_inc), dltax_k_dec = mean(dltax_k_dec), dltax_k_noc = mean(dltax_k_noc)), by = .(treatment, variable, tt_event)]

#Normalize at t = - 2
avg.pre.trend[, outcome_atm2 := outcome[tt_event == -2], by = .(treatment, variable)]
avg.pre.trend[, outcome0_atm2 := outcome_res0[tt_event == -2], by = .(treatment, variable)]
avg.pre.trend[, outcome1_atm2 := outcome_res1[tt_event == -2], by = .(treatment, variable)]
avg.pre.trend[, outcome2_atm2 := outcome_res2[tt_event == -2], by = .(treatment, variable)]
avg.pre.trend[, outcome3_atm2 := outcome_res3[tt_event == -2], by = .(treatment, variable)]
avg.pre.trend[, outcome_norm := outcome - outcome_atm2]
avg.pre.trend[, outcome_norm0 := outcome_res0 - outcome0_atm2]
avg.pre.trend[, outcome_norm1 := outcome_res1 - outcome1_atm2]
avg.pre.trend[, outcome_norm2 := outcome_res2 - outcome2_atm2]
avg.pre.trend[, outcome_norm3 := outcome_res3 - outcome3_atm2]


########### First - ln_cpricei
## ln_cpricei
pre.trend.increase <- avg.pre.trend[variable == "ln_cpricei" & treatment == "increase"]
pre.trend.decrease <- avg.pre.trend[variable == "ln_cpricei" & treatment == "decrease"]
pre.trend.nochange <- avg.pre.trend[variable == "ln_cpricei" & treatment == "no change"]


### No FE - Not normalized
#setEPS()
#postscript('Graphs/pretrends/ln_cpricei_yearly_reg0.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome], ylim = c(0.05, 0.2), xlab = "Time to event", ylab = "log consumer price index (1)", lty = 3, col = "green", type = "b")
title(main = "Average log consumer price index", sub = "Raw means")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome], lty = 3, col = "red", type = "b")
legend(x = 1, y = 0.09, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
#dev.off()


### No FE - normalized at -2
#setEPS()
#postscript('Graphs/pretrends/ln_cpricei_yearly_reg0.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_norm], ylim = c(-0.05, 0.07), xlab = "Time to event", ylab = "log consumer price index (1)", lty = 3, col = "green", type = "b")
title(main = "Average log consumer price index", sub = "Raw means")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_norm], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_norm], lty = 3, col = "red", type = "b")
legend(x = 1, y = 0, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
#dev.off()


### Year FE - not normalized
#setEPS()
#postscript('Graphs/pretrends/ln_cpricei_yearly_reg1.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_res0], ylim = c(-.035, 0.01), xlab = "Time to event", ylab = "log consumer price index (1)", lty = 3, col = "green", type = "b")
title(main = "Average log consumer price index", sub = "After year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_res0], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_res0], lty = 3, col = "red", type = "b")
legend(x = 1, y = 0, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
#dev.off()


### Year FE - normalized at -2
#setEPS()
#postscript('Graphs/pretrends/ln_cpricei_yearly_reg1.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_norm0], ylim = c(-.02, 0.01), xlab = "Time to event", ylab = "log consumer price index (1)", lty = 3, col = "green", type = "b")
title(main = "Average log consumer price index", sub = "After year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_norm0], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_norm0], lty = 3, col = "red", type = "b")
legend(x = -2.5, y = -0.01, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
#dev.off()


### Create counter-factual under full-passthrough
pre.trend.nochange[, full_pass_inc := outcome_norm1 + dltax_k_inc - dltax_k_noc]
pre.trend.nochange[, full_pass_dec := outcome_norm1 + dltax_k_dec - dltax_k_noc]

### Module by Year FE - not normalized
#setEPS()
#postscript('Graphs/pretrends/ln_cpricei_yearly_reg2.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_res1], ylim = c(-.006, 0.006), xlab = "Time to event", ylab = "log consumer price index (1)", lty = 3, col = "green", type = "b")
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
legend(x = -3, y = -0.0025, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0", "Full Passthrough"), lty = c(3,1,3, 2), col = c("green", "black", "red", "grey"))
abline(v=0, lty = 2, col = "grey")
dev.off()


### Store by Year FE - not normalized
#setEPS()
#postscript('Graphs/pretrends/ln_cpricei_yearly_reg3.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_res2], ylim = c(-.015, 0.003), xlab = "Time to event", ylab = "log consumer price index (1)", lty = 3, col = "green", type = "b")
title(main = "Average log consumer price index", sub = "After store by year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_res2], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_res2], lty = 3, col = "red", type = "b")
legend(x = 1, y = 0, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
#dev.off()


### Store by Year FE - normalized at -2
### Create counter-factual under full-passthrough
pre.trend.nochange[, full_pass_inc := outcome_norm2 + dltax_k_inc - dltax_k_noc]
pre.trend.nochange[, full_pass_dec := outcome_norm2 + dltax_k_dec - dltax_k_noc]

setEPS()
postscript('Graphs/pretrends/ln_cpricei_yearly_reg3.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_norm2], ylim = c(-.01, 0.007), xlab = "Time to event", ylab = "log consumer price index (1)", lty = 3, col = "green", type = "b")
title(main = "Average log consumer price index", sub = "After store by year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_norm2], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_norm2], lty = 3, col = "red", type = "b")
points(pre.trend.nochange[tt_event >= 0, tt_event], pre.trend.nochange[tt_event >= 0, full_pass_inc], lty = 2, col = "grey", type = "b")
points(pre.trend.nochange[tt_event >= 0, tt_event], pre.trend.nochange[tt_event >= 0, full_pass_dec], lty = 2, col = "grey", type = "b")
legend(x = -3, y = -0.0025, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0", "Full Passthrough"), lty = c(3,1,3, 2), col = c("green", "black", "red", "grey"))
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


### Module by Year + Module by Year FE - normalized at -2
### Create counter-factual under full-passthrough
pre.trend.nochange[, full_pass_inc := outcome_norm3 + dltax_k_inc - dltax_k_noc]
pre.trend.nochange[, full_pass_dec := outcome_norm3 + dltax_k_dec - dltax_k_noc]

#setEPS()
postscript('Graphs/pretrends/ln_cpricei_yearly_reg4.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_norm3], ylim = c(-.01, 0.007), xlab = "Time to event", ylab = "log consumer price index (1)", lty = 3, col = "green", type = "b")
title(main = "Average log consumer price index", sub = "After store by year + module by year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_norm3], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_norm3], lty = 3, col = "red", type = "b")
points(pre.trend.nochange[tt_event >= 0, tt_event], pre.trend.nochange[tt_event >= 0, full_pass_inc], lty = 2, col = "grey", type = "b")
points(pre.trend.nochange[tt_event >= 0, tt_event], pre.trend.nochange[tt_event >= 0, full_pass_dec], lty = 2, col = "grey", type = "b")
legend(x = -3, y = -0.0025, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0", "Full Passthrough"), lty = c(3,1,3, 2), col = c("green", "black", "red", "grey"))
abline(v=0, lty = 2, col = "grey")
dev.off()


########### Second - ln_cpricei2
## ln_cpricei
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
legend(x = 1.2, y = -1.12, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
#dev.off()


### No FE - normalized at -2
#setEPS()
#postscript('Graphs/pretrends/ln_cpricei2_yearly_reg0.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_norm], ylim = c(-0.022, 0.12), xlab = "Time to event", ylab = "log consumer price index (2)", lty = 3, col = "green", type = "b")
title(main = "Average log consumer price index", sub = "Raw means")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_norm], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_norm], lty = 3, col = "red", type = "b")
legend(x = -3, y = 0.08, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
#dev.off()


### Year FE - not normalized
#setEPS()
#postscript('Graphs/pretrends/ln_cpricei2_yearly_reg1.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_res0], ylim = c(-0.1, 0.45), xlab = "Time to event", ylab = "log consumer price index (2)", lty = 3, col = "green", type = "b")
title(main = "Average log consumer price index", sub = "After year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_res0], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_res0], lty = 3, col = "red", type = "b")
legend(x = 1, y = 0.1, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
#dev.off()


### Year FE - normalized at -2
#NOTE: it looks ugly but I think that it is because the post-period is unbalanced
#setEPS()
#postscript('Graphs/pretrends/ln_cpricei2_yearly_reg1.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_norm0], ylim = c(-.02, 0.035), xlab = "Time to event", ylab = "log consumer price index (2)", lty = 3, col = "green", type = "b")
title(main = "Average log consumer price index", sub = "After year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_norm0], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_norm0], lty = 3, col = "red", type = "b")
legend(x = -2.5, y = 0.03, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
#dev.off()


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
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_norm1], ylim = c(-.01, 0.008), xlab = "Time to event", ylab = "log consumer price index (2)", lty = 3, col = "green", type = "b")
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
legend(x = 1, y = 0.06, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
#dev.off()


### Store by Year FE - normalized at -2
### Create counter-factual under full-passthrough
pre.trend.nochange[, full_pass_inc := outcome_norm2 + dltax_k_inc - dltax_k_noc]
pre.trend.nochange[, full_pass_dec := outcome_norm2 + dltax_k_dec - dltax_k_noc]

setEPS()
postscript('Graphs/pretrends/ln_cpricei2_yearly_reg3.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_norm2], ylim = c(-.009, 0.01), xlab = "Time to event", ylab = "log consumer price index (2)", lty = 3, col = "green", type = "b")
title(main = "Average log consumer price index", sub = "After store by year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_norm2], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_norm2], lty = 3, col = "red", type = "b")
points(pre.trend.nochange[tt_event >= 0, tt_event], pre.trend.nochange[tt_event >= 0, full_pass_inc], lty = 2, col = "grey", type = "b")
points(pre.trend.nochange[tt_event >= 0, tt_event], pre.trend.nochange[tt_event >= 0, full_pass_dec], lty = 2, col = "grey", type = "b")
legend(x = -3, y = -0.0025, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0", "Full Passthrough"), lty = c(3,1,3, 2), col = c("green", "black", "red", "grey"))
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

#setEPS()
postscript('Graphs/pretrends/ln_cpricei2_yearly_reg4.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_norm3], ylim = c(-.01, 0.007), xlab = "Time to event", ylab = "log consumer price index (2)", lty = 3, col = "green", type = "b")
title(main = "Average log consumer price index", sub = "After store by year + module by year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_norm3], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_norm3], lty = 3, col = "red", type = "b")
points(pre.trend.nochange[tt_event >= 0, tt_event], pre.trend.nochange[tt_event >= 0, full_pass_inc], lty = 2, col = "grey", type = "b")
points(pre.trend.nochange[tt_event >= 0, tt_event], pre.trend.nochange[tt_event >= 0, full_pass_dec], lty = 2, col = "grey", type = "b")
legend(x = -3, y = -0.0025, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0", "Full Passthrough"), lty = c(3,1,3, 2), col = c("green", "black", "red", "grey"))
abline(v=0, lty = 2, col = "grey")
dev.off()




########### Third - ln_quantity
## ln_cpricei
pre.trend.increase <- avg.pre.trend[variable == "ln_quantity" & treatment == "increase"]
pre.trend.decrease <- avg.pre.trend[variable == "ln_quantity" & treatment == "decrease"]
pre.trend.nochange <- avg.pre.trend[variable == "ln_quantity" & treatment == "no change"]


### No FE - Not normalized
#setEPS()
#postscript('Graphs/pretrends/ln_quantity_yearly_reg0.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome], ylim = c(8.3, 8.45), xlab = "Time to event", ylab = "log quantity (1)", lty = 3, col = "green", type = "b")
title(main = "Average log quantity", sub = "Raw means")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome], lty = 3, col = "red", type = "b")
legend(x = -3, y = 8.35, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
#dev.off()


### No FE - normalized at -2
#setEPS()
#postscript('Graphs/pretrends/ln_quantity_yearly_reg0.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_norm], ylim = c(-0.075, 0.02), xlab = "Time to event", ylab = "log quantity (1)", lty = 3, col = "green", type = "b")
title(main = "Average log quantity", sub = "Raw means")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_norm], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_norm], lty = 3, col = "red", type = "b")
legend(x = 1, y = 0, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
#dev.off()


### Year FE - not normalized
#setEPS()
#postscript('Graphs/pretrends/ln_quantity_yearly_reg1.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_res0], ylim = c(-0.06, 0.01), xlab = "Time to event", ylab = "log quantity (1)", lty = 3, col = "green", type = "b")
title(main = "Average log quantity", sub = "After year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_res0], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_res0], lty = 3, col = "red", type = "b")
legend(x = 1, y = 0, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
#dev.off()


### Year FE - normalized at -2
#setEPS()
#postscript('Graphs/pretrends/ln_quantity_yearly_reg1.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_norm0], ylim = c(-.03, 0.01), xlab = "Time to event", ylab = "log quantity (1)", lty = 3, col = "green", type = "b")
title(main = "Average log quantity", sub = "After year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_norm0], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_norm0], lty = 3, col = "red", type = "b")
legend(x = -2.5, y = -0.01, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
#dev.off()


### Module by Year FE - not normalized
#setEPS()
#postscript('Graphs/pretrends/ln_quantity_yearly_reg2.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_res1], ylim = c(-.05, 0.05), xlab = "Time to event", ylab = "log quantity (1)", lty = 3, col = "green", type = "b")
title(main = "Average log quantity", sub = "After module by year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_res1], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_res1], lty = 3, col = "red", type = "b")
legend(x = 1, y = 0.02, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
#dev.off()


### Module by Year FE - normalized at -2
setEPS()
postscript('Graphs/pretrends/ln_quantity_yearly_reg2.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_norm1], ylim = c(-.008, 0.021), xlab = "Time to event", ylab = "log quantity (1)", lty = 3, col = "green", type = "b")
title(main = "Average log quantity", sub = "After module by year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_norm1], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_norm1], lty = 3, col = "red", type = "b")
legend(x = -3, y = 0.015, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
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
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_norm2], ylim = c(-.025, 0.007), xlab = "Time to event", ylab = "log quantity (1)", lty = 3, col = "green", type = "b")
title(main = "Average log quantity", sub = "After store by year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_norm2], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_norm2], lty = 3, col = "red", type = "b")
legend(x = 1, y = -0.01, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
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


### Module by Year + Module by Year FE - normalized at -2

#setEPS()
postscript('Graphs/pretrends/ln_quantity_yearly_reg4.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_norm3], ylim = c(-.001, 0.009), xlab = "Time to event", ylab = "log quantity (1)", lty = 3, col = "green", type = "b")
title(main = "Average log quantity", sub = "After store by year + module by year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_norm3], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_norm3], lty = 3, col = "red", type = "b")
legend(x = -3, y = 0.007, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
dev.off()

########### Fourth - ln_quantity (2)
## ln_cpricei
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
legend(x = 1, y = 9.9, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
#dev.off()


### No FE - normalized at -2
#setEPS()
#postscript('Graphs/pretrends/ln_quantity2_yearly_reg0.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_norm], ylim = c(-0.1, 0.02), xlab = "Time to event", ylab = "log quantity (2)", lty = 3, col = "green", type = "b")
title(main = "Average log quantity", sub = "Raw means")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_norm], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_norm], lty = 3, col = "red", type = "b")
legend(x = -3, y = -0.04, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
#dev.off()


### Year FE - not normalized
#setEPS()
#postscript('Graphs/pretrends/ln_quantity2_yearly_reg1.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_res0], ylim = c(-0.5, 0.1), xlab = "Time to event", ylab = "log quantity (2)", lty = 3, col = "green", type = "b")
title(main = "Average log quantity", sub = "After year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_res0], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_res0], lty = 3, col = "red", type = "b")
legend(x = 1, y = 0, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
#dev.off()


### Year FE - normalized at -2
#setEPS()
#postscript('Graphs/pretrends/ln_quantity2_yearly_reg1.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_norm0], ylim = c(-.06, 0.015), xlab = "Time to event", ylab = "log quantity (2)", lty = 3, col = "green", type = "b")
title(main = "Average log quantity", sub = "After year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_norm0], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_norm0], lty = 3, col = "red", type = "b")
legend(x = -2.5, y = -0.02, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
#dev.off()


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
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_norm1], ylim = c(-.008, 0.021), xlab = "Time to event", ylab = "log quantity (2)", lty = 3, col = "green", type = "b")
title(main = "Average log quantity", sub = "After module by year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_norm1], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_norm1], lty = 3, col = "red", type = "b")
legend(x = -3, y = 0.015, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
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
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_norm2], ylim = c(-.025, 0.007), xlab = "Time to event", ylab = "log quantity (2)", lty = 3, col = "green", type = "b")
title(main = "Average log quantity", sub = "After store by year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_norm2], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_norm2], lty = 3, col = "red", type = "b")
legend(x = 1, y = -0.01, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
dev.off()


### Store by Year + Module by Year FE - not normalized
#setEPS()
#postscript('Graphs/pretrends/ln_quantity2_yearly_reg4.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_res3], ylim = c(-.023, 0.005), xlab = "Time to event", ylab = "log quantity (2)", lty = 3, col = "green", type = "b")
title(main = "Average log quantity", sub = "After store by year + module by year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_res3], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_res3], lty = 3, col = "red", type = "b")
legend(x = -2.5, y = -0.001, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
#dev.off()


### Module by Year + Module by Year FE - normalized at -2

#setEPS()
postscript('Graphs/pretrends/ln_quantity2_yearly_reg4.eps')
plot(pre.trend.increase[, tt_event], pre.trend.increase[, outcome_norm3], ylim = c(-.001, 0.01), xlab = "Time to event", ylab = "log quantity (2)", lty = 3, col = "green", type = "b")
title(main = "Average log quantity", sub = "After store by year + module by year FE")
points(pre.trend.nochange[, tt_event], pre.trend.nochange[, outcome_norm3], lty = 1, col = "black", type = "b")
points(pre.trend.decrease[, tt_event], pre.trend.decrease[, outcome_norm3], lty = 3, col = "red", type = "b")
legend(x = -3, y = 0.007, legend = c("Tax increase at t=0", "No tax change at t=0", "Tax decrease at t=0"), lty = c(3,1,3), col = c("green", "black", "red"))
abline(v=0, lty = 2, col = "grey")
dev.off()


### Second run regressions for difference specifications
