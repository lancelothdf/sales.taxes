### In this R-file we explore a specification meant to estimate long-run pass-through and sales/quantity response to tax changes

library(data.table)
library(lfe)
library(futile.logger)
library(AER)
library(kableExtra)

setwd("/project2/igaarder")
#setwd("/Users/lancelothenrydefrahan/Documents/University of Chicago/Fifth year/Sales tax/To_work_at_home")


## useful filepaths ------------------------------------------------------------
all_goods_pi_path <- "Data/Nielsen/price_quantity_indices_allitems_2006-2016_notaxinfo.csv"
FE_pindex_path <- "Data/Nielsen/Pindex_FE_yearly_all_years.csv"
output_yearly <- "Data/Nielsen/yearly_nielsen_data.csv"
#taxable_pi_path <- "Data/Nielsen/price_quantity_indices_taxableitems_2006-2016.csv"
#eventstudy_tr_path <- "Data/event_study_tr_groups_comprehensive_firstonly_no2012q4_2013q1q2.csv"
#tr_groups_path <- "Data/tr_groups_comprehensive_firstonly_no2012q4_2013q1q2.csv"


options(scipen = 999) ## To avoid scientific notation
######
LRdiff_res <- fread("Data/LRdiff_results.csv")
LRdiff_res <- LRdiff_res[time_controls %in% c("time FE", "module-time FE", "module-region-time FE", "module-division-time FE")]
LRdiff_res$estimate <- round(LRdiff_res$estimate, digits = 3)
LRdiff_res$est_formatted <- paste(LRdiff_res$estimate)
LRdiff_res[pval <= 0.005]$est_formatted <- paste("$", LRdiff_res[pval <= 0.005]$est_formatted, "^{***}", "$", sep = "")
LRdiff_res[pval > 0.005 & pval <= 0.01]$est_formatted <- paste("$", LRdiff_res[pval > 0.005 & pval <= 0.01]$est_formatted, "^{**}", "$", sep = "")
LRdiff_res[pval > 0.01 & pval <= 0.05]$est_formatted <- paste("$", LRdiff_res[pval > 0.01 & pval <= 0.05]$est_formatted, "^{*}", "$", sep = "")
LRdiff_res[pval > 0.05]$est_formatted <- paste("$", LRdiff_res[pval > 0.05]$est_formatted, "$", sep = "")
LRdiff_res$se <- paste("(", format(LRdiff_res$se, digits = 1), ")", sep = "")
LRdiff_res$pval <- round(LRdiff_res$pval, digits = 3)
LRdiff_res$adj.Rsq <- round(LRdiff_res$adj.Rsq, digits = 4)
#LRdiff_res$N_obs <- paste(round(LRdiff_res$N_obs/1000000), ",", round((LRdiff_res$N_obs/1000)-(floor(LRdiff_res$N_obs/1000000)*1000)), ",", LRdiff_res$N_obs -(floor(LRdiff_res$N_obs/1000)*1000), sep = "")
LRdiff_res$N_obs <- format(LRdiff_res$N_obs, digits = 0, big.mark = ",")
LRdiff_res$N_stores <- format(LRdiff_res$N_stores, digits = 0, big.mark = ",")
LRdiff_res$N_counties <- format(LRdiff_res$N_counties, digits = 0, big.mark = ",")
LRdiff_res$N_county_modules <- format(LRdiff_res$N_county_modules, digits = 0, big.mark = ",")


##### First - Passthrough Regressions
### Create a matrix that will be converted to a latex table
table.reg <- matrix(0, nrow = 13, ncol = 8)
table.reg[1,1:4] <- LRdiff_res[outcome == "ln_cpricei"]$estimate
table.reg[1,5:8] <- LRdiff_res[outcome == "ln_cpricei2"]$estimate
table.reg[2,1:4] <- LRdiff_res[outcome == "ln_cpricei"]$se
table.reg[2,5:8] <- LRdiff_res[outcome == "ln_cpricei2"]$se
#table.reg[3,1:4] <- LRdiff_res[outcome == "ln_cpricei"]$pval
#table.reg[3,5:8] <- LRdiff_res[outcome == "ln_cpricei2"]$pval
table.reg[3,] <- c("x", "", "", "", "x", "", "", "")
table.reg[4,] <- c("", "x", "", "", "", "x", "", "")
table.reg[5,] <- c("", "", "x", "", "", "", "x", "")
table.reg[6,] <- c("", "", "", "x", "", "", "", "x")
table.reg[7,1:4] <- LRdiff_res[outcome == "ln_cpricei"]$adj.Rsq
table.reg[7,5:8] <- LRdiff_res[outcome == "ln_cpricei2"]$adj.Rsq
table.reg[8,] <- LRdiff_res[outcome == "ln_cpricei"]$N_obs ## Same for ln_cpricei and ln_cpricei2
table.reg[9,] <- LRdiff_res[outcome == "ln_cpricei"]$N_modules ## Same for ln_cpricei and ln_cpricei2
table.reg[10,] <- LRdiff_res[outcome == "ln_cpricei"]$N_stores ## Same for ln_cpricei and ln_cpricei2
table.reg[11,] <- LRdiff_res[outcome == "ln_cpricei"]$N_counties ## Same for ln_cpricei and ln_cpricei2
table.reg[12,] <- LRdiff_res[outcome == "ln_cpricei"]$N_years ## Same for ln_cpricei and ln_cpricei2
table.reg[13,] <- LRdiff_res[outcome == "ln_cpricei"]$N_county_modules ## Same for ln_cpricei and ln_cpricei2

table.label <- c("log(1+t)", "se", "Year FE", "Module by Year FE", "Mod by Region by Year FE", "Mod by Division by Year FE","Adj. Rsq" ,"Observations", "Modules", "Stores", "Counties", "Years", "CountyXModules")
table.reg <- matrix(c(table.label, table.reg), nrow = 13)
colnames(table.reg) <- c("", "(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)")

latex.table <- kable(table.reg, "latex", booktabs = T, linesep = "", caption = "Passthrough of Sales Taxes (yearly data)") %>%
  kable_styling(latex_options = "scale_down") %>%
  add_header_above(c(" ", "Laspeyres Index" = 4, "Price Index" = 4)) %>%
  row_spec(6, hline_after = T)
write(latex.table, file = "Regressions/Passthrough_reg_yearly_FE.tex")


##### Second - Quantity Regressions
### Create a matrix that will be converted to a latex table
table.reg <- matrix(0, nrow = 14, ncol = 8)
table.reg[1,1:4] <- LRdiff_res[outcome == "ln_quantity"]$est_formatted
table.reg[1,5:8] <- LRdiff_res[outcome == "ln_quantity2"]$est_formatted
table.reg[2,1:4] <- LRdiff_res[outcome == "ln_quantity"]$se
table.reg[2,5:8] <- LRdiff_res[outcome == "ln_quantity2"]$se
table.reg[3,1:4] <- LRdiff_res[outcome == "ln_quantity"]$pval
table.reg[3,5:8] <- LRdiff_res[outcome == "ln_quantity2"]$pval
table.reg[4,] <- c("x", "", "", "", "x", "", "", "")
table.reg[5,] <- c("", "x", "", "", "", "x", "", "")
table.reg[6,] <- c("", "", "x", "", "", "", "x", "")
table.reg[7,] <- c("", "", "", "x", "", "", "", "x")
table.reg[8,1:4] <- LRdiff_res[outcome == "ln_cpricei"]$adj.Rsq
table.reg[8,5:8] <- LRdiff_res[outcome == "ln_cpricei2"]$adj.Rsq
table.reg[9,] <- LRdiff_res[outcome == "ln_cpricei"]$N_obs ## Same for ln_cpricei and ln_cpricei2
table.reg[10,] <- LRdiff_res[outcome == "ln_cpricei"]$N_modules ## Same for ln_cpricei and ln_cpricei2
table.reg[11,] <- LRdiff_res[outcome == "ln_cpricei"]$N_stores ## Same for ln_cpricei and ln_cpricei2
table.reg[12,] <- LRdiff_res[outcome == "ln_cpricei"]$N_counties ## Same for ln_cpricei and ln_cpricei2
table.reg[13,] <- LRdiff_res[outcome == "ln_cpricei"]$N_years ## Same for ln_cpricei and ln_cpricei2
table.reg[14,] <- LRdiff_res[outcome == "ln_cpricei"]$N_county_modules ## Same for ln_cpricei and ln_cpricei2

table.label <- c("log(1+t)", "se", "p-value" ,"Year FE", "Module by Year FE", "Mod by Region by Year FE", "Mod by Division by Year FE", "Adj. Rsq" ,"Observations", "Modules", "Stores", "Counties", "Years", "CountyXModules")
table.reg <- matrix(c(table.label, table.reg), nrow = 14)
colnames(table.reg) <- c("", "(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)")

latex.table <- kable(table.reg, "latex", booktabs = T, linesep = "", caption = "Effect of Sales Taxes on Quantity (yearly data)", escape = F) %>%
  kable_styling(latex_options = "scale_down") %>%
  add_header_above(c(" ", "Definition 1" = 4, "Definition 2" = 4)) %>%
  row_spec(7, hline_after = T)
write(latex.table, file = "Regressions/Quantity_reg_yearly_FE.tex")


##### Tables for Long-Run specification
# options(scipen = 999) ## To avoid scientific notation
######
# LRdiff_res <- fread("Data/LRdiff_results_LRspec.csv")
# LRdiff_res$pval <- 2*(1 - pnorm(abs(LRdiff_res$estimate/LRdiff_res$se)))
# LRdiff_res$estimate <- round(LRdiff_res$estimate, digits = 3)
# LRdiff_res$est_formatted <- paste(LRdiff_res$estimate)
# LRdiff_res[pval <= 0.005]$est_formatted <- paste("$", LRdiff_res[pval <= 0.005]$est_formatted, "^{***}", "$", sep = "")
# LRdiff_res[pval > 0.005 & pval <= 0.01]$est_formatted <- paste("$", LRdiff_res[pval > 0.005 & pval <= 0.01]$est_formatted, "^{**}", "$", sep = "")
# LRdiff_res[pval > 0.01 & pval <= 0.05]$est_formatted <- paste("$", LRdiff_res[pval > 0.01 & pval <= 0.05]$est_formatted, "^{*}", "$", sep = "")
# LRdiff_res[pval > 0.05]$est_formatted <- paste("$", LRdiff_res[pval > 0.05]$est_formatted, "$", sep = "")
# LRdiff_res$se <- paste("(", format(LRdiff_res$se, digits = 1), ")", sep = "")
# LRdiff_res$pval <- round(LRdiff_res$pval, digits = 3)
# LRdiff_res$adj.Rsq <- round(LRdiff_res$adj.Rsq, digits = 4)
# #LRdiff_res$N_obs <- paste(round(LRdiff_res$N_obs/1000000), ",", round((LRdiff_res$N_obs/1000)-(floor(LRdiff_res$N_obs/1000000)*1000)), ",", LRdiff_res$N_obs -(floor(LRdiff_res$N_obs/1000)*1000), sep = "")
# LRdiff_res$N_obs <- format(LRdiff_res$N_obs, digits = 0, big.mark = ",")
# LRdiff_res$N_stores <- format(LRdiff_res$N_stores, digits = 0, big.mark = ",")
# LRdiff_res$N_counties <- format(LRdiff_res$N_counties, digits = 0, big.mark = ",")
# LRdiff_res$N_county_modules <- format(LRdiff_res$N_county_modules, digits = 0, big.mark = ",")


##### First - Passthrough Regressions
### Create a matrix that will be converted to a latex table
# table.reg <- matrix(0, nrow = 12, ncol = 2)
# table.reg[1,1] <- LRdiff_res[outcome == "ln_cpricei2"]$est_formatted
# table.reg[1,2] <- LRdiff_res[outcome == "ln_quantity2"]$est_formatted
# table.reg[2,1] <- LRdiff_res[outcome == "ln_cpricei2"]$se
# table.reg[2,2] <- LRdiff_res[outcome == "ln_quantity2"]$se
# table.reg[3,1] <- LRdiff_res[outcome == "ln_cpricei2"]$pval
# table.reg[3,2] <- LRdiff_res[outcome == "ln_quantity2"]$pval
# table.reg[4,] <- c("x", "x")
# table.reg[5,] <- c("x", "x")
# table.reg[6,1] <- LRdiff_res[outcome == "ln_cpricei2"]$adj.Rsq
# table.reg[6,2] <- LRdiff_res[outcome == "ln_quantity2"]$adj.Rsq
# table.reg[7,] <- LRdiff_res[outcome == "ln_cpricei2"]$N_obs ## Same for ln_cpricei and ln_cpricei2
# table.reg[8,] <- LRdiff_res[outcome == "ln_cpricei2"]$N_modules ## Same for ln_cpricei and ln_cpricei2
# table.reg[9,] <- LRdiff_res[outcome == "ln_cpricei2"]$N_stores ## Same for ln_cpricei and ln_cpricei2
# table.reg[10,] <- LRdiff_res[outcome == "ln_cpricei2"]$N_counties ## Same for ln_cpricei and ln_cpricei2
# table.reg[11,] <- LRdiff_res[outcome == "ln_cpricei2"]$N_years ## Same for ln_cpricei and ln_cpricei2
# table.reg[12,] <- LRdiff_res[outcome == "ln_cpricei2"]$N_county_modules ## Same for ln_cpricei and ln_cpricei2

# table.label <- c("log(1+t)", "se", "p-value", "Module FE", "Store FE", "Adj. Rsq" ,"Observations", "Modules", "Stores", "Counties", "Years", "CountyXModules")
# table.reg <- matrix(c(table.label, table.reg), nrow = 12)
# colnames(table.reg) <- c("", "Price", "Quantity")

# latex.table <- kable(table.reg, "latex", booktabs = T, linesep = "", caption = "Passthrough of Sales Taxes (LR specification)", escape = F) %>%
#   #kable_styling(latex_options = "scale_down") %>%
#   row_spec(5, hline_after = T)
# write(latex.table, file = "Regressions/Passthrough_reg_LR.tex")


## Turn off options(scipen)
options(scipen = NULL)


##### Tables for IV - Estimation of Demand elasticity (FE)
options(scipen = 999) ## To avoid scientific notation
######
LRdiff_res <- fread("Data/LRdiff_iv_results.csv")
LRdiff_res <- LRdiff_res[time_controls %in% c("time FE", "module-time FE", "region-module-time FE")]
LRdiff_res$estimate <- round(LRdiff_res$estimate, digits = 3)
LRdiff_res$est_formatted <- paste(LRdiff_res$estimate)
LRdiff_res[pval <= 0.005]$est_formatted <- paste("$", LRdiff_res[pval <= 0.005]$est_formatted, "^{***}", "$", sep = "")
LRdiff_res[pval > 0.005 & pval <= 0.01]$est_formatted <- paste("$", LRdiff_res[pval > 0.005 & pval <= 0.01]$est_formatted, "^{**}", "$", sep = "")
LRdiff_res[pval > 0.01 & pval <= 0.05]$est_formatted <- paste("$", LRdiff_res[pval > 0.01 & pval <= 0.05]$est_formatted, "^{*}", "$", sep = "")
LRdiff_res[pval > 0.05]$est_formatted <- paste("$", LRdiff_res[pval > 0.05]$est_formatted, "$", sep = "")
LRdiff_res$se <- paste("(", format(LRdiff_res$se, digits = 3), ")", sep = "")
LRdiff_res$pval <- round(LRdiff_res$pval, digits = 3)
LRdiff_res$adj.Rsq <- round(LRdiff_res$adj.Rsq, digits = 4)
#LRdiff_res$N_obs <- paste(round(LRdiff_res$N_obs/1000000), ",", round((LRdiff_res$N_obs/1000)-(floor(LRdiff_res$N_obs/1000000)*1000)), ",", LRdiff_res$N_obs -(floor(LRdiff_res$N_obs/1000)*1000), sep = "")
LRdiff_res$N_obs <- format(LRdiff_res$N_obs, digits = 0, big.mark = ",")
LRdiff_res$N_stores <- format(LRdiff_res$N_stores, digits = 0, big.mark = ",")
LRdiff_res$N_counties <- format(LRdiff_res$N_counties, digits = 0, big.mark = ",")
LRdiff_res$N_county_modules <- format(LRdiff_res$N_county_modules, digits = 0, big.mark = ",")

LRdiff_res$frst_coef <- round(LRdiff_res$frst_coef, digits = 3)
LRdiff_res$frest_formatted <- paste(LRdiff_res$frst_coef)
LRdiff_res[frst_pval <= 0.005]$frest_formatted <- paste("$", LRdiff_res[frst_pval <= 0.005]$frest_formatted, "^{***}", "$", sep = "")
LRdiff_res[frst_pval > 0.005 & frst_pval <= 0.01]$frest_formatted <- paste("$", LRdiff_res[frst_pval > 0.005 & frst_pval <= 0.01]$frest_formatted, "^{**}", "$", sep = "")
LRdiff_res[frst_pval > 0.01 & frst_pval <= 0.05]$frest_formatted <- paste("$", LRdiff_res[frst_pval > 0.01 & frst_pval <= 0.05]$frest_formatted, "^{*}", "$", sep = "")
LRdiff_res[frst_pval > 0.05]$frest_formatted <- paste("$", LRdiff_res[frst_pval > 0.05]$frest_formatted, "$", sep = "")
LRdiff_res$frst_se <- paste("(", format(LRdiff_res$frst_se, digits = 3), ")", sep = "")
LRdiff_res$frst_F <- format(LRdiff_res$frst_F, digits = 3)


### Create a matrix that will be converted to a latex table
table.reg <- matrix(0, nrow = 16, ncol = 6)
table.reg[1,1:3] <- LRdiff_res[outcome == "Demand elasticity (1)"]$est_formatted
table.reg[1,4:6] <- LRdiff_res[outcome == "Demand elasticity (2)"]$est_formatted
table.reg[2,1:3] <- LRdiff_res[outcome == "Demand elasticity (1)"]$se
table.reg[2,4:6] <- LRdiff_res[outcome == "Demand elasticity (2)"]$se
table.reg[3,1:3] <- LRdiff_res[outcome == "Demand elasticity (1)"]$pval
table.reg[3,4:6] <- LRdiff_res[outcome == "Demand elasticity (2)"]$pval
table.reg[4,1:3] <- LRdiff_res[outcome == "Demand elasticity (1)"]$frest_formatted
table.reg[4,4:6] <- LRdiff_res[outcome == "Demand elasticity (2)"]$frest_formatted
table.reg[5,1:3] <- LRdiff_res[outcome == "Demand elasticity (1)"]$frst_se
table.reg[5,4:6] <- LRdiff_res[outcome == "Demand elasticity (2)"]$frst_se
table.reg[6,1:3] <- LRdiff_res[outcome == "Demand elasticity (1)"]$frst_F
table.reg[6,4:6] <- LRdiff_res[outcome == "Demand elasticity (2)"]$frst_F
table.reg[7,] <- c("x", "", "", "x", "", "")
table.reg[8,] <- c("", "x", "", "", "x", "")
table.reg[9,] <- c("", "", "x", "", "", "x")
table.reg[10,1:3] <- LRdiff_res[outcome == "Demand elasticity (1)"]$adj.Rsq
table.reg[10,4:6] <- LRdiff_res[outcome == "Demand elasticity (2)"]$adj.Rsq
table.reg[11,] <- LRdiff_res[outcome == "Demand elasticity (1)"]$N_obs ## Same for ln_cpricei and ln_cpricei2
table.reg[12,] <- LRdiff_res[outcome == "Demand elasticity (1)"]$N_modules ## Same for ln_cpricei and ln_cpricei2
table.reg[13,] <- LRdiff_res[outcome == "Demand elasticity (1)"]$N_stores ## Same for ln_cpricei and ln_cpricei2
table.reg[14,] <- LRdiff_res[outcome == "Demand elasticity (1)"]$N_counties ## Same for ln_cpricei and ln_cpricei2
table.reg[15,] <- LRdiff_res[outcome == "Demand elasticity (1)"]$N_years ## Same for ln_cpricei and ln_cpricei2
table.reg[16,] <- LRdiff_res[outcome == "Demand elasticity (1)"]$N_county_modules ## Same for ln_cpricei and ln_cpricei2

table.label <- c("log(1+t)", "se", "p-value", "1st Stage", "se", "F-state" ,"Year FE", "Module by Year FE", "Mod. by Region by Year FE", "Adj. Rsq" ,"Observations", "Modules", "Stores", "Counties", "Years", "CountyXModules")
table.reg <- matrix(c(table.label, table.reg), nrow = 16)
colnames(table.reg) <- c("", "(1)", "(2)", "(3)", "(4)", "(5)", "(6)")

latex.table <- kable(table.reg, "latex", booktabs = T, linesep = "", caption = "Estimates of Demand Elasticity (yearly data)", escape = F) %>%
  kable_styling(latex_options = "scale_down") %>%
  add_header_above(c(" ", "Definition 1" = 3, "Definition 2" = 3)) %>%
  row_spec(3, hline_after = T) %>%
  row_spec(6, hline_after = T)
write(latex.table, file = "Regressions/Demand_ivreg_yearly_FE.tex")


## Turn off options(scipen)
options(scipen = NULL)


options(scipen = 999) ## To avoid scientific notation
######
## Diff-in-Diff
LRdiff_res <- fread("Data/LRdiff_results_diff_leadlags_1reg.csv")
LRdiff_res <- LRdiff_res[time_controls %in% c("year FE", "Module by year FE", "Module by region by year FE")]
LRdiff_res <- LRdiff_res[lead_lag == "avg. post"]
LRdiff_res$estimate <- round(LRdiff_res$estimate, digits = 3)
LRdiff_res$est_formatted <- paste(LRdiff_res$estimate)
LRdiff_res[pval <= 0.005]$est_formatted <- paste("$", LRdiff_res[pval <= 0.005]$est_formatted, "^{***}", "$", sep = "")
LRdiff_res[pval > 0.005 & pval <= 0.01]$est_formatted <- paste("$", LRdiff_res[pval > 0.005 & pval <= 0.01]$est_formatted, "^{**}", "$", sep = "")
LRdiff_res[pval > 0.01 & pval <= 0.05]$est_formatted <- paste("$", LRdiff_res[pval > 0.01 & pval <= 0.05]$est_formatted, "^{*}", "$", sep = "")
LRdiff_res[pval > 0.05]$est_formatted <- paste("$", LRdiff_res[pval > 0.05]$est_formatted, "$", sep = "")
LRdiff_res$se <- paste("(", format(LRdiff_res$se, digits = 1), ")", sep = "")
LRdiff_res$pval <- round(LRdiff_res$pval, digits = 3)
LRdiff_res$adj.Rsq <- round(LRdiff_res$adj.Rsq, digits = 4)
#LRdiff_res$N_obs <- paste(round(LRdiff_res$N_obs/1000000), ",", round((LRdiff_res$N_obs/1000)-(floor(LRdiff_res$N_obs/1000000)*1000)), ",", LRdiff_res$N_obs -(floor(LRdiff_res$N_obs/1000)*1000), sep = "")
LRdiff_res$N_obs <- format(LRdiff_res$N_obs, digits = 0, big.mark = ",")
LRdiff_res$N_stores <- format(LRdiff_res$N_stores, digits = 0, big.mark = ",")
LRdiff_res$N_counties <- format(LRdiff_res$N_counties, digits = 0, big.mark = ",")
LRdiff_res$N_county_modules <- format(LRdiff_res$N_county_modules, digits = 0, big.mark = ",")

##FE
LRdiff_res_fe <- fread("Data/LRdiff_results.csv")
LRdiff_res_fe <- LRdiff_res_fe[time_controls %in% c("time FE", "module-time FE", "module-region-time FE")]
LRdiff_res_fe$estimate <- round(LRdiff_res_fe$estimate, digits = 3)
LRdiff_res_fe$est_formatted <- paste(LRdiff_res_fe$estimate)
LRdiff_res_fe[pval <= 0.005]$est_formatted <- paste("$", LRdiff_res_fe[pval <= 0.005]$est_formatted, "^{***}", "$", sep = "")
LRdiff_res_fe[pval > 0.005 & pval <= 0.01]$est_formatted <- paste("$", LRdiff_res_fe[pval > 0.005 & pval <= 0.01]$est_formatted, "^{**}", "$", sep = "")
LRdiff_res_fe[pval > 0.01 & pval <= 0.05]$est_formatted <- paste("$", LRdiff_res_fe[pval > 0.01 & pval <= 0.05]$est_formatted, "^{*}", "$", sep = "")
LRdiff_res_fe[pval > 0.05]$est_formatted <- paste("$", LRdiff_res_fe[pval > 0.05]$est_formatted, "$", sep = "")
LRdiff_res_fe$se <- paste("(", format(LRdiff_res_fe$se, digits = 1), ")", sep = "")
LRdiff_res_fe$pval <- round(LRdiff_res_fe$pval, digits = 3)
LRdiff_res_fe$adj.Rsq <- round(LRdiff_res_fe$adj.Rsq, digits = 4)
#LRdiff_res_fe$N_obs <- paste(round(LRdiff_res_fe$N_obs/1000000), ",", round((LRdiff_res_fe$N_obs/1000)-(floor(LRdiff_res_fe$N_obs/1000000)*1000)), ",", LRdiff_res_fe$N_obs -(floor(LRdiff_res_fe$N_obs/1000)*1000), sep = "")
LRdiff_res_fe$N_obs <- format(LRdiff_res_fe$N_obs, digits = 0, big.mark = ",")
LRdiff_res_fe$N_stores <- format(LRdiff_res_fe$N_stores, digits = 0, big.mark = ",")
LRdiff_res_fe$N_counties <- format(LRdiff_res_fe$N_counties, digits = 0, big.mark = ",")
LRdiff_res_fe$N_county_modules <- format(LRdiff_res_fe$N_county_modules, digits = 0, big.mark = ",")


##### First - Passthrough Regressions
### Create a matrix that will be converted to a latex table
table.reg <- matrix(0, nrow = 12, ncol = 6)
table.reg[1,1:3] <- LRdiff_res[outcome == "d_lcpricei"]$estimate
table.reg[1,4:6] <- LRdiff_res_fe[outcome == "ln_cpricei"]$estimate
table.reg[2,1:3] <- LRdiff_res[outcome == "d_lcpricei"]$se
table.reg[2,4:6] <- LRdiff_res_fe[outcome == "ln_cpricei"]$se
#table.reg[3,1:3] <- LRdiff_res[outcome == "d_lcpricei"]$pval
#table.reg[3,4:6] <- LRdiff_res_fe[outcome == "ln_cpricei"]$pval
table.reg[3,] <- c("x", "", "", "x", "", "")
table.reg[4,] <- c("", "x", "", "", "x", "")
table.reg[5,] <- c("", "", "x", "", "", "x")
table.reg[6,1:3] <- LRdiff_res[outcome == "d_lcpricei"]$adj.Rsq
table.reg[6,4:6] <- LRdiff_res_fe[outcome == "ln_cpricei"]$adj.Rsq
table.reg[7,1:3] <- LRdiff_res[outcome == "d_lcpricei"]$N_obs
table.reg[7,4:6] <- LRdiff_res_fe[outcome == "ln_cpricei"]$N_obs
table.reg[8,1:3] <- LRdiff_res[outcome == "d_lcpricei"]$N_modules
table.reg[8,4:6] <- LRdiff_res_fe[outcome == "ln_cpricei"]$N_modules
table.reg[9,1:3] <- LRdiff_res[outcome == "d_lcpricei"]$N_stores
table.reg[9,4:6] <- LRdiff_res_fe[outcome == "ln_cpricei"]$N_stores
table.reg[10,1:3] <- LRdiff_res[outcome == "d_lcpricei"]$N_counties
table.reg[10,4:6] <- LRdiff_res_fe[outcome == "ln_cpricei"]$N_counties
table.reg[11,1:3] <- LRdiff_res[outcome == "d_lcpricei"]$N_years
table.reg[11,4:6] <- LRdiff_res_fe[outcome == "ln_cpricei"]$N_years
table.reg[12,1:3] <- LRdiff_res[outcome == "d_lcpricei"]$N_county_modules
table.reg[12,4:6] <- LRdiff_res_fe[outcome == "ln_cpricei"]$N_county_modules

table.label <- c("log(1+t)", "se", "Year FE", "Module by Year FE", "Mod by Region by Year FE", "Adj. Rsq" ,"Observations", "Modules", "Stores", "Counties", "Years", "CountyXModules")
table.reg <- matrix(c(table.label, table.reg), nrow = 12)
colnames(table.reg) <- c("", "(1)", "(2)", "(3)", "(4)", "(5)", "(6)")

latex.table <- kable(table.reg, "latex", booktabs = T, linesep = "", caption = "Passthrough of Sales Taxes (yearly data)") %>%
  kable_styling(latex_options = "scale_down") %>%
  add_header_above(c(" ", "Diff-in-Diff" = 3, "FE" = 3)) %>%
  row_spec(5, hline_after = T)
write(latex.table, file = "Regressions/Passthrough_reg_yearly_FE_vs_diff.tex")



##### Second - Passthrough Regressions (Definition 2)
### Create a matrix that will be converted to a latex table
table.reg <- matrix(0, nrow = 12, ncol = 6)
table.reg[1,1:3] <- LRdiff_res[outcome == "d_lcpricei2"]$estimate
table.reg[1,4:6] <- LRdiff_res_fe[outcome == "ln_cpricei2"]$estimate
table.reg[2,1:3] <- LRdiff_res[outcome == "d_lcpricei2"]$se
table.reg[2,4:6] <- LRdiff_res_fe[outcome == "ln_cpricei2"]$se
#table.reg[3,1:3] <- LRdiff_res[outcome == "d_lcpricei2"]$pval
#table.reg[3,4:6] <- LRdiff_res_fe[outcome == "ln_cpricei2"]$pval
table.reg[3,] <- c("x", "", "", "x", "", "")
table.reg[4,] <- c("", "x", "", "", "x", "")
table.reg[5,] <- c("", "", "x", "", "", "x")
table.reg[6,1:3] <- LRdiff_res[outcome == "d_lcpricei2"]$adj.Rsq
table.reg[6,4:6] <- LRdiff_res_fe[outcome == "ln_cpricei2"]$adj.Rsq
table.reg[7,1:3] <- LRdiff_res[outcome == "d_lcpricei2"]$N_obs
table.reg[7,4:6] <- LRdiff_res_fe[outcome == "ln_cpricei2"]$N_obs
table.reg[8,1:3] <- LRdiff_res[outcome == "d_lcpricei2"]$N_modules
table.reg[8,4:6] <- LRdiff_res_fe[outcome == "ln_cpricei2"]$N_modules
table.reg[9,1:3] <- LRdiff_res[outcome == "d_lcpricei2"]$N_stores
table.reg[9,4:6] <- LRdiff_res_fe[outcome == "ln_cpricei2"]$N_stores
table.reg[10,1:3] <- LRdiff_res[outcome == "d_lcpricei2"]$N_counties
table.reg[10,4:6] <- LRdiff_res_fe[outcome == "ln_cpricei2"]$N_counties
table.reg[11,1:3] <- LRdiff_res[outcome == "d_lcpricei2"]$N_years
table.reg[11,4:6] <- LRdiff_res_fe[outcome == "ln_cpricei2"]$N_years
table.reg[12,1:3] <- LRdiff_res[outcome == "d_lcpricei2"]$N_county_modules
table.reg[12,4:6] <- LRdiff_res_fe[outcome == "ln_cpricei2"]$N_county_modules

table.label <- c("log(1+t)", "se", "Year FE", "Module by Year FE", "Mod by Region by Year FE", "Adj. Rsq" ,"Observations", "Modules", "Stores", "Counties", "Years", "CountyXModules")
table.reg <- matrix(c(table.label, table.reg), nrow = 12)
colnames(table.reg) <- c("", "(1)", "(2)", "(3)", "(4)", "(5)", "(6)")

latex.table <- kable(table.reg, "latex", booktabs = T, linesep = "", caption = "Passthrough of Sales Taxes (yearly data) (2)") %>%
  kable_styling(latex_options = "scale_down") %>%
  add_header_above(c(" ", "Diff-in-Diff" = 3, "FE" = 3)) %>%
  row_spec(5, hline_after = T)
write(latex.table, file = "Regressions/Passthrough_reg_yearly_FE_vs_diff_2.tex")


##### Quantity Regressions (definition 1)
### Create a matrix that will be converted to a latex table
table.reg <- matrix(0, nrow = 12, ncol = 6)
table.reg[1,1:3] <- LRdiff_res[outcome == "d_lquantity"]$est_formatted
table.reg[1,4:6] <- LRdiff_res_fe[outcome == "ln_quantity"]$est_formatted
table.reg[2,1:3] <- LRdiff_res[outcome == "d_lquantity"]$se
table.reg[2,4:6] <- LRdiff_res_fe[outcome == "ln_quantity"]$se
#table.reg[3,1:3] <- LRdiff_res[outcome == "d_lquantity"]$pval
#table.reg[3,4:6] <- LRdiff_res_fe[outcome == "ln_quantity"]$pval
table.reg[3,] <- c("x", "", "", "x", "", "")
table.reg[4,] <- c("", "x", "", "", "x", "")
table.reg[5,] <- c("", "", "x", "", "", "x")
table.reg[6,1:3] <- LRdiff_res[outcome == "d_lquantity"]$adj.Rsq
table.reg[6,4:6] <- LRdiff_res_fe[outcome == "ln_quantity"]$adj.Rsq
table.reg[7,1:3] <- LRdiff_res[outcome == "d_lquantity"]$N_obs
table.reg[7,4:6] <- LRdiff_res_fe[outcome == "ln_quantity"]$N_obs
table.reg[8,1:3] <- LRdiff_res[outcome == "d_lquantity"]$N_modules
table.reg[8,4:6] <- LRdiff_res_fe[outcome == "ln_quantity"]$N_modules
table.reg[9,1:3] <- LRdiff_res[outcome == "d_lquantity"]$N_stores
table.reg[9,4:6] <- LRdiff_res_fe[outcome == "ln_quantity"]$N_stores
table.reg[10,1:3] <- LRdiff_res[outcome == "d_lquantity"]$N_counties
table.reg[10,4:6] <- LRdiff_res_fe[outcome == "ln_quantity"]$N_counties
table.reg[11,1:3] <- LRdiff_res[outcome == "d_lquantity"]$N_years
table.reg[11,4:6] <- LRdiff_res_fe[outcome == "ln_quantity"]$N_years
table.reg[12,1:3] <- LRdiff_res[outcome == "d_lquantity"]$N_county_modules
table.reg[12,4:6] <- LRdiff_res_fe[outcome == "ln_quantity"]$N_county_modules

table.label <- c("log(1+t)", "se", "Year FE", "Module by Year FE", "Mod by Region by Year FE", "Adj. Rsq" ,"Observations", "Modules", "Stores", "Counties", "Years", "CountyXModules")
table.reg <- matrix(c(table.label, table.reg), nrow = 12)
colnames(table.reg) <- c("", "(1)", "(2)", "(3)", "(4)", "(5)", "(6)")

latex.table <- kable(table.reg, "latex", booktabs = T, linesep = "", caption = "Effect of Sales Taxes on Quantity (yearly data)", escape = F) %>%
  kable_styling(latex_options = "scale_down") %>%
  add_header_above(c(" ", "Diff-in-Diff" = 3, "FE" = 3)) %>%
  row_spec(5, hline_after = T)
write(latex.table, file = "Regressions/Quantity_reg_yearly_FE_vs_diff.tex")



##### Quantity Regressions (Definition 2)
### Create a matrix that will be converted to a latex table
table.reg <- matrix(0, nrow = 12, ncol = 6)
table.reg[1,1:3] <- LRdiff_res[outcome == "d_lquantity2"]$est_formatted
table.reg[1,4:6] <- LRdiff_res_fe[outcome == "ln_quantity2"]$est_formatted
table.reg[2,1:3] <- LRdiff_res[outcome == "d_lquantity2"]$se
table.reg[2,4:6] <- LRdiff_res_fe[outcome == "ln_quantity2"]$se
#table.reg[3,1:3] <- LRdiff_res[outcome == "d_lquantity2"]$pval
#table.reg[3,4:6] <- LRdiff_res_fe[outcome == "ln_quantity2"]$pval
table.reg[3,] <- c("x", "", "", "x", "", "")
table.reg[4,] <- c("", "x", "", "", "x", "")
table.reg[5,] <- c("", "", "x", "", "", "x")
table.reg[6,1:3] <- LRdiff_res[outcome == "d_lquantity2"]$adj.Rsq
table.reg[6,4:6] <- LRdiff_res_fe[outcome == "ln_quantity2"]$adj.Rsq
table.reg[7,1:3] <- LRdiff_res[outcome == "d_lquantity2"]$N_obs
table.reg[7,4:6] <- LRdiff_res_fe[outcome == "ln_quantity2"]$N_obs
table.reg[8,1:3] <- LRdiff_res[outcome == "d_lquantity2"]$N_modules
table.reg[8,4:6] <- LRdiff_res_fe[outcome == "ln_quantity2"]$N_modules
table.reg[9,1:3] <- LRdiff_res[outcome == "d_lquantity2"]$N_stores
table.reg[9,4:6] <- LRdiff_res_fe[outcome == "ln_quantity2"]$N_stores
table.reg[10,1:3] <- LRdiff_res[outcome == "d_lquantity2"]$N_counties
table.reg[10,4:6] <- LRdiff_res_fe[outcome == "ln_quantity2"]$N_counties
table.reg[11,1:3] <- LRdiff_res[outcome == "d_lquantity2"]$N_years
table.reg[11,4:6] <- LRdiff_res_fe[outcome == "ln_quantity2"]$N_years
table.reg[12,1:3] <- LRdiff_res[outcome == "d_lquantity2"]$N_county_modules
table.reg[12,4:6] <- LRdiff_res_fe[outcome == "ln_quantity2"]$N_county_modules

table.label <- c("log(1+t)", "se", "Year FE", "Module by Year FE", "Mod by Region by Year FE", "Adj. Rsq" ,"Observations", "Modules", "Stores", "Counties", "Years", "CountyXModules")
table.reg <- matrix(c(table.label, table.reg), nrow = 12)
colnames(table.reg) <- c("", "(1)", "(2)", "(3)", "(4)", "(5)", "(6)")

latex.table <- kable(table.reg, "latex", booktabs = T, linesep = "", caption = "Effect of Sales Taxes on Quantity (yearly data) (2)", escape = F) %>%
  kable_styling(latex_options = "scale_down") %>%
  add_header_above(c(" ", "Diff-in-Diff" = 3, "FE" = 3)) %>%
  row_spec(5, hline_after = T)
write(latex.table, file = "Regressions/Quantity_reg_yearly_FE_vs_diff_2.tex")


## Turn off options(scipen)
options(scipen = NULL)



### Table with quarterly results
options(scipen = 999) ## To avoid scientific notation
######
LRdiff_res <- fread("Data/quarterly_pi_output.csv")
LRdiff_res <- LRdiff_res[time_controls %in% c("year FE", "module-by-year FE")]
LRdiff_res$estimate <- round(LRdiff_res$estimate, digits = 3)
LRdiff_res$est_formatted <- paste(LRdiff_res$estimate)
LRdiff_res[pval <= 0.005]$est_formatted <- paste("$", LRdiff_res[pval <= 0.005]$est_formatted, "^{***}", "$", sep = "")
LRdiff_res[pval > 0.005 & pval <= 0.01]$est_formatted <- paste("$", LRdiff_res[pval > 0.005 & pval <= 0.01]$est_formatted, "^{**}", "$", sep = "")
LRdiff_res[pval > 0.01 & pval <= 0.05]$est_formatted <- paste("$", LRdiff_res[pval > 0.01 & pval <= 0.05]$est_formatted, "^{*}", "$", sep = "")
LRdiff_res[pval > 0.05]$est_formatted <- paste("$", LRdiff_res[pval > 0.05]$est_formatted, "$", sep = "")
LRdiff_res$se <- paste("(", format(LRdiff_res$se, digits = 1), ")", sep = "")
LRdiff_res$pval <- round(LRdiff_res$pval, digits = 3)
LRdiff_res$adj.Rsq <- round(LRdiff_res$adj.Rsq, digits = 4)




##### First - Passthrough Regressions
### Create a matrix that will be converted to a latex table
table.reg <- matrix(0, nrow = 6, ncol = 4)
table.reg[1,1:2] <- LRdiff_res[outcome == "ln_cpricei"]$est_formatted
table.reg[1,3:4] <- LRdiff_res[outcome == "ln_cpricei2"]$est_formatted
table.reg[2,1:2] <- LRdiff_res[outcome == "ln_cpricei"]$se
table.reg[2,3:4] <- LRdiff_res[outcome == "ln_cpricei2"]$se
table.reg[3,1:2] <- LRdiff_res[outcome == "ln_cpricei"]$pval
table.reg[3,3:4] <- LRdiff_res[outcome == "ln_cpricei2"]$pval
table.reg[4,] <- c("x", "", "x", "")
table.reg[5,] <- c("", "x", "", "x")
table.reg[6,1:2] <- LRdiff_res[outcome == "ln_cpricei"]$adj.Rsq
table.reg[6,3:4] <- LRdiff_res[outcome == "ln_cpricei2"]$adj.Rsq


table.label <- c("log(1+t)", "se", "p-value", "Year FE", "Module by Year FE","Adj. Rsq")
table.reg <- matrix(c(table.label, table.reg), nrow = 6)
colnames(table.reg) <- c("", "(1)", "(2)", "(3)", "(4)")

latex.table <- kable(table.reg, "latex", booktabs = T, linesep = "", caption = "Passthrough of Sales Taxes (Quarterly data)", escape = F) %>%
  #kable_styling(latex_options = "scale_down") %>%
  add_header_above(c(" ", "Laspeyres Index" = 2, "Price Index" = 2)) %>%
  row_spec(3, hline_after = T) %>%
  row_spec(5, hline_after = T)
write(latex.table, file = "Regressions/Passthrough_reg_quarterly_FE.tex")


##### Quantity Regressions
### Create a matrix that will be converted to a latex table
table.reg <- matrix(0, nrow = 6, ncol = 4)
table.reg[1,1:2] <- LRdiff_res[outcome == "ln_quantity"]$est_formatted
table.reg[1,3:4] <- LRdiff_res[outcome == "ln_quantity2"]$est_formatted
table.reg[2,1:2] <- LRdiff_res[outcome == "ln_quantity"]$se
table.reg[2,3:4] <- LRdiff_res[outcome == "ln_quantity2"]$se
table.reg[3,1:2] <- LRdiff_res[outcome == "ln_quantity"]$pval
table.reg[3,3:4] <- LRdiff_res[outcome == "ln_quantity2"]$pval
table.reg[4,] <- c("x", "", "x", "")
table.reg[5,] <- c("", "x", "", "x")
table.reg[6,1:2] <- LRdiff_res[outcome == "ln_quantity"]$adj.Rsq
table.reg[6,3:4] <- LRdiff_res[outcome == "ln_quantity2"]$adj.Rsq


table.label <- c("log(1+t)", "se", "p-value", "Year FE", "Module by Year FE","Adj. Rsq")
table.reg <- matrix(c(table.label, table.reg), nrow = 6)
colnames(table.reg) <- c("", "(1)", "(2)", "(3)", "(4)")

latex.table <- kable(table.reg, "latex", booktabs = T, linesep = "", caption = "Effect of Sales Taxes on Quantity (Quarterly data)", escape = F) %>%
  #kable_styling(latex_options = "scale_down") %>%
  add_header_above(c(" ", "(1)" = 2, "(2)" = 2)) %>%
  row_spec(3, hline_after = T) %>%
  row_spec(5, hline_after = T)
write(latex.table, file = "Regressions/Quantity_reg_quarterly_FE.tex")


## Turn off options(scipen)
options(scipen = NULL)

