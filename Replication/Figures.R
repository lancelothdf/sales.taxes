##### Santiago Lacouture
#' Sales Taxes
#' Replication File. Updated on 9/20/2022
#' Step 15 (a): Produce Figures in Draft
#' This code the output of previous codes to produce final Figures included in the draft. 
#' Currently, figures are unnumbered but the order follow naturally 


library(data.table)
library(ggplot2)
library(zoo)
library(tidyverse)
library(stringr)
library(extrafont)
library(RColorBrewer)
library(latex2exp)

setwd("/project2/igaarder/Data/Replication")

# Font: Need to install TTFs of latin modern roman
loadfonts()
fontsize <- 16
##### Figure #: Passthrough and Quantity response to sales taxes - dynamic response. ------

# Open data
data <- fread("LRdiff_semesterly_main.csv")

# Keep only cummulative effects and 
data <- data[str_detect(rn, "cumul") & sample == "all"]

# Capture time of event
data[, tt_event := (1:.N) - 6, by = .(outcome, controls)]

# Keep from -4 to 4
data <- data[abs(tt_event) <= 4]

# Change names to produce plot
setnames(data, old = c("Estimate", "Cluster s.e."), new = c("estimate", "se"))

# Make Se =0 if NA (base time)
data[, se:= ifelse(is.na(se),0,se) ]

### (a) Dynamic response of consumer price to sales taxes

gg <- ggplot(data = data[outcome == "D.ln_cpricei2" & controls == "division_by_module_by_time"], 
             mapping = aes(x = tt_event)) +
  geom_line(aes(y = estimate), size = 0.8, alpha = 0.5) +
  geom_line(aes(y = estimate + 1.96 * se), alpha = 0.5, size = 0.8, linetype = "55") +
  geom_line(aes(y = estimate - 1.96 * se), alpha = 0.5, size = 0.8, linetype = "55") +
  theme_bw(base_size = fontsize) +
  scale_y_continuous(limits = c(-0.5, 1.5), breaks = seq(-0.5, 1.5, 0.25)) +
  scale_x_continuous(breaks = seq(-4, 4, 1)) +
  labs(x = "Semester Relative to Event", y = "Estimate") +
  geom_hline(yintercept = 0, color = "red", linetype = "55") +
  geom_vline(xintercept =  -0.5, color = "black", alpha = .8) +
  theme(legend.position = "none",
        text = element_text(family = "Garamond"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5))
ggsave("figsandtabs/F1_prices.png",
       height = 120, width = 200, units = "mm")



### (b) Dynamic response of quantity to sales taxes

gg <- ggplot(data = data[outcome == "D.ln_quantity" & controls == "division_by_module_by_time"], 
             mapping = aes(x = tt_event)) +
  geom_line(aes(y = estimate), size = 0.8, alpha = 0.5) +
  geom_line(aes(y = estimate + 1.96 * se), alpha = 0.5, size = 0.8, linetype = "55") +
  geom_line(aes(y = estimate - 1.96 * se), alpha = 0.5, size = 0.8, linetype = "55") +
  theme_bw(base_size = fontsize) +
  scale_y_continuous(limits = c(-1.75, 0.5), breaks = seq(-1.75, 0.5, 0.25)) +
  scale_x_continuous(breaks = seq(-4, 4, 1)) +
  labs(x = "Semester Relative to Event", y = "Estimate") +
  geom_hline(yintercept = 0, color = "red", linetype = "55") +
  geom_vline(xintercept =  -0.5, color = "black", alpha = .8) +
  theme(legend.position = "none",
        text = element_text(family = "Garamond"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5))
ggsave("figsandtabs/F1_quantities.png",
       height = 120, width = 200, units = "mm")

rm(data, gg)




##### Figure #: Cross-markets effects of sales taxes. -----

# Open data
data <- fread("LRdiff_semesterly_spillovers.csv")


## A. Comparing time-spans

# Keep only cummulative effects 
data <- data[str_detect(rn, "cumul") & spec == "DL year" & sample == "all"]

# Capture time of event
data[, tt_event := (1:.N) - 4, by = .(outcome, controls, subsample, econ)]

# Keep from -2 to 2
data <- data[abs(tt_event) <= 2]

# Change names to produce plot
setnames(data, old = c("Estimate", "Cluster s.e."), new = c("estimate", "se"))

# Make Se =0 if NA
data[, se:= ifelse(is.na(se),0,se) ]

# Define groups and restrict to them manually
data[controls == "division_by_module_by_time" & econ == 0, group := "Div. FE - no econ contr."]
data[controls == "division_by_module_by_time" & econ == 2, group := "Div. FE - econ contr."]
data[controls == "region_by_module_by_time" & econ == 0, group := "Reg. FE - no econ contr."]
data <- data[!is.na(group) & subsample == "all_taxexempt"]

### (a) Cross-markets effects of sales taxes on prices
# 
# gg <- ggplot(data = data[outcome == "D.ln_cpricei2"], 
#              mapping = aes(x = tt_event, color = factor(group))) +
#   geom_line(aes(y = estimate), size = 0.8, alpha = 0.5) +
#   geom_line(aes(y = estimate + 1.96 * se), alpha = 0.5, size = 0.8, linetype = "55") +
#   geom_line(aes(y = estimate - 1.96 * se), alpha = 0.5, size = 0.8, linetype = "55") +
#   theme_bw(base_size = fontsize) +
#   scale_y_continuous(limits = c(-0.75, 1.75), breaks = seq(-0.75, 1.75, 0.25)) +
#   scale_x_continuous(breaks = seq(-2, 2, 1)) +
#   labs(x = "Year Relative to Event", y = "Estimate", color = NULL) +
#   geom_hline(yintercept = 0, color = "red", linetype = "55") +
#   geom_vline(xintercept =  -0.5, color = "black", alpha = .8) +
#   theme(legend.position = "bottom",
#         text = element_text(family = "Garamond"),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank(),
#         panel.grid.minor.y = element_blank(),
#         panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5))
# ggsave("figsandtabs/SF1_prices.png",
#        height = 120, width = 200, units = "mm")



gg <- ggplot(data = data[outcome == "D.ln_cpricei2" & tt_event >= 0], 
             mapping = aes(x = factor(tt_event, levels = c(0,1,2),
                                      labels = c("On-impact", "1 year", "2 years")), 
                           color = factor(group)
             )) +
  geom_point(aes(y = estimate), position = position_dodge(width=0.4), size = 2) +
  geom_errorbar(aes(ymax = estimate + 1.96 * se, ymin = estimate - 1.96 * se), 
                size = 0.8, position = position_dodge(width=0.4), width = 0.2) +
  theme_bw(base_size = 16) +
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.25)) +
  labs(x = NULL, y = "Estimate", color = NULL) +
  geom_hline(yintercept = 0, color = "red", linetype = "55") +
  theme(legend.position = "bottom",
        text = element_text(family = "Garamond"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5))
ggsave("figsandtabs/SF1_prices.png",
       height = 120, width = 200, units = "mm")

### (b) Dynamic response of quantity to sales taxes

# gg <- ggplot(data = data[outcome == "D.ln_quantity3"], 
#              mapping = aes(x = tt_event, color = factor(group))) +
#   geom_line(aes(y = estimate), size = 0.8, alpha = 0.5) +
#   geom_line(aes(y = estimate + 1.96 * se), alpha = 0.5, size = 0.8, linetype = "55") +
#   geom_line(aes(y = estimate - 1.96 * se), alpha = 0.5, size = 0.8, linetype = "55") +
#   theme_bw(base_size = fontsize) +
#   scale_y_continuous(limits = c(-1.75, 1.5), breaks = seq(-1.75, 1.5, 0.25)) +
#   scale_x_continuous(breaks = seq(-2, 2, 1)) +
#   labs(x = "Year Relative to Event", y = "Estimate", color = NULL) +
#   geom_hline(yintercept = 0, color = "red", linetype = "55") +
#   geom_vline(xintercept =  -0.5, color = "black", alpha = .8) +
#   theme(legend.position = "bottom",
#         text = element_text(family = "Garamond"),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank(),
#         panel.grid.minor.y = element_blank(),
#         panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5))
# ggsave("figsandtabs/SF1_quantities.png",
#        height = 120, width = 200, units = "mm")
# rm(data, gg)

gg <- ggplot(data = data[outcome == "D.ln_quantity3" & tt_event >= 0], 
             mapping = aes(x = factor(tt_event, levels = c(0,1,2),
                                      labels = c("On-impact", "1 year", "2 years")), 
                           color = factor(group)
             )) +
  geom_point(aes(y = estimate), position = position_dodge(width=0.4), size = 2) +
  geom_errorbar(aes(ymax = estimate + 1.96 * se, ymin = estimate - 1.96 * se), 
                size = 0.8, position = position_dodge(width=0.4), width = 0.2) +
  theme_bw(base_size = 16) +
  scale_y_continuous(limits = c(-1.5, 1.75), breaks = seq(-1.5, 1.75, 0.25)) +
  labs(x = NULL, y = "Estimate", color = NULL) +
  geom_hline(yintercept = 0, color = "red", linetype = "55") +
  theme(legend.position = "bottom",
        text = element_text(family = "Garamond"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5))
ggsave("figsandtabs/SF1_quantities.png",
       height = 120, width = 200, units = "mm")
  



##### Figure #: Reduced form evidence of non-linearities in the demand curve. -----

### (a) IV of demand for each of 5 quintiles of the distribution of lagged prices.
# Open data
data <- fread("IV_subsamples_initprice.csv")

# Change names to produce plot
setnames(data, old = c("Estimate", "Cluster s.e."), new = c("estimate", "se"))

# Subset to presented case
data <- data[n.groups == 5]

# Recover corect s.e.s
estimates.boot <- fread("IV_subsamples_initprice_boot.csv")
setnames(estimates.boot, old = c("Estimate", "Std. Error"), new = c("estimate", "se"))
estimates.boot <- estimates.boot[n.groups == 5 ]
estimates.boot <- estimates.boot[, .(mean = mean(estimate), 
                                     se = sd(estimate), 
                                     ll90 = quantile(estimate, probs = 0.1),
                                     ul90 = quantile(estimate, probs = 0.9)), by = .(group, controls)]

data <- merge(data[, -c("se")], estimates.boot, by = c("group", "controls"))

data <- data[, c("group", "estimate", "mean", "se", "ll90", "ul90", "controls")]
data[, ll90.norm := estimate - 1.645*se]
data[, ul90.norm := estimate + 1.645*se]

labels <- paste0("Q", unique(data$group))




# Produce plots
# Only division FEs
gg <- ggplot(data = data[controls == "division_by_module_by_time"], 
             mapping = aes(x = group, y = estimate)) +
  geom_point(size = 2.2, alpha = .8) +
  geom_errorbar(mapping = aes(ymax = ul90.norm,
                              ymin = ll90.norm),
                width = .3) +
  theme_bw(base_size = fontsize) +
  scale_y_continuous(limits = c(-2, 1), breaks = seq(-2, 1, 0.5)) +
  scale_x_continuous(limits = c(0.5, 5.5), breaks = seq(1, 5, 1), labels = labels) +
  labs(x = "Initial Price Level Quantile", y = "IV Estimate") +
  geom_hline(yintercept = 0, color = "red", linetype = "55", alpha = .8) +
  theme(legend.position = "none",
        text = element_text(family = "Garamond"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5))
ggsave("figsandtabs/F2_Subsample_IVs_boot.png",
       height = 120, width = 200, units = "mm")


# both FEs
gg <- ggplot(data = data, 
             mapping = aes(x = group, y = estimate, color =factor(control, 
                                                                  levels = c("division_by_module_by_time", "region_by_module_by_time"),
                                                                  labels = c("Div FE", "Reg FE")))) +
  geom_point(size = 2.2, alpha = .8, position_dodge(width = 0.3)) +
  geom_errorbar(mapping = aes(ymax = ul90.norm,
                              ymin = ll90.norm),
                width = .3, position_dodge(width = 0.3)) +
  theme_bw(base_size = fontsize) +
  scale_y_continuous(limits = c(-2, 1), breaks = seq(-2, 1, 0.5)) +
  scale_x_continuous(limits = c(0.5, 5.5), breaks = seq(1, 5, 1), labels = labels) +
  labs(x = "Initial Price Level Quantile", y = "IV Estimate") +
  geom_hline(yintercept = 0, color = "red", linetype = "55", alpha = .8) +
  theme(legend.position = "bottom",
        text = element_text(family = "Garamond"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5))
ggsave("figsandtabs/F2_Subsample_IVs_boot_FEs.png",
       height = 120, width = 200, units = "mm")

rm(data, estimates.boot, gg)

### (b) Distribution of current prices in each of 5 quintiles of the distribution of lagged prices

# (b) Distribution of current prices in each quintiles of the distribution of lagged prices
data <- fread("Emp_price_subsamples_initprice.csv")

# tag FE as forgot to do so
data[, nr := seq_along(.I), by = c("quantile", "n.groups", "d.lp", "treated", "w")]
data[, controls := ifelse(nr == 1, "region_by_module_by_time", "division_by_module_by_time")]

# identify data and steps
data <- data[n.groups == 5 & controls == "division_by_module_by_time" & is.na(treated)]
step <- (max(data$log.n.p) - min(data$log.n.p))/1500
data[, dens.n.log.p := dens.n.log.p/(100*step)]

# Compute the CDFs
data <- data[order(log.n.p, quantile, w)]
data[, CDF.n.log.p := cumsum(dens.n.log.p), by = .(quantile, w)] 
data[, CDF.n.log.p := CDF.n.log.p/max(CDF.n.log.p, na.rm=T), by = .(quantile, w)] # divided into max due to approximation (floor)

# Version 1: bases.sales weighted
gg <- ggplot(data[w == "base.sales"], 
             aes(x = log.n.p, y = CDF.n.log.p, color = factor(quantile))) + 
  geom_line() +
  theme_bw(base_size = fontsize) +
  scale_x_continuous(limits = c(-0.3, 0.3), breaks = seq(-0.25,0.25,0.125)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0,1,0.25)) +
  scale_color_brewer(palette="Set1") +
  labs(x = "Log(price)", y = "CDF", color = "Quantile",
       caption = "Price normalized to the Module x Semester") +
  theme(legend.position = "bottom",
        text = element_text(family = "Garamond"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5))
ggsave("figsandtabs/F2_Prices_CDF_Subsamples.png",
       height = 140, width = 200, units = "mm")


# Version 1: ``cohort-corrected'' bases.sales weighted
gg <- ggplot(data[w == "base.sales.q"], 
             aes(x = log.n.p, y = CDF.n.log.p, color = factor(quantile))) + 
  geom_line() +
  theme_bw(base_size = fontsize) +
  scale_x_continuous(limits = c(-0.3, 0.3), breaks = seq(-0.25,0.25,0.125)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0,1,0.25)) +
  scale_color_brewer(palette="Set1") +
  labs(x = "Log(price)", y = "CDF", color = "Quantile",
       caption = "Normalized to the Module x Semester") +
  theme(legend.position = "bottom",
        text = element_text(family = "Garamond"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5))
ggsave("figsandtabs/F2_Prices_CDF_Subsamples_corrw.png",
       height = 140, width = 200, units = "mm")

rm(data, gg, step)

##### Figure #: Passthrough estimates in each of 5 quintiles of the distribution of lagged prices. ----
# Open data
data <- fread("Passthrough_nonlinear_sat_initial_price_semester_boot.csv")

# Keep 5 quintiles case, div x module x time FE and 50 iters
data <- data[n.groups == 5 & controls == "group_division_by_module_by_time"]
# Change names to produce plot
setnames(data, old = c("Estimate", "Std. Error"), new = c("estimate", "se"))

labels <- paste0("Q", unique(data[iter==0]$group))
print(labels)

# Capture bootstrapped s.e.s
data.0 <- data[iter == 0]
data.boot <- data[iter != 0]
data.boot <- data.boot[, .(mean = mean(estimate), 
                          se = sd(estimate), 
                               ll90 = quantile(estimate, probs = 0.1),
                               ul90 = quantile(estimate, probs = 0.9)), by = .(group, initprice)]
data <- merge(data.0[, -c("se")], data.boot, by = c("group", "initprice"))

data <- data[, c("initprice", "group", "estimate", "mean", "se", "ll90", "ul90")]
data[, ll90.norm := estimate - 1.645*se]
data[, ul90.norm := estimate + 1.645*se]


# Produce passthrough estimates plot
# Consumer price
ggplot(data = data[initprice == "dm.L.ln_cpricei2"], mapping = aes(x = group, y = estimate)) +
  geom_point(size = 2.2, alpha = .8) +
  geom_errorbar(aes(ymax = estimate + 1.645 * se,
                    ymin = estimate - 1.645 * se),
                width = .3) +
  theme_bw(base_size = fontsize) +
  scale_y_continuous(limits = c(0.55, 1.25), breaks = seq(0.55, 1.25, 0.1)) +
  scale_x_continuous(limits = c(0.5, 5.5), breaks = seq(1, 5, 1), labels = labels) +
  labs(x = "Initial Price Level", y = "Estimate") +
  geom_hline(yintercept = 1, color = "red", linetype = "55", alpha = .8) +
  theme(legend.position = "none",
        #text = element_text(family = "Garamond"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5))
ggsave("figsandtabs/F2_NonLinearpassthrough_main.png",
       height = 120, width = 200, units = "mm")

# Producer price
ggplot(data = data[initprice == "dm.L.ln_pricei2"], mapping = aes(x = group, y = estimate)) +
  geom_point(size = 2.2, alpha = .8) +
  geom_errorbar(aes(ymax = estimate + 1.645 * se,
                    ymin = estimate - 1.645 * se),
                width = .3) +
  theme_bw(base_size = fontsize) +
  scale_y_continuous(limits = c(0.55, 1.25), breaks = seq(0.55, 1.25, 0.1)) +
  scale_x_continuous(limits = c(0.5, 5.5), breaks = seq(1, 5, 1), labels = labels) +
  labs(x = "Initial Price Level", y = "Estimate") +
  geom_hline(yintercept = 1, color = "red", linetype = "55", alpha = .8) +
  theme(legend.position = "none",
        #text = element_text(family = "Garamond"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5))
ggsave("figsandtabs/F2_NonLinearpassthrough_pp.png",
       height = 120, width = 200, units = "mm")



rm(data, data.0, data.boot, labels)

##### Figure #: Estimated demand function  and elasticity under K^d = L^d = 2. -------

## Add options to plots
min.price <- -0.15
max.price <- 0.15

data <- fread("Demand_theta_sat_initial_price_semester_boot_r.csv")

# Keep 2 quintiles case, div x module x time FE 
data <- data[n.groups == 2 & controls == "group_division_by_module_by_time"]

# Keep interest variables to dcast data
data <- data[, c("beta_n", "beta_hat", "iter")]

# Extract boot data
data.0 <- data[iter == 0]
data.boot <- data[iter != 0]
## dcast betas
data.boot <- dcast(data.boot, iter ~ beta_n,  fun=sum, value.var = c("beta_hat"))
setnames(data.boot, old = c("0", "1", "2"), new = paste0("beta_",0:2))

## calculate variances and covariances
data.covs <- data.boot[, .(mean0 = mean(beta_0), mean1 = mean(beta_1), mean2 = mean(beta_2),
                           var0 = (sd(beta_0))^2, var1 = (sd(beta_1))^2, 
                           var2 = (sd(beta_2))^2,
                           cov01 = cov(beta_0, beta_1), cov02 = cov(beta_0, beta_2),
                           cov12 = cov(beta_2, beta_1))]
rm(data.boot)


p <- seq(min.price, max.price, 0.005)

## Estimations across p
beta_hat <- data.0[["beta_hat"]]
rm(data.0, data.boot)

demand <- rep(beta_hat[1], length(p))
elasticity <- rep(0, length(p))
s.e.elas <- rep(0, length(p))
s.e.dmd <- rep(data.covs[["var0"]], length(p))
# Calculate Demand, Elas and s.e.s
for (i in 1:2) {
  
  demand <- demand + beta_hat[i+1]*p^i
  elasticity <- elasticity + i*beta_hat[i+1]*p^(i-1)
  
  s.e.dmd <- s.e.dmd + (p^(2*i))*(data.covs[[paste0("var",i)]]) + 2*(p^i)*(data.covs[[paste0("cov0",i)]])
  s.e.elas <- s.e.elas + (i^2)*(p^(2*(i-1)))*(data.covs[[paste0("var",i)]])
  for (j in i:2) {
    if (i != j) {
      s.e.dmd <- s.e.dmd + 2*(p^(i+j))*(data.covs[[paste0("cov",i,j)]])
      s.e.elas <- s.e.elas + 2*i*j*(p^(i+j-2))*(data.covs[[paste0("cov",i,j)]])
    }
  }
}
s.e.dmd <- (s.e.dmd)^(1/2)
s.e.elas <- (s.e.elas)^(1/2)
# Save
data <- data.table(p, demand, elasticity, s.e.dmd, s.e.elas)
rm(data.covs, s.e.dmd, p, demand, elasticity, s.e.elas, beta_hat)

#### (a) Estimated demand function
data[, ll := demand - 1.96 * s.e.dmd]
data[, ul := demand + 1.96 * s.e.dmd]
data[, ll90 := demand - 1.645 * s.e.dmd]
data[, ul90 := demand + 1.645 * s.e.dmd]

gg <- ggplot(data = data, aes(x = p, y = demand)) +
  geom_line() +
  theme_bw(base_size = fontsize) +
  geom_ribbon(data = data, aes(ymax = ul, ymin = ll), alpha = 0.2) +
  geom_ribbon(data = data, aes(ymax = ul90, ymin = ll90), alpha = 0.4) +
  scale_y_continuous(limits = c(7.5, 8), breaks = seq(7.5, 8, 0.1), labels = scales::number_format(accuracy = 0.1)) +
  scale_x_continuous(limits = c(min.price, max.price), breaks = seq(min.price, max.price, 0.05), labels = scales::number_format(accuracy = 0.01)) +
  labs(x = "Price", y = "Estimated Demand", color = NULL) +
  theme(legend.position = "none",
        text = element_text(family = "Garamond"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5))

ggsave("figsandtabs/F3_Point_Demand_2.png",
       height = 120, width = 200, units = "mm")


# Plot Estimation: Elasticity
data[, ll := elasticity - 1.96 * s.e.elas]
data[, ul := elasticity + 1.96 * s.e.elas]

data[, ll90 := elasticity - 1.645 * s.e.elas]
data[, ul90 := elasticity + 1.645 * s.e.elas]

gg <- ggplot(data = data, aes(x = p, y = elasticity)) +
  geom_line() +
  geom_hline(yintercept = 0, color = "red", linetype = "55", alpha = .8) +
  geom_ribbon(data = data, aes(ymax = ul, ymin = ll), alpha = 0.2) +
  geom_ribbon(data = data, aes(ymax = ul90, ymin = ll90), alpha = 0.4) +
  theme_bw(base_size = fontsize) +
  scale_x_continuous(limits = c(min.price, max.price), breaks = seq(min.price, max.price, 0.05), 
                     labels = scales::number_format(accuracy = 0.01)) +
  scale_y_continuous(limits = c(-2, 2), breaks = seq(-2, 2, 0.5), labels = scales::number_format(accuracy = 0.1)) +
  labs(x = "Price", y = "Estimated Elasticity", color = NULL) +
  theme(legend.position = "none",
        text = element_text(family = "Garamond"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5))
ggsave("figsandtabs/F3_Point_Elasticity_2.png",
       height = 120, width = 200, units = "mm")

rm(data.0, gg, min.price, max.price)


##### Figure #: Estimates of non-linearities in the demand curve are robust to allowing for potential heterogeneity in elasticities along observables. ------


# Open estimated demand file
estimates <- fread("Demand_theta_robust_demog_initial_price_semester.csv")

# Average 
estimates <- estimates[het != "Het.Sample", .(beta_hat = weighted.mean(beta_hat, w = P.het.d)), 
                       by = .(beta_n, n.groups, controls, het, n.het.g)]

# Open the estimates in het sample
full.estimates <- fread("Demand_theta_robust_demog_initial_price_semester.csv")
full.estimates <- full.estimates[het  == "Het.Sample", ][, n.het.g := 1]


# Price sequence to plot
price <- seq(-0.15, 0.15, 0.01)

## Options to generate plots
demographics <- c("av_hh_income_trips", 'per_bachelor_25_trips', 'per_black_trips', 'per65_trips')

#Keep same range as other plots to make comparisons
data <- data[abs(p)<=0.15]

for (dem in demographics) {
  full.estimates[, het := dem]
  outfile <- paste0("figsandtabs/SF3_Het_elas_quad_", dem, ".png")
  
  # Capture data that we will use
  case.data <- rbind(estimates[n.groups == "2" & controls == "group_division_by_module_by_time" & het == dem],
                     full.estimates[n.groups == "2" & controls == "group_division_by_module_by_time"], fill = T) 
  
  # Loop across number of groups of heterogeneity. Estimate elasticities
  data.plot <- data.table(NULL)
  for (m in 1:5) {
    
    betas <- case.data[n.het.g == m][["beta_hat"]]
    elas <- rep(0, length(price))
    for (k in 2:3) {
      elas <- elas + (k-1)*(betas[k])*price^(k-2)
    }
    
    data.plot <- rbind(data.plot, data.table(price, elas, m))
    
  }      
  
  ## Plot
  gg <- ggplot(data = data.plot, mapping = aes(x = price, y = elas, color = factor(m))) +
    geom_line(size = 1.1, alpha = .5) +
    theme_bw(base_size = fontsize) +
    scale_y_continuous(limits = c(-1.3, 0.5), breaks = round(seq(-1.2, 0.4, 0.4),2)) +
    scale_x_continuous(breaks = round(seq(-0.15, 0.15, 0.05),2)) +
    labs(x = "Price", y = "Elasticity", color = "M") +
    scale_color_brewer(palette="Set1") +
    theme(legend.position = "bottom",
          text = element_text(family = "Garamond"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5))
  ggsave(outfile,
         height = 120, width = 200, units = "mm")
  
}




##### Figure #: Estimated bounds on the point elasticity of demand and the demand function. -----


data <- fread("partial_point_results.csv")

#Keep same range as other plots to make comparisons
data <- data[abs(p)<=0.15]

### (a) Bounds on the point elasticity of demand using 1 linear IV (Ld = 1).
gg <- ggplot(data[L==1 & K %in% c(2,3,5,8)], aes(x = p, color = factor(K))) +
  geom_line(aes(y = elas.up)) +
  geom_line(aes(y = elas.down)) +
  theme_bw(base_size = fontsize) +
  scale_x_continuous(limits = c(-0.15, 0.15), breaks = seq(-0.15, 0.15, 0.05), 
                     labels = scales::number_format(accuracy = 0.01)) +
  scale_color_brewer(palette="YlOrRd", direction = -1) +
  labs(x = "Log(price)", y = "Elasticity", color = "K") +
  theme(legend.position = "bottom",
        text = element_text(family = "Garamond"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5))
ggsave("figsandtabs/F4_bounds_elas_L1.png",
       height = 140, width = 200, units = "mm")


### (b) Bounds on the point elasticity of demand using 2 linear IVs (Ld = 2).
gg <- ggplot(data[L==2 & K %in% c(3,4,5,8)], aes(x = p, color = factor(K))) +
  geom_line(aes(y = elas.up)) +
  geom_line(aes(y = elas.down)) +
  theme_bw(base_size = fontsize) +
  scale_x_continuous(limits = c(-0.15, 0.15), breaks = seq(-0.15, 0.15, 0.05), 
                     labels = scales::number_format(accuracy = 0.01)) +
  scale_color_brewer(palette="YlOrRd", direction = -1) +
  labs(x = "Log(price)", y = "Elasticity", color = "K") +
  theme(legend.position = "bottom",
        text = element_text(family = "Garamond"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5))
ggsave("figsandtabs/F4_bounds_elas_L2.png",
       height = 140, width = 200, units = "mm")




### (c) Bounds on demand when using 1 linear IV (Ld = 1).
gg <- ggplot(data[L==1 & K %in% c(2,3,5,8)], aes(x = p, color = factor(K))) +
  geom_line(aes(y = dd.up)) +
  geom_line(aes(y = dd.down)) +
  theme_bw(base_size = fontsize) +
  scale_x_continuous(limits = c(-0.15, 0.15), breaks = seq(-0.15, 0.15, 0.05), 
                     labels = scales::number_format(accuracy = 0.01)) +
  scale_color_brewer(palette="YlOrRd", direction = -1) +
  labs(x = "Log(price)", y = "Elasticity", color = "K") +
  theme(legend.position = "bottom",
        text = element_text(family = "Garamond"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5))
ggsave("figsandtabs/F4_bounds_dd_L1.png",
       height = 140, width = 200, units = "mm")


### (d) Bounds on demand when using 2 linear IVs (Ld = 2).
gg <- ggplot(data[L==2 & K %in% c(3,4,5,8)], aes(x = p, color = factor(K))) +
  geom_line(aes(y = elas.up)) +
  geom_line(aes(y = elas.down)) +
  theme_bw(base_size = 16) +
  scale_x_continuous(limits = c(-0.15, 0.15), breaks = seq(-0.15, 0.15, 0.05), 
                     labels = scales::number_format(accuracy = 0.01)) +
  scale_color_brewer(palette="YlOrRd", direction = -1) +
  labs(x = "Log(price)", y = "Elasticity", color = "K") +
  theme(legend.position = "bottom",
        text = element_text(family = "Garamond"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5))
ggsave("figsandtabs/F4_bounds_dd_L2.png",
       height = 140, width = 200, units = "mm")




rm(data, gg)



##### Figure #: Set of pairs (\varepsilon^s, \theta) that are consistent with the estimated demand function and passthrough. ------

data <- fread("salience_conduct_parameter_at_p.csv")

# Compute mean among negative values
#av.theta <- data[q1 < 0 , .(theta = mean(theta, na.rm = T)), by = .(sigma, es.val, K)]
av.theta <- data[, .(theta = mean(theta, na.rm = T)), by = .(sigma, es.val, K)]
# Capture elasticity of demand under perfect salience
ed <- 0.56704857131384 # Added manually from full sample estimates

# Create the value of the interest parameter (not e^s but e^s/(e^s+e^d)))  

av.theta[, x := es.val/(es.val+ed)]
av.theta[, x := ifelse(is.nan(x),1,x)]


# Plot original paper figure
elas <- -0.56704857131384 ## From IV
avg.tax <- 0.06232442 ## From data

theta.at.inf <- av.theta[sigma==1 & is.infinite(es.val) & K==2, mean(theta)] ## Extracted operatively
theta.at.inf <- ((1 + avg.tax + theta.at.inf)/(1+avg.tax))/(1+(avg.tax/(1+avg.tax))*elas)
theta.at.1 <- av.theta[sigma==1 & es.val == 1 & K==2, mean(theta)] ## Extracted operatively
theta.at.1 <- ((1 + avg.tax + theta.at.1)/(1+avg.tax))/(1+(avg.tax/(1+avg.tax))*elas)

gg <- ggplot(av.theta[sigma==1 & K==2], mapping = aes(x = x, y = theta)) +
  geom_line() +
  geom_point() +
  theme_bw(base_size = fontsize) +
  geom_rect(aes(xmin=0, xmax=1/(1+ed), ymin=0.0666, ymax=Inf), fill = "red", alpha = 0.05) +
  geom_rect(aes(xmin=1/(1+ed), xmax = 1, ymin = 0, ymax = 0.0666), fill = "green", alpha = 0.05) +
  geom_vline(xintercept = 1/(1+ed), linetype = "dashed") + 
  labs(x = TeX('$\\frac{\\epsilon_S}{\\epsilon_S + \\epsilon_D}$'), 
       y = TeX("Av. $\\theta$")) +
  scale_y_continuous(limits = c(-0.02, 0.5), breaks = seq(0, 0.5, 0.1),
                     sec.axis = sec_axis(~ ((1 + avg.tax + .)/(1+avg.tax))/(1+(avg.tax/(1+avg.tax))*elas), 
                                         name = "MVPF", breaks = c(seq(1, 1.5, 0.1),theta.at.inf, theta.at.1), 
                                         labels = c("1", "", "1.2", "", "1.4", "", 
                                                    round(theta.at.inf, digits = 3), 
                                                    round(theta.at.1, digits = 3))
                     )) +
  theme(legend.position = "bottom",
        # text = element_text(family = "Garamond"), # have to comment out for it produces a weird error
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5))
ggsave("figsandtabs/F5_average_theta_sigma1_K2.png",
       height = 120, width = 200, units = "mm")  



### Triplets (\theta, \varepsilon_s, sigma) that are consistent with the data.

## To be modified

# Plot
gg <- ggplot(av.theta[K==2 & sigma %in% c(0.25, 0.3, 0.5, 0.75, 1)], 
             mapping = aes(x = x, y = theta, color = factor(sigma))) +
      geom_line() +
      geom_point() +
      theme_bw(base_size = fontsize) +
      labs(x = TeX('$\\frac{\\epsilon_S}{\\epsilon_S + \\epsilon_D}$'), 
           y = TeX("Av. $\\theta$"), 
           color = TeX("$\\sigma$  ")) +
      scale_color_brewer(palette="RdYlBu", direction = -1) +
      # geom_rect(aes(xmin=0, xmax=1, ymin=1, ymax=Inf), color = "gray", fill = "gray3", alpha = 0.02) +
      # geom_rect(aes(xmin=0, xmax=1/(1+ed), ymin=0.0666, ymax=1), color = "brown3", fill = "brown3", alpha = 0.015) +
      # geom_rect(aes(xmin=1/(1+ed), xmax = 1, ymin = 0, ymax = 0.0666), color = "green", fill = "green", alpha = 0.015) +
      coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
      scale_y_continuous(breaks = seq(0, 1, 0.1)) +
    scale_color_brewer(palette = "Dark2") +
    geom_vline(xintercept = 1/(1+ed), linetype = "dashed") + 
      theme(legend.position = "bottom",
            legend.margin = unit(0, "mm"),
            # text = element_text(family = "Garamond"), # have to comment out for it produces a weird error
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5))
ggsave("figsandtabs/SF5_average_theta_sigmaall_K2.png",
       height = 120, width = 200, units = "mm")

rm(gg, av.theta)


##### Figure #: Extrapolation. Nationwide Average. Perfect competition and salience -------

data.all <- fread("average_nationwide_extrapolation_bootrel.csv")

data.0 <- data.all[iter==0]

data.boot <- data.all[iter!=0]
data.boot <- data.boot[, .(mean = mean(value),
                           se = sd(value, na.rm = T),#),
                           ll90 = quantile(value, probs = 0.1, na.rm = T),
                           ul90 = quantile(value, probs = 0.9, na.rm = T)),
                       by = .(est, K, L, sigma, theta)]


data.all <- merge(data.0, data.boot, by = c("est", "K", "L", "sigma", "theta"))
rm(data.0, data.boot)
data.all <- data.all[, -c("iter", "it.n")]
data.all[, point := as.integer(K == L)] # identify points
data.all[, ll90.norm := value - 1.645*se]
data.all[, ul90.norm := value + 1.645*se]
# create labeler variable
data.all[, labeler:= paste0("L = ",L,", K =", K)]

ggplot(data = NULL, aes(x = labeler, y = value)) +
  geom_line(data = data.all[point==0], size = 8, color = "black") +
  geom_point(data = data.all[point==1 & est == "LB"], size = 3, color = "black") + 
  geom_errorbar(data = data.all[point==0], 
                aes(ymin = ll90.norm, ymax = ul90.norm, color = factor(est)), 
                width = 0.2) + 
  geom_errorbar(data = data.all[point==1 & est == "LB"], 
                aes(ymin = ll90.norm, ymax = ul90.norm, color = factor(est)), 
                width = 0.2, color = "gray") + 
  theme_bw(base_size = fontsize) +
  labs(x =NULL,    y = "Av. MVPF", color = NULL) +
  theme(legend.position = "bottom",
        # text = element_text(family = "Garamond"), # have to comment out for it produces a weird error
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5))
ggsave("figsandtabs/F6_average_mvpf_nationwide_sigma1_theta0_boot.png",
       height = 120, width = 200, units = "mm")  

rm(data.all)

##### Figure #: Extrapolation. Heterogeneity across states -------


# Prepare tax and prices data to use for plots
data.st <- fread("extraction_state_binned_tax.csv")
data.st <- data.st[, .(tau = weighted.mean(tau, eta_m),
                       p_m = weighted.mean(p_m, eta_m)),
                   by = (fips_state)]

# merge in state initials
data.st <- merge(data.st, fread("fips_states.csv"), by = c("fips_state"))



# Order staes
# By price
data.st[, state.ord := factor(p_m,
                              levels = c(data.st$p_m[order(data.st$p_m)]),
                              labels = c(data.st$state[order(data.st$p_m)]), 
                              ordered = T)]

# By tax
data.st[, state.ord.t := factor(tau, 
                                levels = c(data.st$tau[order(data.st$tau)]),
                                labels = c(data.st$state[order(data.st$tau)]), 
                                ordered = T)]




### Marginal changes


# Prepare MVPF results to plot them
data.all <- fread("state_welfare_extrapolation_marginal.csv")

# Drop duplicates that we may have left
data.all <- data.all[!duplicated(data.all[, c('est', 'L', 'K', 'sigma', 'theta', 'state')]),]
# dcast to have min and max
data.all <- dcast(data.all, state + theta + sigma + K + L ~ est, value.var = 'value')


# Merge state attributes
setnames(data.all, "state", "fips_state")
data.all <- merge(data.all, data.st, by = c("fips_state"))



# Order states
# By price
data.all[, state.ord := factor(p_m, 
                               levels = c(data.st$p_m[order(data.st$p_m)]),
                               labels = c(data.st$state[order(data.st$p_m)]), 
                               ordered = T)]
# By tax
data.all[, state.ord.t := factor(tau, 
                                 levels = c(data.st$tau[order(data.st$tau)]),
                                 labels = c(data.st$state[order(data.st$tau)]), 
                                 ordered = T)]
### sigma = 1, theta = 0 case

# Plot, ordering by price 
ggplot(data = NULL, aes(x = state.ord)) +
  geom_linerange(data = data.all[K==8 & sigma==1 & theta == 0],
                 aes(ymin = LB, ymax = UB, color = factor(L)), 
                 position = position_dodge(width = 0.5), size = 1.5) +
  geom_point(data = data.st, aes(y = 1 + tau)) +
  geom_text(data = data.st, aes(y = 1+tau, label = state), vjust=-1, size = 2) +
  theme_bw(base_size = fontsize) +
  scale_y_continuous(
    # Features of the first axis
    name = "MVPF",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~(.-1)*100, name="Average Tax rate (%)")
  ) +
  labs(x = TeX("States (ordered by average $p_{it}^c$)"), color = TeX("$L^d$") ) +
  coord_cartesian(ylim = c(1,1.15)) + 
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5))
ggsave("figsandtabs/F7_mvpf_state_marg_sigma1_theta0_byp.png",
       height = 120, width = 200, units = "mm")  


# Plot, ordering by tax
ggplot(data = NULL, aes(x = state.ord.t)) +
  geom_linerange(data = data.all[K==8 & sigma==1 & theta == 0],
                 aes(ymin = LB, ymax = UB, color = factor(L)), 
                 position = position_dodge(width = 0.5), size = 1.5) +
  geom_point(data = data.st, aes(y = 1 + tau)) +
  geom_text(data = data.st, aes(y = 1+tau, label = state), vjust=-1, size = 2) +
  theme_bw(base_size = fontsize) +
  scale_y_continuous(
    # Features of the first axis
    name = "MVPF",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~(.-1)*100, name="Average Tax rate (%)")
  ) +
  labs(x = TeX("States (ordered by average $\\tau_{it}$)"), color = TeX("$L^d$") ) +
  coord_cartesian(ylim = c(1,1.15)) + 
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5))

ggsave("figsandtabs/F7_mvpf_state_marg_sigma1_theta0_byt.png",
       height = 120, width = 200, units = "mm")  




### sigma = 0.5, theta = 0 case

# Plot, ordering by price 
ggplot(data = NULL, aes(x = state.ord)) +
  geom_linerange(data = data.all[K==8 & sigma==0.5 & theta == 0],
                 aes(ymin = LB, ymax = UB, color = factor(L)), 
                 position = position_dodge(width = 0.5), size = 1.5) +
  geom_point(data = data.st, aes(y = 1 + tau)) +
  geom_text(data = data.st, aes(y = 1+tau, label = state), vjust=-1, size = 2) +
  theme_bw(base_size = fontsize) +
  scale_y_continuous(
    # Features of the first axis
    name = "MVPF",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~(.-1)*100, name="Average Tax rate (%)")
  ) +
  labs(x = TeX("States (ordered by average $p_{it}^c$)"), color = TeX("$L^d$") ) +
  coord_cartesian(ylim = c(1,1.15)) + 
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5))
ggsave("figsandtabs/F7_mvpf_state_marg_sigma05_theta0_byp.png",
       height = 120, width = 200, units = "mm")  


# Plot, ordering by tax
ggplot(data = NULL, aes(x = state.ord.t)) +
  geom_linerange(data = data.all[K==8 & sigma==0.5 & theta == 0],
                 aes(ymin = LB, ymax = UB, color = factor(L)), 
                 position = position_dodge(width = 0.5), size = 1.5) +
  geom_point(data = data.st, aes(y = 1 + tau)) +
  geom_text(data = data.st, aes(y = 1+tau, label = state), vjust=-1, size = 2) +
  theme_bw(base_size = fontsize) +
  scale_y_continuous(
    # Features of the first axis
    name = "MVPF",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~(.-1)*100, name="Average Tax rate (%)")
  ) +
  labs(x = TeX("States (ordered by average $\\tau_{it}$)"), color = TeX("$L^d$") ) +
  coord_cartesian(ylim = c(1,1.15)) + 
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5))

ggsave("figsandtabs/F7_mvpf_state_marg_sigma05_theta0_byt.png",
       height = 120, width = 200, units = "mm")  


### sigma = 1, theta = 0 case

# Plot, ordering by price 
ggplot(data = NULL, aes(x = state.ord)) +
  geom_linerange(data = data.all[K==8 & sigma==1 & theta == 0.067569],
                 aes(ymin = LB, ymax = UB, color = factor(L)), 
                 position = position_dodge(width = 0.5), size = 1.5) +
  geom_point(data = data.st, aes(y = 1 + tau)) +
  geom_text(data = data.st, aes(y = 1+tau, label = state), vjust=-1, size = 2) +
  theme_bw(base_size = fontsize) +
  scale_y_continuous(
    # Features of the first axis
    name = "MVPF",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~(.-1)*100, name="Average Tax rate (%)")
  ) +
  labs(x = TeX("States (ordered by average $p_{it}^c$)"), color = TeX("$L^d$") ) +
  coord_cartesian(ylim = c(1,1.15)) + 
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5))
ggsave("figsandtabs/F7_mvpf_state_marg_sigma1_thetanon0_byp.png",
       height = 120, width = 200, units = "mm")  


# Plot, ordering by tax
ggplot(data = NULL, aes(x = state.ord.t)) +
  geom_linerange(data = data.all[K==8 & sigma==1 & theta == 0.067569],
                 aes(ymin = LB, ymax = UB, color = factor(L)), 
                 position = position_dodge(width = 0.5), size = 1.5) +
  geom_point(data = data.st, aes(y = 1 + tau)) +
  geom_text(data = data.st, aes(y = 1+tau, label = state), vjust=-1, size = 2) +
  theme_bw(base_size = fontsize) +
  scale_y_continuous(
    # Features of the first axis
    name = "MVPF",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~(.-1)*100, name="Average Tax rate (%)")
  ) +
  labs(x = TeX("States (ordered by average $\\tau_{it}$)"), color = TeX("$L^d$") ) +
  coord_cartesian(ylim = c(1,1.15)) + 
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5))

ggsave("figsandtabs/F7_mvpf_state_marg_sigma1_thetanon0_byt.png",
       height = 120, width = 200, units = "mm")  






### Non-Marginal changes


# Prepare MVPF results to plot them
data.all <- fread("state_welfare_extrapolation_nonmarginal.csv")

# Drop duplicates that we may have left
data.all <- data.all[!duplicated(data.all[, c('est', 'L', 'K', 'sigma', 'theta', 'state')]),]
# dcast to have min and max
data.all <- dcast(data.all, state + theta + sigma + K + L + sc ~ est, value.var = 'value')


# Merge state attributes
setnames(data.all, "state", "fips_state")
data.all <- merge(data.all, data.st, by = c("fips_state"))



# Order states
# By price
data.all[, state.ord := factor(p_m, 
                               levels = c(data.st$p_m[order(data.st$p_m)]),
                               labels = c(data.st$state[order(data.st$p_m)]), 
                               ordered = T)]
# By tax
data.all[, state.ord.t := factor(tau, 
                                 levels = c(data.st$tau[order(data.st$tau)]),
                                 labels = c(data.st$state[order(data.st$tau)]), 
                                 ordered = T)]


#### No tax

### sigma = 1, theta = 0 case

# Plot, ordering by price 
ggplot(data = NULL, aes(x = state.ord)) +
  geom_linerange(data = data.all[K==8 & sigma==1 & theta == 0 & sc == "No Tax"],
                 aes(ymin = LB, ymax = UB, color = factor(L)), 
                 position = position_dodge(width = 0.5), size = 1.5) +
  geom_point(data = data.st, aes(y = 0.7*(1.5 + p_m))) +
  geom_text(data = data.st, aes(y = 0.7*(1.5 + p_m), label = state), vjust=-1, size = 2) +
  theme_bw(base_size = fontsize) +
  scale_y_continuous(
    # Features of the first axis
    name = "MVPF",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~(./0.7-1.5), name="Average Price")
  ) +
  labs(x = TeX("States"), color = TeX("$L^d$") ) +
  coord_cartesian(ylim = c(1,1.15)) + 
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5))
ggsave("figsandtabs/F7_mvpf_state_nonmarg0_sigma1_theta0_byp.png",
       height = 120, width = 200, units = "mm")  


# Plot, ordering by tax
ggplot(data = NULL, aes(x = state.ord.t)) +
  geom_linerange(data = data.all[K==8 & sigma==1 & theta == 0  & sc == "No Tax"],
                 aes(ymin = LB, ymax = UB, color = factor(L)), 
                 position = position_dodge(width = 0.5), size = 1.5) +
  geom_point(data = data.st, aes(y = 1 + tau)) +
  geom_text(data = data.st, aes(y = 1+tau, label = state), vjust=-1, size = 2) +
  theme_bw(base_size = fontsize) +
  scale_y_continuous(
    # Features of the first axis
    name = "MVPF",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~(.-1)*100, name="Average Tax rate (%)")
  ) +
  labs(x = TeX("States (ordered by average $\\tau_{it}$)"), color = TeX("$L^d$") ) +
  coord_cartesian(ylim = c(1,1.15)) + 
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5))

ggsave("figsandtabs/F7_mvpf_state_nonmarg0_sigma1_theta0_byt.png",
       height = 120, width = 200, units = "mm")  




### sigma = 0.5, theta = 0 case

# Plot, ordering by price 
ggplot(data = NULL, aes(x = state.ord)) +
  geom_linerange(data = data.all[K==8 & sigma==0.5 & theta == 0  & sc == "No Tax"],
                 aes(ymin = LB, ymax = UB, color = factor(L)), 
                 position = position_dodge(width = 0.5), size = 1.5) +
  geom_point(data = data.st, aes(y = 0.7*(1.5 + p_m))) +
  geom_text(data = data.st, aes(y = 0.7*(1.5 + p_m), label = state), vjust=-1, size = 2) +
  theme_bw(base_size = fontsize) +
  scale_y_continuous(
    # Features of the first axis
    name = "MVPF",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~(./0.7-1.5), name="Average Price")
  ) +
  labs(x = TeX("States"), color = TeX("$L^d$") ) +
  coord_cartesian(ylim = c(1,1.15)) + 
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5))
ggsave("figsandtabs/F7_mvpf_state_nonmarg0_sigma05_theta0_byp.png",
       height = 120, width = 200, units = "mm")  


# Plot, ordering by tax
ggplot(data = NULL, aes(x = state.ord.t)) +
  geom_linerange(data = data.all[K==8 & sigma==0.5 & theta == 0 & sc == "No Tax"],
                 aes(ymin = LB, ymax = UB, color = factor(L)), 
                 position = position_dodge(width = 0.5), size = 1.5) +
  geom_point(data = data.st, aes(y = 1 + tau)) +
  geom_text(data = data.st, aes(y = 1+tau, label = state), vjust=-1, size = 2) +
  theme_bw(base_size = fontsize) +
  scale_y_continuous(
    # Features of the first axis
    name = "MVPF",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~(.-1)*100, name="Average Tax rate (%)")
  ) +
  labs(x = TeX("States (ordered by average $\\tau_{it}$)"), color = TeX("$L^d$") ) +
  coord_cartesian(ylim = c(1,1.15)) + 
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5))
ggsave("figsandtabs/F7_mvpf_state_nonmarg0_sigma05_theta0_byt.png",
       height = 120, width = 200, units = "mm")  


### sigma = 1, theta = 0 case

# Plot, ordering by price 
ggplot(data = NULL, aes(x = state.ord)) +
  geom_linerange(data = data.all[K==8 & sigma==1 & theta == 0.067569 & sc == "No Tax"],
                 aes(ymin = LB, ymax = UB, color = factor(L)), 
                 position = position_dodge(width = 0.5), size = 1.5) +
  geom_point(data = data.st, aes(y = 0.7*(1.5 + p_m))) +
  geom_text(data = data.st, aes(y = 0.7*(1.5 + p_m), label = state), vjust=-1, size = 2) +
  theme_bw(base_size = fontsize) +
  scale_y_continuous(
    # Features of the first axis
    name = "MVPF",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~(./0.7-1.5), name="Average Price")
  ) +
  labs(x = TeX("States"), color = TeX("$L^d$") ) +
  coord_cartesian(ylim = c(1,1.15)) + 
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5))
ggsave("figsandtabs/F7_mvpf_state_nonmarg0_sigma1_thetanon0_byp.png",
       height = 120, width = 200, units = "mm")  


# Plot, ordering by tax
ggplot(data = NULL, aes(x = state.ord.t)) +
  geom_linerange(data = data.all[K==8 & sigma==1 & theta == 0.067569 & sc == "No Tax"],
                 aes(ymin = LB, ymax = UB, color = factor(L)), 
                 position = position_dodge(width = 0.5), size = 1.5) +
  geom_point(data = data.st, aes(y = 1 + tau)) +
  geom_text(data = data.st, aes(y = 1+tau, label = state), vjust=-1, size = 2) +
  theme_bw(base_size = fontsize) +
  scale_y_continuous(
    # Features of the first axis
    name = "MVPF",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~(.-1)*100, name="Average Tax rate (%)")
  ) +
  labs(x = TeX("States (ordered by average $\\tau_{it}$)"), color = TeX("$L^d$") ) +
  coord_cartesian(ylim = c(1,1.15)) + 
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5))
ggsave("figsandtabs/F7_mvpf_state_nonmarg0_sigma1_thetanon0_byt.png",
       height = 120, width = 200, units = "mm")  


#### Plus 5 tax

### sigma = 1, theta = 0 case

# Plot, ordering by price 
ggplot(data = NULL, aes(x = state.ord)) +
  geom_linerange(data = data.all[K==8 & sigma==1 & theta == 0 & sc == "plus 5 Tax"],
                 aes(ymin = LB, ymax = UB, color = factor(L)), 
                 position = position_dodge(width = 0.5), size = 1.5) +
  geom_point(data = data.st, aes(y = 0.7*(1.5 + p_m))) +
  geom_text(data = data.st, aes(y = 0.7*(1.5 + p_m), label = state), vjust=-1, size = 2) +
  theme_bw(base_size = fontsize) +
  scale_y_continuous(
    # Features of the first axis
    name = "MVPF",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~(./0.7-1.5), name="Average Price")
  ) +
  labs(x = TeX("States"), color = TeX("$L^d$") ) +
  coord_cartesian(ylim = c(1,1.15)) + 
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5))
ggsave("figsandtabs/F7_mvpf_state_nonmarg5_sigma1_theta0_byp.png",
       height = 120, width = 200, units = "mm")  


# Plot, ordering by tax
ggplot(data = NULL, aes(x = state.ord.t)) +
  geom_linerange(data = data.all[K==8 & sigma==1 & theta == 0 & sc == "plus 5 Tax"],
                 aes(ymin = LB, ymax = UB, color = factor(L)), 
                 position = position_dodge(width = 0.5), size = 1.5) +
  geom_point(data = data.st, aes(y = 1 + tau)) +
  geom_text(data = data.st, aes(y = 1+tau, label = state), vjust=-1, size = 2) +
  theme_bw(base_size = fontsize) +
  scale_y_continuous(
    # Features of the first axis
    name = "MVPF",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~(.-1)*100, name="Average Tax rate (%)")
  ) +
  labs(x = TeX("States (ordered by average $\\tau_{it}$)"), color = TeX("$L^d$") ) +
  coord_cartesian(ylim = c(1,1.15)) + 
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5))

ggsave("figsandtabs/F7_mvpf_state_nonmarg5_sigma1_theta0_byt.png",
       height = 120, width = 200, units = "mm")  




### sigma = 0.5, theta = 0 case

# Plot, ordering by price 
ggplot(data = NULL, aes(x = state.ord)) +
  geom_linerange(data = data.all[K==8 & sigma==0.5 & theta == 0 & sc == "plus 5 Tax"],
                 aes(ymin = LB, ymax = UB, color = factor(L)), 
                 position = position_dodge(width = 0.5), size = 1.5) +
  geom_point(data = data.st, aes(y = 0.7*(1.5 + p_m))) +
  geom_text(data = data.st, aes(y = 0.7*(1.5 + p_m), label = state), vjust=-1, size = 2) +
  theme_bw(base_size = fontsize) +
  scale_y_continuous(
    # Features of the first axis
    name = "MVPF",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~(./0.7-1.5), name="Average Price")
  ) +
  labs(x = TeX("States"), color = TeX("$L^d$") ) +
  coord_cartesian(ylim = c(1,1.15)) + 
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5))
ggsave("figsandtabs/F7_mvpf_state_nonmarg5_sigma05_theta0_byp.png",
       height = 120, width = 200, units = "mm")  


# Plot, ordering by tax
ggplot(data = NULL, aes(x = state.ord.t)) +
  geom_linerange(data = data.all[K==8 & sigma==0.5 & theta == 0 & sc == "plus 5 Tax"],
                 aes(ymin = LB, ymax = UB, color = factor(L)), 
                 position = position_dodge(width = 0.5), size = 1.5) +
  geom_point(data = data.st, aes(y = 1 + tau)) +
  geom_text(data = data.st, aes(y = 1+tau, label = state), vjust=-1, size = 2) +
  theme_bw(base_size = fontsize) +
  scale_y_continuous(
    # Features of the first axis
    name = "MVPF",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~(.-1)*100, name="Average Tax rate (%)")
  ) +
  labs(x = TeX("States (ordered by average $\\tau_{it}$)"), color = TeX("$L^d$") ) +
  coord_cartesian(ylim = c(1,1.15)) + 
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5))

ggsave("figsandtabs/F7_mvpf_state_nonmarg5_sigma05_theta0_byt.png",
       height = 120, width = 200, units = "mm")  


### sigma = 1, theta = 0 case

# Plot, ordering by tax 
ggplot(data = NULL, aes(x = state.ord.t)) +
  geom_linerange(data = data.all[K==8 & sigma==1 & theta == 0.067569 & sc == "plus 5 Tax"],
                 aes(ymin = LB, ymax = UB, color = factor(L)), 
                 position = position_dodge(width = 0.5), size = 1.5) +
  geom_point(data = data.st, aes(y = 1 + tau)) +
  geom_text(data = data.st, aes(y = 1+tau, label = state), vjust=-1, size = 2) +
  theme_bw(base_size = fontsize) +
  scale_y_continuous(
    # Features of the first axis
    name = "MVPF",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~(.-1)*100, name="Average Tax rate (%)")
  ) +
  labs(x = TeX("States (ordered by average $p_{it}^c$)"), color = TeX("$L^d$") ) +
  coord_cartesian(ylim = c(1,1.15)) + 
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5))
ggsave("figsandtabs/F7_mvpf_state_nonmarg5_sigma1_thetanon0_byt.png",
       height = 120, width = 200, units = "mm")  


# Plot, ordering by price
ggplot(data = NULL, aes(x = state.ord)) +
  geom_linerange(data = data.all[K==8 & sigma==1 & theta == 0.067569 & sc == "plus 5 Tax"],
                 aes(ymin = LB, ymax = UB, color = factor(L)), 
                 position = position_dodge(width = 0.5), size = 1.5) +
  geom_point(data = data.st, aes(y = 0.7*(1.5 + p_m))) +
  geom_text(data = data.st, aes(y = 0.7*(1.5 + p_m), label = state), vjust=-1, size = 2) +
  theme_bw(base_size = fontsize) +
  scale_y_continuous(
    # Features of the first axis
    name = "MVPF",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~(./0.7-1.5), name="Average Price")
  ) +
  labs(x = TeX("States"), color = TeX("$L^d$") ) +
  coord_cartesian(ylim = c(1,1.15)) + 
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5))

ggsave("figsandtabs/F7_mvpf_state_nonmarg5_sigma1_thetanon0_byp.png",
       height = 120, width = 200, units = "mm")  








#################################### Simulation results, to be replicated ################################

quit(save = "no")

##### Figure 13: Simulation results - Parameters of the demand curve. ------

# Parameters we aim to recover
beta <- c(2, -1.5, 0.8, -0.3)

## 1. Target Parameters

res.sim <- fread("SimulationTarget_150.csv")

# Calculate mean and s.e.
res.sim <- res.sim[, .(beta_hat = mean(beta_hat),
                       beta_hat_se = sd(beta_hat),
                       beta_hat.t = mean(beta_hat.t),
                       beta_hat.t_se = sd(beta_hat.t)), by = .(initial, beta_n) ]

# Keep initial Price case
res.sim <- res.sim[initial == "Price"]


# Create data to compare
x <- 0:3
true.vals <- data.table(beta, x)


# Plot Estimated Values

outfile <- paste0("figsandtabs/F10_Simulation_Parameters.eps")

gg <- ggplot(data = res.sim, mapping = aes(x = beta_n, y = beta_hat)) +
  geom_point(size = 2, alpha = .5) +
  geom_errorbar(aes(ymax = beta_hat + 1.96 * beta_hat_se,
                    ymin = beta_hat - 1.96 * beta_hat_se),
                width = .6) +
  theme_bw(base_size = fontsize) +
  scale_y_continuous(limits = c(-20, 10), breaks = seq(-20, 10, 5)) +
  scale_x_continuous(breaks = seq(0, nrow(res.sim), 1), labels = TeX(paste0("$\\mu_", 0:nrow(res.sim), "$"))) +
  labs(x = "Parameter", y = "Estimate", color = NULL) +
  theme(legend.position = "bottom",
        text = element_text(family = "Garamond"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5)) +
  geom_point(data = true.vals , aes(x = x, y = beta), size = 4, alpha = .5, color = "red")
ggsave(outfile,
       height = 120, width = 200, units = "mm", device=cairo_ps)


##### Figure 14: Simulation results - Estimated bounds on demand. ------


# Parameters we aim to recover
beta <- c(2, -1.5, 0.8, -0.3)

## 1. Target Parameters

res.sim <- fread("SimulationTarget_150.csv")

# Calculate mean and s.e.
res.sim <- res.sim[, .(beta_hat = mean(beta_hat),
                       beta_hat_se = sd(beta_hat),
                       beta_hat.t = mean(beta_hat.t),
                       beta_hat.t_se = sd(beta_hat.t)), by = .(initial, beta_n) ]

# Keep initial Price case
res.sim <- res.sim[initial == "Price"]


# Create data to compare
x <- 0:3
true.vals <- data.table(beta, x)

# Retrieve covariances, need to re-open the file
data.covs <- fread("SimulationTarget_150.csv")

# dcast betas
data.covs <- dcast(data.covs, iter + initial ~ beta_n,  fun=sum, value.var = c("beta_hat"))
setnames(data.covs, old = c("0", "1", "2", "3"), new = paste0("beta_",0:3))

# calculate empirical covariances
data.covs <- data.covs[, .(cov01 = cov(beta_0, beta_1),
                           cov02 = cov(beta_0, beta_2),
                           cov03 = cov(beta_0, beta_3),
                           cov12 = cov(beta_2, beta_1),
                           cov13 = cov(beta_3, beta_1),
                           cov23 = cov(beta_2, beta_3)), by = .(initial)]

# Create prices and true elasticity
p <- seq(0.01, 0.11, 0.005)
true.elas <- beta[2] + 2*beta[3]*p + 3*beta[4]*p^2 
true.dmd <- beta[1] + beta[2]*p + beta[3]*p^2 + beta[4]*p^3

# Keep Prices case
data.covs <- data.covs[initial == "Price"]

# Calculate average estimated elasticity
beta_hat <- res.sim[["beta_hat"]] 
est.elas <- beta_hat[2] + 2*beta_hat[3]*p + 3*beta_hat[4]*p^2 

# Calculate S.E.s
beta_hat_se <- (res.sim[["beta_hat_se"]])^(2)
s.e.elas <- rep(0, length(p))
for (i in 1:(length(beta_hat)-1)) {
  # Variance
  s.e.elas <- s.e.elas + (i^2)*(p^(2*(i-1)))*beta_hat_se[i+1]
  # Covariances
  for (j in i:(length(beta_hat)-1)) {
    if (i != j) {
      s.e.elas <- s.e.elas + 2*i*j*(p^(i+j-2))*(data.covs[[paste0("cov",i,j)]])
    }
  }
  
}
s.e.elas <- (s.e.elas)^(1/2)

# create data
data.plot <- data.table(p, true.elas, est.elas, s.e.elas)
# create ul and ll
data.plot[, ll := est.elas - 1.96 * s.e.elas]
data.plot[, ul := est.elas + 1.96 * s.e.elas]


# Output path
outfile <- paste0("figsandtabs/F10_Simulation_Elasticty.eps")

# Plot Estimation
gg <- ggplot(data = data.plot, aes(x = p)) + 
  geom_line(aes(y = true.elas), color = "red") +
  geom_line(aes(y = est.elas)) +
  geom_ribbon(data = data.plot, aes(ymax = ul, ymin = ll), alpha = 0.3) +
  theme_bw(base_size = fontsize) +
  scale_y_continuous(limits = c(-1.65, -1.25), breaks = seq(-1.65, -1.25, 0.1)) +
  scale_x_continuous(limits = c(0.01, 0.11), breaks = seq(0.01, 0.11, 0.02)) +
  labs(x = "Price", y = "Estimated Elasticity", color = NULL) +
  theme(legend.position = "none",
        text = element_text(family = "Garamond"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5))
ggsave(outfile,
       height = 120, width = 200, units = "mm", device=cairo_ps)


#### Demand

# Calculate average estimated elasticity
est.dmd <- beta_hat[1] + beta_hat[2]*p + beta_hat[3]*p^2 + beta_hat[4]*p^3

# Calculate S.E.s
s.e.dmd <- rep(0, length(p))
for (i in 0:(length(beta_hat)-1)) {
  # Variance
  s.e.dmd <- s.e.dmd + (p^(2*i))*beta_hat_se[i+1]
  # Covariances
  for (j in i:(length(beta_hat)-1)) {
    if (i != j) {
      s.e.dmd <- s.e.dmd + 2*(p^(i+j))*(data.covs[[paste0("cov",i,j)]])
    }
  }
}
s.e.dmd <- (s.e.dmd)^(1/2)

# create data
data.plot <- data.table(p, true.dmd, est.dmd, s.e.dmd)
# create ul and ll
data.plot[, ll := est.dmd - 1.96 * s.e.dmd]
data.plot[, ul := est.dmd + 1.96 * s.e.dmd]


# Output path
outfile <- paste0("figsandtabs/F10_Simulation_Demand.eps")


# Plot Estimation
gg <- ggplot(data = data.plot, aes(x = p)) +
  geom_line(aes(y = true.dmd), color = "red") +
  geom_line(aes(y = est.dmd)) +
  geom_ribbon(data = data.plot, aes(ymax = ul, ymin = ll), alpha = 0.3) +
  theme_bw(base_size = 24) +
  scale_y_continuous(limits = c(1.84, 2), breaks = seq(1.84, 2, 0.05)) +
  scale_x_continuous(limits = c(0.01, 0.11), breaks = seq(0.01, 0.11, 0.02)) +
  labs(x = "Price", y = "Estimated Demand", color = NULL) +
  theme(legend.position = "none",
        text = element_text(family = "Garamond"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5))

ggsave(outfile,
       height = 120, width = 200, units = "mm", device=cairo_ps)

##### Figure 15: Recovering bounds around the point elasticity of demand in a Monte-Carlo simulation. -------

### Open Data 
boot.bounds <- fread("Simulation_bounds_table.csv")

bound.simul <- boot.bounds[, .(m.elas.down = mean(elas.down),
                               s.elas.down = sd(elas.down),
                               m.elas.up = mean(elas.up),
                               s.elas.up = sd(elas.up),
                               m.dd.down = mean(dd.down),
                               s.dd.down = sd(dd.down),
                               m.dd.up = mean(dd.up),
                               s.dd.up = sd(dd.up)),
                           by = .(p, D, K)]

## Add real elasticity and demand
# Parameters
beta <- c(2, -1.5, 0.8, -0.3)
# Elasticity
p <- unique(bound.simul$p)
elas.real <- rep(0,length(p))
for (k in 1:length(beta)) {
  
  elas.real <- elas.real + (k-1)*beta[k]*p^(k-2)
  
}
# Demand
dd.real <- rep(0,length(p))
for (k in 1:length(beta)) {
  
  dd.real <- dd.real + beta[k]*p^(k-1)
  
}
# Put together
data.real <- data.table(p, elas.real, dd.real)
bound.simul <- merge(bound.simul, data.real, by = "p")

## Cut extreme values
bound.simul <- bound.simul[ p > 0.01 & p < 0.11]
ks <- c(2,3,7,10)
for (d in 1:2) {
  kss <- ks[which(ks > d)]
  for (k in kss) {
    
    ## Keep data in k case
    plot <- bound.simul[K == k & D == d]
    
    ## Plot elasticity
    gg <- ggplot(plot, aes(x = p)) +
      geom_line(aes(y = m.elas.up)) +
      geom_line(aes(y = m.elas.up + 1.96*s.elas.up), linetype="dashed") +
      geom_line(aes(y = m.elas.up - 1.96*s.elas.up), linetype="dashed") +
      geom_line(aes(y = m.elas.down)) +
      geom_line(aes(y = m.elas.down + 1.96*s.elas.down), linetype="dashed") +
      geom_line(aes(y = m.elas.down - 1.96*s.elas.down), linetype="dashed") +
      geom_line(aes(y = elas.real), color = "red") +
      theme_bw(base_size = fontsize) +
      scale_x_continuous(limits = c(0.01, 0.11), breaks = seq(0.01, 0.11, 0.02), labels = scales::number_format(accuracy = 0.01)) +
      labs(x = "Log(price)", y = "Elasticity") +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5))
    ggsave(paste0("Figures and Tables/Sim_Bounds_elas_L", d,"_K", k,".png"),
           height = 140, width = 200, units = "mm")
    
  }
}


#