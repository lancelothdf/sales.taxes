#' Compute de-meaned price distributions at different quantiles of the lagged taxes and prices (for which you observe positive tax changes)
#' Everything in the common supports. First I export a bit of basic info about 
library(tidyverse)
library(data.table)
library(futile.logger)
library(lfe)
library(multcomp)
library(ggplot2)



setwd("/project2/igaarder")


## input filepaths -----------------------------------------------
#' This data is the same as all_goods_pi_path, except it has 2015-2016 data as well.
data.semester <- "Data/Nielsen/semester_nielsen_data.csv"
data.year <- "Data/Nielsen/yearly_nielsen_data.csv"


## output filepaths ----------------------------------------------
output.path <- "../../home/slacouture/NLP"
output.results <- "../../home/slacouture/NLP/price_persistence.csv"

### Set up Semester Data ---------------------------------
all_pi <- fread(data.semester)

# Create de-meaned prices
all_pi[, n.ln_cpricei2 := ln_cpricei2 - mean(ln_cpricei2, na.rm = T), by = .(module_by_time)]


# Create lagged value (initial)
all_pi[, L.ln_sales_tax := ln_sales_tax - D.ln_sales_tax]
# need to demean to compare appropiately
all_pi[, module_by_time := .GRP, by = .(product_module_code, year)]
all_pi[, L.ln_cpricei2 := ln_cpricei2 - D.ln_cpricei2]
all_pi[, dm.L.ln_cpricei2 := L.ln_cpricei2 - mean(L.ln_cpricei2, na.rm = T), by = module_by_time]



#### Simple persistance and variances estimations --------------

## Model 1:
formula1 <- as.formula("ln_cpricei2 ~ L.ln_cpricei2")
res1 <- lm(formula = formula1, data = all_pi,
             weights = all_pi$base.sales)
res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
res1.dt[, sigma.hat := (sd(resid(res1)))^2]
res1.dt[, controls := "no"]

## Model 2:
formula1 <- as.formula("ln_cpricei2 ~ L.ln_cpricei2 | division_by_module_by_time | 0 | module_by_state")
res2 <- felm(formula = formula1, data = all_pi,
             weights = all_pi$base.sales)
res2.dt <- data.table(coef(summary(res2)), keep.rownames=T)
res2.dt[, sigma.hat := (sd(resid(res2)))^2]
res2.dt[, controls := "yes"]

res.dt <- rbind(res1.dt, res2.dt)
fwrite(res.dt, output.results)
#### Tax rate -----------------

# Defining common support
control <- all_pi[D.ln_sales_tax == 0,]
treated <- all_pi[D.ln_sales_tax != 0,]

# Tax rate
pct1.control <- quantile(control$L.ln_sales_tax, probs = 0.01, na.rm = T, weight=all_pi$base.sales)
pct1.treated <- quantile(treated$L.ln_sales_tax, probs = 0.01, na.rm = T, weight=all_pi$base.sales)

pct99.control <- quantile(control$L.ln_sales_tax, probs = 0.99, na.rm = T, weight=all_pi$base.sales)
pct99treated <- quantile(treated$L.ln_sales_tax, probs = 0.99, na.rm = T, weight=all_pi$base.sales)

all_pi[, cs_tax := ifelse(L.ln_sales_tax > max(pct1.treated, pct1.control) & 
                            L.ln_sales_tax < min(pct99treated, pct99.control), 1, 0)]
# Make sure missings are 0s
all_pi[, cs_tax := ifelse(is.na(L.ln_sales_tax), 0, cs_tax)]

## Keep within the common support
all_pi_taxcs <- all_pi[cs_tax == 1,]
all_pi_taxcs[, quantile := cut(L.ln_sales_tax, breaks = quantile(ln_sales_tax_r, probs = seq(0, 1, by = 1/5), 
                                                                 na.rm = T, weight = base.sales),
                               labels = 1:5, right = FALSE)]
all_pi_taxcs <- all_pi_taxcs[!is.na(quantile)]

quantlab <- as.character(round(quantile(all_pi_taxcs$L.ln_sales_tax, 
                                  probs = seq(0, 1, by = 1/5), na.rm = T, 
                                  weight = all_pi_taxcs$base.sales)[-(6)], digits = 4))

##### Plot the kernel densities --------------------------
graphout <- paste0(output.path,"/norm_prices_by_quant_cstax.png")

# Plot - zoom in between -0.7 to 0.7
ggplot(all_pi_taxcs, aes(x=n.ln_cpricei2, color=quantile)) +
  geom_density() +
  labs(x = "Normalized (log) Price (mod x t)", y = "K-Density", title = "Density by Sales Taxes") +
  scale_x_continuous(limits = c(-0.5,0.5), breaks = seq(-0.5, 0.5, 0.2)) +
  scale_color_discrete(name = "Tax levels", labels = quantlab)

ggsave(graphout)

#### Plot the CDF of the kernel densities -------------------

# Split the data by group and calculate the smoothed cumulative density for each group
dens = split(all_pi_taxcs, all_pi_taxcs$quantile) %>% 
  map_df(function(d) {
    dens = density(d$n.ln_cpricei2, from=-1, to=1)
    data.frame(x=dens$x, y=dens$y, cd=cumsum(dens$y)/sum(dens$y), group=d$quantile[1])
  })

graphout <- paste0(output.path,"/norm_prices_by_quant_cstax_cdf.png")
ggplot() +
  geom_line(data=dens, aes(x, cd, colour=group)) +
  theme_classic(base_size = 22) +
  labs(x = "Normalized (log) Price (mod x t)", y = "Cumulative K-Density", 
       title = "CDF by Sales Taxes Quantiles")+
  scale_x_continuous(limits = c(-0.3,0.3), breaks = seq(-0.3, 0.3, 0.15), labels = as.character( seq(-0.3, 0.3, 0.15))) +
  scale_color_discrete(name = "Tax levels", labels = quantlab)

ggsave(graphout)








#### Prices -----------------

pct1.control <- quantile(control$dm.L.ln_cpricei2, probs = 0.01, na.rm = T, weight=all_pi$base.sales)
pct1.treated <- quantile(treated$dm.L.ln_cpricei2, probs = 0.01, na.rm = T, weight=all_pi$base.sales)

pct99.control <- quantile(control$dm.L.ln_cpricei2, probs = 0.99, na.rm = T, weight=all_pi$base.sales)
pct99treated <- quantile(treated$dm.L.ln_cpricei2, probs = 0.99, na.rm = T, weight=all_pi$base.sales)

all_pi[, cs_price := ifelse(dm.L.ln_cpricei2 > max(pct1.treated, pct1.control) & 
                              dm.L.ln_cpricei2 < min(pct99treated, pct99.control), 1, 0)]
# Make sure missings are 0s
all_pi[, cs_price := ifelse(is.na(dm.L.ln_cpricei2), 0, cs_price)]

## Keep within the common support
all_pi_pcs <- all_pi[cs_price == 1,]
all_pi_pcs[, quantile := cut(dm.L.ln_cpricei2, breaks = quantile(ln_sales_tax_r, probs = seq(0, 1, by = 1/5), 
                                                                 na.rm = T, weight = base.sales),
                               labels = 1:5, right = FALSE)]
all_pi_pcs <- all_pi_pcs[!is.na(quantile)]

quantlab <- as.character(round(quantile(all_pi_pcs$dm.L.ln_cpricei2, 
                                        probs = seq(0, 1, by = 1/5), na.rm = T, 
                                        weight = all_pi_pcs$base.sales)[-(6)], digits = 4))

##### Plot the kernel densities --------------------------
graphout <- paste0(output.path,"/norm_prices_by_quant_csp.png")

# Plot - zoom in between -0.7 to 0.7
ggplot(all_pi_pcs, aes(x=n.ln_cpricei2, color=quantile)) +
  geom_density() +
  labs(x = "Normalized (log) Price (mod x t)", y = "K-Density", title = "Density by Lagged Price") +
  scale_x_continuous(limits = c(-0.5,0.5), breaks = seq(-0.5, 0.5, 0.2)) +
  scale_color_discrete(name = "Price levels", labels = quantlab)

ggsave(graphout)

#### Plot the CDF of the kernel densities -------------------

# Split the data by group and calculate the smoothed cumulative density for each group
dens = split(all_pi_pcs, all_pi_pcs$quantile) %>% 
  map_df(function(d) {
    dens = density(d$n.ln_cpricei2, from=-1, to=1)
    data.frame(x=dens$x, y=dens$y, cd=cumsum(dens$y)/sum(dens$y), group=d$quantile[1])
  })

graphout <- paste0(output.path,"/norm_prices_by_quant_csp_cdf.png")
ggplot() +
  geom_line(data=dens, aes(x, cd, colour=group)) +
  theme_classic(base_size = 22) +
  labs(x = "Normalized (log) Price (mod x t)", y = "Cumulative K-Density", 
       title = "CDF by Lagged Price Quantiles")+
  scale_x_continuous(limits = c(-0.3,0.3), breaks = seq(-0.3, 0.3, 0.15), labels = as.character( seq(-0.3, 0.3, 0.15))) +
  scale_color_discrete(name = "Price levels", labels = quantlab)

ggsave(graphout)




