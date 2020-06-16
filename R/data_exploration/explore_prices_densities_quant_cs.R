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
all_pi[, module_by_time := .GRP, by = .(product_module_code, semester, year)]
all_pi[, n.ln_cpricei2 := ln_cpricei2 - mean(ln_cpricei2, na.rm = T), by = .(module_by_time)]


# Create lagged value (initial)
all_pi[, L.ln_sales_tax := ln_sales_tax - D.ln_sales_tax]
# need to demean to compare appropiately
all_pi[, L.ln_cpricei2 := ln_cpricei2 - D.ln_cpricei2]
all_pi[, dm.L.ln_cpricei2 := L.ln_cpricei2 - mean(L.ln_cpricei2, na.rm = T), by = module_by_time]
all_pi[, dm.ln_cpricei2 := ln_cpricei2 - mean(L.ln_cpricei2, na.rm = T), by = module_by_time]
all_pi[, w.ln_cpricei2 := ln_cpricei2 - mean(ln_cpricei2), by = .(store_by_module)]
all_pi[, w.L.ln_cpricei2 := L.ln_cpricei2 - mean(L.ln_cpricei2), by = .(store_by_module)]


# Defining common support
control <- all_pi[D.ln_sales_tax == 0,]
treated <- all_pi[D.ln_sales_tax != 0,]



#### Prices -----------------

pct1.control <- quantile(control$dm.L.ln_cpricei2, probs = 0.01, na.rm = T, weight=control$base.sales)
pct1.treated <- quantile(treated$dm.L.ln_cpricei2, probs = 0.01, na.rm = T, weight=treated$base.sales)

pct99.control <- quantile(control$dm.L.ln_cpricei2, probs = 0.99, na.rm = T, weight=control$base.sales)
pct99treated <- quantile(treated$dm.L.ln_cpricei2, probs = 0.99, na.rm = T, weight=treated$base.sales)

all_pi[, cs_price := ifelse(dm.L.ln_cpricei2 > max(pct1.treated, pct1.control) & 
                              dm.L.ln_cpricei2 < min(pct99treated, pct99.control), 1, 0)]
# Make sure missings are 0s
all_pi[, cs_price := ifelse(is.na(dm.L.ln_cpricei2), 0, cs_price)]

## Keep within the common support
all_pi_pcs <- all_pi[cs_price == 1,]

## cut the tails (keep between 1st and 99th percentile)
pct1 <- quantile(all_pi_pcs$dm.ln_cpricei2, probs = 0.01, na.rm = T, weight=base.sales)
pct99 <- quantile(all_pi_pcs$dm.ln_cpricei2, probs = 0.99, na.rm = T, weight=base.sales)
all_pi_pcs <- all_pi_pcs[(dm.ln_cpricei2 > pct1 & dm.ln_cpricei2 < pct99),]


## Price persistence estimates----------

# Raw
simple.fit <- lm(ln_cpricei2~L.ln_cpricei2, data=all_pi_pcs, weights = all_pi_pcs$base.sales)
simple.dt <- data.table(coef(summary(simple.fit)), keep.rownames=T)

sdc <- all_pi_pcs[, sd(ln_cpricei2, na.rm = T)]
sdl <- all_pi_pcs[, sd(L.ln_cpricei2, na.rm = T)]
simple.dt[, SD_cur := sdc]
simple.dt[, SD_lag := sdl]

# De-meaned
simple.fit <- lm(dm.ln_cpricei2~ 0 + dm.L.ln_cpricei2, data=all_pi_pcs, weights = all_pi_pcs$base.sales)
dd.dt <- data.table(coef(summary(simple.fit)), keep.rownames=T)

sdc <- all_pi_pcs[, sd(dm.ln_cpricei2, na.rm = T)]
sdl <- all_pi_pcs[, sd(dm.L.ln_cpricei2, na.rm = T)]
dd.dt[, SD_cur := sdc]
dd.dt[, SD_lag := sdl]

# Full controls
formula1 <- as.formula("w.ln_cpricei2 ~ w.L.ln_cpricei2 | module_by_time | 0 ")
res1 <- felm(formula = formula1, data = all_pi_pcs,
             weights = all_pi_pcs$base.sales)

sdc <- all_pi_pcs[, sd(w.ln_cpricei2, na.rm = T)]
sdl <- all_pi_pcs[, sd(w.L.ln_cpricei2, na.rm = T)]
res1[, SD_cur := sdc]
res1[, SD_lag := sdl]


# Export
estimates <- rbind(simple.dt, dd.dt, res1, fill = T)
fwrite(prices_densities, output.results)

## Treat this by N quantiles
for (n.quantiles in 2:7) {
  
  all_pi_pcs[, quantile := cut(dm.L.ln_cpricei2, breaks = quantile(dm.L.ln_cpricei2, probs = seq(0, 1, by = 1/n.quantiles), 
                                                                   na.rm = T, weight = base.sales),
                               labels = 1:n.quantiles, right = FALSE)]
  all_pi_pcs <- all_pi_pcs[!is.na(quantile)]
  
  quantlab <- as.character(round(quantile(all_pi_pcs$dm.L.ln_cpricei2, 
                                          probs = seq(0, 1, by = 1/n.quantiles), na.rm = T, 
                                          weight = all_pi_pcs$base.sales)[-(n.quantiles+1)], digits = 4))
  
  ## Export densities (n = 1500 steps by quantile)
  output.table <- paste0(output.path, "/Pdensity", n.quantiles, ".csv")
  
  step.log.p <- (max(all_pi_pcs$ln_cpricei2, na.rm = T) - min(all_pi_pcs$ln_cpricei2, na.rm = T) )/1500
  step.n.log.p <- (max(all_pi_pcs$n.ln_cpricei2, na.rm = T) - min(all_pi_pcs$n.ln_cpricei2, na.rm = T)) /1500
  min.log.p <- min(all_pi_pcs$ln_cpricei2, na.rm = T)
  min.n.log.p <- min(all_pi_pcs$n.ln_cpricei2, na.rm = T)
  all_pi_pcs[, d.lp := floor((ln_cpricei2 - min.log.p)/step.log.p)]
  all_pi_pcs[, d.n.lp := floor((n.ln_cpricei2 - min.n.log.p)/step.n.log.p)]
  
  d1 <- all_pi_pcs[, .(dens.log.p = sum(base.sales)), by = .(quantile, d.lp)]
  d1[, dens.log.p := dens.log.p/sum(dens.log.p), by =.(quantile)]
  d1[, log.p := d.lp*step.log.p + min.log.p + step.log.p/2]
  d2 <- all_pi_pcs[, .(dens.n.log.p = sum(base.sales)), by = .(quantile, d.n.lp)]
  d2[, dens.n.log.p := dens.n.log.p/sum(dens.n.log.p), by =.(quantile)]
  d2[, log.n.p := d.n.lp*step.n.log.p + min.n.log.p + step.n.log.p/2]
  
  prices_densities <- merge(d1, d2, by.x = c("d.lp", "quantile"), by.y = c("d.n.lp", "quantile"))
  fwrite(prices_densities, output.table)
  
  ##### Plot the kernel densities --------------------------
  graphout <- paste0(output.path,"/norm_prices_by_quant_csp_q", n.quantiles,".png")
  
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
  
  graphout <- paste0(output.path,"/norm_prices_by_quant_csp_cdf_q", n.quantiles,".png")
  ggplot() +
    geom_line(data=dens, aes(x, cd, colour=group)) +
    theme_classic(base_size = 22) +
    labs(x = "Normalized (log) Price (mod x t)", y = "Cumulative K-Density", 
         title = "CDF by Lagged Price Quantiles")+
    scale_x_continuous(limits = c(-0.3,0.3), breaks = seq(-0.3, 0.3, 0.15), labels = as.character( seq(-0.3, 0.3, 0.15))) +
    scale_color_discrete(name = "Price levels", labels = quantlab)
  
  ggsave(graphout)
  
}





