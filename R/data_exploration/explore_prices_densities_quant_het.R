#' Sales Taxes. Heterogeneity
#' Present plots of CDFs of prices by demographics
#' This to motivate the empirical exercise
#' 

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
data.stores <- "Data/Nielsen/stores_all.csv"


## output filepaths ----------------------------------------------
output.path <- "../../home/slacouture/NLP/PHet"

### Set up Semester Data ---------------------------------
all_pi <- fread(data.semester)
all_pi[, w.ln_sales_tax := ln_sales_tax - mean(ln_sales_tax), by = .(store_by_module)]
all_pi[, w.ln_cpricei2 := ln_cpricei2 - mean(ln_cpricei2), by = .(store_by_module)]
all_pi[, w.ln_quantity3 := ln_quantity3 - mean(ln_quantity3), by = .(store_by_module)]

# Need to demean
all_pi[, module_by_time := .GRP, by = .(product_module_code, semester, year)]
all_pi[, L.ln_cpricei2 := ln_cpricei2 - D.ln_cpricei2]
all_pi[, dm.L.ln_cpricei2 := L.ln_cpricei2 - mean(L.ln_cpricei2, na.rm = T), by = module_by_time]
all_pi[, dm.ln_cpricei2 := ln_cpricei2 - mean(ln_cpricei2, na.rm = T), by = module_by_time]
all_pi[, dm.ln_quantity3 := ln_quantity3 - mean(ln_quantity3, na.rm = T), by = module_by_time]


# Defining common support
control <- all_pi[D.ln_sales_tax == 0,]
treated <- all_pi[D.ln_sales_tax != 0,]

# Price 
pct1.control <- quantile(control$dm.L.ln_cpricei2, probs = 0.01, na.rm = T, weight=control$base.sales)
pct1.treated <- quantile(treated$dm.L.ln_cpricei2, probs = 0.01, na.rm = T, weight=treated$base.sales)

pct99.control <- quantile(control$dm.L.ln_cpricei2, probs = 0.99, na.rm = T, weight=control$base.sales)
pct99treated <- quantile(treated$dm.L.ln_cpricei2, probs = 0.99, na.rm = T, weight=treated$base.sales)

all_pi[, cs_price := ifelse(dm.L.ln_cpricei2 > max(pct1.treated, pct1.control) & 
                              dm.L.ln_cpricei2 < min(pct99treated, pct99.control), 1, 0)]
# Make sure missings are 0s
all_pi[, cs_price := ifelse(is.na(dm.L.ln_cpricei2), 0, cs_price)]

## Keep within the common support
all_pi <- all_pi[cs_price == 1,]

## cut the tails (keep between 1st and 99th percentile)
pct1 <- quantile(all_pi$dm.ln_cpricei2, probs = 0.01, na.rm = T, weight=base.sales)
pct99 <- quantile(all_pi$dm.ln_cpricei2, probs = 0.99, na.rm = T, weight=base.sales)
all_pi <- all_pi[(dm.ln_cpricei2 > pct1 & dm.ln_cpricei2 < pct99),]

## Merge Stores characteristics
stores <- fread(data.stores)
all_pi <- merge(all_pi, stores, by = c("year", "store_code_uc"), all.x = T)

## Keep only the stores with observed characteristics 
all_pi <- all_pi[!is.na(av_hh_income_sales)]

## Loop across dmographics
demographics <- c("av_hh_income_trips", 'per_bachelor_25_trips', 'median_age_trips')

for (dem in demographics) {
  ## Treat this by N quantiles
  for (n.quantiles in 2:5) {
    
    all_pi[, quantile := cut(get(dem), breaks = quantile(get(dem), probs = seq(0, 1, by = 1/n.quantiles), 
                                                    na.rm = T, weight = base.sales),
                             labels = 1:n.quantiles, right = FALSE)]
    all_pi <- all_pi[!is.na(quantile)]
    
    ##### Plot the kernel densities --------------------------
    graphout <- paste0(output.path,"/dens_dm_prices_by_quant_q", n.quantiles,"_", dem,".png")
    
    # Plot - zoom in between -0.7 to 0.7
    ggplot(all_pi, aes(x=dm.ln_cpricei2, color=quantile)) +
      geom_density() +
      labs(x = "Normalized (log) Price (mod x t)", y = "K-Density", title = paste0("Density by ", dem)) +
      scale_x_continuous(limits = c(-0.5,0.5), breaks = seq(-0.5, 0.5, 0.2)) +
      scale_color_discrete(name = "Quintile", labels = 1:n.quantiles)
    
    ggsave(graphout)
    
    #### Plot the CDF of the kernel densities -------------------
    
    # Split the data by group and calculate the smoothed cumulative density for each group
    dens = split(all_pi, all_pi$quantile) %>% 
      map_df(function(d) {
        dens = density(d$dm.ln_cpricei2, from=-1, to=1)
        data.frame(x=dens$x, y=dens$y, cd=cumsum(dens$y)/sum(dens$y), group=d$quantile[1])
      })
    
    graphout <- paste0(output.path,"/cdf_dm_prices_by_quant_q", n.quantiles,"_", dem,".png")
    ggplot() +
      geom_line(data=dens, aes(x, cd, colour=group)) +
      theme_classic(base_size = 22) +
      labs(x = "Normalized (log) Price (mod x t)", y = "Cumulative K-Density", 
           title = paste0("CDF by ", dem))+
      scale_x_continuous(limits = c(-0.3,0.3), breaks = seq(-0.3, 0.3, 0.15), labels = as.character( seq(-0.3, 0.3, 0.15))) +
      scale_color_discrete(name = "Quintile", labels = quantlab)
    
    ggsave(graphout)
    
  }
}



