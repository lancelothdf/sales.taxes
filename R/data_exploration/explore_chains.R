#' Sales Taxes Project
#' Robustness Check: Price uniformity in US Retail as in Dellavigna and Gentzkow (DG)
#' In this Code we explore how price uniformity looks like in our sample as they do in Figure 1
#' They strategically select 1 (2) chain. We want to do this for all chains
#' Also we explore geographical distribution of stores within chains (as opposed to their plot across states)

library(data.table)
library(futile.logger)
library(sp)
library(raster)
library(rgdal)
library(rgeos)
library(plyr)
library(sf)
library(stringr)
library(ggplot2)

setwd("/project2/igaarder")


## input filepaths -----------------------------------------------
#' This data is the same as all_goods_pi_path, except it has 2015-2016 data as well.
data.semester <- "Data/Nielsen/semester_nielsen_data.csv"
data.stores <- "Data/Nielsen/stores_all.csv"

county.shp.path <- "Data/Spatial_Data_LODES/US county shape file"

## output filepaths -----------------
folder.price <- "/home/slacouture/Stores/Price"
folder.maps <- "/home/slacouture/Stores/Maps"


####### 1. Describe Chains Geographically -------------------------

## Open Stores File
stores.all <- fread("Data/Nielsen/stores_all.csv")
# Create full fips county code
stores.all[, fips_county_full := fips_state_code*1000 + fips_county_code]
# Extract the identified sample to plot chains
stores.dg <- stores.all[DGsample == 1]
# Collapse at the store level
stores.dg <- stores.dg[, .(n_stores = 1), by = .(fips_county_full, chain, store_code_uc, fips_state_code)]
# Collapse at the county level and chain to plot
stores.dg <- stores.dg[, .(n_stores = .N), by = .(fips_county_full, chain, fips_state_code)]
breaks <- c(0, unique(quantile(stores.dg$n_stores, probs = seq(0, 1, by = 1/30), na.rm = T)))
# Identify each chains to plot each chain geographically
chains <- unique(stores.dg$chain)

## open US States shapefile
counties <- readOGR(county.shp.path, "tl_2019_us_county")
counties@data$fips_county_full <- as.integer(as.character(counties@data$GEOID))
counties@data$fips_state <- floor(counties@data$fips_county_full/1000)
# Drop uninteresting states
counties <- counties[counties@data$fips_state != 2 & counties@data$fips_state != 15 & counties@data$fips_state < 57,]
# Extract county skeleton
county.skel <- data.table(counties@data)
county.skel <- county.skel[, c("fips_county_full") ]

## Loop across Chains and Plot
# for (ch in chains) {
#   
#   ## Keep interest chain
#   stores.chain <- stores.dg[chain == ch]
#   ## Merge to skel and replace missing
#   stores.chain <- merge(stores.chain, county.skel, by = "fips_county_full", all = T)
#   stores.chain[, n_stores:= ifelse(is.na(n_stores), 0, n_stores)]
#   
#   ## Merge data to plot
#   counties.plot <- merge(counties, stores.chain, by = "fips_county_full")
#   
#   ## Arrange variable to plot uniformly across chains
#   counties.plot@data$n_stores_cut <- cut(counties.plot@data$n_stores, breaks = c(breaks, Inf), right = F)
# 
#   ## Create Plot
#   plot.name <- paste0(folder.maps, "/map_stores_chain_", ch, ".png")
#   plot <- spplot(counties.plot, "n_stores_cut", main = paste("Stores Distribution Chain ", ch), col = "transparent", 
#                  colorkey = list(height = 1, labels = list(at = seq(0.5, length(breaks) -0.5), labels = breaks)))
#   
#   ## Export
#   png(plot.name)
#   print(plot)
#   dev.off()
# }

## Explore # stores distribution across chains and # counties with presence across chains

## Stores
# Full distribution
chains.dg <- stores.dg[, .(n_stores = sum(n_stores)), by = .(chain)]

graphout <- paste0(folder.maps,"/n_stores_hist_full.png")
hist <- ggplot(data=chains.dg, aes(n_stores)) + 
  geom_histogram(alpha = 0.5, aes(y=..density..)) +    
  theme_classic(base_size = 24) +
  labs(x = "No. stores per chain", y = "Density", color = NULL) +
ggsave(graphout)

# Less than 1500
chains.dg <- chains.dg[n_stores < 1000]
graphout <- paste0(folder.maps,"/n_stores_hist_zoom.png")
hist <- ggplot(data=chains.dg, aes(n_stores)) + 
  geom_histogram(alpha = 0.5, aes(y=..density..)) +    
  theme_classic(base_size = 24) +
  labs(x = "No. stores per chain", y = "Density", color = NULL) +
  ggsave(graphout)

## Counties
# Full Distribution
chains.dg <- stores.dg[, .(n_counties = .N), by = .(chain)]

graphout <- paste0(folder.maps,"/n_counties_hist_full.png")
hist <- ggplot(data=chains.dg, aes(n_counties)) + 
  geom_histogram(alpha = 0.5, aes(y=..density..)) +    
  theme_classic(base_size = 24) +
  labs(x = "No. counties per chain", y = "Density", color = NULL) +
  ggsave(graphout)

# Less than 200
chains.dg <- chains.dg[n_counties < 200]

graphout <- paste0(folder.maps,"/n_counties_hist_zoom.png")
hist <- ggplot(data=chains.dg, aes(n_counties)) + 
  geom_histogram(alpha = 0.5, aes(y=..density..)) +    
  theme_classic(base_size = 24) +
  labs(x = "No. counties per chain", y = "Density", color = NULL) +
  ggsave(graphout)

## Chains by state
chains.dg <- stores.dg[, .(n_stores = sum(n_stores)), by = .(chain, fips_state_code)]
chains.dg <- chains.dg[, .(n_states = .N,
                           n_stores = sum(n_stores)), by = .(chain)]

chains.dg[, state_gr := cut(n_states, c(0,1,2,3,4,5, Inf))]
chains.dg <- chains.dg[, .(n_chains = .N, n_stores = sum(n_stores)), by = .(state_gr)]
fwrite(chains.dg, paste0(folder.maps,"/n_states.csv"))


### 2. Explore Price uniformity in our sample with our measures of prices ------

## Open Final data
all_pi <- fread(data.semester)

## Merge with DG sample
stores.dg <- stores.all[DGsample == 1][, -c("fips_county_full", "fips_state_code", "fips_county_code")]
all_pi <- merge(all_pi, stores.dg, by = c("store_code_uc", "year"))

## Create "average sales tax across time" for these plots
all_pi[, av_sales_tax := mean(exp(ln_sales_tax)), by = c("store_code_uc", "product_module_code") ]

## de-mean log prices by the module mean across stores
all_pi[, price_plot := ln_cpricei2 - mean(ln_cpricei2, na.rm = T), by = product_module_code]

###### Plot Prices as in DG. 
## For now, don't fix missings in store characteristics
## Products to plot: Orange juice (1040), chocolate (1293), cat food (1306) as they do. 
## Use both income (their argument) and taxes (our argument)
products <- c(1040, 1293, 1306)
all_pi[, time := year + (semester-1)*0.5+0.25] #

for (pr in products) {
  for (ch in chains) {
    
    # Restrict data to plot
    chain_product <- all_pi[chain == ch & product_module_code == pr]

    # Restrict data to have nice plots
    plot1.data <- chain_product[!is.na(av_hh_income_sales)]
    plot2.data <- chain_product[!is.na(av_sales_tax)]
    
    # Plot if enough data (more than 3 complete stores)
    if (nrow(plot1.data) > 39) {
      
      # Order stores
      plot1.data <- plot1.data[order(time, av_hh_income_sales),]
      plot1.data[, Y := seq_len(.N), by = .(time)]

      nstores <- unique(plot1.data$store_code_uc)
      max <- max(plot1.data$Y, na.rm = T)
      min.lab <- min(plot1.data$av_sales_tax, na.rm = T)
      max.lab <- max(plot1.data$av_sales_tax, na.rm = T)
      # Plot by income and export
      graphout <- paste0(folder.price,"/", pr,"/price_income_chain_", ch,".png")
      ggplot(data = plot1.data, aes(x  = time, y = Y)) +
        geom_tile(aes(fill = price_plot)) +
        labs(x=NULL, y="Stores, sorted by income", title = paste0("Chain ", ch, ": ", nstores, " Stores"),  fill = NULL) +
        scale_x_continuous(breaks = 2008:2014) +
        scale_y_continuous(breaks = c(1,max), labels = c(min.lab, max.lab))
      ggsave(graphout)
    }
    
    if (nrow(plot2.data) > 39) {
      # Order stores
      plot2.data <- plot2.data[order(time, av_sales_tax),]
      plot2.data[, Y := seq_len(.N), by = .(time)]
      
      nstores <- unique(plot2.data$store_code_uc)
      max <- max(plot2.data$Y, na.rm = T)
      min.lab <- min(plot2.data$av_sales_tax, na.rm = T)
      max.lab <- max(plot2.data$av_sales_tax, na.rm = T)
      # Plot by tax and export
      graphout <- paste0(folder.price,"/", pr,"/price_tax_chain_", ch,".png")
      ggplot(data = plot2.data, aes(x  = time, y = Y)) +
        geom_tile(aes(fill = price_plot)) +
        labs(x=NULL, y="Stores, sorted by average tax rate", title = paste0("chain ", ch, ": ", nstores, " Stores"), fill = NULL) +
        scale_x_continuous(breaks = 2008:2014) +
        scale_y_continuous(breaks = c(1,max), labels = c(min.lab, max.lab))
      ggsave(graphout)
      
    }
  }
}



