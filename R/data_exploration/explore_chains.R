#' Sales Taxes Project
#' Robustness Check: Price uniformity in US Retail as in Dellavigna and Gentzkow
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

setwd("/project2/igaarder")


## input filepaths -----------------------------------------------
#' This data is the same as all_goods_pi_path, except it has 2015-2016 data as well.
data.semester <- "Data/Nielsen/semester_nielsen_data.csv"
data.stores <- "Data/Nielsen/stores_all.csv"

county.shp.path <- "Data/Spatial_Data_LODES/US county shape file"

## output filepaths -----------------
folder.plots <- "/home/slacouture/Stores/Price"
folder.maps <- "/home/slacouture/Stores/Price"


####### 1. Describe Chains Geographically -------------------------

## Open Stores File
stores.all <- fread("Data/Nielsen/stores_all.csv")
# Create full fips county code
stores.all[, fips_county_full := fips_state_code*1000 + fips_county_code]
# Extract the identified sample to plot chains
stores.dg <- stores.all[DGsample == 1]
# Collapse at the county level and chain to plot
stores.dg <- stores.dg[, .(n_stores = .N), by = .(fips_county_full, chain)]
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
for (ch in chains) {
  
  ## Keep interest chain
  stores.chain <- stores.dg[chain == ch]
  ## Merge to skel and replace missing
  stores.chain <- merge(stores.chain, county.skel, by = "fips_county_full", all = T)
  stores.chain[, n_stores:= ifelse(is.na(n_stores), 0, n_stores)]
  ## Merge data to plot
  counties.plot <- merge(counties, stores.chain, by = "fips_county_full")

  ## Create Plot
  plot.name <- paste0(folder.plots, "/map_stores_chain_", ch)
  plot <- spplot(counties.plot, "n_stores", main = paste("Stores Distribution Chain ", ch))
  
  ## Export
  png(plot.name)
  print(plot)
  dev.off()
}






