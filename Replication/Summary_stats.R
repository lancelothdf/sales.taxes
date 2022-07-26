#' Sales Taxes
#' Summary Stats
#' Author: Santiago Lacouture
#' 


library(data.table)
library(futile.logger)
library(lfe)


setwd("/project2/igaarder")
rm(list = ls())

## input filepaths ----------------------------------------------
all_pi <- fread("Data/Replication/all_pi.csv")

## output filepaths ----------------------------------------------
output.results.file <- "Data/Replication/Summary_data.csv"


## Variables to examine
outcomes <- c("D.ln_cpricei", "D.ln_cpricei2", 
              "D.ln_quantity", "D.ln_quantity2", "D.ln_quantity3", 
              "D.ln_sales_share", "D.ln_sales_tax")

## Key subsets
all_pi[, treated := ifelse(D.ln_sales_tax == 0, 0, 1)]

### Produce summary stats
stats <- data.table(NULL)
## Stats for whole data set
means <- all_pi[, lapply(.SD, mean, na.rm = T), .SDcols = outcomes]
sd <- all_pi[, lapply(.SD, sd, na.rm = T), .SDcols = outcomes]
min <- all_pi[, lapply(.SD, min, na.rm = T), .SDcols = outcomes]
max <- all_pi[, lapply(.SD, max, na.rm = T), .SDcols = outcomes]

means[, `:=` (stat = "mean", sample = "full")]
sd[, `:=` (stat = "sd", sample = "full")]
min[, `:=` (stat = "min", sample = "full")]
max[, `:=` (stat = "mean", sample = "full")]

stats <- rbind(stats, 
               means, sd, min, max)

## Stats for whole data set by treatment
means <- all_pi[, lapply(.SD, mean, na.rm = T), by = .(treated) , .SDcols = outcomes]
sd <- all_pi[, lapply(.SD, sd, na.rm = T), by = .(treated), .SDcols = outcomes]
min <- all_pi[, lapply(.SD, min, na.rm = T), by = .(treated), .SDcols = outcomes]
max <- all_pi[, lapply(.SD, max, na.rm = T), by = .(treated), .SDcols = outcomes]

means[, `:=` (stat = "mean", sample = "full")]
sd[, `:=` (stat = "sd", sample = "full")]
min[, `:=` (stat = "min", sample = "full")]
max[, `:=` (stat = "mean", sample = "full")]

stats <- rbind(stats, 
               means, sd, min, max, fill=T)

## Stats for by semester
means <- all_pi[, lapply(.SD, mean, na.rm = T), by = .(year, semester) , .SDcols = outcomes]
sd <- all_pi[, lapply(.SD, sd, na.rm = T), by = .(year, semester), .SDcols = outcomes]
min <- all_pi[, lapply(.SD, min, na.rm = T), by = .(year, semester), .SDcols = outcomes]
max <- all_pi[, lapply(.SD, max, na.rm = T), by = .(year, semester), .SDcols = outcomes]

means[, `:=` (stat = "mean", sample = "semester")]
sd[, `:=` (stat = "sd", sample = "semester")]
min[, `:=` (stat = "min", sample = "semester")]
max[, `:=` (stat = "mean", sample = "semester")]

stats <- rbind(stats, 
               means, sd, min, max, fill=T)

## Stats for by semester and treatment
means <- all_pi[, lapply(.SD, mean, na.rm = T), by = .(year, semester, treated) , .SDcols = outcomes]
sd <- all_pi[, lapply(.SD, sd, na.rm = T), by = .(year, semester, treated), .SDcols = outcomes]
min <- all_pi[, lapply(.SD, min, na.rm = T), by = .(year, semester, treated), .SDcols = outcomes]
max <- all_pi[, lapply(.SD, max, na.rm = T), by = .(year, semester, treated), .SDcols = outcomes]

means[, `:=` (stat = "mean", sample = "semester")]
sd[, `:=` (stat = "sd", sample = "semester")]
min[, `:=` (stat = "min", sample = "semester")]
max[, `:=` (stat = "mean", sample = "semester")]

stats <- rbind(stats, 
               means, sd, min, max, fill=T)

### Save results
fwrite(stats, output.results.file)