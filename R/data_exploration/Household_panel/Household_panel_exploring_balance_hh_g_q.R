#' Author: Santiago Lacouture
#'
#' Use the most recent clean Nielsen household panel data (consumer-group-quarter level) 
#' to check how unbalanced the panel is. We create measures of unbalancedness



library(data.table)
library(futile.logger)
library(readstata13)
library(multcomp)
library(psych)
library(ggplot2)

setwd("/project2/igaarder/Data/Nielsen/Household_panel")

# Open data
purchases.sample <- fread("cleaning/consumer_panel_q_hh_group_2006-2016.csv")

# Restrict yo interest years
purchases.sample <- purchases.sample[ year <= 2014 & year >= 2008, ]

# Create the calculated time (quarters from 2008)
purchases.sample <- purchases.sample[, calc_time := (year - 2008)*4 + quarter  ]

# Compute the number of occurences of each household x group and the number of possible occurences
balance.check <- purchases.sample[expenditures != 0 & !is.na(projection_factor), ] # zeroes are not occurences and use estimation sample
balance.check <- balance.check[, list( first = calc_time[1L], last = calc_time[.N], occur = .N),
                                   by = .(household_code, product_group_code)]

balance.check <- balance.check[, pos_occur := last - first]
balance.check <- balance.check[, prop_occur := occur/pos_occur]

## Run some descriptives on this
# Tabular
descriptives <- describe(balance.check)
descriptives  <- data.table(descriptives, keep.rownames=T)
fwrite(descriptives, "../../../../../home/slacouture/HMS/Balance/Basic_Count_hh_g_q.csv")

# Graphic
outcomes <- c("first", "last", "occur", "pos_occur", "prop_occur")

for (Y in outcomes) {
  
  outfile <- paste0("../../../../../home/slacouture/HMS/Balance/histogram_", Y, "_hh_g_q.png")
  ggplot(balance.check, aes(x = get(Y), y = ..density..), na.rm = T) +
    geom_histogram(bins = 24)
  ggsave(outfile)
  
}


## Produce a transition matrix
# Identify first and last occurence and drop evrything outside it
transition.matrix <- merge(purchases.sample, balance.check, by = c("household_code", "product_group_code"), all.x = T)
transition.matrix <- transition.matrix[calc_time >= first & calc_time <= last, 
                                       c("expenditures", "calc_time", "household_code", "product_group_code")]
# Identify purchases as 1 or 0
transition.matrix <- transition.matrix[, t := ifelse(expenditures >0, 1, 0)]
# Identify lead value of purchase
transition.matrix <- transition.matrix[order(household_code, product_group_code, cal_time),] ##Sort on hh by year-quarter (in ascending order)
transition.matrix <- transition.matrix[, t_1 := shift(t, n=1, type="lead")]
# Drop last occurence 
transition.matrix <- transition.matrix[!is.na(t_1), ]
# Count by type of transition
transition.matrix <- transition.matrix[, list( N = .N), by = .(t, t_1) ]
fwrite(transition.matrix, "../../../../../home/slacouture/HMS/Balance/Transition_matrix.csv")

