#' Author: John Bonney
#'
#' Explore covariates in an event-study fashion.

library(data.table)

county_weights <- read.csv("C:/Users/John Bonney/Dropbox/Sales tax/Data/county_population.csv")


#' Hmm. I need to start by getting county x month/quarter covariates compiled,
#' and then merge on treatment groups to counties, then create the same
#' "pseudo-control" groups, and then collapse down. I shouldn't have to do this
#' on the server.

# Load county x time covariates ------------------------------------------------

# Merge on treatment -----------------------------------------------------------

# Collapse by treatment status -------------------------------------------------
