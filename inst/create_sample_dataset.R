#' The goal of this paper is to create and save a sample dataset I can use to
#' test my code (without using the server).

library(data.table)

nielsen_data <- fread("/project2/igaarder/Data/Nielsen/allyears_module_store_level.csv")

nielsen_data <- nielsen_data[fips_state == 56 & (year == 2008 | year == 2009)]
fwrite(nielsen_data, "/project2/igaarder/Data/Nielsen/sample_test_subset.csv")
