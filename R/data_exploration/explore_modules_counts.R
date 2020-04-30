#' Explore Sample size
#' Final Retailer Data used
#' Explore at module level to understand the patterns
#' 



library(data.table)
library(futile.logger)


setwd("/project2/igaarder/Data")

# useful paths
data.semester <- "Nielsen/semester_nielsen_data.csv"

## Identify taxability as of december 2014
taxability_panel <- fread("taxability_state_panel.csv")
# For now, make reduced rate another category
taxability_panel[, taxability := ifelse(!is.na(reduced_rate), 2, taxability)]
# We will use taxability as of December 2014
taxability_panel <- taxability_panel[(month==12 & year==2014),][, .(product_module_code, fips_state, taxability)]

# Read data (export before common support and estimation)
all_pi <- fread(data.semester)

# Collapse at module x store level
all_pi <- all_pi[, .(sales = sum(sales),
                     N_semesters = .N),
                 by = .(product_module_code, fips_state, store_code_uc)]

# Collapse at module level
all_pi <- all_pi[, .(sales = sum(sales),
                     N_stores = .N,
                     N_stores_semester = sum(N_semesters)),
                 by = .(product_module_code, fips_state)]

# Merge taxability to capture 0s
all_pi <- merge(all_pi, taxability_panel, by = c("product_module_code", "fips_state"), all.y = T)

# Export
fwrite(all_pi, "module_state_observed.csv")

