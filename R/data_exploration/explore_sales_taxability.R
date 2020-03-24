#' Sales tax
#' Taxability of sales by state
#' USe Nielsen Semester and Taxability panel


library(data.table)
library(futile.logger)
library(lfe)
library(multcomp)

setwd("/project2/igaarder")


## input filepaths -----------------------------------------------
data.semester <- "Data/Nielsen/semester_nielsen_data.csv"
data.taxability <- "Data/taxability_state_panel.csv"

## Output ---------
results <- "Data/sales_taxability_state_panel.csv"


# Semesterly data
all_pi <- fread(data.semester)

## Open Taxability panel
taxability <- fread(data.taxability)

# collapse taxability to the semester
taxability[, semester := ceiling(month/6)]
taxability <- taxability[, .(taxability = mean(taxability),
                             reduced_rate = mean(reduced_rate, na.rm = T)), 
                         by = .(product_module_code, semester, year, fips_state)]
taxability[, taxability := ifelse(!is.nan(reduced_rate), 2, taxability)]



# Merge data sets
all_pi <- merge(all_pi, taxability, by = c("year", "semester", "fips_state", "product_module_code"), all.x = T)

# Collapse Sales
all_pi <- all_pi[, .(sales = sum(exp(ln_quantity + ln_cprice2))), by = .(year, semester, fips_state, taxability)]

fwrite(all_pi, results)
