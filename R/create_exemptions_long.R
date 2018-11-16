#' Create module_exemptions file (not a function)
#'

library(data.table)
library(xlsx)
library(tidyr)
library(stringr)

module_exemptions_path <- "C:/Users/John Bonney/Dropbox/Sales tax/Data/List_modules_sample_with_description.xlsx"
module_exemptions <- read.xlsx(module_exemptions_path, sheetIndex = 1)
module_exemptions <- gather(module_exemptions, AL:WY, key = "state", value = "taxable")
module_exemptions <- as.data.table(module_exemptions)[, .(Module, state, taxable)]
setnames(module_exemptions, old = "Module", new = "product_module_code")
module_exemptions[, taxable := as.integer(taxable)]
me_det <- data.table(expand.grid(state = c("AL", "AK", "AZ", "AR", "CA", "CO",
                                           "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS",
                                           "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE",
                                           "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA",
                                           "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI",
                                           "WY"),
                                  year = 2008:2014,
                                 month = 1:12))
me_det <- me_det[order(state, year, month)]
me_det <- merge(me_det, module_exemptions, by = "state", allow.cartesian = T)

fips_codes <- structure(list(state = c("AL", "AK", "AZ", "AR", "CA", "CO",
                                       "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS",
                                       "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE",
                                       "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA",
                                       "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI",
                                       "WY", "AS", "GU", "MP", "PR", "UM", "VI"),
                             fips_state = c(1L, 2L, 4L, 5L, 6L, 8L, 9L, 10L, 11L, 12L, 13L, 15L, 16L, 17L, 18L,
                                            19L, 20L, 21L, 22L, 23L, 24L, 25L, 26L, 27L, 28L, 29L, 30L, 31L,
                                            32L, 33L, 34L, 35L, 36L, 37L, 38L, 39L, 40L, 41L, 42L, 44L, 45L,
                                            46L, 47L, 48L, 49L, 50L, 51L, 53L, 54L, 55L, 56L, 60L, 66L, 69L,
                                            72L, 74L, 78L)), row.names = c(NA, -57L), class = "data.frame")
me_det <- merge(me_det, as.data.table(fips_codes),
                           by = c("state"))

me_det[, tax_status := taxable]

# The below lines are from project2/igaarder/Code/10132018_First_stage.do.
# However, I have converted the "quarter" conditions to "month" conditions.

stata_lines <-
"
replace sales_tax = 0.02 if taxable == 2 & fips_state == 5 & year <= 2010
replace taxable = 1 if taxable == 2 & fips_state == 5 & year <= 2010

replace sales_tax = 0.015 if taxable == 2 & fips_state == 5 & year >= 2011
replace taxable = 1 if taxable == 2 & fips_state == 5 & year >= 2011
replace tax_status = 4 if taxable == 2 & fips_state == 5

replace taxable = 0 if taxable == 2 & fips_state == 8 & (year <= 2009 | (year == 2010 & month < 5))
replace taxable = 1 if taxable == 2 & fips_state == 8 & (year >= 2011 | (year == 2010 & month >= 5))
replace tax_status = 3 if taxable == 2 & fips_state == 8

replace sales_tax = 0.01 if taxable == 2 & fips_state == 17
replace taxable = 1 if taxable == 2 & fips_state == 17
replace tax_status = 2 if taxable == 2 & fips_state == 17

replace sales_tax = 0.01 if taxable == 3 & fips_state == 17 & (year == 2008 | (year == 2009 & month < 9))
replace taxable = 1 if taxable == 3 & fips_state == 17 & (year == 2008 | (year == 2009 & month < 9))
replace taxable = 1 if taxable == 3 & fips_state == 17 & (year >= 2010 | (year == 2009 & month >= 9))
replace tax_status = 4 if taxable == 3 & fips_state == 17

replace taxable = 0 if taxable == 2 & fips_state == 23 & (year <= 2012 | (year == 2013 & month < 10))
replace taxable = 1 if taxable == 2 & fips_state == 23 & (year >= 2014 | (year == 2013 & month >= 10))
replace tax_status = 3 if taxable == 2 & fips_state == 23

replace sales_tax = 0.01225 if taxable == 2 & fips_state == 29
replace taxable = 1 if taxable == 2 & fips_state == 29
replace tax_status = 4 if taxable == 2 & fips_state == 29

replace sales_tax = 0.02 if taxable == 2 & fips_state == 37
replace taxable = 1 if taxable == 2 & fips_state == 37
replace tax_status = 4 if taxable == 2 & fips_state == 37

replace taxable = 0 if taxable == 2 & fips_state == 44 & (year <= 2010 | (year == 2011 & month < 10))
replace taxable = 1 if taxable == 2 & fips_state == 44 & (year >= 2012 | (year == 2011 & month >= 10))
replace tax_status = 3 if taxable == 2 & fips_state == 44

replace sales_tax = 0.0525 if taxable == 2 & fips_state == 47 & (year <= 2012 | (year == 2013 & month < 6))
replace taxable = 1 if taxable == 2 & fips_state == 47 & (year <= 2012 | (year == 2013 & month < 6))

replace sales_tax = 0.05 if taxable == 2 & fips_state == 47 & (year >= 2014 | (year == 2013 & month >= 6))
replace taxable = 1 if taxable == 2 & fips_state == 47 & (year >= 2014 | (year == 2013 & month >= 6))
replace tax_status = 4 if taxable == 2 & fips_state == 47

replace sales_tax = 0.03 if taxable == 2 & fips_state == 49
replace taxable = 1 if taxable == 2 & fips_state == 49
replace tax_status = 4 if taxable == 2 & fips_state == 49

replace sales_tax = 0.015 if taxable == 2 & fips_state == 51
replace taxable = 1 if taxable == 2 & fips_state == 51
replace tax_status = 4 if taxable == 2 & fips_state == 51

replace taxable = 0 if taxable == 2 & fips_state == 53 & (year <= 2009 | (year == 2010 & month < 6) | year >= 2011 | (year == 2010 & month == 12))
replace taxable = 1 if taxable == 2 & fips_state == 53 & (year == 2010 & month >= 6 & month <= 11)
replace tax_status = 3 if taxable == 2 & fips_state == 53

replace sales_tax = 0.04 if taxable == 2 & fips_state == 54 & (year == 2008 & month <= 6)
replace taxable = 1 if taxable == 2 & fips_state == 54 & (year == 2008 & month <= 6)

replace sales_tax = 0.03 if taxable == 2 & fips_state == 54 & ((year == 2008 & month >= 7) | (year >= 2009 & year <= 2011))
replace taxable = 1 if taxable == 2 & fips_state == 54 & ((year == 2008 & month >= 7) | (year >= 2009 & year <= 2011))

replace sales_tax = 0.02 if taxable == 2 & fips_state == 54 & (year == 2012 & month <= 6)
replace taxable = 1 if taxable == 2 & fips_state == 54 & (year == 2012 & month <= 6)

replace sales_tax = 0.01 if taxable == 2 & fips_state == 54 & ((year == 2012 & month >= 7) | (year == 2013 & month <= 6))
replace taxable = 1 if taxable == 2 & fips_state == 54 & ((year == 2012 & month >= 7) | (year == 2013 & month <= 6))

replace taxable = 0 if taxable == 2 & fips_state == 54 & ((year == 2013 & month >= 7) | year >= 2014)
replace tax_status = 4 if taxable == 2 & fips_state == 54
"
stata_lines <- strsplit(stata_lines, "\n")
stata_table <- data.table(lines = unlist(stata_lines))
stata_table <- stata_table[lines != ""]

stata_table[, c("action", "conditions") := tstrsplit(lines, " if ", fixed=TRUE)]
stata_table[, action := gsub("replace ", "", action)]
stata_table[, c("replace_var", "new_val") := tstrsplit(action, " = ", fixed=TRUE)]
stata_table[, new_val := as.numeric(new_val)]
stata_table[, r_command := paste("me_det[", conditions, ",", replace_var, ":=", new_val, "]")]

for (command in stata_table$r_command){
  eval(parse(text = command))
}

me_det <- me_det[!is.na(taxable)]
fwrite(me_det, "C:/Users/John Bonney/Dropbox/Sales tax/Data/modules_exemptions_long.csv")
