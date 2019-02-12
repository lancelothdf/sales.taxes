#' Test to see if new delivers same as old

setwd("C:/Users/John Bonney/Desktop/Magne_projects/sales_taxes/output/server/pi_data")

library(data.table)

all.ct.old <- fread("pi_all_calendar.csv")
taxable.ct.old <- fread("taxable_pi_collapsed.csv")
all.et.old <- fread("pi_allgoods_es.csv")
taxable.et.old <- fread("pi_taxable_es.csv")
taxexempt.et.old <- fread("pi_taxexempt_es.csv")

all.ct.new <- fread("new/pi_all_calendar.csv")
taxable.ct.new <- fread("new/taxable_pi_collapsed.csv")
all.et.new <- fread("new/pi_allgoods_es.csv")
taxable.et.new <- fread("new/pi_taxable_es.csv")
taxexempt.et.new <- fread("new/pi_taxexempt_es.csv")
