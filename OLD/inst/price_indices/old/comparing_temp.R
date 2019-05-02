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

setorder(all.ct.old, tr_group, year, quarter)
setorder(all.ct.new, tr_group, year, quarter)
all.equal(all.ct.old$mean.cpricei, all.ct.new$mean.cpricei)

# slightly different...
setorder(taxable.ct.old, tr_group, year, quarter)
setorder(taxable.ct.new, tr_group, year, quarter)
all.equal(taxable.ct.old$mean.cpricei, taxable.ct.new$mean.cpricei)

setorder(all.et.old, tr_group, tt_event)
setorder(all.et.new, tr_group, tt_event)
all.equal(all.et.old$mean_pi, all.et.new$mean_pi)

setorder(taxable.et.old, tr_group, tt_event)
setorder(taxable.et.new, tr_group, tt_event)
all.equal(taxable.et.old$mean_pi, taxable.et.new$mean_pi)

setorder(taxexempt.et.old, tr_group, tt_event)
setorder(taxexempt.et.new, tr_group, tt_event)
all.equal(taxexempt.et.old$mean_pi, taxexempt.et.new$mean_pi)
