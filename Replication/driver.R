##### Wesley Janson
#' Sales Taxes
#' Replication File. Updated on 5/21/2022
#' This Code is the main driver file for replicating the
#' full code. We call individual scripts to perform specific
#' tasks here.



setwd("/project2/igaarder")

###Run Code
source("cleaning_code.R") #DONE
source("reduced_form_evidence.R") #DONE
source("reduced_form_evidence_controls.R")
source("reduced_form_evidence_spillovers.R")
source("cross_section_retailer.R")
source("cross_section_consumer_panel.R")
source("twfe_pre_trends.R")
source("point_estimates_bootstrap.R")




