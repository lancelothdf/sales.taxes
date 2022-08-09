##### Wesley Janson and Santiago Lacouture
#' Sales Taxes
#' Replication File. Updated on 7/30/2022
#' This Code is the main driver file for replicating the
#' full code. We call individual scripts to perform specific
#' tasks here.
#' Evidently, when running in the server, we will need to sbatch jobs



setwd("/project2/igaarder")

###Run Code
source("cleaning_code.R") #DONE
source("reduced_form_evidence.R") #DONE
source("reduced_form_evidence_controls.R") #DONE
source("reduced_form_evidence_spillovers.R") #DONE
source("cross_section_retailer.R") #DONE
source("cross_section_consumer_panel.R") #DONE
source("twfe_pre_trends.R") #DONE
source("reduced_form_cohort_twfe_parallel.R") #DONE
source("point_estimates_bootstrap.R")




