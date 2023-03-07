##### Wesley Janson and Santiago Lacouture
#' Sales Taxes
#' Replication File. Updated on 9/13/2022
#' This Code is the main driver file for replicating the
#' full code. We call individual scripts to perform specific
#' tasks here.
#' Evidently, when running in the server, we need to sbatch 
#' jobs separately for each code



setwd("/project2/igaarder")

###Run Code
source("cleaning_code_v2.R") #DONE - Revisiting to reduce disk use
source("summary_data_v2.R") #DONE - Revisiting to reduce disk use
source("cleaning_store_characteristics_v2.R") #DONE
source("reduced_form_evidence_v2.R") #DONE
source("reduced_form_evidence_controls_v2.R") #DONE
source("reduced_form_evidence_spillovers_v2.R") #DONE
source("cross_section_retailer_v2.R") #DONE
source("cross_section_consumer_panel_v2.R") #DONE
#source("twfe_pre_trends_v2.R") #DONE
source("DiD_v2.R") #DONE
#source("reduced_form_cohort_twfe_parallel_v2.R") #DONE ## I don't think we need to run this one anymore
source("reduced_form_evidence_nonlinearities_v2.R") #DONE 

source("point_estimates_bootstrap_v2.R") #DONE
source("point_estimates_robust_storecharact_v2.R") #DONE
source("partial_id_matrices_bootstrap_v2.R") #DONE
source("partial_id_solve_bootstrap_v2.R") #DONE
source("compute_average_elasticities_v2.R") #DONE 
source("imperfect_salience_estimates_v2.R") #DONE 
source("imperfect_salience_matrices_v2.R") #DONE 
source("average_conduct_parameter_salience_v2.R") #DONE 
source("welfare_mincriteria_v2.R") #DONE 
source("welfare_nationwide_v2.R")  #DONE 
source("welfare_nationwide_bootrel_v2.R")  #DONE
source("welfare_state_marginal_v2.R") #DONE
source("welfare_state_nonmarginal_v2.R") 
source("simulation_point_v2.R") #DONE
source("simulation_partial_v2.R")
source("Figures_v2.R") 
# Tables are produced in Stata with /Tables.do



