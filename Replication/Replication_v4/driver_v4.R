##### Wesley Janson and Santiago Lacouture
#' Sales Taxes
#' Replication File. Updated on 04/18/2023
#' This Code is the main driver file for replicating the
#' full code. We call individual scripts to perform specific
#' tasks here.
#' Evidently, when running in the server, we need to sbatch 
#' jobs separately for each code



setwd("/project/igaarder")

###Run Code
source("cleaning_code_DL_v4.R") #DONE - Revisiting to reduce disk use -- Prepare dataset for Distributed Lags specification
source("cleaning_code_main_v4.R") #DONE - Revisiting to reduce disk use -- Prepare dataset for long-difference specifications + household sample + econ covariates + spillover datasets

source("summary_data_v4.R") #DONE - Revisiting to reduce disk use
source("cleaning_store_characteristics_v4.R") 
## For now run temp_cleaning_store_characteristics_v4.R

source("reduced_form_evidence_v4.R") #DONE
source("DiD_v4.R") 
source("DiD_nonlinearities_v4.R") 
source("DiD_controls_v4.R") 
source("DiD_spillovers_v4.R") 

source("DiD_IV_v4.R")
source("DiD_IV_controls_v4.R")

# For now use 2010-2014 because of structure of all_pi.csv --> Maybe see what we get with 2008-2014 since there would not be any imputed data?
source("cross_section_retailer_v4.R") 

source("cross_section_consumer_panel_v4.R") 
source("cross_section_consumer_panel-2010_2014_v4.R") 

#source("reduced_form_cohort_twfe_parallel_v2.R") #DONE ## I don't think we need to run this one anymore - we used to estimate cohort specific effects and take a weighted average.  Lack statistical power.

source("point_estimates_bootstrap_v4.R") 

source("point_estimates_robust_storecharact_v4.R")


source("partial_id_matrices_bootstrap_v3.R") 
source("partial_id_solve_bootstrap_v3.R")
source("compute_average_elasticities_v3.R") 
source("imperfect_salience_estimates_v3.R")
source("imperfect_salience_matrices_v3.R") 
source("average_conduct_parameter_salience_v3.R")
source("welfare_mincriteria_v3.R") 
source("welfare_nationwide_v3.R") 
source("welfare_nationwide_bootrel_v3.R") 
source("welfare_state_marginal_v3.R")
source("welfare_state_nonmarginal_v3.R") 
source("simulation_point_v3.R") 
source("simulation_partial_v3.R")
source("Figures_v3.R") 
# Tables are produced in Stata with /Tables.do



