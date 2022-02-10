#!/bin/bash
#SBATCH --job-name=ModuleXtime_cohort
#SBATCH --output=ModuleXtime_cohort.out
#SBATCH --error=ModuleXtime_cohort.err
#SBATCH --time=36:00:00
#SBATCH --partition=bigmem2
#SBATCH --mem=256000

module load R
R CMD BATCH /project2/igaarder/Code/sales.taxes/R/regressions/Semester_regressions/ModuleXtime_cohort_main_regression.R
