#!/bin/bash
#SBATCH --job-name=LR_border
#SBATCH --output=LR_diff_diff_regression_robustness_border_counties.out
#SBATCH --error=LR_diff_diff_regression_robustness_border_counties.err
#SBATCH --time=36:00:00
#SBATCH --partition=bigmem2
#SBATCH --mem=256000


module load R
R CMD BATCH /project2/igaarder/Code/sales.taxes/R/regressions/Yearly_regressions/LR_diff_diff_regression_robustness_border_counties.R




