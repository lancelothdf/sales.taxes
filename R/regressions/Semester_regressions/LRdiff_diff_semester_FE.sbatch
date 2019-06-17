#!/bin/bash
#SBATCH --job-name=sem_FE
#SBATCH --output=LRdiff_diff_semester_FE.out
#SBATCH --error=LRdiff_diff_semester_FE.err
#SBATCH --time=36:00:00
#SBATCH --partition=bigmem2
#SBATCH --mem=128000


module load R
R CMD BATCH /project2/igaarder/Code/sales.taxes/R/regressions/Semester_regressions/LRdiff_diff_semester_FE.R




