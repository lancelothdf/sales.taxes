#!/bin/bash
#SBATCH --job-name=Main_semester
#SBATCH --output=Main_semesterly_regressions_COUNTY.out
#SBATCH --error=Main_semesterly_regressions_COUNTY.err
#SBATCH --time=24:00:00
#SBATCH --partition=bigmem2
#SBATCH --mem=256000

module load R
R CMD BATCH /project2/igaarder/Code/sales.taxes/R/regressions/Semester_regressions/Main_semesterly_regressions_COUNTY.R
