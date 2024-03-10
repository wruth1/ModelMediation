#!/bin/bash
#SBATCH --time=0:5:0
#SBATCH --ntasks=1
#SBATCH --mem-per-cpu=500M
#SBATCH --array=1-1
#SBATCH --output="./output/%A_%a.out"

module load gcc r

Rscript test.R
