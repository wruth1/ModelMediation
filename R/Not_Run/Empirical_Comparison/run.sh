#!/bin/bash
#SBATCH --time=1:0:0
#SBATCH --ntasks=4
#SBATCH --mem-per-cpu=2000M
#SBATCH --array=1-27
#SBATCH --output="./output/%A_%a.out"

module load gcc r

export NODESLIST=$(echo $(srun hostname | cut -f 1 -d '.'))
Rscript One_Setting.R
