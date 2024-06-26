#!/bin/bash
#SBATCH --time=0:30:0
#SBATCH --ntasks=2
#SBATCH --mem-per-cpu=2000M
#SBATCH --array=1
#SBATCH --output="./output/%A_%a.out"
#SBATCH --mail-user=wruth@sfu.ca
#SBATCH --mail-type=END

module load gcc r

export NODESLIST=$(echo $(srun hostname | cut -f 1 -d '.'))
Rscript Par-One_Setting.R
