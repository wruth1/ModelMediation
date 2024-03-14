#!/bin/bash
#SBATCH --time=4:0:0
#SBATCH --ntasks=2
#SBATCH --mem-per-cpu=2000M
#SBATCH --array=19,25,26,27
#SBATCH --output="./output/%A_%a.out"
#SBATCH --mail-user=wruth@sfu.ca
#SBATCH --mail-type=END

module load gcc r

export NODESLIST=$(echo $(srun hostname | cut -f 1 -d '.'))
Rscript One_Setting.R $SLURM_ARRAY_TASK_ID
