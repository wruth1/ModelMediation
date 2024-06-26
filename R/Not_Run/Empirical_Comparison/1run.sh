#!/bin/bash
#SBATCH --time=2:0:0
#SBATCH --ntasks=48
#SBATCH --mem-per-cpu=2000M
# #SBATCH --array=1-9
#SBATCH --array=1
#SBATCH --output="./output/%A_%a.out"
#SBATCH --mail-user=wruth@sfu.ca
#SBATCH --mail-type=END

module load gcc r

export NODESLIST=$(echo $(srun hostname | cut -f 1 -d '.'))
Rscript get_EDF_med_effs.R $SLURM_ARRAY_TASK_ID
