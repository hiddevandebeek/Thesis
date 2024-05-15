#!/bin/bash
#SBATCH --job-name=simulation_analysis
#SBATCH --array=1-18
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=6
#SBATCH --time=28:00:00
#SBATCH --mem-per-cpu=10G

R --vanilla < "scripts/hpc/execute_analysis_template.R" $SLURM_ARRAY_TASK_ID