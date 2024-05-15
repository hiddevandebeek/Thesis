#!/bin/bash
#SBATCH --job-name=simulation_analysis
#SBATCH --array=1-18
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=2
#SBATCH --time=0:10:00
#SBATCH --mem-per-cpu=10G

R --vanilla < "/home/julius_bs/hvandebeek/analysis/R/send_hpc/execute_analysis.R" $SLURM_ARRAY_TASK_ID