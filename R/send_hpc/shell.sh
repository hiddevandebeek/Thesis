#!/bin/bash
#
#SBATCH --job=test1
#SBATCH --output="./hello_from_hpc_test1.txt"
#
#SBATCH --ntasks=1
#SBATCH --time=00:10:00
#SBATCH --mem-per-cpu=1G
#SBATCH --chdir="./"
#SBATCH --mail-type=ALL
#SBATCH --mail-user=h.vandebeek-7@umcutrecht.nl
#
#SBATCH --array=0-100

Rscript "./execute.R" $SLURM_ARRAY_TASK_ID