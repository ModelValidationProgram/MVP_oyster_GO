#!/bin/bash

#SBATCH --job-name=TrainGF082525
#SBATCH --mail-user=rumberger.c@northeastern.edu
#SBATCH --mail-type=FAIL
#SBATCH --partition=lotterhos
#SBATCH --mem=5G
#SBATCH --nodes=1
#SBATCH --array=2-952%70
#SBATCH --output=/projects/lotterhos/MVP_oyster_GO/results/outputs/slurm_log_20250825/TrainGF082525_%j.out
#SBATCH --error=/projects/lotterhos/MVP_oyster_GO/results/outputs/slurm_log_20250825/TrainGF082525_%j.err

source ~/anaconda3/bin/activate slim_sims_clonal

# setting up error protocols
set -e
set -u
set -o pipefail

#### User modified values ####

# Local working path (this should navigate to the MVP repo)
mypath="/projects/lotterhos/MVP_oyster_GO"
cd ${mypath}

# Folder within MVP where you want are your output files
outpath="results/batch/"
mkdir -p ${outpath} # make outpath directory if it doesn't exist

# run script
Rscript src/offset/trainGF.R
