#!/bin/bash
#SBATCH --job-name=TrainGF20260420
#SBATCH --mail-user=rumberger.c@northeastern.edu
#SBATCH --mail-type=FAIL
#SBATCH --partition=lotterhos
#SBATCH --mem=500G
#SBATCH --nodes=1
#SBATCH --array=2-2%2
#SBATCH --output=/projects/lotterhos/MVP_oyster_GO/results/outputs/slurm_log_20260420/TrainGF20260420_%j.out
#SBATCH --error=/projects/lotterhos/MVP_oyster_GO/results/outputs/slurm_log_20260420/TrainGF20260420_%j.err

# setting up error protocols
set -e
set -u
set -o pipefail

# Local working path (this should navigate to the MVP repo)
mypath="/projects/lotterhos/MVP_oyster_GO"
cd ${mypath}

# run script
apptainer run -B "/projects:/projects,/scratch:/scratch" slims_test.sif Rscript src/offset/trainGF.R
