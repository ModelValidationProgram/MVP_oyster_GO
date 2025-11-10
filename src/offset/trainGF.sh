#!/bin/bash
#SBATCH --job-name=TrainGF110425
#SBATCH --mail-user=rumberger.c@northeastern.edu
#SBATCH --mail-type=FAIL
#SBATCH --partition=lotterhos
#SBATCH --mem=40G
#SBATCH --nodes=1
#SBATCH --array=2-2%2
#SBATCH --output=/projects/lotterhos/MVP_oyster_GO/results/outputs/slurm_log_20251109/TrainGF20251109_%j.out
#SBATCH --error=/projects/lotterhos/MVP_oyster_GO/results/outputs/slurm_log_20251109/TrainGF20251109_%j.err

# setting up error protocols
set -e
set -u
set -o pipefail

#### User modified values ####

# Local working path (this should navigate to the MVP repo)
mypath="/projects/lotterhos/MVP_oyster_GO"
cd ${mypath}

# run script
apptainer run -B "/projects:/projects,/scratch:/scratch" slims_test.sif Rscript src/offset/trainGF.R

#gzip -f "results/lg_results/gf_af_2025-11-09.RDS"
#gzip -f "results/lg_results/gf_geno_2025-11-09.RDS"


