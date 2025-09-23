#!/bin/bash
#SBATCH --job-name=TrainGF091725
#SBATCH --mail-user=rumberger.c@northeastern.edu
#SBATCH --mail-type=FAIL
#SBATCH --partition=lotterhos
#SBATCH --mem=20G
#SBATCH --nodes=1
#SBATCH --array=2-952%70
#SBATCH --output=/projects/lotterhos/MVP_oyster_GO/results/outputs/slurm_log_20250917/TrainGF20250917_%j.out
#SBATCH --error=/projects/lotterhos/MVP_oyster_GO/results/outputs/slurm_log_20250917/TrainGF20250917_%j.err

podman run containers.rc.northeastern.edu/lotterhos/slim_sims_clonal

source ~/anaconda3/bin/activate R_env_clones

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
module load R

Rscript src/offset/trainGF.R

gzip -f "results/lg_results/gf_geno_2025-09-17.RDS"
gzip -f "results/lg_results/gf_geno_2025-09-17.RDS"


