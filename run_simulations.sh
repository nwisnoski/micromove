#!/bin/bash -l

#SBATCH --account=microbiome
#SBATCH --time=24:00:00
#SBATCH --nodes=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=64GB
#SBATCH --mail-type=ALL
#SBATCH --mail-user=nathan.wisnoski@uwyo.edu
#SBATCH --job-name=microbe_dispersal_kernel

cd /gscratch/nwisnosk/GitHub/micromove

module load swset/2018.05  gcc/7.3.0 r/4.0.5-py27

R CMD BATCH --no-restore --no-save movement_distributions.R
