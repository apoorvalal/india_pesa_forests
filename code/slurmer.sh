#!/bin/bash
#SBATCH --job-name=panelmatch
#SBATCH --begin=now
#SBATCH --dependency=singleton
#SBATCH --time=02:00:00
#SBATCH --partition=hns
#SBATCH --mem=20G
#SBATCH --mail-type=all
#SBATCH --mail-user=$USER@stanford.edu

## Insert the command to run below. Here, we're just storing the date in a
## cron.log file
# some script here
Rscript 9_panelmatch.R

## Resubmit the job for the next execution
# sbatch $0
