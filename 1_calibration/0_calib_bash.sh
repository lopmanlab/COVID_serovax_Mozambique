#!/bin/bash
#SBATCH --job-name=calib
#SBATCH --partition=lopman
#SBATCH --ntasks=1
# # SBATCH --mem-per-cpu=35GB
#SBATCH --cpus-per-task=32
#SBATCH --mail-user=carol.liu@emory.edu
#SBATCH --mail-type=END
#SBATCH --mail-type=FAIL

# # This puts all output files in a separate directory.
# SBATCH --output=Out/TestRunCheck.%A_%a.out
# SBATCH —error=Err/TestRunCheck.%A_%a.err

# # Submitting 100 instances of srun commands listed below
# # SBATCH —array=0-100

# # For notification purposes. Use your Emory email address only!


module purge

module load R

Rscript 0_model_batch.R

