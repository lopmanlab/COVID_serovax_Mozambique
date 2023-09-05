#!/bin/bash
#SBATCH --job-name=randtime_sens
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

module --ignore-cache load "R/4.0.2"

#Rscript 0_control_runs.R
#Rscript 0_comp_ind_runs.R
Rscript 2_compile_res_hpc2.R


# #srun /home/cliu369/WHO_SAGE_COVID_Sero_Vax/code/0_calib_runs/0_model_simulations_v2.R