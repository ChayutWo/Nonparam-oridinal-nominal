#! /bin/bash
#
#SBATCH --mem=2G
#SBATCH --array=1-5,101-105,201-205,301-305
module load R/3.6.0
export R_LIBS=/work/cw403/Nonparam-oridinal-nominal/Analysis/run_model/R_lib
cd /work/cw403/Nonparam-oridinal-nominal/Analysis/run_model/
R CMD BATCH "--args  $SLURM_ARRAY_TASK_ID"  ./run_batch.R 

