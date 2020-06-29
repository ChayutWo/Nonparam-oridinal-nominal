#! /bin/bash
#
#SBATCH --array=1-400
module load R/3.6.0
export R_LIBS=$HOME/Nonparam-oridinal-nominal/Analysis/run_model/R_lib
cd ~/Nonparam-oridinal-nominal/Analysis/run_model/
R CMD BATCH "--args  $SLURM_ARRAY_TASK_ID"  ./test_run_batch.R 

