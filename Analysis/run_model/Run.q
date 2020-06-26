#! /bin/bash
#
#SBATCH --array=1-400
#SBATCH --output=test.out
export R_LIBS=$HOME/Rlib
cd ~/Nonparam-oridinal-nominal/Analysis/run_model/
R CMD BATCH "--args  $SLURM_ARRAY_TASK_ID"  ./run_batch.R rraray.Rout$SLURM_ARRAY_TASK_ID
