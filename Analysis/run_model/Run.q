#! /bin/bash
#
#SBATCH --array=1-400
export R_LIBS=$HOME/Rlib
cd /dscrhome/oma9/Code
R CMD BATCH "--args  $SLURM_ARRAY_TASK_ID"  ./run_batch.R rraray.Rout$SLURM_ARRAY_TASK_ID
