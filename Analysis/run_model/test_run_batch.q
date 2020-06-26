#! /bin/bash
#
#SBATCH --array=1-400

cd /dscrhome/oma9/Code
R CMD BATCH "--args  $SLURM_ARRAY_TASK_ID"  ./test_run_batch.R rraray.Rout$SLURM_ARRAY_TASK_ID
