#! /bin/bash
#
#SBATCH --array=1-400
#SBATCH --output=test.out

cd ~/Nonparam-oridinal-nominal/Analysis/run_model/
R CMD BATCH "--args  $SLURM_ARRAY_TASK_ID"  ./test_run_batch.R rraray.Rout$SLURM_ARRAY_TASK_ID
