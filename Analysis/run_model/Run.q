#! /bin/bash
#
#SBATCH --array=1-400
module load R/3.6.0
module load Python/3.6.4
export R_LIBS=/work/cw403/Nonparam-oridinal-nominal/Analysis/run_model/R_lib
cd /work/cw403/Nonparam-oridinal-nominal/Analysis/run_model/
R CMD BATCH "--args  $SLURM_ARRAY_TASK_ID"  ./run_batch.R rraray.Rout$SLURM_ARRAY_TASK_ID 

