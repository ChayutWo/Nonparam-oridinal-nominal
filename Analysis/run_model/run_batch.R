# Code to be called on batch mode:
# R CMD BATCH '--args 1' run_batch.R
# the argument is the index for the dataset
library(tidyverse)
# clear workspace and load data
args <- commandArgs(TRUE)
print(args)
repindex <- as.numeric(args[1])
print(repindex)
source('../../utils/models/run_all_models.R')

# define the dataset name
root_fully_observed = 'fully_observed/fully_observed_'
root_MCAR_30 = 'MCAR_30/MCAR_30_'
root_MCAR_45 = 'MCAR_45/MCAR_45_'
root_MAR_30 = 'MAR_30/MAR_30_'
root_MAR_45 = 'MAR_45/MAR_45_'

# define simulation hyper parameter
n_imputations = 50
max_nway = 3

if (repindex <= 500) {
  # repindex 1 - 500: MCAR_30 dataset
  i = repindex
  data_name = paste(root_fully_observed, i, sep='')
  missing_data_name = paste(root_MCAR_30, i, sep = '')
}else if (repindex <= 1000) {
  # repindex 501-1000: MCAR_45 dataset
  i = repindex - 500
  data_name = paste(root_fully_observed, i, sep='')
  missing_data_name = paste(root_MCAR_45, i, sep = '')
}else if (repindex <= 1500) {
  # repindex 1001-1500: MAR_30 dataset
  i = repindex - 1000
  data_name = paste(root_fully_observed, i, sep='')
  missing_data_name = paste(root_MAR_30, i, sep = '')
}else if (repindex <= 2000) {
  # repindex 1500-2000: MAR_45 dataset
  i = repindex - 1500
  data_name = paste(root_fully_observed, i, sep='')
  missing_data_name = paste(root_MAR_45, i, sep = '')
}
cat(paste('>> start running:', data_name, missing_data_name))
run_all_models(data_name, missing_data_name, n_imputations, max_nway)
