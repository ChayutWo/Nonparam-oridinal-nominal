# import function
library(tidyverse)

# sample MCAR dataset from PUMS
source("../../utils/sampleMCAR.R")
n = 10000
missing_col = c(1,3,7,9,10,11)
missing_prob = 0.3
set.seed(0)

dummy <- sampleMCAR(n, missing_prob)
source('../../utils/models/run_all_models.R')

# define the dataset name
root_fully_observed = 'fully_observed/fully_observed_'
root_MCAR_30 = 'MCAR_30/MCAR_30_'
root_MCAR_45 = 'MCAR_45/MCAR_45_'
root_MAR_30 = 'MAR_30/MAR_30_'
root_MAR_45 = 'MAR_45/MAR_45_'

# define simulation hyper parameter
n_imputations = 10
max_nway = 4

for (repindex in c(100,200,300,400)) {
  if (repindex <= 100) {
    # repindex 1 - 100: MCAR_30 dataset
    i = repindex
    data_name = paste(root_fully_observed, i, sep='')
    missing_data_name = paste(root_MCAR_30, i, sep = '')
  }else if (repindex <= 200) {
    # repindex 101-200: MCAR_45 dataset
    i = repindex - 100
    data_name = paste(root_fully_observed, i, sep='')
    missing_data_name = paste(root_MCAR_45, i, sep = '')
  }else if (repindex <= 300) {
    # repindex 201-300: MAR_30 dataset
    i = repindex - 200
    data_name = paste(root_fully_observed, i, sep='')
    missing_data_name = paste(root_MAR_30, i, sep = '')
  }else if (repindex <= 400) {
    # repindex 301-400: MAR_45 dataset
    i = repindex - 300
    data_name = paste(root_fully_observed, i, sep='')
    missing_data_name = paste(root_MAR_45, i, sep = '')
  }
  print(paste('>> start running:', data_name, missing_data_name))
  run_all_models(data_name, missing_data_name, n_imputations, max_nway)
  
}

