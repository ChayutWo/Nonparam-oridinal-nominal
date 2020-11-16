# This code generate 100 subsample of the PUMS original data each with 10000 observations
# It then made each of those 100 dataset missing according to 4 missing mechanism
library(MASS)
library(tidyverse)

setwd("~/DS_Projects/Nonparam-oridinal-nominal/Analysis/data_generation")
source("../../utils/sample_PUMS.R")
source("../../utils/make_MCAR.R")
source("../../utils/make_MAR_30.R")
source("../../utils/make_MAR_45.R")
source('../../utils/load_data.R')
# Specify parameters of the dataset and missing data mechanism
data_name = 'fully_observed/fully_observed_236'

# Root for different dataset
root_MCAR_45 <- '../../Datasets/MCAR_45/MCAR_45_'
#root_MCAR_30 <- '../../Datasets/MCAR_30/MCAR_30_'
i = 236
df = load_data(data_name)
df_MCAR_45 <- make_MCAR(df, 0.45)
#df_MCAR_30 <- make_MCAR(df, 0.30)

# Obtain a full path for the file to be saved
name_MCAR_45 <- paste(root_MCAR_45, i,'.csv', sep = '')
#name_MCAR_30 <- paste(root_MCAR_30, i,'.csv', sep = '')

# Save them according to the name and file structure
write.table(df_MCAR_45, name_MCAR_45, row.names = FALSE, sep = ',')

cat(paste('finish generating',i,'datasets','\n'))
cat(name_MCAR_45)

