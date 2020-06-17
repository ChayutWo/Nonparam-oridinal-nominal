# This code generate 100 subsample of the PUMS original data each with 10000 observations
# It then made each of those 100 dataset missing according to 4 missing mechanism
set.seed(0)
source("../../utils/sample_PUMS.R")
source("../../utils/make_MCAR.R")
source("../../utils/make_MAR_30.R")
source("../../utils/make_MAR_45.R")

# Specify parameters of the dataset and missing data mechanism
n <- 10000
missing_col <- c(1,3,7,9,10,11)
set.seed(0)

# Root for different dataset
root_fully_observed <- '../../Datasets/fully_observed/fully_observed_'
root_MCAR_30 <- '../../Datasets/MCAR_30/MCAR_30_'
root_MCAR_45 <- '../../Datasets/MCAR_45/MCAR_45_'
root_MAR_30 <- '../../Datasets/MAR_30/MAR_30_'
root_MAR_45 <- '../../Datasets/MAR_45/MAR_45_'

for (i in 1:2) {
  # Generate a full set of fully observed and missing dataframes
  df <- sample_PUMS(n)
  df_MCAR_30 <- make_MCAR(df, 0.3)
  df_MCAR_45 <- make_MCAR(df, 0.45)
  df_MAR_30 <- make_MAR_30(df)
  df_MAR_45 <- make_MAR_45(df)
  
  # Obtain a full path for the file to be saved
  name_fully_observed <- paste(root_fully_observed, i,'.csv', sep = '')
  name_MCAR_30 <- paste(root_MCAR_30, i,'.csv', sep = '')
  name_MCAR_45 <- paste(root_MCAR_45, i,'.csv', sep = '')
  name_MAR_30 <- paste(root_MAR_30, i,'.csv', sep = '')
  name_MAR_45 <- paste(root_MAR_45, i,'.csv', sep = '')
  
  # Save them according to the name and file structure
  write.table(df, name_fully_observed, row.names = FALSE, sep = ',')
  write.table(df_MCAR_30, name_MCAR_30, row.names = FALSE, sep = ',')
  write.table(df_MCAR_45, name_MCAR_45, row.names = FALSE, sep = ',')
  write.table(df_MAR_30, name_MAR_30, row.names = FALSE, sep = ',')
  write.table(df_MAR_45, name_MAR_45, row.names = FALSE, sep = ',')
  
  cat(paste('finish generating',i,'datasets'))
}

