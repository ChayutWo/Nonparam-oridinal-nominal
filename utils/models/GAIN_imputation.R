GAIN_imputation <- function(data_name, missing_data_name, n_imputations, df_observed){
  # Perform missing data imputation using GAIN modeling approach
  # data_name and missing_data_name is file location after entering Datasets folder
  # n_imputations: number of imputation to be performed
  # df_observed: dataset with missing values

  # return: imputation_list
  # a list comprising of imputed datasets
  
  # change directory to GAIN
  currentloc <- getwd()
  
  setwd('../../GAIN/')
  
  # create command to run GAIN: command
  #command = paste('/usr/local/bin/python3 main.py --data_name', data_name, '--missing_data_name',
  #                 missing_data_name,'--num_imputations', n_imputations)
  command = paste('python main.py --data_name', data_name, '--missing_data_name',
                  missing_data_name,'--num_imputations', n_imputations)
  # perform GAIN modeling and save n_imputations imputed datasets
  system(command, wait = TRUE)
  
  # change working directory back to current location
  setwd(currentloc)
  
  # load GAIN imputed datasets and format into list
  levels = c(7,7,7,19,5,4,7,2,17,3,13)
  root = '../../GAIN/imputed_dataset/'
  imputation_list = list()
  
  for (i in 1:n_imputations) {
    filename = paste(root, missing_data_name,'_',i,'.csv',sep = '')
    d = read.csv(filename, header = FALSE, sep = ',')
    colnames(d) = colnames(df_observed)
    # format columns of d
    for (col_index in 1:ncol(df_observed)) {
      d[,col_index] = factor(d[,col_index], levels = 1:levels[col_index], ordered = TRUE)
    }

    imputation_list[[i]] = d
    # remove that file
    command = paste('rm', filename)
    system(command, wait = TRUE)
  }
  return(imputation_list)
}