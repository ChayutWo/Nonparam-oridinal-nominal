run_all_models <- function(data_name, missing_data_name, n_imputations, max_nway){
  # Perform missing data imputation using all modeling approaches and save Rubin statistics
  # data_name: the name of the full dataset without missing values to be used in GAIN
  # missing_data_name: the name of the dataset with missing values to be loaded
  # n_imputations: number of imputed dataset
  # max_nway: maximum number of ways in joint distribution calculation
  
  # source all model codes
  source('../../utils/models/MICE_imputation.R')
  source('../../utils/models/MICE_NOM_imputation.R')
  source('../../utils/models/CART_imputation.R')
  source('../../utils/models/FOREST_imputation.R')
  source('../../utils/models/DP_imputation.R')
  source('../../utils/models/GAIN_imputation.R')
  source('../../utils/models/GAIN_CAT_imputation.R')
  source('../../utils/models/GAIN_CAT_DIFF_imputation.R')
  source('../../utils/models/GAIN_DIFF_imputation.R')
  source('../../utils/models/PROBIT_imputation.R')
  # source other utils code
  source('../../utils/load_data.R')
  source("../../utils/calculate_statistics.R")
  
  # load observed data and format it to be ordinal variables
  df_observed = load_data(missing_data_name)
  missing_col = c(1,3,7,9,10,11)
  cat(paste('>> complete loading data', Sys.time(),'\n'))
  
  ##### run MICE #####
  #imputation_list = MICE_imputation(df_observed, n_imputations)
  #cal_save_stat(imputation_list, 'MICE', missing_data_name, max_nway)
  #cat(paste('>> complete MICE imputation on',missing_data_name, Sys.time(),'\n'))
  #print(paste('MICE',length(imputation_list)))
  ##### run MICE_NOM #####
  #imputation_list = MICE_NOM_imputation(df_observed, n_imputations)
  #cal_save_stat(imputation_list, 'MICE_NOM', missing_data_name, max_nway)
  #cat(paste('>> complete MICE_NOM imputation on',missing_data_name, Sys.time(),'\n'))
  #print(paste('MICE_NOM',length(imputation_list)))
  ##### run CART #####
  #imputation_list = CART_imputation(df_observed, n_imputations)
  #cal_save_stat(imputation_list, 'CART', missing_data_name, max_nway)
  #cat(paste('>> complete CART imputation on',missing_data_name, Sys.time(),'\n'))
  #print(paste('CART',length(imputation_list)))
  ##### run FOREST #####
  #imputation_list = FOREST_imputation(df_observed, n_imputations)
  #cal_save_stat(imputation_list, 'FOREST', missing_data_name, max_nway)
  #cat(paste('>> complete FOREST imputation on',missing_data_name, Sys.time(),'\n'))
  #print(paste('FOREST',length(imputation_list)))
  ##### run DP #####
  #imputation_list = DP_imputation(df_observed, n_imputations)
  #cal_save_stat(imputation_list, 'DP', missing_data_name, max_nway)
  #cat(paste('>> complete DP imputation on',missing_data_name, Sys.time(),'\n'))
  #print(paste('DP',length(imputation_list)))
  ##### run GAIN #####
  imputation_list = GAIN_imputation(data_name, missing_data_name, n_imputations, df_observed)
  cal_save_stat(imputation_list, 'GAIN', missing_data_name, max_nway)
  cat(paste('>> complete GAIN imputation on',missing_data_name, Sys.time(),'\n'))
  print(paste('GAIN',length(imputation_list)))
  ##### run GAIN_CAT #####
  imputation_list = GAIN_CAT_imputation(data_name, missing_data_name, n_imputations, df_observed)
  cal_save_stat(imputation_list, 'GAIN_CAT', missing_data_name, max_nway)
  cat(paste('>> complete GAIN_CAT imputation on',missing_data_name, Sys.time(),'\n'))
  print(paste('GAIN_CAT',length(imputation_list)))
  ##### run GAIN_DIFF #####
  #imputation_list = GAIN_DIFF_imputation(data_name, missing_data_name, n_imputations, df_observed)
  #cal_save_stat(imputation_list, 'GAIN_DIFF', missing_data_name, max_nway)
  #cat(paste('>> complete GAIN_DIFF imputation on',missing_data_name, Sys.time(),'\n'))
  #print(paste('GAIN_DIFF',length(imputation_list)))
  ##### run GAIN_CAT_DIFF #####
  #imputation_list = GAIN_CAT_DIFF_imputation(data_name, missing_data_name, n_imputations, df_observed)
  #cal_save_stat(imputation_list, 'GAIN_CAT_DIFF', missing_data_name, max_nway)
  #cat(paste('>> complete GAIN_CAT_DIFF imputation on',missing_data_name, Sys.time(),'\n'))
  #print(paste('GAIN_CAT_DIFF',length(imputation_list)))
  ##### run PROBIT #####
  #imputation_list = PROBIT_imputation(df_observed, n_imputations)
  #cal_save_stat(imputation_list, 'PROBIT', missing_data_name, max_nway)
  #cat(paste('>> complete PROBIT imputation on',missing_data_name, Sys.time(),'\n'))
  #print(paste('PROBIT',length(imputation_list)))
}