library(mice)
MICE_NOM_imputation <- function(df_observed, n_imputations){
  # Perform missing data imputation using MICE approach with nominal data
  # df_observed: observed dataset
  # n_imputations: number of imputed dataset that want to generate

  # return: imputation_list
  # a list comprising of imputed datasets
  
  # levels of each of the columns
  levels = c(7,7,7,19,5,4,7,2,17,3,13)
  
  # format to be nominal variables
  df_temp = df_observed
  for (col_index in 1:ncol(df_temp)) {
    df_temp[,col_index] = factor(df_temp[,col_index], levels = 1:levels[col_index], ordered = FALSE)
  }
  
  # perform MICE imputation
  imputed_df <-  mice(df_temp,m=n_imputations,print=F)
  
  # collect results into list
  imputation_list = list()
  for (i in 1:n_imputations) {
    d = complete(imputed_df, i)
    # format columns of d to be ordinal variables
    for (col_index in 1:ncol(df_observed)) {
      d[,col_index] = factor(d[,col_index], levels = 1:levels[col_index], ordered = TRUE)
    }
    imputation_list[[i]] = d
  }
  return(imputation_list)
}