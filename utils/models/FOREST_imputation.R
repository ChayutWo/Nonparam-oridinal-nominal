FOREST_imputation <- function(df_observed, n_imputations){
  # Perform missing data imputation using random forest approach
  # df_observed: observed dataset
  # n_imputations: number of imputed dataset that want to generate

  # return: imputation_list
  # a list comprising of imputed datasets
  imputation_list = list()
  for (i in 1:n_imputations) {
    df.imp <- missForest(df_observed, verbose = FALSE)
    imputation_list[[i]] = df.imp$ximp
  }
  return(imputation_list)
}