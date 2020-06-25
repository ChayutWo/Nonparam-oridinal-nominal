library(mice)
MICE_imputation <- function(df_observed, n_imputations){
  # Perform missing data imputation using MICE approach
  # df_observed: observed dataset
  # n_imputations: number of imputed dataset that want to generate

  # return: imputation_list
  # a list comprising of imputed datasets
  imputed_df <-  mice(df_observed,m=n_imputations,print=F)
  imputation_list = list()
  for (i in 1:n_imputations) {
    imputation_list[[i]] = complete(imputed_df, i)
  }
  return(imputation_list)
}