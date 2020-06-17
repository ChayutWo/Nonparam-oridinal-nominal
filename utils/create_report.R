create_report <- function(imputation_list, max_nway, missing_col, df_observed){
  # Create report from an imputed datasets
  # Coverage, rmse, mae, and distribution comparison with the population data
  
  # Calculate coverage
  source("../../utils/coverage.R")
  cat('##### Coverage ##### \n')
  for (i in 1:max_nway) {
    val <- coverage(imputation_list, n_way=i, missing_col)
    cat(paste("Coverage ", i, ' way: ', round(val*100, 2), ' percent', "\n", sep = ''))
  }
  cat('\n')
  
  # Calculate rmse
  source("../../utils/rmse.R")
  cat('##### RMSE ##### \n')
  for (i in 1:max_nway) {
    val <- rmse(imputation_list, n_way=i, missing_col)
    cat(paste("RMSE ", i, ' way: ', round(val, 6), "\n", sep = ''))
  }
  cat('\n')
  
  # Calculate mae
  source("../../utils/mae.R")
  cat('##### MAE ##### \n')
  for (i in 1:max_nway) {
    val <- mae(imputation_list, n_way=i, missing_col)
    cat(paste("MAE ", i, ' way: ', round(val, 6), "\n", sep = ''))
  }
  cat('\n')

  # Compare distribution
  source("../../utils/plot_hist.R")
  source("../../utils/plot_pmf.R")
  plot_hist(imputation_list, df_observed, missing_col)
  for (i in 1:max_nway) {
    plot_pmf(imputation_list, n_way=i, missing_col)
  }
}