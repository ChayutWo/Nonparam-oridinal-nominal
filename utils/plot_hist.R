plot_hist <- function(imputation_list, df_observed, n_way, missing_col){
  # Plot histogram comparing orignal, observed and imputed pmf
  # imputation_list: a list containing multiple imputation df
  # df_observed: dataframe with missing values
  # n_way: the number of way the contingency table should consider (1 for univariate pmf, 2 for bivariate, etc)
  # missing_col: The column subjected to missing values
  
  # return: NULL
  
  # Load original dataset
  load('../../Datasets/ordinalPUMS.Rdata')
  
  # Extract imputed dataset from imputaion_list and format it
  d1 = imputation_list[[1]]
  d2 = imputation_list[[2]]
  d3 = imputation_list[[3]]
  d4 = imputation_list[[4]]
  d5 = imputation_list[[5]]
  imputed_sets = rbind(d1, d2, d3, d4, d5)
  
  for (var_index in missing_col) {
    # Population distribution
    y_original = df[,var_index]
    original_pmf = table(y_original)/length(y_original)
    
    # Observed distribution
    missing_indicator = is.na(df_observed)[,var_index]
    y_observed = df_observed[!missing_indicator, var_index]
    observed_pmf = table(y_observed)/length(y_observed)
    
    # Extract variable from imputed data
    imputed_pmf = table(imputed_sets[, var_index])/length(imputed_sets[, var_index])
    
    results = rbind(original_pmf,observed_pmf,imputed_pmf)
    colnames(results)<- 1:dim(imputed_pmf)
    barplot(results, xlab = 'Category', beside = TRUE, 
            legend = TRUE, 
            main = colnames(df)[var_index])
  }
  return(imputed_pmf)
}