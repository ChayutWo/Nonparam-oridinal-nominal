plot_pmf <- function(imputation_list, n_way, missing_col){
  # Plot a scatter plot of original and imputed multi way pmf
  # imputation_list: a list containing multiple imputation df
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
  
  combinations = combn(1:11, n_way)
  original_dist = c()
  imputed_dist = c()
  for (i in 1:(dim(combinations)[2])) {
    variables = combinations[, i]
    if (any(missing_col %in% variables)) {
      # Compute the joint pmf in the original dataframe without missing values
      original_pmf = table(df[,variables])
      original_pmf = original_pmf/sum(original_pmf)
      
      # Compute the joint pmf in the imputed dataset and average over imputations
      imputed_pmf = table(imputed_sets[,variables])
      imputed_pmf = imputed_pmf/sum(imputed_pmf)
      
      original_dist = rbind(original_dist, matrix(original_pmf, ncol = 1))
      imputed_dist = rbind(imputed_dist, matrix(imputed_pmf, ncol = 1))
    }
  }
  title = paste('Assess imputed pmf:', n_way, 'way')
  plot(original_dist, imputed_dist, 
       xlab = 'Original joint pmf', ylab = 'Imputed joint pmf', 
       main = title)
  abline(0,1, col = 'gray')
  abline(0,1.1, col = 'red')
  abline(0, 0.9, col = 'red')
}