PROBIT_imputation <- function(df_observed, n_imputations){
  # Perform missing data imputation using Bayesian Nonparametric modeling approach
  # df_observed: observed dataset
  # n_imputations: number of imputed dataset that want to generate

  # return: imputation_list
  # a list comprising of imputed datasets
  
  # Import imputation function
  source("../../utils/models/probitBayes_by_cluster.r")
  # MCMC parameter
  N = 50
  Mon = 1000
  B = 5000
  thin.int = 10
  
  # function(y, N = 40, Mon = 2000, B = 300, thin.int = 5, seed = 0)
  sampled_y <- probitBayesImputation(df_observed, N, Mon, B, thin.int)
  
  # extract n_imputations imputed dataset from probit model
  imputation_index = as.integer(seq(1,dim(sampled_y)[1], length.out = n_imputations))
  imputation_list = list()
  
  levels = c(7,7,7,19,5,4,7,2,17,3,13)
  for (i in 1:length(imputation_index)) {
    index = imputation_index[i]
    # need to plus 1 here because the class index of DP function starts at 0
    d = sampled_y[index,,]
    d = data.frame(d)
    colnames(d) = colnames(df_observed)
    # format columns of d
    for (col_index in 1:ncol(df_observed)) {
      d[,col_index] = factor(d[,col_index], levels = 1:levels[col_index], ordered = TRUE)
    }
    imputation_list[[i]] = d
  }
  
  return(imputation_list)
}