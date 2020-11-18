source("../../utils/load_result.R")
source("../../utils/get_true_pmf.R")
source("../../utils/format_list.R")
compute_bias <- function(model_name, data_name, n_way, TRUE_Q){
  # compute bias for each of the pmf estimate
  # model_name: name of imputation model (MICE, CART, FOREST, etc)
  # data_name: type of missing mechanism (MCAR_45, MAR_30, etc)
  # n_way: the number of way in joint distribution to be consider
  # TRUE_Q: true pmf from PUMS dataset
  
  # return: bias
  # an array of bias for each of the pmf estimate (E[q]-Q)
  n_replicates = 500
  # get mean estimate from imputed dataset
  output_list = load_result(model_name, data_name, n_way)
  q_bar = output_list[['q_bar']] # mean estimate
  rm(output_list)
  

  # compute the numerator and denominator part of the relative MSE
  Q = (matrix(rep(TRUE_Q, n_replicates), nrow = n_replicates, byrow = TRUE))
  bias = apply(q_bar-Q, MARGIN = 2, FUN = mean)
  print(paste('>> finish computing bias - model:', model_name,', dataset:', data_name,', n way:', n_way))
  
  # remove column where TRUE_Q = 0
  indicator = TRUE_Q !=0
  return(bias[indicator])
}

bias_models <- function(data_name, n_way){
  # compute bias for specific dataset and n_way for all models
  # data_name: type of missing mechanism (MCAR_45, MAR_30, etc)
  # n_way: the number of way in joint distribution to be consider
  
  # return: BIAS
  # a list of bias for each of the models
  models = c('MICE_NOM', 'MICE', 'CART', 'FOREST', 'GAIN', 'GAIN_CAT', 'DP', 'PROBIT')
  
  # get true pmf
  TRUE_Q = get_true_pmf(n_way)

  # output
  BIAS = list()
  
  for (i in 1:length(models)) {
    model_name = models[i]
    bias = compute_bias(model_name, data_name, n_way, TRUE_Q)
    BIAS[[i]] = bias
  }
  names(BIAS) = models
  return(BIAS)
}