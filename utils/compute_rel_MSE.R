source("../../utils/load_result.R")
source("../../utils/get_true_pmf.R")
source("../../utils/get_observed_pmf.R")
source("../../utils/format_list.R")
compute_rel_MSE <- function(model_name, data_name, n_way, TRUE_Q, OBSERVED_Q){
  # compute relative MSE for each of the pmf
  # model_name: name of imputation model (MICE, CART, FOREST, etc)
  # data_name: type of missing mechanism (MCAR_45, MAR_30, etc)
  # n_way: the number of way in joint distribution to be consider
  # TRUE_Q: true pmf from PUMS dataset
  # OBSERVED_Q: observed pmf from fully observed dataset
  
  # return: rel_MSE
  # an array of relative MSE in fraction for each of the pmf

  # load result
  output_list = load_result(model_name, data_name, n_way)
  q_bar = output_list[['q_bar']] # mean estimate
  rm(output_list)
  
  # compute the numerator and denominator part of the relative MSE
  Q = matrix(rep(TRUE_Q, 100), nrow = 100, byrow = TRUE)
  imputed_MSE = apply((q_bar - Q)^2, MARGIN = 2, FUN = sum)
  observed_MSE = apply((OBSERVED_Q - Q)^2, MARGIN = 2, FUN = sum)
  rel_MSE = imputed_MSE/observed_MSE
  
  print(paste('>> finish computing relative MSE - model:', model_name,', dataset:', data_name,', n way:', n_way))
  
  # remove column where TRUE_Q = 0
  indicator = TRUE_Q !=0
  return(rel_MSE[indicator])
}

rel_MSE_models <- function(data_name, n_way){
  # compute relative MSE for specific dataset and n_way for all models
  # data_name: type of missing mechanism (MCAR_45, MAR_30, etc)
  # n_way: the number of way in joint distribution to be consider

  # return: RELATIVE_MSE
  # a list of relative mse for each of the models
  models = c('MICE_NOM', 'MICE', 'CART', 'FOREST', 'GAIN', 'DP', 'PROBIT')
  
  # get true pmf
  TRUE_Q = get_true_pmf(n_way)
  
  # get observed pmf
  OBSERVED_Q = get_observed_pmf(n_way)
  
  # output
  RELATIVE_MSE = list()
  
  for (i in 1:length(models)) {
    model_name = models[i]
    mse = compute_rel_MSE(model_name, data_name, n_way, TRUE_Q, OBSERVED_Q)
    RELATIVE_MSE[[i]] = mse
  }
  names(RELATIVE_MSE) = models
  return(RELATIVE_MSE)
}
