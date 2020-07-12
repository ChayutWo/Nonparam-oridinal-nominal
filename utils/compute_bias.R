source("../../utils/load_result.R")
source("../../utils/get_true_pmf.R")

compute_bias <- function(model_name, data_name, n_way){
  # compute bias for each of the pmf estimate
  # model_name: name of imputation model (MICE, CART, FOREST, etc)
  # data_name: type of missing mechanism (MCAR_45, MAR_30, etc)
  # n_way: the number of way in joint distribution to be consider

  # return: bias
  # an array of bias for each of the pmf estimate (E[q]-Q)
  
  # get true pmf
  TRUE_Q = get_true_pmf(n_way)
  
  # get mean estimate from imputed dataset
  output_list = load_result(model_name, data_name, n_way)
  q_bar = output_list[['q_bar']] # mean estimate
  u_bar = output_list[['u_bar']] # within group variance
  b = output_list[['b']] # between group variance
  dof = output_list[['dof']] # degree of freedom
  
  # compute the numerator and denominator part of the relative MSE
  Q = matrix(rep(TRUE_Q, 100), nrow = 100, byrow = TRUE)
  
  bias = apply(q_bar-Q, MARGIN = 2, FUN = mean)
  print(paste('>> finish computing bias - model:', model_name,', dataset:', data_name,', n way:', n_way))
  return(bias)
}