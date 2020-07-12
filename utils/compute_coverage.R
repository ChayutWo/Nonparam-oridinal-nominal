source("../../utils/load_result.R")
source("../../utils/get_true_pmf.R")
source("../../utils/format_list.R")
compute_coverage <- function(model_name, data_name, n_way, n_imputations){
  # compute coverage for each of the pmf
  # model_name: name of imputation model (MICE, CART, FOREST, etc)
  # data_name: type of missing mechanism (MCAR_45, MAR_30, etc)
  # n_way: the number of way in joint distribution to be consider
  # n_imputations: the number of imputed dataset that all estimates are based on

  # return: coverage
  # an array of coverage in fraction for each of the pmf
  
  # get true pmf
  TRUE_Q = get_true_pmf(n_way)
  # load result
  output_list = load_result(model_name, data_name, n_way)
  q_bar = output_list[['q_bar']] # mean estimate
  u_bar = output_list[['u_bar']] # within group variance
  b = output_list[['b']] # between group variance
  dof = output_list[['dof']] # degree of freedom
  rm(output_list)
  # calculate total sd
  total_sd = sqrt((1+1/n_imputations)*b + u_bar)
  rm(b, u_bar)
  # calculate upper bound and lower bound of the 95% CI
  q_alpha = qt(0.975, df = dof)
  rm(dof)
  upper_bound = q_bar + q_alpha*total_sd
  lower_bound = q_bar - q_alpha*total_sd
  rm(q_bar, total_sd, q_alpha)
  # calculate coverage 0/1 and return the mean of each pmf
  Q = matrix(rep(TRUE_Q, 100), nrow = 100, byrow = TRUE)
  coverage = apply((lower_bound<=Q) & (Q<=upper_bound), MARGIN = 2, FUN = mean)
  print(paste('>> finish computing coverage - model:', model_name,', dataset:', data_name,', n way:', n_way))
  return(coverage)
}

coverage_models <- function(data_name, n_way, n_imputations){
  # compute coverage for specific dataset and n_way for all models
  # data_name: type of missing mechanism (MCAR_45, MAR_30, etc)
  # n_way: the number of way in joint distribution to be consider
  # n_imputations: the number of imputed dataset that all estimates are based on
  
  # return: 
  # 
  models = c('MICE', 'CART', 'FOREST', 'GAIN', 'DP', 'PROBIT')
  
  # output
  COVERAGE = list()
  
  for (i in 1:length(models)) {
    model_name = models[i]
    coverage = compute_coverage(model_name, data_name, n_way, n_imputations)
    COVERAGE[[i]] = coverage
  }
  names(COVERAGE) = models
  return(COVERAGE)
}