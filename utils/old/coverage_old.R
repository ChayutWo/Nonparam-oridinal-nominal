coverage <- function(imputation_list, n_way, missing_col){
  # Perform multiple imputation inference using Rubin statistics (1987) and report coverage
  # imputation_list: a list containing multiple imputation df
  # n_way: the number of way the contingency table should consider (1 for univariate pmf, 2 for bivariate, etc)
  # missing_col: The column subjected to missing values

  # return: coverage
  # coverage: a fraction of coverage of the calculated confidence interval compared with population estimand
  
  # Load true population dataset
  load('../../Datasets/ordinalPUMS.Rdata')
  # Unpack imputation_list
  d1 = imputation_list[[1]]
  d2 = imputation_list[[2]]
  d3 = imputation_list[[3]]
  d4 = imputation_list[[4]]
  d5 = imputation_list[[5]]
  
  n_observation = dim(d1)[1]
  n_imputations = 5
  
  # Prepare output format
  coverage = c()
  
  # Identify possible combinations
  combinations = combn(1:11, n_way)
  
  for (i in 1:(dim(combinations)[2])) {
    variables = combinations[, i]
    if (any(missing_col %in% variables)) {
      # Calculate true contingency probability
      true_cont_table = c(table(df[, variables]))
      true_cont_table = true_cont_table/sum(true_cont_table)
      # if one of the combination has missing values
      # Calculate contingency table respect to that combination
      cont_table_1 = c(table(d1[,variables]))
      cont_table_2 = c(table(d2[,variables]))
      cont_table_3 = c(table(d3[,variables]))
      cont_table_4 = c(table(d4[,variables]))
      cont_table_5 = c(table(d5[,variables]))
      # Normalize contingency table
      cont_table_1 = cont_table_1/sum(cont_table_1)
      cont_table_2 = cont_table_2/sum(cont_table_2)
      cont_table_3 = cont_table_3/sum(cont_table_3)
      cont_table_4 = cont_table_4/sum(cont_table_4)
      cont_table_5 = cont_table_5/sum(cont_table_5)
      
      # Combind them into one table: n_imputationxn_combinations
      cont_table = rbind(cont_table_1, cont_table_2, cont_table_3, cont_table_4, cont_table_5)
      
      # Calculate MI estimate of P
      mean_estimate = apply(cont_table, MARGIN = 2, FUN = mean)
      
      # Calculate within group variance: p(1-p)/n
      within_group_var = apply(cont_table*(1-cont_table)/n_observation, MARGIN = 2, FUN = mean)
      
      # Calculate across group variance: sum(p-p_bar)^2/(m-1)
      across_group_var = apply((t(t(cont_table) - mean_estimate))^2, MARGIN = 2, FUN = sum)/(n_imputations-1)
      
      # Calculate total variance: (1+1/m)*across_var+within_var
      total_var = (1+1/n_imputations)*across_group_var + within_group_var
      total_sd = sqrt(total_var)
      
      # Calculate Rubin degree of freedom:dof
      rM = (1+1/n_imputations)*across_group_var/within_group_var
      dof = (n_imputations-1)*(1+1/rM)^2
      
      # Calculate quantile for 95% confidence interval from t distribution
      q_alpha = qt(0.975, df = dof)
      
      # Calculate upper and lower bound of 95% confidence interval
      upper_bound = mean_estimate+q_alpha*total_sd
      lower_bound = mean_estimate-q_alpha*total_sd
      
      # Check whether true value falls in the interval
      coverage = c(coverage,lower_bound<=true_cont_table & true_cont_table<=upper_bound)
    }
  }
  return(mean(coverage[!is.na(coverage)]))
}