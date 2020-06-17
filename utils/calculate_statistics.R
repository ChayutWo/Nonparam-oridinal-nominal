calculate_statistics <- function(imputation_list, n_way, missing_col){
  # Calculate pooled statistics from Rubin method 
  # imputation_list: a list containing multiple imputation df
  # n_way: the number of way the contingency table should consider (1 for univariate pmf, 2 for bivariate, etc)
  # missing_col: The column subjected to missing values

  # return: output_list
  # a list comprising of mean (q_bar), within group variance (u_bar), 
  # across group variance (b) and degree of freedom (dof) of each level combination
  
  # Unpack imputation_list
  d1 = imputation_list[[1]]
  d2 = imputation_list[[2]]
  d3 = imputation_list[[3]]
  d4 = imputation_list[[4]]
  d5 = imputation_list[[5]]
  
  n_observation = dim(d1)[1]
  n_imputations = 5
  
  # Prepare output format
  q_bar = c() # mean estimate across five imputation sets
  u_bar = c() # within group variance
  b = c() # between group variance
  dof = c() # degree of freedom
  
  # Identify possible combinations
  combinations = combn(1:11, n_way)
  
  for (i in 1:(dim(combinations)[2])) {
    variables = combinations[, i]
    if (any(missing_col %in% variables)) {
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
      q_bar = c(q_bar, mean_estimate)
      
      # Calculate within group variance: p(1-p)/n
      within_group_var = apply(cont_table*(1-cont_table)/n_observation, MARGIN = 2, FUN = mean)
      u_bar = c(u_bar, within_group_var)
      
      # Calculate across group variance: sum(p-p_bar)^2/(m-1)
      across_group_var = apply((t(t(cont_table) - mean_estimate))^2, MARGIN = 2, FUN = sum)/(n_imputations-1)
      b = c(b, across_group_var)
      
      # Calculate Rubin degree of freedom:dof
      rM = (1+1/n_imputations)*across_group_var/within_group_var
      dof = c(dof, (n_imputations-1)*(1+1/rM)^2)
      
    }
  }
  output_list = list(q_bar, u_bar, b, dof)
  names(output_list) = c('q_bar', 'u_bar', 'b', 'dof')
  
  return(output_list)
}