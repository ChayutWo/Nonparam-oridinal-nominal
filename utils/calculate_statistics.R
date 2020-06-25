calculate_statistics <- function(imputation_list, n_way, missing_col){
  # Calculate pooled statistics from Rubin method 
  # imputation_list: a list containing multiple imputation df
  # n_way: the number of way the contingency table should consider (1 for univariate pmf, 2 for bivariate, etc)
  # missing_col: The column subjected to missing values

  # return: output_list
  # a list comprising of mean (q_bar), within group variance (u_bar), 
  # across group variance (b) and degree of freedom (dof) of each level combination
  
  n_observation = dim(imputation_list[[1]])[1]
  n_imputations = length(imputation_list)
  if (n_observation!=10000) {
    print('Warning: n_observation is not 10000')
  }
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
      cont_table = c()
      for (imputation in 1:n_imputations) {
        # extract dataframe of that imputation
        d = imputation_list[[imputation]]
        temp_cont_table = c(table(d[,variables]))
        temp_cont_table = temp_cont_table/sum(temp_cont_table)
        cont_table = rbind(cont_table, temp_cont_table)
      }

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

cal_save_stat <- function(imputation_list, model_name, missing_data_name, max_nway){
  # Calculate statistics by calling calculate_statistics function upto prespecified nway
  # Save it to result folder 
  # imputation_list: a list containing multiple imputation df
  # model_name: name of the model and folder to save file
  # missing_data_name: prefix of the file to be saved
  # max_nway: maximum number of ways in joint distribution calculation
  
  # Prepare root to save the file
  root = paste('../../Results/',model_name,'/', missing_data_name, sep = '')
  missing_col = c(1,3,7,9,10,11)
  for (i in 1:max_nway) {
    # calculate statistics for that number of way
    output_list = calculate_statistics(imputation_list, n_way=i, missing_col)
    # extract result dof, mean_estimate, within group variance and between group variance
    dof = output_list[['dof']]
    mean_estimate = output_list[['q_bar']]
    within_group_var = output_list[['u_bar']]
    across_group_var = output_list[['b']]
    
    # save result
    dof_name = paste(root,'_dof_',i,'way.csv', sep = '')
    write.table(dof, dof_name, row.names = FALSE, sep = ',')
    
    q_bar_name = paste(root,'_q_bar_',i,'way.csv', sep = '')
    write.table(mean_estimate, q_bar_name, row.names = FALSE, sep = ',')
    
    u_bar_name = paste(root,'_u_bar_',i,'way.csv', sep = '')
    write.table(within_group_var, u_bar_name, row.names = FALSE, sep = ',')
    
    b_name = paste(root,'_b_',i,'way.csv', sep = '')
    write.table(across_group_var, b_name, row.names = FALSE, sep = ',')
  }
}


