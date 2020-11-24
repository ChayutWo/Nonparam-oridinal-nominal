load_result <- function(model_name, data_name, n_way){
  # load result from the provided model, data name and the number of way in joint distribution
  # model_name: name of imputation model (MICE, CART, FOREST, etc)
  # data_name: type of missing mechanism (MCAR_45, MAR_30, etc)
  # n_way: the number of way in joint distribution to be consider

  # return: output_list
  # a list comprising of mean estimate (q_bar), within group variance (u_bar), 
  # between group variance (b) and degree of freedom (dof)
  
  
  # matrix for storing output data
  n_replicates = 500
  
  if (n_way == 1) {
    size = 54
  }else if(n_way == 2){
    size = 3149
  }else if(n_way == 3){
    size = 79653
  }
  # degree of freedom
  DOF = matrix(NA, nrow = n_replicates, ncol = size)
  # mean estimate
  Q_BAR = matrix(NA, nrow = n_replicates, ncol = size)
  # within group variance
  U_BAR = matrix(NA, nrow = n_replicates, ncol = size)
  # between group variance
  B = matrix(NA, nrow = n_replicates, ncol = size)
  
  savepath = paste('../../Results/',model_name,'/',data_name,'/', sep = '')
  for (i in 1:n_replicates) {
    # loop over dataset 1:n_replicates
    root = paste(savepath, data_name, '_', i, sep = '')
    dof_name = paste(root, '_dof_', n_way, 'way.csv', sep = '')
    q_bar_name = paste(root, '_q_bar_', n_way, 'way.csv', sep = '')
    u_bar_name = paste(root, '_u_bar_', n_way, 'way.csv', sep = '')
    b_name = paste(root, '_b_', n_way,'way.csv', sep = '')
    # read result from that dataset
    dof = read_csv(dof_name)
    q_bar = read_csv(q_bar_name)
    u_bar = read_csv(u_bar_name)
    b = read_csv(b_name)
    # collect results within matrix
    DOF[i,] = t(dof)
    Q_BAR[i,] = t(q_bar)
    U_BAR[i,] = t(u_bar)
    B[i,] = t(b)
  }
  output_list = list(Q_BAR, U_BAR, B, DOF)
  names(output_list) = c('q_bar', 'u_bar', 'b', 'dof')
  print(paste('>> finish loading result - model:', model_name,', dataset:', data_name,', n way:', n_way))
  return(output_list)
}