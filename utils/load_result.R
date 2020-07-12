load_result <- function(model_name, data_name, n_way){
  # load result from the provided model, data name and the number of way in joint distribution
  # model_name: name of imputation model (MICE, CART, FOREST, etc)
  # data_name: type of missing mechanism (MCAR_45, MAR_30, etc)
  # n_way: the number of way in joint distribution to be consider

  # return: output_list
  # a list comprising of mean estimate (q_bar), within group variance (u_bar), 
  # between group variance (b) and degree of freedom (dof)
  
  
  # matrix for storing output data
  # degree of freedom
  DOF = c()
  # mean estimate
  Q_BAR = c() 
  # within group variance
  U_BAR = c()
  # between group variance
  B = c()
  
  savepath = paste('../../Results/',model_name,'/',data_name,'/', sep = '')
  for (i in 1:100) {
    # loop over dataset 1:100
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
    DOF = rbind(DOF, t(dof))
    Q_BAR = rbind(Q_BAR, t(q_bar))
    U_BAR = rbind(U_BAR, t(u_bar))
    B = rbind(B, t(b))
  }
  output_list = list(Q_BAR, U_BAR, B, DOF)
  names(output_list) = c('q_bar', 'u_bar', 'b', 'dof')
  print(paste('>> finish loading result - model:', model_name,', dataset:', data_name,', n way:', n_way))
  return(output_list)
}