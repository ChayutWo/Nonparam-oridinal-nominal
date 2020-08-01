format_list <- function(input_list){
  # format output list of results from different models into a dataframe
  # input_list: an output list of results from different models
  
  # return: df
  # a dataframe which each column is the result from each model

  models = c('MICE_NOM', 'MICE', 'CART', 'FOREST', 'GAIN', 'DP', 'PROBIT')
  df = cbind(input_list[['MICE_NOM']],input_list[['MICE']], input_list[['CART']], input_list[['FOREST']], 
             input_list[['GAIN']], input_list[['DP']], input_list[['PROBIT']])
  colnames(df) = c('MI-nom', 'MI-ord', 'MI-cart', 'Forest', 'GAIN', 'DPMPM', 'DPMMVN')
  return(df)
}
