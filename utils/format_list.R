format_list <- function(input_list){
  # format output list of results from different models into a dataframe
  # input_list: an output list of results from different models
  
  # return: df
  # a dataframe which each column is the result from each model

  models = c('MICE_NOM', 'MICE','CART', 'MICE_RF', 'FOREST', 'GAIN_CAT', 'DP', 'PROBIT')
  df = cbind(input_list[['MICE_NOM']],input_list[['MICE']], input_list[['CART']], input_list[['MICE_RF']],
             input_list[['FOREST']],  input_list[['GAIN_CAT']], input_list[['DP']], 
             input_list[['PROBIT']])
  colnames(df) = c('MI-Multireg', 'MI-Polr', 'MI-Cart', 'MI-Forest', 'missForest', 'GAIN', 
                   'MI-DPMPM', 'MI-DPMMVN')
  
  #models = c('MICE_NOM', 'MICE', 'CART', 'FOREST', 'GAIN', 'GAIN_DIFF', 'GAIN_CAT', 'GAIN_CAT_DIFF', 'DP', 'PROBIT')
  #df = cbind(input_list[['MICE_NOM']],input_list[['MICE']], input_list[['CART']], input_list[['FOREST']], 
  #           input_list[['GAIN']],input_list[['GAIN_DIFF']], input_list[['GAIN_CAT']],input_list[['GAIN_CAT_DIFF']], 
  #           input_list[['DP']], input_list[['PROBIT']])
  #colnames(df) = c('MI-nom', 'MI-ord', 'MI-cart', 'Forest', 'GAIN', 'GAIN_diff', 'GAINcat', 'GAINcat_diff', 'DPMPM', 'DPMMVN')
  return(df)
}
