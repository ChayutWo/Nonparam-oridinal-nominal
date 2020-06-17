make_MCAR <- function(df, missing_prob){
  # Make some prespecified columns MCAR
  # df: fully observed dataset
  # missing_prob: the probability of missing 

  # return: df_observed
  # df_observed: df with missing values in the missing_col
  
  n = dim(df)[1]
  # columns to be made MCAR
  missing_col = c(1,3,7,9,10,11)
  
  # create MCAR scneario with missing_prob chance of missing: df_observed
  df_observed <- df
  for (col in missing_col) {
    missing_ind <- rbernoulli(n, p = missing_prob)
    df_observed[missing_ind, col] <- NA
  }
  return(df_observed)
}