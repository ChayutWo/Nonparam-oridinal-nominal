sampleMCAR <- function(n, missing_prob){
  # Sample subset observations of PUMS and make some prespecified columns MCAR
  # n: number of observations to be sampled
  # missing_col: a vector of columns to be made MCAR
  # missing_prob: the probability of missing 

  # return: df, df_observed
  # df: the sampled observations from the full dataset
  # df_observed: df with missing values in the missing_col
  
  # load dataset: df
  load('../../Datasets/ordinalPUMS.Rdata')
  
  # columns to be made MCAR
  missing_col = c(1,3,7,9,10,11)
  
  # sample n rows from the full dataset
  sample <- sample(nrow(df), size = n)
  df <- df[sample,]
  
  # create MCAR scneario with missing_prob chance of missing: df_observed
  df_observed <- df
  for (col in missing_col) {
    missing_ind <- rbernoulli(n, p = missing_prob)
    df_observed[missing_ind, col] <- NA
  }
  output_list <- list(df, df_observed)
  names(output_list) <- c('df', 'df_observed')
  return(output_list)
}