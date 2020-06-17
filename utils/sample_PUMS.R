sample_PUMS <- function(n){
  # Sample subset observations of PUMS 
  # n: number of observations to be sampled

  # return: df
  # df: the sampled observations from the full dataset

  # load dataset: df
  load('../../Datasets/ordinalPUMS.Rdata')
  
  # sample n rows from the full dataset
  sample <- sample(nrow(df), size = n)
  df <- df[sample,]
  
  return(df)
}