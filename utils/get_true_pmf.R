get_true_pmf <- function(n_way){
  # Calculate true pmf from the whole dataset
  # n_way: the number of way in joint distribution to be consider

  # return: TRUE_Q
  # TRUE_Q: array of true joinf pmf of specified number of way of variables with missing values
  
  load('../../Datasets/ordinalPUMS.Rdata')
  # Identify missing column
  missing_col = c(1,3,7,9,10,11)
  # Identify possible combinations
  combinations = combn(1:11, n_way)
  # array for storing true joint pmf
  TRUE_Q = c()
  for (i in 1:(dim(combinations)[2])) {
    variables = combinations[, i]
    if (any(missing_col %in% variables)) {
      true_cont_table = c(table(df[, variables]))
      true_cont_table = true_cont_table/sum(true_cont_table)
      TRUE_Q = c(TRUE_Q, true_cont_table)
    }
  }
  print('>> finish computing true pmf from original PUMS dataset')
  return(TRUE_Q)
}