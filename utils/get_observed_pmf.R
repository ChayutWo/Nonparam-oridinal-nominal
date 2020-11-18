get_observed_pmf <- function(n_way){
  # Calculate observed pmf from all 500 fully observed datasets
  # n_way: the number of way in joint distribution to be consider

  # return: OBSERVED_Q
  # OBSERVED_Q: array of observed joinf pmf of specified number of way of variables with missing values
  source("../../utils/load_data.R")
  OBSERVED_Q = c()
  n_replicates = 500
  filename_root = 'fully_observed/fully_observed_'
  missing_col = c(1,3,7,9,10,11)
  combinations = combn(1:11, n_way)
  for (id in 1:n_replicates) {
    # load fully observed data
    filename = paste(filename_root, id, sep = '')
    observed_df = load_data(filename)
    observed_pmf = c()
    for (i in 1:(dim(combinations)[2])) {
      variables = combinations[, i]
      if (any(missing_col %in% variables)) {
        # compute observed pmf
        observed_cont_table = c(table(observed_df[, variables]))
        observed_cont_table = observed_cont_table/sum(observed_cont_table)
        observed_pmf = c(observed_pmf, observed_cont_table)
      }
    }
    # store it into rows of OBSERVED_Q
    OBSERVED_Q = rbind(OBSERVED_Q, observed_pmf)
  }
  print('>> finish computing observed pmf from fully observed data')
  return(OBSERVED_Q)
}