sampleMAR45 <- function(n){
  # Sample subset observations of PUMS and make some prespecified columns MAR 45%
  # n: number of observations to be sampled

  # return: df, df_observed
  # df: the sampled observations from the full dataset
  # df_observed: df with missing values in the missing_col
  # load dataset: df
  load('../../Datasets/ordinalPUMS.Rdata')
  sample <- sample(nrow(df), size = n)
  df <- df[sample,]
  
  # create MCAR scneario with 30% chance of missing: df_observed
  missing_prob = 0.45
  df_observed <- df
  missing_col = c(1,3,7,9,10,11)
  
  # Make VEH and WKL MCAR
  missing_col_MCAR = c(1,10)
  for (col in missing_col_MCAR) {
    missing_ind <- rbernoulli(n,p = missing_prob)
    df_observed[missing_ind, col] <- NA
  }
  
  # Make the rest MAR
  numeric_df = sapply(df, as.numeric)
  normalized_df = t(t(numeric_df-1)/(apply(numeric_df, MARGIN = 2, FUN = max)-1))
  missing_col_MAR = c(3,7,9,11)
  fully_observed_col = c(2,4,5,6,8)
  beta_NP = c(0, 15, -10, 14, 0)
  beta0_NP = -1
  beta_SCHL = c(2.5, -10, 2.5, 0, 0)
  beta0_SCHL = -1
  beta_AGEP = c(4.5, 6, 0, -10, 0)
  beta0_AGEP = -1
  beta_PINCP = c(-10, 0, 4.5, 4.5, 0)
  beta0_PINCP = -1
  
  # missing probability for NP
  prob_NP = apply(t(t(normalized_df[, fully_observed_col])*beta_NP), MARGIN = 1, sum)+beta0_NP
  prob_NP = exp(prob_NP)/(exp(prob_NP)+1)
  indicator = rbernoulli(n, p = prob_NP)
  df_observed[indicator, missing_col_MAR[1]] <- NA
  
  # missing probability for SCHL
  prob_SCHL = apply(t(t(normalized_df[, fully_observed_col])*beta_SCHL), MARGIN = 1, sum) + beta0_SCHL
  prob_SCHL = exp(prob_SCHL)/(exp(prob_SCHL)+1)
  indicator = rbernoulli(n, p = prob_SCHL)
  df_observed[indicator, missing_col_MAR[2]] <- NA
  
  # missing probability for AGEP
  prob_AGEP = apply(t(t(normalized_df[, fully_observed_col])*beta_AGEP), MARGIN = 1, sum) + beta0_AGEP
  prob_AGEP = exp(prob_AGEP)/(exp(prob_AGEP)+1)
  indicator = rbernoulli(n, p = prob_AGEP)
  df_observed[indicator, missing_col_MAR[3]] <- NA
  
  # missing probability for PINCP
  prob_PINCP = apply(t(t(normalized_df[, fully_observed_col])*beta_PINCP), MARGIN = 1, sum) + beta0_PINCP
  prob_PINCP = exp(prob_PINCP)/(exp(prob_PINCP)+1)
  indicator = rbernoulli(n, p = prob_PINCP)
  df_observed[indicator, missing_col_MAR[4]] <- NA

  
  output_list <- list(df, df_observed)
  names(output_list) <- c('df', 'df_observed')
  return(output_list)
}