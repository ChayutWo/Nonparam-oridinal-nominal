DP_imputation <- function(df_observed, n_imputations){
  # Perform missing data imputation using DPMPM approach
  # df_observed: observed dataset
  # n_imputations: number of imputed dataset that want to generate

  # return: imputation_list
  # a list comprising of imputed datasets
  N = 40
  Mon = 10000
  B = 10000
  thin.int = 5
  
  # 1. Create and initialize the Rcpp_Lcm model object
  model = CreateModel(X = df_observed, MCZ = NULL, K = N, Nmax = 0,
                      aalpha = 0.25, balpha = 0.25, seed = 0)
  # 2. Set tracer
  model$SetTrace(c('k_star', 'psi', 'ImputedX', 'alpha'),Mon)
  
  # 3. Run model using Run(burnin, iter, thinning)
  model$Run(B,Mon,thin.int)
  
  # Extract results and format output
  output <- model$GetTrace()
  k_star <- output$k_star
  psi <- output$psi
  imputed_df <- output$ImputedX
  alpha <- output$alpha
  
  #retrieve parameters from the final iteration 
  result <- model$snapshot
  
  #convert ImputedX matrix to dataframe, using proper factors/names etc. 
  ImputedX <- GetDataFrame(result$ImputedX,df)
  
  # extract 5 imputed dataset from DP model
  imputation_index = as.integer(seq(1,dim(imputed_df)[1], length.out = n_imputations))
  imputation_list = list()
  levels = c(7,7,7,19,5,4,7,2,17,3,13)
  for (i in 1:length(imputation_index)) {
    index = imputation_index[i]
    # need to plus 1 here because the class index of DP function starts at 0
    d = imputed_df[index,] + 1
    dim(d) = dim(t(df_observed))
    d = data.frame(t(d))
    colnames(d) = colnames(df_observed)
    # format columns of d
    for (col_index in 1:ncol(df_observed)) {
      d[,col_index] = factor(d[,col_index], levels = 1:levels[col_index], ordered = TRUE)
    }
    imputation_list[[i]] = d
  }

  return(imputation_list)
}