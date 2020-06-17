load_data <- function(filename){
  # load data from the provided filename and format it according to PUMS schema
  # filename: filename to be loaded including root

  # return: d
  # d: dataframe with columns of ordinal variables
  
  # levels of each of the columns
  levels = c(7,7,7,19,5,4,7,2,17,3,13)
  # read file
  full_name = paste('../../Datasets/', filename,'.csv',sep = '')
  d <- read_csv(full_name)
  d <- data.frame(d)
  # format to be ordinal variables
  for (col_index in 1:ncol(d)) {
    d[,col_index] = factor(d[,col_index], levels = 1:levels[col_index], ordered = TRUE)
  }
  return(d)
}