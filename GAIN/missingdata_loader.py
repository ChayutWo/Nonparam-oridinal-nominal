'''
Data loader for PUMS dataset.
Author: Chayut Wongkamthong 07/05/2020
'''

# Necessary packages
import numpy as np
import pandas as pd


def data_loader (missing_data_name, data_name = None):
  '''Loads datasets.
  
  Args:
    - missing_data_name: name of .csv file that contains data with missingness
    - data_name: name of .csv file that contains original data
    
  Returns:
    data_x: original data
    miss_data_x: data with missing values
    data_m: indicator matrix for missing components
  '''

  # Load data with NA values
  file_name = '../Datasets/PUMS_missing/'+missing_data_name+'.csv'
  miss_data_x = pd.read_csv(file_name, index_col=False, na_values='NA', dtype=np.float64)
  miss_data_x = miss_data_x.values
  if data_name is not None:
    # Load data without NA values
    file_name = '../Datasets/PUMS_missing/'+data_name+'.csv'
    data_x = pd.read_csv(file_name, index_col=False, na_values='NA', dtype=np.float64)
    data_x = data_x.values
  else:
    data_x = None

  data_m = 1-np.isnan(miss_data_x)
  return data_x, miss_data_x, data_m