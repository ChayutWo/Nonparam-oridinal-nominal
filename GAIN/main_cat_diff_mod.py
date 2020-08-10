'''
Main function for PUMS datasets.
Reference: J. Yoon, J. Jordon, M. van der Schaar, "GAIN: Missing Data
           Imputation using Generative Adversarial Nets," ICML, 2018.
Editor: Chayut Wongkamthong 05/07/2020
'''
# Necessary packages
from __future__ import absolute_import
from __future__ import division
from __future__ import print_function

import argparse
import numpy as np

# edited: overwrite data_loader
from missingdata_loader import data_loader
from gain_categorical_diff_mod import gain
from utils import rmse_loss

def main (args):
  '''Main function for PUMS datasets.
  
  Args:
    - missing_data_name: name of .csv file that contains data with missingness
    - data_name: name of .csv file that contains original data
    - batch:size: batch size
    - hint_rate: hint rate
    - alpha: hyperparameter, balance for two losses in generator
    - iterations: iterations
    
  Returns:
    - imputed_data_x: imputed data
    - rmse: Root Mean Squared Error
  '''

  data_name = args.data_name
  missing_data_name = args.missing_data_name
  num_imputations = args.num_imputations
  gain_parameters = {'batch_size': args.batch_size,
                     'hint_rate': args.hint_rate,
                     'alpha': args.alpha,
                     'iterations': args.iterations}
  
  # Load data and introduce missingness
  ori_data_x, miss_data_x, data_m = data_loader(missing_data_name, data_name)
  
  # Impute missing data
  for i in range(num_imputations):
    imputed_data_x = gain(miss_data_x, i, gain_parameters, filename = missing_data_name)
  
  return imputed_data_x

if __name__ == '__main__':  
  """
  Example call:
  python3 main.py --data_name PUMS01 --missing_data_name MCAR_PUMS01_30percent_alpha100 
  --num_imputations 5 --batch_size 128 --hint_rate 0.9 --alpha 100 --iterations 10000
  """
  # Inputs for the main function
  parser = argparse.ArgumentParser()
  parser.add_argument(
      '--data_name',
      default=None,
      type=str)
  parser.add_argument(
      '--missing_data_name',
      type=str)
  parser.add_argument(
      '--num_imputations',
      default=1,
      type=int
  )
  parser.add_argument(
      '--batch_size',
      help='the number of samples in mini-batch',
      default=256,
      type=int)
  parser.add_argument(
      '--hint_rate',
      help='hint probability',
      default=0.5,
      type=float)
  parser.add_argument(
      '--alpha',
      help='hyperparameter',
      default=100,
      type=float)
  parser.add_argument(
      '--iterations',
      help='number of training interations',
      default=500,
      type=int)
  
  args = parser.parse_args() 
  
  # Calls main function  
  imputed_data = main(args)
