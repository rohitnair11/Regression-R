# clear workspace
rm(list=ls(all=T))

# source hw3.R
source('./regression.R')

# set your working directory
# setwd()

# install all necessary packages
required_packages = c('glmnet')
for(package in required_packages){
  if(!(package %in% installed.packages())){
    install.packages(package, dependencies = T)
  }   
}


# load the packages
library('glmnet') # for linear and lasso regression

# set a seed for reproducibility
set.seed(2)
############################################################################################################
# Helper functions
load_data <- function(data_folder='./data/'){
  # this method will read data and return list containing two data frames:

  # first 19 columns (x1-x19) are attributes, last column (y) is your dependent variable (continuous))
  
  train_df <- read.csv(paste0(data_folder, 'regression-train.csv'), header=T)
  test_df <- read.csv(paste0(data_folder, 'regression-test.csv'), header=T)
  
  return(list(train_df, test_df))
}

calculate_rmse <- function(y_true, y_pred){
  # Inputs:
  # y_true: ground truth dependent variable values, of type vector
  # y_pred: prediction outcomes from any regression method, with the same length as y_true
  
  # Outputs:
  # a single value of type double, with the RMSE value
  return(sqrt(sum(y_true - y_pred)^2/length(y_true)))
}

##########################################################################################################
# Load data
reg_data <- load_data(data_folder='./data/')
reg_train_df <- reg_data[[1]]
reg_test_df <- reg_data[[2]]

############################################################################################################ 
# Learning and parameter tuning 

###############################################
# Regression

# slr
# simple_linear_regression_result: list, first argument is the model, second argument are the predicted values
simple_linear_regression_result <- alda_regression(x_train=as.matrix(reg_train_df[,-20]),x_test=as.matrix(reg_test_df[,-20]), 
                                      y_train=reg_train_df[,20], regression_type='linear') 

print(calculate_rmse(reg_test_df[,20],simple_linear_regression_result))
# This should return a result of 1300.289

# lasso
# lasso_regression_result: list, first argument is the model, second argument are the predicted values
lasso_regression_result <- alda_regression(x_train=as.matrix(reg_train_df[,-20]),x_test=as.matrix(reg_test_df[,-20]), 
                                              y_train=reg_train_df[,20], regression_type = 'lasso')

print(calculate_rmse(reg_test_df[,20],lasso_regression_result))
# This should return a result of 321.3975, or 321.3463.
