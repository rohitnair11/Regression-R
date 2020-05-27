alda_regression <- function(x_train, x_test, y_train, regression_type){
  # Perform regression (linear/lasso)
  
  # Inputs:
    # x_train: training data frame(19 variables, x1-x19)
    # x_test: test data frame(19 variables, x1-x19)
    # y_train: dependent variable, training data (vector, continous type)
    # regression_type: specifies type of regression, string variable, can be of type 'linear' or 'lasso'
  
  # Output:
    # A list with two elements, first element = model generated, second element = predictions on test data (vector) 
  
  # Linear Regression
  if(regression_type == 'linear'){ 
    fit = glmnet(x_train, y_train, lambda = 0)
    predict(fit, newx = x_test)
    
  # Lasso Regression
  }else{
    cvfit = cv.glmnet(x_train, y_train, type.measure = "mse")
    predict(cvfit, newx = x_test)
  }
  
}

alda_regression_compare <- function(x_train, x_test, y_train) {
  
  cvfit = cv.glmnet(x_train, y_train, type.measure = "mse")
  coef(cvfit, s = "lambda.min")
  
}



