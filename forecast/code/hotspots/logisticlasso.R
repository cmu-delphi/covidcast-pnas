#------------------------------------------------------------------------------#
# A wrapper around glmnet::glmnet.
#
# Parameters
# -- x Matrix of predictors.
# -- y Vector of responses.
#
# TODO:
# -- (1) Fix this to match cross validated logistic lasso returned object.
#------------------------------------------------------------------------------#
lgstlasso <- function(x, y, lambda, penalty.factor = rep(1,ncol(x)), weights = NULL, intercept = TRUE,
                       standardize = TRUE,verbose = FALSE){

  # Setup up x,y, weights
  a = setup_xy(x,y, weights)
  x = a$x
  y = a$y
  weights = a$weights

  # Solve the logistic lasso problem
  glmnet.control(fdev = 0,devmax = 1) # Solve exactly the problem we're asking for.
  train_obj <- glmnet(x = x, y = y, family = "binomial", lambda = lambda,
                         penalty.factor = penalty.factor,weights = weights,
                         standardize = standardize, intercept = intercept)
  return(train_obj)
}

predict.lgstlasso <- function(object,newx,s = NULL,type = "response"){
  # TODO: figure out why using predict::predict.glmnet here
  #       gives predictions on the linear scale.
  predict(object,newx,s,type)
}
