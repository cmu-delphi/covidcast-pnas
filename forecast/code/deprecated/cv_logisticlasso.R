# Cross validation for the logistic lasso. 
#
# NOTE: We cannot simply use glmnet::cv.glmnet() because cv.glmnet will train 
# on all data except the held-out fold. This is not appropriate for e.g.
# forward validation.
cv_lgstlasso <- function(x, y, lambda = NULL, nlambda = 30,
                         lambda.min.ratio = 1e-3, weights = NULL, 
                         no_pen_vars = c(), nfolds = 5,
                         train_test_inds = NULL, intercept = TRUE, 
                         standardize = TRUE, verbose = FALSE){
  p <- ncol(x)
  penalty.factor = rep(1,p)
  if(length(no_pen_vars) > 0) penalty.factor[no_pen_vars] = 0
  
  # Set up the lambda sequence, if we need to
  if (is.null(lambda)) {
    lambda <- get_lambda_seq(x = x, y = y, penalty.factor = penalty.factor,
                             nlambda = nlambda, 
                             lambda.min.ratio = lambda.min.ratio, weights = weights,
                             standardize = standardize, intercept = intercept)
  }
  
  # Grab the specified training and test inds, or else build them
  if (!is.null(train_test_inds)) {
    train = train_test_inds$train
    test = train_test_inds$test
    fit = train_test_inds$fit
    nfolds = length(train)
  }
  else {
    folds = rep(1:nfolds, nrow(x))[sample(nrow(x))]
    train = test = vector(mode="list", length=nfolds)
    for (k in 1:nfolds) {
      train[[k]] = which(folds != k)
      test[[k]] = which(folds == k)
    }
    fit = 1:nrow(x)
  }
  
  yhat = array(NA, dim = c(nrow(x),length(lambda)))
  
  for (k in 1:nfolds) {
    if (verbose) cat(sprintf("CV fold %i ...\n", k))
    # Adjustment factor for lambda values, accounting for the differences in
    # training set sizes
    adj = length(train[[k]]) / length(fit)
    
    # Fit on training set
    obj = lgstlasso(x=x[train[[k]],,drop=FALSE], y=y[train[[k]]],
                    penalty.factor = penalty.factor,
                    lambda = lambda*adj,weights = weights[train[[k]]],
                    intercept = intercept, standardize = standardize)
    
    # Predict on test set.
    # Here, we need to gracefully handle errors in the case where
    # glmnet did not choose to return a fitted model for each lambda.
    yhat[test[[k]],] = 
      tryCatch(predict.lgstlasso(obj, x[test[[k]],,drop=FALSE], s = lambda*adj, type = "response"),
          error = function(e){matrix(NA,nrow = length(test[[k]]),ncol = length(lambda))}
      )
  }
  
  # Record CV errors, according to auc
  # TODO: a cosmetic change: find minimum of -auc, rather than maximum of auc.
  #       We are typically more used to minimizing loss.
  if (verbose) cat("Computing CV auc and optimum lambdas ...\n")
  cv_vec = array(NA,length(lambda))
  for(j in 1:length(lambda))
  {
    pred_indx_j = which(!is.na(yhat[,j]))
    cv_vec[j] = auc(yhat[pred_indx_j,j],y[pred_indx_j])
  }
  lambda_max = lambda[which.max(cv_vec)]
  
  # Fit lgstlasso on full training set, with optimal lambda.
  lgstlasso_obj = lgstlasso(x = x[fit,,drop=FALSE],y = y[fit],lambda = lambda_max,
                            penalty.factor = penalty.factor,
                            weights = weights, intercept = intercept,
                            standardize = standardize)
  obj = enlist(lgstlasso_obj,cv_vec,lambda_max,lambda)
  class(obj) = "cv_lgstlasso"
  return(obj)
}

predict.cv_lgstlasso <- function(object,newx,s = NULL,type = "response"){
  predict.lgstlasso(object$lgstlasso_obj,newx,s,type)
}

get_lambda_seq <- function(x, y, nlambda, lambda.min.ratio,
                           penalty.factor = rep(1,ncol(x)),
                           weights = NULL,
                           intercept = TRUE,standardize = TRUE){
  # Setup up x,y, weights
  a = setup_xy(x,y, weights)
  x = a$x
  y = a$y
  weights = a$weights
  
  # Get a sequence of lambda
  glmnet_obj <- glmnet::glmnet(x = x, y = y, family = "binomial", nlambda = nlambda, 
                         lambda.min.ratio = lambda.min.ratio,
                         weights = weights, penalty.factor = penalty.factor,
                         standardize = standardize, intercept = intercept)
  
  # If glmnet converged, return the lambda sequence it chose.
  # Otherwise, something weird is happening. Default to unpenalized regression.
  # TODO: check to make sure this is reasonable.
  return(if(!(all(glmnet_obj[["lambda"]] == Inf))) glmnet_obj[["lambda"]] else 0)
}
