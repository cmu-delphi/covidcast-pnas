# Compute area under curve.
auc <- function(preds,actual,alpha = seq(.01,1,by = .01)){
  n_pos = sum(actual == 1,na.rm = T)
  n_neg = sum(actual == 0,na.rm = T)
  
  pred_pos <- preds[actual == 1]
  pred_neg <- preds[actual == 0]
  
  power <- roc(preds,actual,alpha) %>% arrange(alpha) 
  
  slider::slide_dbl(power,
                    .before = 1,
                    .f = ~ .x$result[2] * (.x$alpha[2] - .x$alpha[1])) %>%
    sum(na.rm = T)
}


# Compute a receiver operator curve (ROC curve).
# The ROC curve describes the power for a given level of type I error.
# alpha: level of type I error
roc <- function(preds,actual,alpha){
  pred_pos <- sort(preds[actual == 1])
  pred_neg <- sort(preds[actual == 0])
  n_pos = length(pred_pos)
  n_neg = length(pred_neg)
  
  power <- numeric(length(alpha))
  for(ii in 1:length(alpha)){
    a <- alpha[ii]
    t = if(a == 1) 0 else pred_neg[floor(n_neg * (1 - a))]
    power[ii] <- sum(pred_pos > t)/n_pos
  }
  return(data.frame(alpha = alpha,result = power))
}

# Aggregation function is area under the receiver-operator curve (ROC) curve,
# aka area under curve (AUC). x is a vector of (signed) errors, i.e.
# predicted values minus actual values, so that
# x < 0 means the truth was 1 and 1 + x was the prediction, and
# x > 0 means the truth was 0 and x was the prediction.
auc_aggr <- function(x){
  pred_pos <- sort(1 + x[x < 0])
  pred_neg <- sort(x[x > 0])
  
  n_pos = length(pred_pos)
  n_neg = length(pred_neg)
  
  if(n_pos == 0 | n_neg == 0){NA} else{
    # Same as sum(outer(pred_neg,pred_pos, "<")).
    # Slower, but takes less memory.
    sapply(pred_pos,FUN = function(p){max(which(p >= pred_neg))} ) %>%
      pmax(.,0) %>% sum() / (n_pos*n_neg)
  }
}

roc_aggr <- function(x,alpha){
  actual <- ifelse(x > 0,0,1)
  preds <-  ifelse(x > 0,x,1 + x)
  roc(preds,actual,alpha)
}

logloss <- function(preds,actual,pad = 0){
  ifelse(
    preds == actual, 
    0,  
    ifelse(preds == (1 - actual), 
           -log(pad),
           -(actual*log(pad + preds) + (1 - actual)*log(1 - preds + pad))
    )
  )
}


