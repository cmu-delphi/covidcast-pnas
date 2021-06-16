find_common_forecast_dates <- function(df) {
  # Get forecast dates for each forecaster
  forecast_dates_list <- lapply(unique(df$forecaster), 
                                FUN = function(forecaster) {
                                  filter(df, forecaster == !!forecaster) %>%
                                    pull(forecast_date) %>% 
                                    unique()
                                  }
                                )
  # Take the intersection
  forecast_dates <- as.Date(Reduce(lubridate::intersect, forecast_dates_list), 
                            origin = "1970-01-01")
  return(forecast_dates)
}



# Compute area under curve.
auc <- function(preds, actual, alpha = seq(.01, 1, by = .01)) {
  power <- roc(preds, actual, alpha) %>% arrange(alpha) 
  
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
  for (ii in seq_along(alpha)) {
    a <- alpha[ii]
    t = if(a == 1) 0 else pred_neg[floor(n_neg * (1 - a))]
    power[ii] <- sum(pred_pos > t) / n_pos
  }
  return(data.frame(alpha = alpha, result = power))
}