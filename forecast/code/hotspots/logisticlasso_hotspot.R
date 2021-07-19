#------------------------------------------------------------------------------#
# A simple autoregressive hotspot forecaster using logistic lasso.
#
# Parameters
# -- df Data frame of signal values to use for forecasting, of the format
#       returned by [covidcast::covidcast_signals()].
# -- forecast_date
# -- signals. Tibble with columns `data_source` and `signal` that specifies
#     which variables are being fetched from the COVIDcast API. The first
#     row of signal is taken to be the response.
#     Optional column of `start_day`, which can be a Date or a string in
#     YYYY-MM-DD form, indicating the earliest date from which data should be
#     taken. Importantly, `start_day` can also be a function (represented as a list
#     column) that takes a forecast date and returns a start date for model
#     training (again, Date object or string in the form "YYYY-MM-DD").
# -- incidence_period
# -- ahead
# -- n Size of the training window.
# -- lags
# -- hotspot_detector. (???)
# -- cv_type One of "forward" or "random", indicating the type of
#     cross-validation to perform.
# -- model_dir. If not NULL, then a relative path to a directory where fitted
#               models should be saved.
# -- verbose
# -- ... Additional arguments. lambda,
#
# TODO
#   (1) Compare with `quantgen_forecaster()` to make sure nothing has been
#       unintentionally changed.
#------------------------------------------------------------------------------#

lgstlasso_hotspot_forecaster <- function(df_list, forecast_date,
                                         ahead,
                                         hotspot_detector,
                                         n = 28,
                                         lags = 0,
                                         feature_type = c("both", "pct_difference","raw"),
                                         cv_type = c("forward","random"),
                                         zero_impute= NULL,
                                         resample=NULL,
                                         verbose = F, model_dir = NULL,
                                         ...){

  # TODO: Check lags vector or list.
  if (!is.list(lags)) lags = rep(list(lags), length(df_list))

  # Define dt by flipping the sign of lags, include dt = +ahead as a response
  # shift, for each ahead value, for convenience later
  dt <- lapply(lags,"-")

  # Data frame with hotspots, defined in terms of the response.
  df_hotspot <- hotspot_detector(df_list[[1]])

  # Data frames with pct changes.
  feature_type = match.arg(feature_type)
  if(feature_type %in% c("both","pct_difference")){
    # TODO: Clean this up. Names should be better,
    #       and possibly the functionality should be siphoned off
    #       into a separate function.
    hotspot_lag = environment(hotspot_detector)[["hotspot_lag"]]
    FUN = function(x,n){
      alden_pct_change(x,n) %>%
        select(-c(value,lag_value)) %>%
        rename(value = pct_change) %>%
        mutate(signal = paste(signal, "pct_change",sep = "_"))
    }
    df_pct_change <- lapply(df_list,FUN,n = hotspot_lag)
    if(feature_type == "both"){
      df <- c(df_list,df_pct_change)
      dt <- rep(dt,2)
    } else if(feature_type == "pct_difference"){
      df <- df_pct_change
    }
  }

  # Append shifts, and aggregate into wide format
  df_response_wide <- covidcast::aggregate_signals(df_hotspot,dt = ahead, format = "wide")
  df_features_wide <- covidcast::aggregate_signals(df, dt = dt, format = "wide")
  df_wide <- full_join(df_response_wide,df_features_wide, by = c("geo_value","time_value"))

  # Separate out into feature data frame.
  # TODO: Include option to featurize if we need to.
  df_features = df_wide %>%
    select(geo_value, time_value, tidyselect::matches("^value(\\+0|-)"))
  feature_end_date = df_features %>%
    summarize(max(time_value)) %>% pull()

  # Identify params for logistic lasso training and prediction functions
  params = list(...)

  cv = (is.null(params[["lambda"]]) || length(params$lambda) > 1)
  if (cv) {
    if (length(no_pen_vars) == ncol(df_features %>% select(-c(geo_value,time_value))))
    {
      cv <- FALSE
      params[["lambda"]] <- 0
    }

    train_fun = cv_lgstlasso
    predict_fun = predict.cv_lgstlasso
  }
  else {
    train_fun = lgstlasso
    predict_fun = predict.lgstlasso
  }

  train_names = names(as.list(args(train_fun)))
  predict_names = names(as.list(args(predict_fun)))
  train_params = params[names(params) %in% train_names]
  predict_params = params[names(params) %in% predict_names]

  # Check cv_type
  if (cv) cv_type = match.arg(cv_type)

  # Test objects that remain invariant over ahead values

  # For each (geo_value,feature) pair, the test_x should be
  # the value of the feature at the most recently available time_value.
  # This is equivalent to last-value-forward imputation.
  #
  # TODO: I don't know how this logic will work if the feature has never been available.
  #       Also, this is pretty laborious. Maybe there is a simpler
  #       way to write it?

  # Temporal test info.
  newx_info = df_features %>%
    group_by(geo_value) %>%
    summarise(across(tidyselect::matches("^value(\\+0|-)"),
                     ~ max(time_value[!is.na(.x)])), .groups = "drop_last") %>%
    tidyr::pivot_longer(-geo_value,names_to = "feature_name", values_to = "time_value")

  # Test features
  df_features_test = left_join(newx_info,
                               df_features %>%
                                 filter(time_value %in% unique(newx_info$time_value)) %>%
                                 tidyr::pivot_longer(-c(geo_value,time_value),
                                 names_to = "feature_name", values_to = "value"),
                               by = c("feature_name","geo_value","time_value")
  ) %>% select(-time_value) %>%
    tidyr::pivot_wider(names_from = "feature_name",values_from = "value")

  test_geo_value = df_features_test %>% pull(geo_value)
  newx = df_features_test %>% select(-geo_value) %>% as.matrix()


  # Addison's code for zero imputation.
  if (!is.null(zero_impute)) {
    impute_cols = grep(zero_impute, colnames(newx))
    impute_mat = newx[, impute_cols]
    impute_mat[is.na(impute_mat)] = 0
    newx = newx[, -impute_cols]
    newx = cbind(newx, impute_mat)
  }

  # Addison's code for bootstrapping.
  if (!is.null(resample)) {
    resample_cols = grep(resample$substring, colnames(newx))
    resampled_mat = newx[, resample_cols]
    colnames(resampled_mat) = paste0(colnames(newx)[resample_cols], '_resampled')
    if (resample$overwrite) {
      newx = newx[, -resample_cols]
    }
    newx = cbind(newx, resampled_mat)
  }

  # Loop over ahead values, fit model, make predictions
  result = vector(mode = "list", length = length(ahead))
  model = vector(mode = "list", length = length(ahead))

  for (i in 1:length(ahead)) {
    a = ahead[i]
    if (verbose) cat(sprintf("%s%i", ifelse(i == 1, "\nahead = ", ", "), a))

    # Training end date
    response_end_date = df_wide %>%
      select(time_value, tidyselect::starts_with(sprintf("value+%i:", a))) %>%
      tidyr::drop_na() %>%
      summarize(max(time_value)) %>% pull()
    train_end_date = min(feature_end_date, response_end_date)

    # Number of days needed for training + validation
    if(cv & cv_type == "forward"){
      nfolds = ifelse(!is.null(params$nfolds), params$nfolds, 5)
      ntrain =  n + a + nfolds # Total number of days, needed for training + validation.
    } else {
      ntrain = n
    }

    # Training x
    x = df_features %>%
      filter(between(time_value,
                     train_end_date - ntrain + 1,
                     train_end_date)) %>%
      select(-c(geo_value, time_value)) %>% as.matrix()

    # Addison's code for zero imputation
    if (!is.null(zero_impute)) {
      impute_cols = grep(zero_impute, colnames(x))
      impute_mat = x[, impute_cols]
      impute_mat[is.na(impute_mat)] = 0
      x = x[, -impute_cols]
      x = cbind(x, impute_mat)
    }

    # Addison's code for bootstrapping
    if (!is.null(resample)) {
      resample_cols = grep(resample$substring, colnames(x))
      resampled_mat = x[sample(1:nrow(x), nrow(x), replace=TRUE), resample_cols]
      colnames(resampled_mat) = paste0(colnames(x)[resample_cols], '_resampled')
      if (resample$overwrite) {
        x = x[, -resample_cols]
      }
      x = cbind(x, resampled_mat)
    }

    # Training y.
    y = df_wide %>%
      filter(between(time_value,
                     train_end_date - ntrain + 1,
                     train_end_date)) %>%
      select(tidyselect::starts_with(sprintf("value+%i:", a))) %>% pull()

    # Define forward-validation folds, if we need to
    if (cv & cv_type == "forward") {

      # Training time values
      train_time_value = df_wide %>%
        filter(between(time_value,
                       train_end_date - (nfolds + a + n) + 1,
                       train_end_date)) %>%
        select(time_value) %>% pull()

      train_test_inds = list(train = vector(mode = "list", length = nfolds),
                             test = vector(mode = "list", length = nfolds),
                             fit = c())
      for (k in 1:nfolds) {
        validation_forecast_date = train_end_date - nfolds + k
        train_test_inds$train[[k]] = which(
          between(train_time_value,
                  validation_forecast_date - (a + n) + 1,
                  validation_forecast_date - a
          )
        )
        train_test_inds$test[[k]] = which(
          train_time_value == validation_forecast_date)
      }
      train_test_inds$fit = which(
        between(train_time_value,
                train_end_date - n + 1,
                train_end_date
        )
      )
      train_params$train_test_inds = train_test_inds
    }

    # Add training x and y to training params list, fit model
    train_params$x = x
    train_params$y = y;

    train_obj = do.call(train_fun, train_params)

    predict_params$object = train_obj
    predict_params$newx = newx
    predict_params$type = "response"
    predict_mat = do.call(predict_fun, predict_params)

    # Do some wrangling to get it into evalcast "long" format
    predict_df = data.frame(geo_value = test_geo_value,
                            point = predict_mat[,1]) %>%
      tidyr::pivot_longer(cols = -geo_value,
                   names_to = "quantile",
                   values_to = "value") %>%
      mutate(geo_value = as.character(geo_value),
             quantile = NA,
             ahead = a)

    # TODO: Find a better way to keep around the fitted model information.
    model[[i]] = list(train_obj = train_obj,
                      ahead = a,
                      forecast_date = forecast_date)
    result[[i]] = predict_df
  }
  if (verbose) cat("\n")

  # Save fitted models
  if(!is.null(model_dir)) {
    if(!file.exists(model_dir)) dir.create(model_dir,recursive = TRUE)
    saveRDS(model, file = here::here(model_dir, "fitted_model",
                                     sprintf("%s.rds", forecast_date)))
  }

  # Collapse predictions into one big data frame, and return
  return(dplyr::bind_rows(result))
}
