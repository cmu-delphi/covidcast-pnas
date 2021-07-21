# Find the set of forecast dates at which at forecasters made forecasts.
# -- df, a data frame with, at minimum, columns forecaster and forecast_date.
find_common_forecast_dates <- function(df){
  # Get forecast dates for each forecaster
  forecast_dates_list <- lapply(unique(df$forecaster), FUN = function(forecaster){
    filter(df,forecaster == !!forecaster) %>% pull(forecast_date) %>% unique()
  })
  
  # Take the intersection
  forecast_dates <- as.Date(Reduce(lubridate::intersect,forecast_dates_list), 
                            origin = "1970-01-01")
  return(forecast_dates)
}

# Clean a data frame for apples to apples comparison in evaluation.
# -- df, a data frame with the structure of evals. In other words, it must have
#        columns forecaster, geo_value, ahead, forecast_date, value, and actual.
clean_df <- function(df,
                     omitted_forecasters = "AR3GG3",
                     omitted_aheads = c(1:6,22),
                     omitted_forecast_dates = seq(lubridate::ymd("2021-01-01"),
                                                   lubridate::today(),
                                                   by = "day")){
  # Filter to desired forecasters, forecast dates, and aheads
  df <- df %>% filter(!(forecaster %in% omitted_forecasters) & 
                              !(ahead %in% omitted_aheads) &
                              !(forecast_date %in% omitted_forecast_dates))
  
  # Filter to common forecast dates
  common_forecast_dates <- find_common_forecast_dates(df)
  df <- df %>% filter(forecast_date %in% common_forecast_dates)
  
  # Retain forecast tasks which all forecasters have completed.
  na_tasks <- df %>% 
    filter(is.na(value) | is.na(actual)) %>%
    select(geo_value, forecaster, forecast_date, ahead) %>%
    select(-forecaster) %>% distinct()

  df <- df %>% anti_join(na_tasks)
  
}

# Get the set of locations at which all signals are available on all forecast dates.
get_common_locations <- function(list_of_signals,max_missing_permitted = 0){
  signals_df <- bind_rows(list_of_signals) %>% distinct()
  data <- covidcast_signals(data_source = signals_df$data_source,
                           signal = signals_df$signal,
                           start_day = min(forecast_dates),
                           end_day = max(forecast_dates),
                           geo_type = geo_type) %>%
    bind_rows() %>%
    latest_issue()
  
  # Number of observations for each signal, at each location.
  observations_per_signal <- data %>% 
    group_by(data_source,signal,geo_value) %>%
    summarize(n_obs = n(),.groups = "drop")
  
  # Maximum number of observations possible.
  max_obs = (max(forecast_dates) - min(forecast_dates) + 1)
  
  # Keep locations which are observed sufficiently often, for every signal.
  common_locations <- observations_per_signal %>% 
    filter(n_obs >= max_obs - max_missing_permitted) %>%
    group_by(geo_value) %>%
    summarize(n_signals = n(),.groups = "drop") %>%
    filter(n_signals == nrow(signals_df)) %>%
    pull(geo_value)
  
  return(common_locations)
}

# Parse feature names
parse_feature_name <- function(feature_name){
  # Abbreviate
  feature_name <- gsub("doctor-visits", "dv", feature_name)
  feature_name <- gsub("google-symptoms","gg",feature_name)
  feature_name <- gsub("smoothed","smth",feature_name)
  feature_name <- gsub("value|-csse|-survey|_num|_sum|_search","",feature_name)
  
  # Put lags at the end
  feature_name <- sapply(strsplit((feature_name),":"),FUN = function(str){
    if(length(str) == 1) str else paste(str[2],str[1],sep = "")
  })
  
  # Coerce to factor, put (Intercept) at the end
  pct_change_vars <- grep("pct_change",unique(feature_name),value = TRUE)
  feature_name <- factor(feature_name) %>% 
    forcats::fct_relevel(pct_change_vars,after = Inf) %>%
    forcats::fct_relevel("(Intercept)",after = Inf)
  
  feature_name
}

# Read all rds files from a directory into a list.
readRDS_from_dir <- function(dir){
  lapply(file.path(dir,list.files(dir,".rds")),readRDS)
}

# Create a long df containing coefficients from a list of saved models.
# The saved models each have are each a list, with elements:
# 
# --a: value of ahead at which the model was trained 
# --forecast_date: forecast date at which the model was trained
# --train_obj$lgstlasso_obj: Output of glmnet::glmnet.
# 
# TODO: this function is both slow and hacky. The only reason I haven't tended
# to it is not clear we will need it for any final version of code.
create_coef_df <- function(list_of_models){
  # Wrangle data into format suitable for easily making plots highlighting several
  # different variables.
  beta <- vector(mode = "list",length = length(list_of_models))
  for(ii in 1:length(list_of_models))
  {
    model_ii <- list_of_models[[ii]]
    beta[[ii]] <- vector(mode = "list",length = length(model_ii))
    for(jj in 1:length(model_ii)){
      train_obj <- model_ii[[jj]]$train_obj
      # TODO: get rid of this when we fix the inconsistencies in cv vs non-cv logistic lasso
      if("glmnet" %in% class(train_obj)){
        lgstlasso_obj <- train_obj
      } else{lgstlasso_obj <- train_obj$lgstlasso_obj}
      beta[[ii]][[jj]] <- 
        as.data.frame(t(as.matrix(glmnet::coef.glmnet(lgstlasso_obj)))) %>%
        mutate(ahead = model_ii[[jj]]$a,
               forecast_date = model_ii[[jj]]$forecast_date)
    }
    beta[[ii]] <- bind_rows(beta[[ii]])
  }
  coefs <- bind_rows(beta) %>% 
    pivot_longer(-c(forecast_date,ahead),
                 names_to = "feature_name",values_to = "value") %>%
    mutate(target_date = forecast_date + ahead)
  return(coefs)
}

create_cv_df <- function(list_of_models){
  cv_list <- vector(mode = "list",length = length(list_of_models))
  for(ii in 1:length(list_of_models)){
    model_ii <- list_of_models[[ii]]
    cv_list[[ii]] <- vector(mode = "list",length = length(model_ii))
    for(jj in 1:length(model_ii)){
      cv_vec <- model_ii[[jj]]$train_obj$cv_vec
      # TODO: implement for logistic regression and logistic lasso.
      params <- switch(class(model_ii[[jj]]$train_obj)[1],
                       cv_xg_boost = 1:model_ii[[jj]]$train_obj$nrounds,
                       cv_lgstlasso = model_ii[[jj]]$train_obj$lambda,
                       lognet = NULL
      )
      cv_list[[ii]][[jj]] <- data.frame(value = cv_vec,parameter = params) %>% 
        mutate(ahead = model_ii[[jj]]$ahead,
               forecast_date = model_ii[[jj]]$forecast_date)
      
    }
    cv_list[[ii]] <- bind_rows(cv_list[[ii]])
  }
  cv_df <- bind_rows(cv_list) %>%
    mutate(target_date = forecast_date + ahead)
  return(cv_df)
}

# Slide a function over a grouped data frame by forecast date, 
# separately for each group.
slide_one_group <- function(.data_group, slide_fun, n, col_name,...){
  slide_values = slider::slide_index_dbl(.x = .data_group,
                                         .i = .data_group$forecast_date,
                                         .f = slide_fun,..., 
                                         .before = lubridate::days(n-1))
  return(mutate(.data_group, !!col_name := slide_values))
}




