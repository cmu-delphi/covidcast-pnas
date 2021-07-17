#------------------------------------------------------------------------------#
# Experiments to evaluate the quality of DELPHI's various indicators,
# by fitting auto-regressive models to predict hotspots.
#
# Hotspots are defined according to a hotspot detection function.
# Two options:
#  -- hotspot by simple thresholding,
#  -- hotspot by test for statistical significance.
#
# Several different models are then fit.
# -- The most basic is an AR3 model.
# -- All others are AR3 + 3 lagged values of a particular signal.
#
# Fitting is done using logistic regression.
#
# Where possible I follow Ryan's quantgen-forecast vignette, which can be found
# in the modeltools package.
#
# TODO:
#  (1) Integrate logisticlasso_forecaster, hotspot_detector in modeltools pkg.
#------------------------------------------------------------------------------#

# We use the following versions of DELPHI packages.
# covidcast from CRAN
# devtools::install_github("cmu-delphi/covidcast", ref = "evalcast",
#                          subdir = "R-packages/evalcast")
# devtools::install_local(c("../covidcast/R-packages/modeltools"))

## Source
library(assertthat)
library(covidcast)
library(dplyr)
library(evalcast)
library(glmnet)
library(purrr)
library(tibble)
library(modeltools)
source("auc.R")
source("alden_evalcast.R") # Used to get predictions from already downloaded data
source("cv_logisticlasso.R")
source("hotspot_detection.R")
source("logisticlasso.R")
source("logisticlasso_hotspot.R")
source("misc.R")
source("utils.R")

## Setup
save_model <- FALSE # Should we save trained model to file?

# What type of analysis are we doing
analysis_type <- "logistic-regression-basic-hotspot_dishonest_as_of"

offline_signal_dir <- "data/offline_signals/honest_as_of"





# Load data, if necessary.
if(load_preds)
{
  analysis_dir <- file.path("data",geo_type,analysis_type)
  load(file.path(analysis_dir, paste(analysis_type,"prediction.rda",sep = "-")))
}

# What are we forecasting?
response_source <- "jhu-csse"
response_signal <- "confirmed_7dav_incidence_num"
incidence_period <- "day"
ahead <- 7:21
geo_type <- "hrr"
forecast_dates <- seq(as.Date("2020-06-16"), as.Date("2021-03-31"), by = "day")

# How do we detect a hotspot?
hotspot_lag = 7            # Number of days in the past to compare to.
upswing_threshold = .25     # Threshold for null hypothesis.
min_threshold = 30
detect_hotspots = detect_upswings_basic
# Arguments for our forecaster to take.
forecaster_args <- list(
  ahead = 7:21,
  n = 21, # training set size
  lags = c(0, 7, 14), # in days for features
  hotspot_detector = detect_hotspots,
  feature_type = "pct_difference",
  lambda = 0 # no penalty
)


# What signals to use
signals_df_1 = tribble(
  ~data_source,         ~signal,
  response_data_source, response_signal,
  'fb-survey',          'smoothed_hh_cmnty_cli',
  'doctor-visits',      'smoothed_adj_cli',
  'chng',               'smoothed_adj_outpatient_cli',
  'chng',               'smoothed_adj_outpatient_covid',
  'google-symptoms',    'sum_anosmia_ageusia_smoothed_search',
  'google-symptoms',    'sum_anosmia_ageusia_smoothed_search',
)
signals_df_2 = tribble(
  ~geo_values,  ~name,              ~zero_impute,
  '*',          'AR3',              NULL,
  '*',          'AR3FBCLI3',        NULL,
  '*',          'AR3DVCLI3',        NULL,
  '*',          'AR3CHCLI3',        NULL,
  '*',          'AR3CHCOV3',        NULL,
  '*',          'AR3GSSAA3_Zero',   'anosmia',
  '*',          'AR3GSSAA3_Subset', NULL,
)
signals_df = bind_cols(signals_df_1, signals_df_2)


list_of_forecaster_args <- list()
for(ii in 1:7){
  list_of_forecaster_args[[ii]] <- forecaster_args
  # Add bootstrap arguments
  if(ii > 1)
  {
    substring_ii <- list_of_signals[[ii]]$signal[2]
    list_of_forecaster_args[[ii]]$resample =list(substring=substring_ii, overwrite=TRUE)
  }
}
list_of_forecaster_args[[7]]$zero_impute <- "anosmia"

preds <- vector(mode = "list",length = length(list_of_signals))
names(preds) = names(list_of_signals)


for(ii in 1:length(list_of_signals)){
  # Information regarding task for forecaster ii
  forecaster_signals = list_of_signals[[ii]]
  forecaster_args = list_of_forecaster_args[[ii]]
  forecaster_name = names(list_of_signals)[ii]
  if(save_model){
    forecaster_args$model_dir = file.path("data",geo_type,analysis_type,"models",forecaster_name)
  }

  # Information regarding forecast dates for forecaster ii
  dates_to_be_forecast = forecast_dates


  # Get predictions for forecaster ii
  new_preds <- alden_get_predictions(
    forecaster = lgstlasso_hotspot_forecaster,
    name_of_forecaster = forecaster_name,
    signals = forecaster_signals,
    forecast_dates = dates_to_be_forecast,
    incidence_period = incidence_period,
    forecaster_args = forecaster_args,
    offline_signal_dir = offline_signal_dir
  ) %>% mutate(signal = "hotspot")

  ## Save appropriately
  if(forecaster_name %in% names(preds)){
    preds[[forecaster_name]] <- bind_rows(preds[[forecaster_name]],
                                          new_preds)
  } else{
    preds[[forecaster_name]] <- new_preds
  }
}

## Evaluate forecasts

# Compute ``ground truth'' hotspots, i.e. whether or not a hotspot
# actually occurred. This needs to be done independently, because the
# ground truth hotspots are not available as a covidcast signal.
target_dates_df <- tidyr::expand_grid(forecast_date = forecast_dates,ahead) %>%
  mutate(target_date = forecast_date + ahead)
response_df <- readRDS(sprintf('%s/%s_%s_%s.RDS',
                               "data/offline_signals/test",
                               response_source,
                               response_signal,
                               "2021-05-18"))
hotspots_df <- detect_hotspots(response_df) %>%
  filter(time_value %in% unique(target_dates_df$target_date)) %>%
  left_join(target_dates_df, by = c("time_value" = "target_date")) %>%
  select(geo_value,value,ahead,forecast_date,data_source,signal) %>%
  rename(actual = value)


# Ideally, the next step is
#
# evals <- evaluate_predictions(
# predictions_cards = bind_rows(preds),
# err_measures = list(err =  function(quantile,value,actual) value - actual),
# side_truth = hotspots_df,
# backfill_buffer = 0
# )
#
# However,
#   (1) that's way too cumbersome, and in fact it errors out.
#   (2) the way evalcast is currently set up, you cannot supply side truth
#       AND get back both preds and actuals columns.
# So I do the following instead.
slim_preds <- bind_rows(preds) %>%
  select(geo_value,value, ahead, forecast_date, forecaster)
slim_hotspots <- hotspots_df %>% select(geo_value,actual,ahead,forecast_date)

evals <- left_join(slim_preds,slim_hotspots) %>%
  mutate(target_end_date = forecast_date + ahead)

# Save everything to file.
save(list = ls(),
     file = file.path("data",geo_type,analysis_type,
                      paste(analysis_type,"prediction.rda",sep = "-")),
     compress = "xz")
