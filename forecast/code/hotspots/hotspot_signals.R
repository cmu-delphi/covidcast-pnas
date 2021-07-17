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

# What type of analysis are we doing
train_types <- c("honest", "dishonest", "honest_bootstrapped")

# What are we forecasting?
response_source <- "jhu-csse"
response_signal <- "confirmed_7dav_incidence_num"
incidence_period <- "day"
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
  '*',          'AR3FB3',           NULL,
  '*',          'AR3DV3',           NULL,
  '*',          'AR3CHCLI3',        NULL,
  '*',          'AR3CHCOV3',        NULL,
  '*',          'AR3GG3_imputed',   'anosmia',
  '*',          'AR3GG3',           NULL,
)
signals_df = bind_cols(signals_df_1, signals_df_2)

debug_dir <- here::here("data", "debug_results", "hotspots")
if (!dir.exists(debug_dir)) dir.create(debug_dir)
preds_dir <- here::here("data", "predictions", "hotspots")
if (!dir.exists(preds_dir)) dir.create(preds_dir)

for (tt in train_type) {
  for (idx in 1:nrow(signals_df)) {
    train_type = stringr::str_extract(tt, "[^_]*")
    offline_signal_dir = here::here("data", "offline_signals",
                                    sprintf('%s_as_of', train_type))

    # Information regarding task for forecaster ii

    signals_ar = tibble::tibble(
      data_source = unique(c(response_data_source,
                             signals_df$data_source[idx])),
      signal = unique(c(response_signal,
                        signals_df$signal[idx])),
      geo_values = list(signals_df$geo_values[[idx]]),
      geo_type = 'hrr')
    message(signals_df$name[idx])
    # Information regarding forecast dates for forecaster ii


    forecast_args$model_dir = here::here(debug_dir, sprintf("%s_as_of", tt), signals_df$name[idx])
    if (!dir.exists(forecast_args$model_dir)) dir.create(forecaster_args$model_dir)
    # Get predictions for forecaster ii
    preds <- alden_get_predictions(
      forecaster = lgstlasso_hotspot_forecaster,
      name_of_forecaster = signals_df$name[idx],
      signals = signals_ar,
      forecast_dates,
      incidence_period = incidence_period,
      offline_signal_dir = offline_signal_dir,
      forecaster_args = forecaster_args
    ) %>% mutate(signal = "hotspot")

    saveRDS(preds, here::here(preds_dir,
                        sprintf('%s_%s.RDS', signals_df$name[idx], tt)))
  }
}


