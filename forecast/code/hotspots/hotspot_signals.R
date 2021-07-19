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
source(here::here("code", "pkgs_and_common.R"))
source(here::here("code", "hotspots", "logisticlasso_hotspot.R"))
source(here::here("code", "hotspots", "logisticlasso.R"))
source(here::here("code", "hotspots", "misc.R"))
source(here::here("code", "hotspots", "utils.R"))
source(here::here("code", "hotspots", "hotspot_detection.R"))

# What are we forecasting?
forecast_dates <- hotspot_forecast_dates

# How do we detect a hotspot?
hotspot_lag = 7            # Number of days in the past to compare to.
upswing_threshold = .25     # Threshold for null hypothesis.
min_threshold = 30
detect_hotspots = detect_upswings_basic


# Arguments for our forecaster to take.
forecaster_args <- list(
  ahead = ahead,
  n = ntrain, # training set size
  lags = lags, # in days for features
  hotspot_detector = detect_hotspots,
  feature_type = "pct_difference",
  lambda = 0 # no penalty
)

# different names for hotspots
signals_df$name <- c("AR3", "AR3FB3", "AR3DV3", "AR3CHCLI3", "AR3CHCOV3",
                     "AR3GG3_imputed", "AR3GG3")


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
    preds <- offline_get_predictions(
      forecaster = lgstlasso_hotspot_forecaster,
      name_of_forecaster = signals_df$name[idx],
      signals = signals_ar,
      forecast_dates,
      incidence_period = incidence_period,
      offline_signal_dir = offline_signal_dir,
      forecaster_args = forecaster_args
    ) %>%
      mutate(signal = "hotspot")

    saveRDS(preds,
            here::here(preds_dir, sprintf('%s_%s.RDS', signals_df$name[idx], tt)))
  }
}


