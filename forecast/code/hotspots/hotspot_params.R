hotspot_lag = 7            # Number of days in the past to compare to.
upswing_threshold = .25     # Threshold for null hypothesis.
min_threshold = 30
detect_hotspots = detect_upswings_basic

response_signal = 'confirmed_7dav_incidence_num'

# Arguments for our forecaster to take.
forecaster_args <- list(
  ahead = ahead,
  n = ntrain, # training set size
  lags = lags, # in days for features
  hotspot_detector = detect_hotspots,
  feature_type = "pct_difference",
  lambda = 0 # no penalty
)
