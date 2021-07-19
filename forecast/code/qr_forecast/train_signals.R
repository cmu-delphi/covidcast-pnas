source(here::here("code", "pkgs_and_common.R"))
source(here::here("code", "qr_forecast", "quantgen.R"))


lags = qr_lags
forecast_dates <- qr_forecast_dates
tau = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975)


# To user with future_map, we must evaluate these globals ahead of time
make_start_day_ar = function(ahead, ntrain, lags) {
  offset = eval(1 - max(ahead) - ntrain - max(lags))
  start_day_ar = function(forecast_date) {
    return(as.Date(forecast_date) + offset)
  }
  return(start_day_ar)
}

start_day_ar = make_start_day_ar(ahead, ntrain, lags)
preds_dir <- here::here("data", "predictions", "quantreg")
if (!dir.exists(preds_dir)) dir.create(preds_dir)

for (tt in train_types) {
  for (idx in 1:nrow(signals_df)) {
    train_type = stringr::str_extract(tt, "[^_]*")
    offline_signal_dir = here::here("data", "offline_signals", sprintf('%s_as_of', train_type))
    debug_dir = here::here("data", "debug_results", "quantreg", sprintf("%s_as_of", tt))
    if (!dir.exists(debug_dir)) dir.create(debug_dir)
    signals_ar = tibble::tibble(
      data_source = unique(c(response_data_source, signals_df$data_source[idx])),
      signal = unique(c(response_signal, signals_df$signal[idx])),
      start_day = list(start_day_ar),
      geo_values = list(signals_df$geo_values[[idx]]),
      geo_type=geo_type)
    message(signals_df$name[idx])
    t0 = Sys.time()
    with_progress(
      preds <- offline_get_predictions(
        quantgen_forecaster,
        signals_df$name[idx],
        signals_ar,
        forecast_dates,
        incidence_period = 'day',
        offline_signal_dir = offline_signal_dir,
        forecaster_args=list(
          signals=signals_ar,
          incidence_period = incidence_period,
          ahead=ahead,
          geo_type=geo_type,
          tau=tau,
          n=ntrain,
          lags=lags,
          lambda=0,
          lp_solver = lp_solver,
          sort = TRUE,
          zero_impute=signals_df$zero_impute[[idx]],
          resample = str_detect(tt, "bootstrapped"),
          debug = here::here(debug_dir, sprintf("%s_as_of", tt), signals_df$name[idx]))
      ))
    t1 = Sys.time()
    print(t1-t0)

    saveRDS(preds, here(preds_dir, sprintf('%s_%s.RDS', signals_df$name[idx], tt)))
  }
}
