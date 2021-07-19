source(here::here("code", "pkgs_and_common.R"))
lags = qr_lags
forecast_dates <- qr_forecast_dates



# To user with future_map, we must evaluate these globals ahead of time
make_start_day_baseline = function(ntrain) {
  offset = eval(1 - ntrain - 4)
  start_day_baseline = function(forecast_date) {
    return(as.Date(forecast_date) + offset)
  }
  return(start_day_baseline)
}

start_day_baseline = make_start_day_baseline(ntrain)

signals_baseline = tibble::tibble(
                      data_source = response_data_source,
                      signal = response_signal,
                      start_day = list(start_day_baseline),
                      geo_values='*',
                      geo_type='hrr') # also ignored because reading from disk.

preds_dir <- here::here("data", "predictions", "quantreg")
if (!dir.exists(preds_dir)) dir.create(preds_dir)


for (train_type in c('honest', 'dishonest')) {
  offline_signal_dir = here::here(
    "data", "offline_signals", sprintf('%s_as_of', train_type))
  t0 = Sys.time()
  with_progress(preds <- get_predictions(baseline_forecaster,
                                         'Baseline',
                                         signals_baseline,
                                         forecast_dates,
                                         incidence_period='day',
                                         offline_signal_dir=offline_signal_dir,
                                         forecaster_args=list(
                                           incidence_period='day',
                                           ahead=ahead)
  ))
  t1 = Sys.time()
  print(t1-t0)
  saveRDS(preds,
          here::here(preds_dir, sprintf('%s_%s.RDS', 'Baseline', train_type)))
}
