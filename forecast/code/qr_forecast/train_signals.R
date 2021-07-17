library(furrr)
future::plan(multisession)

library(progressr)

library(dplyr)
library(tibble)
library(covidcast)
library(evalcast)
source(here::here("qr_forecast", "quantgen.R"))


geo_type <- "hrr"
ntrain = 21
lags = c(0, 7, 14)
forecast_dates <- seq(as.Date('2020-06-09'),
                      as.Date('2021-03-31'),
                      by = "day")
ahead = 7:21
tau = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975)
response_data_source = 'jhu-csse'
response_signal = 'confirmed_7dav_incidence_prop'
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

for (tt in c('honest', 'dishonest', 'honest_bootstrapped')) {
  for (idx in 1:nrow(signals_df)) {
    train_type = stringr::str_extract(tt, "[^_]*")
    offline_signal_dir = here::here("data", "offline_signals", sprintf('%s_as_of', train_type))
    debug_dir = here::here("data", "debug_results", sprintf("%s_as_of", tt))
    if (!dir.exists(debug_dir)) dir.create(debug_dir)
    signals_ar = tibble::tibble(
                          data_source = unique(c(response_data_source,
                                                 signals_df$data_source[idx])),
                          signal = unique(c(response_signal,
                                                 signals_df$signal[idx])),
                          start_day = list(start_day_ar),
                          geo_values=list(signals_df$geo_values[[idx]]),
                          geo_type='hrr')
    message(signals_df$name[idx])
    t0 = Sys.time()
    with_progress(preds <- get_predictions(quantgen_forecaster,
                          signals_df$name[idx],
                          signals_ar,
                          forecast_dates,
                          incidence_period='day',
                          offline_signal_dir=offline_signal_dir,
                          forecaster_args=list(
                              signals=signals_ar,
                              incidence_period='day',
                              ahead=ahead,
                              geo_type=geo_type,
                              tau=tau,
                              n=ntrain,
                              lags=lags,
                              lambda=0,
                              zero_impute=signals_df$zero_impute[[idx]],
                              resample = str_detect(tt, "bootstrapped"),
                              debug=here::here("data", "debug_results",
                                               "quantreg",
                                               sprintf("%s_as_of", tt),
                                               signals_df$name[idx])
                              )
                          ))
    t1 = Sys.time()
    print(t1-t0)

    saveRDS(preds, here("data", "predictions", "quantreg",
                        sprintf('%s_%s.RDS', signals_df$name[idx], tt)))
  }
}
