library(dplyr)
library(tibble)
library(covidcast)
library(evalcast)
library(here)

# Parameters for determining which data (and how much) we need --------
geo_type <- "hrr"
ntrain = 21
ahead = 7:21
lags = c(0, 7, 14)
hotspot_lag = 7
end_forecast_date = '2021-03-31'
named_function = function(x) lubridate::ymd("2021-05-18") # finalized as_of

honest_as_of_signals = tribble(
  ~data_source,         ~signal,                              ~start_forecast_date,
  'jhu-csse',           'confirmed_7dav_incidence_prop',      '2020-06-09',
  'jhu-csse',           'confirmed_7dav_incidence_num',       '2020-06-09',
  'fb-survey',          'smoothed_hh_cmnty_cli',              '2020-06-09',
  'doctor-visits',      'smoothed_adj_cli',                   '2020-06-09',
  'chng',               'smoothed_adj_outpatient_cli',        '2021-02-22',
  'chng',               'smoothed_adj_outpatient_covid',      '2021-02-22',
)

dishonest_as_of_signals = tribble(
  ~data_source,         ~signal,                              ~start_forecast_date,
  'jhu-csse',           'confirmed_7dav_incidence_prop',      '2020-06-09',
  'jhu-csse',           'confirmed_7dav_incidence_num',       '2020-06-09',
  'fb-survey',          'smoothed_hh_cmnty_cli',              '2020-06-09',
  'doctor-visits',      'smoothed_adj_cli',                   '2020-06-09',
  'chng',               'smoothed_adj_outpatient_cli',        '2020-06-09',
  'chng',               'smoothed_adj_outpatient_covid',      '2020-06-09',
  'google-symptoms',    'sum_anosmia_ageusia_smoothed_search','2020-06-09',
)




# Functions ---------------------------------------------------------------

signal_downloader <- function(df_list,
                              forecast_date,
                              offline_signal_dir = NULL) {
  if (class(df_list)[1] == "list") df_list <- df_list[[1]]
  data_source = unique(df_list$data_source)
  signal = unique(df_list$signal)
  if ((length(data_source) > 1) || (length(signal) > 1)) {
    stop('Data source / signal not unique.')
  }
  saveRDS(
    df_list,
    file.path(offline_signal_dir,
              sprintf('%s_%s_%s.RDS', data_source, signal, forecast_date)))
  # Return dummy predictions to appease evalcast
  output = tibble(ahead = 1, geo_value = '1', quantile = 0.5, value = 0)
  return(output)
}


start_day_download <- function(forecast_date) {
  return(as.Date(forecast_date) - max(ahead) - ntrain - max(lags) - hotspot_lag + 1)
}


# Download honest data ----------------------------------------------------


signals_df = honest_as_of_signals
offline_signal_dir = here("data", "offline_signals", "honest_as_of")
if (!dir.exists(offline_signal_dir)) dir.create(offline_signal_dir)
for (idx in 1:nrow(honest_as_of_signals)) {
  forecast_dates <- seq(as.Date(signals_df$start_forecast_date[idx]),
                        as.Date(end_forecast_date), by = "day")
  signals_download = tibble::tibble(
    data_source = signals_df$data_source[idx],
    signal = signals_df$signal[idx],
    start_day = list(start_day_download),
    as_of = list(function(x) x),
    geo_values='*',
    geo_type='hrr')
  preds = get_predictions(signal_downloader,
                          'Download',
                          signals_download,
                          forecast_dates,
                          incidence_period = 'day',
                          forecaster_args = list(
                            offline_signal_dir=offline_signal_dir
                          )
  )
}


# Download "finalized" data -----------------------------------------------


signals_df = dishonest_as_of_signals
offline_signal_dir = here("data", "offline_signals", "dishonest_as_of")
if (!dir.exists(offline_signal_dir)) dir.create(offline_signal_dir)
for (idx in 1:nrow(dishonest_as_of_signals)) {
    forecast_dates <- seq(as.Date(signals_df$start_forecast_date[idx]),
                          as.Date(end_forecast_date), by = "day")
    signals_download = tibble::tibble(
                          data_source = signals_df$data_source[idx],
                          signal = signals_df$signal[idx],
                          start_day = list(start_day_download),
                          as_of = list(named_function),
                          geo_values='*',
                          geo_type='hrr')
    preds = get_predictions(signal_downloader,
                          'Download',
                          signals_download,
                          forecast_dates,
                          incidence_period='day',
                          forecaster_args=list(
                              offline_signal_dir=offline_signal_dir
                              )
                          )
}



# Copy finalized google to honest -----------------------------------------
# See the manuscript for a discussion of why we use finalized google

google_sigs <- list.files(offline_signal_dir, "google-symptoms")
file.copy(google_sigs, here("data", "offline_signals", "honest_as_of"))


# Create case / prop actuals ----------------------------------------------
df <- covidcast_signals(
  data_source = "jhu-csse",
  signal = c("confirmed_7dav_incidence_prop", "confirmed_7dav_incidence_num"),
  start_day = "2020-06-01",
  end_day = "2021-05-01",
  as_of = named_function(1)
)

saveRDS(df[[1]] %>%
          select(geo_value, time_value, value) %>%
          rename(target_end_date = time_value, actual = value),
        here("data", "confirmed_7dav_incidence_prop.RDS"))

saveRDS(df[[1]] %>%
          select(geo_value, time_value, value),
        here("data", "confirmed_7dav_incidence_num.RDS"))

