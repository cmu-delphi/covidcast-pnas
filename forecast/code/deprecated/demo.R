library(dplyr)
library(tibble)
library(covidcast)
library(evalcast)

response_source <- "jhu-csse"
response_signal <- "confirmed_7dav_incidence_prop"
incidence_period <- "day"
ahead <- 7:21
geo_type <- "hrr" 
ntrain = 21
forecast_dates <- seq(as.Date("2020-06-09"), as.Date("2020-06-10"), by = "day")

start_day_baseline <- function(forecast_date) {
  return(as.Date(forecast_date) - ntrain - 4 + 1)
}

# Example
signals_baseline = tibble::tibble(
                      data_source = response_source, 
                      signal = response_signal,
                      start_day = list(start_day_baseline),
                      as_of = list(function(x) x), # Warning: ignored because reading from disk
                             # (Specify honest vs dishonest by pointing to the each directory)
                      geo_values='*',
                      geo_type='hrr') # also ignored because reading from disk.

preds = get_predictions(baseline_forecaster,
                        'Baseline',
                        signals_baseline,
                        forecast_dates,
                        incidence_period='day',
                        offline_signal_dir='./honest_as_of/',
                        forecaster_args=list(
                            incidence_period='day',
                            ahead=ahead)
                        )
                        
# Example: only train on subset, e.g., for Google Symptoms
signals_baseline_subset = tibble::tibble(
                      data_source = response_source, 
                      signal = response_signal,
                      start_day = list(start_day_baseline),
                      as_of = list(function(x) x),
                      geo_values=list(c('1', '10')),
                      geo_type='hrr')

preds_subset = get_predictions(baseline_forecaster,
                        'Baseline',
                        signals_baseline_subset,
                        forecast_dates,
                        incidence_period='day',
                        offline_signal_dir='./honest_as_of/',
                        forecaster_args=list(
                            incidence_period='day',
                            ahead=ahead)
                        )
                        

