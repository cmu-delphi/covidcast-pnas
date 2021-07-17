
## Evaluate hotspots
library(dplyr)
library(purrr)
library(tibble)
source("hotspot_detection.R")
source("misc.R")
source("utils.R")
detect_hotspots = detect_upswings_basic

# Compute ``ground truth'' hotspots, i.e. whether or not a hotspot
# actually occurred. This needs to be done independently, because the
# ground truth hotspots are not available as a covidcast signal.
forecast_dates <- seq(as.Date("2020-06-16"), as.Date("2021-03-31"), by = "day")
ahead <- 7:21
AR_models = c(
  'AR3',
  'AR3FB3',
  'AR3DVCLI3',
  'AR3CHCLI3',
  'AR3CHCOV3',
  'AR3GG3',
  'AR3GG3_imputed')



target_dates_df <- tidyr::expand_grid(forecast_date = forecast_dates, ahead) %>%
  mutate(target_date = forecast_date + ahead)

response_df <- readRDS(here::here("data", "actuals.RDS"))
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
#
train_types = c('honest', 'dishonest', 'honest_bootstrapped')

for (tt in train_types) {
  # Read baseline
  train_type <- stringr::str_extract(tt, "[^_]*")
  # Ingest AR predictions
  preds_list = vector('list', length(AR_models)+1)

  for (idx in 1:length(AR_models)) {
    preds_list[[idx]] = readRDS(
      here::here("data","predictions", "hotspots",
                 sprintf('%s_%s.RDS', AR_models[[idx]], tt))
    )
  }

  preds <- bind_rows(preds_list)
  rm(preds_list)

  slim_preds <- preds %>%
    select(geo_value,value, ahead, forecast_date, forecaster)
  slim_hotspots <- hotspots_df %>% select(geo_value,actual,ahead,forecast_date)

  results <- left_join(slim_preds,slim_hotspots) %>%
    mutate(target_end_date = forecast_date + ahead)

  # Save everything to file.
  saveRDS(results, here::here("data", sprintf('hotspots_%s.RDS', tt)))
}
