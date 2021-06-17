
# ny_preds <- readRDS(file.path(path_to_data, "predictions_honest.RDS")) %>%
#   filter(geo_value == "303", 
#          forecast_date == "2020-10-15",
#          forecaster == "AR3") %>%
#   mutate(target_end_date = forecast_date + days(ahead),
#          incidence_period = "day")
# 
# ny_actuals <- readRDS(file.path(path_to_data, "actuals.RDS")) %>%
#   filter(geo_value == "303", 
#          target_end_date > "2020-09-01", 
#          target_end_date < "2020-11-15") %>%
#   rename(value = actual, time_value = target_end_date)
#   
ny_preds <- readRDS("../data/ny_predictions.RDS")
ny_actuals <- readRDS("../data/ny_actuals.RDS")

pd <- evalcast:::setup_plot_trajectory(ny_preds, side_truth = ny_actuals)
pd$truth_df <- pd$truth_df %>% rename(target_end_date = time_value)

gg <- ggplot(pd$truth_df, mapping = aes(x = target_end_date)) +
  geom_ribbon(
    data = pd$quantiles_df,
    mapping = aes(ymin = lower, ymax = upper, fill = interval)) +
  scale_fill_brewer(palette = "Blues") +
  geom_line(aes(y = value)) + # reported
  geom_point(aes(y = value)) + # reported gets dots
  geom_line(data = pd$points_df, 
            mapping = aes(y = value),
            color = "orange", size = .5) +
  geom_point(data = pd$points_df, 
            mapping = aes(y = value),
            color = "orange", size = 1) +
  geom_vline(xintercept = as.Date("2020-10-15")) +
  theme_bw(base_size = 14) + 
  theme(legend.position = "none") + 
  ylab("AR forecast for case rate in NYC") + 
  xlab("date")
