library(covidcast)
library(evalcast)
library(tidyverse)

ny_preds <- readRDS("data/ny_predictions.RDS")
ny_actuals <- readRDS("data/ny_actuals.RDS")

pd <- evalcast:::setup_plot_trajectory(ny_preds, side_truth = ny_actuals)
pd$truth_df <- pd$truth_df %>% rename(target_end_date = time_value)

ggplot(pd$truth_df, mapping = aes(x = target_end_date)) +
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
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  theme_bw(base_size = 14) + 
  theme(legend.position = "none") + 
  ylab("Cases per 100,000 people") + 
  xlab("Date")

ggsave("../paper/fig/trajectory.pdf", width = 6, height = 4)
