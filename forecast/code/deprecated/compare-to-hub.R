library(aws.s3)
Sys.setenv("AWS_DEFAULT_REGION" = "us-east-2")
s3bucket <- get_bucket("forecast-eval")
n_keeper_weeks <- 6L
n_keeper_locs <- 50L
case_scores <- s3readRDS("score_cards_state_cases.rds", s3bucket)
case_scores <- case_scores %>% 
  mutate(forecast_date = target_end_date - ahead * 7) %>% 
  # fix weirdnesses about submission dates
  filter(forecast_date < "2021-01-01") %>%
  select(ahead, geo_value, forecaster, target_end_date, wis, forecast_date)

strawman <- case_scores %>% filter(forecaster == "COVIDhub-baseline")
case_scores <- left_join(
  case_scores, 
  strawman %>% 
    select(forecast_date, target_end_date, geo_value, wis) %>% 
    rename(strawman_wis = wis)
  )
case_scores <- case_scores %>% 
  filter(forecaster != "COVIDhub-baseline")
n_submitted <- case_scores %>% 
  filter(ahead == 2) %>% 
  group_by(forecaster) %>% 
  summarise(nfcasts = n())
keepers <- n_submitted %>% 
  filter(nfcasts / n_keeper_locs > n_keeper_weeks - .0001) %>% 
  # submitted at least z weeks for x locations
  pull(forecaster)

case_scores <- case_scores %>% filter(forecaster %in% keepers)
all_time_performance <- case_scores %>%
  group_by(forecaster, ahead) %>%
  summarise(rel_wis = Mean(wis) / Mean(strawman_wis),
            geo_wis1 = GeoMean((wis + 1) / (strawman_wis + 1)),
            geo_wis = GeoMean(wis / strawman_wis))

all_time_performance %>%
  pivot_longer(contains("wis")) %>%
  ggplot(aes(ahead, value, color = forecaster)) +
  theme_bw() +
  geom_point() +
  geom_line() +
  scale_color_viridis_d() +
  facet_wrap(~name) +
  geom_hline(yintercept = 1, color = "red") +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  scale_y_log10()

df <- summarizer(fcasts_honest %>% filter(period != "jm") %>%
                   group_by(ahead, forecaster), "wis", NULL, 
                 "strawman_wis", Mean, c("aggr","scale"))

GeoMean1 <- function(x) exp(mean(log(x + 1), na.rm = TRUE))

all_time_performance <- all_time_performance %>%
  filter(forecaster != "COVIDhub-4_week_ensemble")

ggplot(df) +
  geom_line(aes(ahead, wis, color = forecaster)) + 
  geom_point(aes(ahead, wis, color = forecaster)) +
  theme_bw() +
  geom_hline(yintercept = 1, size = 1.5) +
  xlab("Days ahead") +
  ylab("Mean WIS (relative to baseline)") +
  scale_color_manual(values = fcast_colors, guide = guide_legend(nrow = 1)) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  geom_line(data = all_time_performance %>% filter(ahead < 4, forecaster != "COVIDhub-ensemble"), 
            aes(ahead * 7, rel_wis, group = forecaster),
            color = "grey70") +
  geom_line(data = all_time_performance %>% filter(ahead < 4, forecaster == "COVIDhub-ensemble"),
            aes(ahead * 7, rel_wis), color = "lightblue", size = 1.5) +
  scale_y_log10()

df2 <- fcasts_honest %>% 
  filter(period != "jm") %>%
  group_by(ahead, forecaster) %>%
  summarise(wis = GeoMean((wis + 1) / (strawman_wis + 1)))


ggplot(df2) +
  geom_line(aes(ahead, wis, color = forecaster)) + 
  geom_point(aes(ahead, wis, color = forecaster)) +
  theme_bw() +
  geom_hline(yintercept = 1, size = 1.5) +
  xlab("Days ahead") +
  ylab("Geometric mean of WIS (relative to baseline)") +
  scale_color_manual(values = fcast_colors, guide = guide_legend(nrow = 1)) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  geom_line(data = all_time_performance %>% filter(ahead < 4, forecaster != "COVIDhub-ensemble"), 
            aes(ahead * 7, geo_wis1, group = forecaster),
            color = "grey70") +
  geom_line(data = all_time_performance %>% filter(ahead < 4, forecaster == "COVIDhub-ensemble"),
            aes(ahead * 7, geo_wis1), color = "lightblue", size = 1.5) +
  scale_y_log10()
