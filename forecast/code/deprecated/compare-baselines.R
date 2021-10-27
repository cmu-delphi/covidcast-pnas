library(evalcast)
library(covidcast)
library(tidyverse)
library(aws.s3)
Sys.setenv("AWS_DEFAULT_REGION" = "us-east-2")
s3bucket <- get_bucket("forecast-eval")


# Point to the data Addison sent
ours <- readRDS("~/Downloads/results_no_october_honest.RDS")
baseline <- ours %>% filter(forecaster == "Baseline") %>%
  select(-forecaster) %>% rename(strawman_wis = wis)

# these are epiweek / num
bpreds <- s3readRDS("predictions_cards.rds", s3bucket) %>%
  filter(forecaster == "COVIDhub-baseline", 
         signal == "confirmed_incidence_num",
         forecast_date < "2021-01-01", 
         ahead %in% 1:4,
         !(geo_value %in% c("as", "gu", "pr", "vi", "mp", "us")))

# trying with the prop signal, since that's what we use
actuals <- covidcast_signal("jhu-csse", "confirmed_7dav_incidence_prop", 
                            end_day = "2021-02-01", geo_type = "state", 
                            as_of = "2021-05-18") %>% 
  select(geo_value, time_value, value) %>%
  rename(target_end_date = time_value, actual = value)

# Population / 100,000
pop <- covidcast::state_census %>% select(ABBR, POPESTIMATE2019) %>%
  mutate(geo_value = tolower(ABBR), POPESTIMATE2019 = POPESTIMATE2019 / 1e5) %>% 
  select(-ABBR)

# Scale epiweek total to daily prop
bpreds2 <- left_join(bpreds, pop) %>% 
  mutate(value = value / (7 * POPESTIMATE2019)) %>%
  select(-POPESTIMATE2019, -incidence_period)

# Score the Hub Baseline
bscores <- evaluate_predictions(bpreds2, actuals, 
                                grp_vars = c("ahead", "forecast_date", "geo_value"),
                                err_measures = list(wis = weighted_interval_score))

comb <- left_join(bscores %>% 
                    # submissions made on Monday with Sunday target
                    mutate(ahead = ahead * 7 - 2, 
                           forecast_date = target_end_date - ahead) %>%
                    select(forecast_date, target_end_date, geo_value, wis), 
                  baseline %>% 
                    select(forecast_date, target_end_date, geo_value, strawman_wis), 
                  by = c("geo_value", "forecast_date", "target_end_date")) %>%
  filter(!is.na(strawman_wis), !is.na(wis))

ggplot(comb, aes(wis, strawman_wis, color = geo_value)) + 
  geom_point() + 
  theme_bw() +
  theme(legend.position = "none") + 
  geom_abline(intercept =0, slope = 1) + 
  scale_x_log10() +
  scale_y_log10() +
  xlab("WIS of COVIDhub-baseline") + 
  ylab("WIS or our baseline") + 
  coord_equal()
