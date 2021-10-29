
# Import from the Hub -----------------------------------------------------

library(tidyverse)
library(aws.s3)
Sys.setenv("AWS_DEFAULT_REGION" = "us-east-2")
s3bucket <- get_bucket("forecast-eval")
n_keeper_weeks <- 6L
n_keeper_locs <- 50L
case_scores <- s3readRDS("score_cards_state_cases.rds", s3bucket)
case_scores <- case_scores %>% 
  mutate(
    ahead = ahead * 7 - 2, # forecast on a Monday
    forecast_date = target_end_date - ahead) %>% 
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
aheads <- case_scores %>% distinct(ahead) %>% pull()
n_submitted <- case_scores %>% 
  filter(ahead == aheads[2]) %>% 
  group_by(forecaster) %>% 
  summarise(nfcasts = n())
keepers <- n_submitted %>% 
  filter(nfcasts / n_keeper_locs > n_keeper_weeks - .0001) %>% 
  # submitted at least z weeks for x locations
  pull(forecaster)

case_scores <- case_scores %>% filter(forecaster %in% keepers)


# Load our models ---------------------------------------------------------

ours <- readRDS("~/Downloads/results_honest.RDS")
pop <- covidcast::state_census %>% select(ABBR, POPESTIMATE2019) %>%
  mutate(geo_value = tolower(ABBR)) %>% select(-ABBR)
# scale from prop to num
ours <- left_join(ours, pop) %>%
  mutate(wis = wis * POPESTIMATE2019 / 1e5 * 7,
         forecaster = recode(forecaster,
                             AR3 = "AR",
                             AR3CHCLI3 = "CHNG-CLI",
                             AR3CHCOV3 = "CHNG-COVID",
                             AR3DVCLI3 = "DV-CLI",
                             AR3FBCLI3 = "CTIS-CLIIC",
                             AR3GSSAA3_Subset = "drop",
                             AR3GSSAA3_Zero = "Google-AA")) %>%
  filter(forecaster != "drop") %>%
  select(-ae, -POPESTIMATE2019) 

common_fd <- as.Date(intersect(
  case_scores %>% select(forecast_date) %>% distinct() %>% pull(),
  ours %>% select(forecast_date) %>% distinct() %>% pull()),
  "1970-01-01")


our_models <- ours %>% filter(forecaster != "Baseline")
baseline <- ours %>% filter(forecaster == "Baseline") %>%
  select(-forecaster) %>% rename(strawman_wis = wis)
our_models <- left_join(our_models, baseline)
all_models <- bind_rows(hub = case_scores, ours = our_models, .id = "source") 


all_time_performance <- all_models %>%
  filter(forecast_date %in% common_fd) %>%
  group_by(forecaster, ahead, source) %>%
  summarise(rel_wis = Mean(wis) / Mean(strawman_wis),
            geo_wis = GeoMean((wis + 1) / (strawman_wis + 1))) %>%
  pivot_longer(contains("wis"))

facet_labs <- c(geo_wis = "Geometric mean of WIS", rel_wis = "Mean of WIS")
fcast_colors <- c(RColorBrewer::brewer.pal(5, "Set1"), "#000000")
names(fcast_colors) <- c("CHNG-CLI", "CHNG-COVID", "CTIS-CLIIC", "DV-CLI",
                         "Google-AA", "AR")


ggplot(all_time_performance %>% filter(source == "ours"),
       aes(ahead, value, color = forecaster)) +
  geom_line(data = all_time_performance %>% filter(forecaster != "COVIDhub-ensemble"), 
            aes(group = forecaster),
            color = "grey80") +
  geom_line(data = all_time_performance %>% filter(forecaster == "COVIDhub-ensemble"),
            color = "lightblue", size = 1.5) +
  geom_line(aes(ahead, value, color = forecaster)) + 
  geom_point(aes(ahead, value, color = forecaster)) +
  scale_color_manual(values = fcast_colors, guide = guide_legend(nrow = 1)) +
  theme_bw() +
  ylab("relative to baseline") +
  geom_hline(yintercept = 1, size = 1.5) +
  xlab("Days ahead") +
  facet_wrap(~ name, labeller = labeller(name = facet_labs)) +
  theme(legend.position = "bottom", legend.title = element_blank())

