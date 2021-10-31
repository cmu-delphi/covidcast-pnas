
# Import from the Hub -----------------------------------------------------

library(tidyverse)
library(aws.s3)
Sys.setenv("AWS_DEFAULT_REGION" = "us-east-2")
s3bucket <- get_bucket("forecast-eval")
n_keeper_weeks <- 6L
n_keeper_locs <- 50L

case_preds <- s3readRDS("predictions_cards.rds", s3bucket) %>%
  filter(signal == "confirmed_incidence_num",
         forecast_date < "2021-01-01",
         !(geo_value %in% c("as", "gu", "pr", "vi", "mp", "us")))
actuals <- covidcast::covidcast_signal("jhu-csse", "confirmed_7dav_incidence_num",
                                       geo_type = "state", as_of = "2021-05-18",
                                       end_day = "2021-03-01") %>%
  mutate(value = value * 7) %>%
  rename(target_end_date = time_value, actual = value) %>%
  select(geo_value, target_end_date, actual)
case_scores <- evalcast::evaluate_predictions(
  case_preds, actuals,
  err_measures = list(wis = evalcast::weighted_interval_score),
  grp_vars = c("ahead", "forecaster", "forecast_date", "geo_value")
)
rm(case_preds)


case_scores <- case_scores %>% 
  mutate(
    ahead = ahead * 7 - 2, # forecast on a Tuesday
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
case_scores <- case_scores %>% mutate(
  forecaster = recode(forecaster, `COVIDhub-ensemble` = "Ensemble"))

# Load our models ---------------------------------------------------------

ours <- readRDS(here::here("data", "results_honest_states.RDS"))
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

Mean <- function(x) mean(x, na.rm = TRUE)
GeoMean <- function(x) exp(mean(log(x), na.rm = TRUE))

all_time_performance <- all_models %>%
  filter(forecast_date %in% common_fd) %>%
  group_by(forecaster, ahead, source) %>%
  summarise(rel_wis = Mean(wis) / Mean(strawman_wis),
            geo_wis = GeoMean(wis) / GeoMean(strawman_wis)) %>%
  pivot_longer(contains("wis"))

our_performance <- our_models %>% 
  group_by(forecaster, ahead) %>%
  summarise(rel_wis = Mean(wis) / Mean(strawman_wis),
            geo_wis = GeoMean(wis) / GeoMean(strawman_wis)) %>%
  pivot_longer(contains("wis"))

facet_labs <- c(geo_wis = "Geometric mean", rel_wis = "Mean")
fcast_colors <- c("#000000", RColorBrewer::brewer.pal(5, "Set1"))
names(fcast_colors) <- c("AR", "CHNG-CLI", "CHNG-COVID", "CTIS-CLIIC", "DV-CLI",
                         "Google-AA")
       
ggplot(all_time_performance %>% filter(source == "ours"),
       aes(ahead, value, color = forecaster)) +
  geom_line(data = all_time_performance %>%
              filter(source == "hub", !(forecaster %in% c("Ensemble", 
                                         "OliverWyman-Navigator"))), 
            # Data bug? OliverWyman has GeoMean(RelWis) = 0 at ahead = 5
            aes(group = forecaster),
            color = "grey80") +
  geom_line(data = all_time_performance %>% filter(forecaster == "Ensemble"),
            color = "lightblue", size = 1.5) +
  geom_point(data = all_time_performance %>% filter(forecaster == "Ensemble"),
            color = "lightblue", size = 1.5) +
  geom_line(aes(ahead, value, color = forecaster)) +
  geom_point(aes(ahead, value, color = forecaster)) +
  scale_color_manual(values = c(fcast_colors, "Ensemble" = "lightblue"),
                     guide = guide_legend(nrow = 2)) +
  ylab("WIS (relative to baseline)") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  xlab("Days ahead") +
  facet_wrap(~ name, labeller = labeller(name = facet_labs)) +
  theme_bw() + theme(legend.pos = "bottom", legend.title = element_blank())

ggsave(here::here("paper", "fig", "compare-states-to-hub.pdf"), 
       width = 6.5, height = 4.5)
