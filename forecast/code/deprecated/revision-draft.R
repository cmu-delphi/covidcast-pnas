# Cumulative mean prediction error plot -----------------------------------

tt <- fcasts_honest %>%
  group_by(forecaster, forecast_date) %>%
  summarise(w = sum(wis) / sum(strawman_wis)) %>%
  arrange(forecast_date) %>%
  mutate(n = n(), w = cummean(w))


fcast_colors <- c(RColorBrewer::brewer.pal(5, "Set1"), "#000000")
names(fcast_colors) <- c("CHNG-CLI", "CHNG-COVID", "CTIS-CLIIC", "DV-CLI",
                         "Google-AA", "AR")


ggplot(tt %>% filter(forecast_date < "2021-01-01"), 
       aes(forecast_date, w, color = forecaster)) + 
  geom_line() +
  geom_hline(yintercept = 1) +
  xlab("forecast date") +
  ylab("cummean(sum(wis_forecaster[t]) /\n sum(wis_strawman[t]))") +
  scale_color_manual(values = fcast_colors, guide = guide_legend(nrow = 1)) +
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank())


# Sign test ---------------------------------------------------------------

fcast_colors2 <- fcast_colors[names(fcast_colors) != "AR"]

st <- fcasts_honest %>%
  mutate(wis = wis / strawman_wis) %>%
  select(forecaster, geo_value, ahead, forecast_date, wis) %>%
  pivot_wider(names_from = forecaster, values_from = wis) %>%
  mutate(across(AR:`Google-AA`, ~ AR -.x)) %>%
  select(-AR) %>%
  pivot_longer(`CHNG-CLI`:`Google-AA`, names_to = "forecaster", values_to = "dif") %>%
  group_by(forecaster, forecast_date, geo_value) %>%
  summarise(dif = sum(dif)) %>%
  group_by(forecaster, forecast_date) %>%
  summarise(p = binom.test(
    x = sum(dif > 0),
    n = n(),
    alternative = "greater")$p.val)

ggplot(st, aes(p)) +
  geom_histogram(
    aes(y = ..count.. / sum(..count..), color = forecaster, fill = forecaster), 
    alpha = 0.4,
    bins = 40) +
  scale_color_manual(values = fcast_colors2, guide = guide_legend(nrow = 1)) +
  scale_fill_manual(values = fcast_colors2, guide = guide_legend(nrow = 1)) +
  facet_wrap(~forecaster) +
  ylab("Frequency") +
  xlab("P-value for WIS_AR < WIS_F") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_bw() +
  theme(legend.position = "bottom")
  
st14 <- fcasts_honest %>%
  mutate(wis = wis / strawman_wis) %>%
  select(forecaster, geo_value, ahead, forecast_date, wis) %>%
  pivot_wider(names_from = forecaster, values_from = wis) %>%
  mutate(across(AR:`Google-AA`, ~ AR -.x)) %>%
  select(-AR) %>%
  pivot_longer(`CHNG-CLI`:`Google-AA`, names_to = "forecaster", values_to = "dif") %>%
  filter(ahead == 14) %>%
  group_by(forecaster, forecast_date) %>%
  summarise(p = binom.test(
    x = sum(dif > 0),
    n = n(),
    alternative = "greater")$p.val)


ggplot(st14, aes(p)) +
  geom_histogram(
    aes(y = ..count.. / sum(..count..), color = forecaster, fill = forecaster), 
    alpha = 0.4,
    bins = 40) +
  scale_color_manual(values = fcast_colors2, guide = guide_legend(nrow = 1)) +
  scale_fill_manual(values = fcast_colors2, guide = guide_legend(nrow = 1)) +
  facet_wrap(~forecaster) +
  ylab("Frequency") +
  xlab("P-value for WIS_AR < WIS_F") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_bw() +
  theme(legend.position = "bottom")


# Diebold Mariano Test ----------------------------------------------------

errs <- fcasts_honest %>% 
  group_by(forecaster, forecast_date) %>%
  summarise(
    mwisd = mean(wis) / mean(strawman_wis), mwis = mean(wis), 
    gmwisd = GeoMean(wis / strawman_wis), gmwis = GeoMean(wis)) %>%
  ungroup()
ar <- errs %>% 
  filter(forecaster == "AR") %>% 
  select(-forecaster) %>%
  pivot_longer(-forecast_date, names_to = "metric", values_to = "AR")
notar <- errs %>%
  filter(forecaster != "AR") %>% 
  pivot_longer(mwisd:gmwis, names_to = "metric", values_to = "err")

all_errs <- left_join(notar, ar)
dms <- all_errs %>% 
  group_by(forecaster, metric) %>% 
  summarise(dm = forecast::dm.test(AR, err, "greater", h = 7 , power = 1)$p.value)
knitr::kable(dms %>% pivot_wider(names_from = forecaster, values_from = dm), digits = 3)




