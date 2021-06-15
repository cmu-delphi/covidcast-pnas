la <- hotspots_honest %>%
  filter(geo_value == "56", period != "jm") %>%
  left_join(actuals %>% filter(geo_value == "56") %>% rename(observed = actual))

la %>%
  ggplot(aes(target_end_date, value, color = ahead)) +
  geom_vline(data = la %>% filter(actual == 1), aes(xintercept = target_end_date),
             color = "orange", size = .2) +
  geom_point() +
  scale_fill_viridis_c() +
  facet_wrap(~forecaster, ncol = 1) +
  theme_bw()

hot_udf <- inner_join(hotspots_honest %>% filter(period != "jm"),
                      up_down %>% select(geo_value, target_end_date, udf))
n_expected <- hot_udf %>% group_by(udf,forecaster) %>% summarise(n_forecasts = n())


tab <- hot_udf %>% 
  mutate(deciles = ntile(value, 10)) %>%
  group_by(udf, deciles, forecaster) %>%
  summarise(m = n()) %>%
  left_join(n_expected) %>%
  mutate(expected = n_forecasts * .1, deviation = m / expected)

ggplot(tab, aes(deciles, deviation, color = forecaster)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 1) +
  facet_wrap(~udf, ncol = 1) +
  scale_color_brewer(palette = "Set1", guide = guide_legend(nrow = 1)) +
  scale_x_continuous(breaks = 1:10, labels = 1:10) +
  theme_bw() +
  ylab("predictions / expected") +
  theme(legend.position = "bottom", legend.title = element_blank())
