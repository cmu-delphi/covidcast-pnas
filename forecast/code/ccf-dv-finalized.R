sigs <- covidcast_signals(
  data_source = c("jhu-csse", "doctor-visits"),
  signal = c("confirmed_7dav_incidence_prop", "smoothed_adj_cli"),
  start_day = "2020-08-01",
  end_day = "2020-09-28",
  as_of = "2020-09-28", #2020-12-21",
  geo_type = "hrr",
  geo_values = "311") %>% # NYC
  aggregate_signals(format = "wide")

nas <- complete.cases(sigs)
sigs <- sigs[nas,]

names(sigs)[3:4] <- c("cases", "doctor-visits")

std_sigs <- sigs %>%
  mutate(across(c("cases","doctor-visits"), ~ (.x - mean(.x)) / sd(.x))) %>%
  rename(Cases = cases, `DV-CLI` = `doctor-visits`) %>%
  select(-geo_value) %>%
  pivot_longer(-time_value)
  
g1 <- std_sigs %>%
  ggplot(aes(time_value, value, color = name)) +
  geom_line() +
  theme_bw() +
  ylab("Standardized signal") +
  xlab("") +
  scale_color_viridis_d(begin=.25, end=.75, name = "") +
  theme(legend.position = "bottom")

cc <- ccf(sigs$`doctor-visits`, sigs$cases, plot = FALSE)
g2 <- ggplot(tibble(lag = drop(cc$lag), ccf = drop(cc$acf)), aes(lag)) +
  geom_segment(aes(xend = lag, yend = ccf), y = 0, color = "cornflowerblue") +
  geom_hline(yintercept = qnorm((1 + .95) / 2) / sqrt(cc$n.used),
             color = "darkorange", linetype = "dashed") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  ylab("Cross correlation") +
  xlab("Lag (a)") +
  theme_bw() +
  theme(legend.position = "none")

leg <- cowplot::get_legend(g1)

gg <- cowplot::plot_grid(g1 + theme(legend.position = "none"), g2)
gg <- cowplot::plot_grid(gg, leg, nrow = 2, rel_heights = c(1,.1))
gg
