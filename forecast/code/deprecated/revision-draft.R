# Cumulative sum/mean prediction error plot -----------------------------------

cumulatives <- fcasts_honest %>%
  group_by(forecaster, forecast_date) %>%
  summarise(mw = Mean(wis),
            smw = Mean(strawman_wis),
            gmw = GeoMean(wis / strawman_wis)) %>%
  arrange(forecast_date) %>%
  mutate(`Cumulative Mean` = cummean(mw) / cummean(smw),
         `Cumulative Sum` = cumsum(mw) / cumsum(smw),
         `Cumulative Geo Mean` = exp(cummean(log(gmw))),
         `GM Regret` = cumsum(gmw) - cumsum(smw/smw),
         `AM Regret` = cumsum(mw) - cumsum(smw),
         `14 day trailing average` = RcppRoll::roll_meanl(mw / smw, n = 14L))

reltoAR <- cumulatives %>% 
  select(forecaster, forecast_date, mw, gmw) %>%
  ungroup()
reltoAR <- left_join(
  reltoAR %>% filter(forecaster != "AR"),
  reltoAR %>% filter(forecaster == "AR") %>% select(-forecaster) %>%
    rename(armw = mw, argmw = gmw)
)  

reltoAR <- reltoAR %>%
  group_by(forecaster) %>%
  arrange(forecast_date) %>%
  mutate(cm = (mw - armw) / armw)

  
fcast_colors <- c(RColorBrewer::brewer.pal(5, "Set1"), "#000000")
names(fcast_colors) <- c("CHNG-CLI", "CHNG-COVID", "CTIS-CLIIC", "DV-CLI",
                         "Google-AA", "AR")



ggplot(cumulatives %>% filter(forecast_date < "2021-01-01"), 
       aes(forecast_date, color = forecaster)) + 
  geom_line(aes(y = `Cumulative Sum`)) +
  geom_hline(yintercept = 1) +
  xlab("forecast date") +
  scale_color_manual(values = fcast_colors, guide = guide_legend(nrow = 1)) +
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank())

ggplot(reltoAR %>% filter(forecast_date < "2021-01-01"), 
       aes(forecast_date, color = forecaster)) + 
  geom_line(aes(y = cm)) +
  geom_hline(yintercept = 0) +
  xlab("forecast date") +
  scale_color_manual(values = fcast_colors, guide = guide_legend(nrow = 1)) +
  theme_bw() +
  scale_y_continuous(labels = scales::percent_format()) +
  theme(legend.position = "bottom", legend.title = element_blank())


# Sign test ---------------------------------------------------------------

fcast_colors2 <- fcast_colors[names(fcast_colors) != "AR"]

st <- fcasts_honest %>%
  mutate(wis = wis / strawman_wis) %>%
  select(forecaster, geo_value, ahead, forecast_date, wis) %>%
  pivot_wider(names_from = forecaster, values_from = wis) %>%
  mutate(across(AR:`Google-AA`, ~ AR - .x)) %>%
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

ggplot(st, aes(forecast_date, p, color = forecaster)) +
  geom_line() +
  scale_color_manual(values = fcast_colors2, guide = guide_legend(nrow = 1)) +
  ylab("P-value") +
  xlab("forecast_date") +
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



# Average gain in days ahead by location ---------------------------------

days_gained <- function(forecaster, reference, aheads, ahead = 14L, 
                        smaller_is_better = TRUE) {
  n <- length(forecaster)
  # check for non-monotonicity
  if (!any(aheads == ahead)) return(NA)
  if (smaller_is_better) {
    if (is.unsorted(forecaster)) return(NA) 
  } else {
    if (is.unsorted(rev(forecaster))) return(NA)
  }
  
  ystar <- reference[aheads == ahead]
  diffs <- forecaster - ystar
  up_idx <- min(which(diffs >= 0), n + 1) # catch Infty
  if (up_idx > n) return(max(aheads - ahead))
  down_idx <- max(which(diffs < 0), 0)
  if (down_idx < 1) return(min(aheads - ahead))
  y1 <- forecaster[up_idx]
  x1 <- aheads[up_idx]
  m <- (y1 - forecaster[down_idx]) / (x1 - aheads[down_idx])
  b <- y1 - m * x1
  xstar <- (ystar - b) / m
  
  return(xstar - ahead)
}

tt <- fcasts_honest %>% 
  filter(period != "jm") %>% 
  select(forecaster, geo_value, ahead, forecast_date, wis, strawman_wis) %>%
  arrange(geo_value, forecast_date, ahead) %>%
  group_by(geo_value, ahead, forecaster) %>%
  summarize(wis = GeoMean(wis)) %>%
  pivot_wider(names_from = forecaster, values_from = wis) %>%
  arrange(geo_value, ahead) %>%
  group_by(geo_value)
  summarize(across(`CHNG-CLI`:`Google-AA`, ~days_gained(.x, AR, ahead))) %>%
  pivot_longer(-geo_value)

tt %>%
  ggplot() +
  geom_density(aes(value, fill = name), color = NA, outline.type = "upper", alpha = .2) +
  geom_vline(xintercept = 0) +
  geom_vline(data = tt %>% group_by(name) %>% summarise(value = Mean(value)), 
             aes(xintercept = value, color = name)) +
  scale_color_manual(values = fcast_colors2) +
  scale_fill_manual(values = fcast_colors2) +
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  xlab("Days gained") +
  ylab("")
