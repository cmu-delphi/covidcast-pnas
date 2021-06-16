# Known major case revision on Dec 29
# HRR 303 is NYC

as_ofs <- seq(as.Date("2020-12-3"), as.Date("2020-12-31"), by = "week")
cases_as_of <- map_dfr(as_ofs, function(as_of) {
  covidcast_signal(data_source = "jhu-csse", 
                   signal = "confirmed_incidence_prop",
                   start_day = "2020-10-01", end_day = "2020-12-01", 
                   geo_type = "hrr", geo_values = "303", as_of = as_of) %>%
    mutate(as_of = as_of)
})
dv_as_of <-  map_dfr(as_ofs, function(as_of) {
  covidcast_signal(data_source = "doctor-visits", 
                   signal = "smoothed_adj_cli",
                   start_day = "2020-10-01", end_day = "2020-12-01", 
                   geo_type = "hrr", geo_values = "303", as_of = as_of) %>%
    mutate(as_of = as_of)
})


pp <- bind_rows(cases_as_of, dv_as_of) %>% 
  mutate(as_of = fct_relabel(factor(as_of), function(x) strftime(x, "%b %d")),
         data_source = recode(data_source, `doctor-visits` = "DV-CLI",
                              `jhu-csse` = "JHU-CSSE")) %>%
  ggplot(aes(x = time_value, y = value)) + 
  geom_line(aes(color = factor(as_of))) + 
  facet_wrap(~data_source, scales = "free_y") +
  theme_bw() +
  labs(x = "", y = "", color = "") +
  theme(legend.position = "bottom") +
  scale_color_viridis_d(end = .9, begin = .1)


