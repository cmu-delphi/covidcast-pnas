# Large spike in cases on 9/25 due to inclusion of positive antigen tests as 
# probable cases. Backfilled by JHU on 10/2 or there abouts.
# See https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data#retrospective-reporting-of-probable-cases-and-deaths

library(covidcast)
library(tidyverse)

as_ofs <- seq(as.Date("2020-09-28"), as.Date("2020-10-19"), by = "week")
cases_as_of <- map_dfr(as_ofs, function(as_of) {
  covidcast_signal(data_source = "jhu-csse", 
                   signal = "confirmed_incidence_prop",
                   start_day = "2020-08-15", end_day = "2020-09-26", 
                   geo_type = "hrr", geo_values = "311", as_of = as_of) %>%
    mutate(as_of = as_of)
})
dv_as_of <-  map_dfr(as_ofs, function(as_of) {
  covidcast_signal(data_source = "doctor-visits", 
                   signal = "smoothed_adj_cli",
                   start_day = "2020-08-15", end_day = "2020-09-26", 
                   geo_type = "hrr", geo_values = "311", as_of = as_of) %>%
    mutate(as_of = as_of)
})

  
bind_rows(cases_as_of, dv_as_of) %>% 
  mutate(as_of = fct_relabel(factor(as_of), function(x) strftime(x, "%b %d")),
         data_source = recode(data_source, `doctor-visits` = "DV-CLI",
                              `jhu-csse` = "JHU-CSSE")) %>%
  ggplot(aes(x = time_value, y = value)) + 
  geom_line(aes(color = factor(as_of))) + 
  facet_wrap(~data_source, scales = "free_y",
             strip.position = "left", 
             labeller = as_labeller(c(`DV-CLI` = "% doctor's visits due to CLI",
                                      `JHU-CSSE` = "Cases per 100,000 people"))) +
  theme_bw() +
  labs(x = "", y = "", color = "As of:") +
  theme(legend.position = "bottom") +
  scale_color_viridis_d(end = .9, begin = .1)

ggsave("../paper/fig/revisions.pdf", width = 5, height = 4)

