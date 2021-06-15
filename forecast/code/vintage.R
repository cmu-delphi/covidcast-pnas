library(covidcast)
library(tidyverse)

as_ofs <- lubridate::ymd("2020-11-01") + 0:6
sigs <- list()
for (i in seq_along(as_ofs)) {
  sigs[[i]] <- covidcast_signals(
    data_source = c("jhu-csse","fb-survey","doctor-visits"),
    signal = c("confirmed_incidence_num","smoothed_wcli","smoothed_cli"),
    geo_type = "hrr",
    geo_values = "56", # LA HRR
    start_day = "2020-10-01",
    end_day = "2020-11-01",
    as_of = as_ofs[i]) %>%
    aggregate_signals(format = "long") %>%
    mutate(as_of = as_ofs[i])
}
sigs <- bind_rows(sigs) %>% select(-c(issue:dt))
nam <- c("jhu-csse" = "cases", 
         "fb-survey" = "fb-survey",
         "doctor-visits" = "doctor-visits")

sigs %>% 
  mutate(vintage = fct_relevel(as.factor(as_of), rev)) %>%
  ggplot(aes(time_value, value, color = vintage)) +
  geom_line() + 
  facet_wrap(~data_source, scales = "free_y", ncol = 1, 
             labeller = labeller(data_source = nam)) +
  theme_bw() +
  xlab("date") + ylab("") +
  scale_color_viridis_d() +
  geom_vline(xintercept = lubridate::ymd("2020-11-01")) +
  scale_x_date(limits = lubridate::ymd(c("2020-10-26","2020-11-01")),
               expand = expansion(c(0,.05)))

ggsave("../gfx/vintage.pdf", width = 6, height = 4)
