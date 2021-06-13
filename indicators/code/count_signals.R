library(covidcast)
library(dplyr)

covidcast_meta() %>%
  group_by(data_source, signal) %>%
  summarize(county = ifelse("county" %in% geo_type, "*", ""),
            msa = ifelse("msa" %in% geo_type, "*", ""),
            hrr = ifelse("hrr" %in% geo_type, "*", ""),
            state = ifelse("state" %in% geo_type, "*", ""),
            max_time = min(max_time, na.rm = TRUE)) %>%
  mutate(signal = paste(data_source, signal, sep=":")) %>%
  ungroup() %>%
  slice(grep("(raw|7dav|\\_w)", signal, invert = TRUE)) %>%
  filter(max_time >= "2021-06-01") %>%
  summarize(num_sources = length(unique(data_source)),
            num_signals = length(unique(signal)))
