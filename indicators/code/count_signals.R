# Count the unique signals in the COVIDcast API so the paper can quote an
# approximate number of signals.

library(covidcast)
library(dplyr)

covidcast_meta() %>%
  filter(data_source != "indicator-combination") %>%
  group_by(data_source, signal) %>%
  summarize(max_time = min(max_time, na.rm = TRUE)) %>%
  mutate(signal = paste(data_source, signal, sep = ":")) %>%
  ungroup() %>%
  slice(grep("(raw|7dav|\\_w)", signal, invert = TRUE)) %>%
  filter(max_time >= "2021-06-01") %>%
  summarize(num_sources = length(unique(data_source)),
            num_signals = length(unique(signal)))
