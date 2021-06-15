library(tidyverse)
fips_pop <- read_csv("https://raw.githubusercontent.com/cmu-delphi/covidcast-indicators/main/_delphi_utils_python/delphi_utils/data/fips_pop.csv",
                     col_types = "ci")
hrrtab <- read_csv("https://raw.githubusercontent.com/cmu-delphi/covidcast-indicators/main/_delphi_utils_python/delphi_utils/data/fips_hrr_table.csv",
                   col_types = "ccd")
hrrs <- left_join(hrrtab, fips_pop, by = "fips")
hrrs <- mutate(hrrs, wpop = pop * weight)
hrr_pop <- hrrs %>%
  group_by(hrr) %>%
  summarise(pop = sum(wpop, na.rm=TRUE))
# note, 02270, 46113, 51515 have pop NA. These seem no longer to exist (renumbered or removed)

hrr_names <- read_csv("Hospital_Referral_Regions.csv", col_types = "_ccc__") %>%
  mutate(state = substr(hrrcity, 1, 2))
hrr_pop <- left_join(hrr_pop, hrr_names, by = c("hrr" = "hrrnum")) %>%
  mutate(name = paste(HRR_lbl,State)) %>%
  select(hrr, name, pop)

ggplot(hrr_pop, aes(pop)) +
  geom_histogram(aes(y=(..count..)/sum(..count..)), bins = 20, fill = "cornflowerblue") +
  xlab("population") + ylab("frequency") +
  scale_x_log10(labels = scales::label_comma()) +
  scale_y_continuous(expand = expansion(c(0,.05)),
                     labels = scales::label_percent(accuracy = 1),
                     breaks = scales::breaks_pretty(4)) +
  theme_bw(base_family = "Times")

ggsave("../gfx/hrr_pop_histogram.pdf", width=6, height=3)
