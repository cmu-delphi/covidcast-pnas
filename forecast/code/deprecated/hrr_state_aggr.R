library(tidyverse)
fips_hrr <- read_csv(
  "https://raw.githubusercontent.com/cmu-delphi/covidcast-indicators/main/_delphi_utils_python/delphi_utils/data/fips_hrr_table.csv", 
  col_types = "cid")

hf <- pivot_wider(fips_hrr, names_from = fips, values_from = weight)
hf <- hf %>% arrange(hrr)
hf_mat <- hf %>% select(-hrr) %>% as.matrix()
hf_mat[is.na(hf_mat)] <- 0
fh_mat <- MASS::ginv(hf_mat)
hrrs <- hf %>% pull(hrr) 
colnames(fh_mat) <- hrrs
fh <- as_tibble(fh_mat)
fh$geo_value <- colnames(hf_mat) %>% 
  substr(1, 2) %>% 
  covidcast::fips_to_abbr() %>% 
  tolower()
sh <- fh %>% 
  group_by(geo_value) %>% 
  summarise(across(everything(), sum))
sh_mat <- sh %>% select(-geo_value) %>% as.matrix()
sh_long <- sh %>% 
  pivot_longer(-geo_value, names_to = "hrr", values_to = "multiplier") %>%
  mutate(hrr = as.character(hrr)) %>%
  arrange(hrr)


# Process preds -----------------------------------------------------------

preds <- preds %>% 
  rename(hrr = geo_value) %>%
  group_by(forecaster, forecast_date, ahead, quantile) %>%
  arrange(hrr) #%>%
  group_modify( ~ {
    left_join(sh_long, .x, by = "hrr") %>%
      mutate(value = multiplier * value) %>%
      group_by(geo_value) %>%
      summarise(value = sum(value))
  })
  
## Need to re-sort and threshold to 0 afterward

