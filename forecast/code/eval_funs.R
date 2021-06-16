library(tidyverse)
library(lubridate)
library(evalcast)

Mean <- function(x) mean(x, na.rm = TRUE)
Median <- function(x) median(x, na.rm = TRUE)
TrimMean01 <- function(x) mean(x, trim = .01, na.rm = TRUE)
TrimMean025 <- function(x) mean(x, trim = .025, na.rm = TRUE)
TrimMean05 <- function(x) mean(x, trim = .05, na.rm = TRUE)
TrimMean1 <- function(x) mean(x, trim = .1, na.rm = TRUE)
GeoMean <- function(x) exp(mean(log(x), na.rm = TRUE))

fcast_colors <- c(RColorBrewer::brewer.pal(5, "Set1"), "#000000")
names(fcast_colors) <- c("CHNG-CLI", "CHNG-COV", "CTIS-CLIIC", "DV-CLI",
                         "Google-AA", "AR")

summarizer <- function(df, y, centerer, scaler, aggr, order_of_operations) {
  for (i in seq_along(order_of_operations)) {
    df <- switch(
      order_of_operations[i],
      center = {
        if (is.null(centerer)) df
        else mutate(df, !!y := !!sym(y) - !!sym(centerer))},
      scale = {
        if (is.null(scaler)) df
        else mutate(df, across(all_of(c(y, centerer)), ~.x / !!sym(scaler)))},
      aggr = summarise(df, across(all_of(c(y, centerer, scaler)), ~aggr(.x))))
  }
  return(df)
}


plotter <- function(df, y, aggr,
                    centerer = NULL, scaler = NULL, 
                    facet_by_period = FALSE,
                    order_of_operations = c("center", "scale", "aggr")) {
  df <- df %>% group_by(ahead, forecaster)
  facet_layer <- NULL
  if (facet_by_period) {
    period_labels <- c(jj = "June - July", as = "Aug - Sep", o = "Oct",
                       nd = "Nov - Dec", jm = "Jan - Mar")
    df <- df %>% group_by(period, .add = TRUE)
    facet_layer <- facet_wrap(~period, scales = "free_y", 
                              labeller = labeller(period = period_labels))
  }
  
  df <- summarizer(df, y, centerer, scaler, aggr, order_of_operations)
  
  
  ggplot(df, aes(ahead, wis, color = forecaster)) + 
    geom_line() + geom_point() +
    facet_layer +
    theme_bw() + 
    xlab("Days ahead") +
    scale_color_manual(values = fcast_colors, guide = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom", legend.title = element_blank())
}

plotter_hotspots <- function(df, facet_by_period = FALSE) {
  df <- df %>% group_by(ahead, forecaster)
  facet_layer <- NULL
  if (facet_by_period) {
    period_labels <- c(jj = "June - July", as = "Aug - Sep", o = "Oct",
                       nd = "Nov - Dec", jm = "Jan - Mar")
    df <- df %>% group_by(period, .add = TRUE)
    facet_layer <- facet_wrap(~period, scales = "free_y", 
                              labeller = labeller(period = period_labels))
  }
  
  df <- summarize(df, auc = auc(value, actual))
  
  ggplot(df, aes(ahead, auc, color = forecaster)) + 
    geom_line() + 
    geom_point() +
    facet_layer +
    theme_bw() + 
    ylab("AUC") +
    xlab("Days ahead") +
    scale_color_manual(values = fcast_colors, guide = guide_legend(nrow=1)) +
    theme(legend.position = "bottom", legend.title = element_blank())
}

period_processing <- function(res) {
  res <- mutate(res, period = case_when(
    month(forecast_date) %in% 6:7 ~ "jj",
    month(forecast_date) %in% 8:9 ~ "as",
    month(forecast_date) %in% 10 ~ "o",
    month(forecast_date) %in% 11:12 ~ "nd",
    month(forecast_date) %in% 1:3 ~ "jm"))
  res <- mutate(res, 
                period = fct_relevel(as.factor(period), 
                                     "jj","as","o","nd", "jm"))
  res
}

process_res_hotspots <- function(res) {
  res <- res %>% 
    mutate(forecaster = recode(forecaster,
                               AR3 = "AR",
                               AR3CHCLI3 = "CHNG-CLI",
                               AR3CHCOV3 = "CHNG-COV",
                               AR3DV3 = "DV-CLI",
                               AR3FB3 = "CTIS-CLIIC",
                               AR3GG3 = "gs_subset",
                               AR3GG3_imputed = "gs"
    ))
  
  # create google subset forecaster that inherits AR3 where missing
  gs_results <- res %>% 
    filter(forecaster == 'gs_subset') %>%
    select(-forecaster) %>%
    rename(value_gs = value)
  
  ar3_results <- res %>% filter(forecaster == 'AR')
  inherited_res <- left_join(ar3_results, gs_results) %>%
    mutate(forecaster = 'gs_inherit',
           value = coalesce(value_gs, value)) %>% 
    select(-value_gs)
  res <- bind_rows(res, inherited_res)
  res <- period_processing(res)
  return(res)
}


process_res_cases <- function(res, actuals) {
  res <- res %>% 
    mutate(forecaster = recode(forecaster,
                               AR3 = "AR",
                               AR3CHCLI3 = "CHNG-CLI",
                               AR3CHCOV3 = "CHNG-COV",
                               AR3DVCLI3 = "DV-CLI",
                               AR3FBCLI3 = "CTIS-CLIIC",
                               AR3GSSAA3_Subset = "gs_subset",
                               AR3GSSAA3_Zero = "gs",
                               Baseline = "strawman"
    ))
  strawman <- filter(res, forecaster == "strawman") %>%
    select(ahead, forecast_date, geo_value, wis, ae) %>%
    rename(strawman_wis = wis, strawman_ae = ae)
  res <- filter(res, forecaster != "strawman")
  
  # create google subset forecaster that inherits AR3 where missing
  gs_results <- res %>% 
    filter(forecaster == 'gs_subset') %>%
    select(-forecaster) %>%
    rename(wis_gs = wis, ae_gs = ae)
  
  ar3_results <- res %>% filter(forecaster == 'AR')
  inherited_res <- left_join(ar3_results, gs_results) %>%
    mutate(forecaster = 'gs_inherit',
           wis = coalesce(wis_gs, wis),
           ae = coalesce(ae_gs, ae)) %>% 
    select(-wis_gs, -ae_gs)
  res <- bind_rows(res, inherited_res)
  res <- left_join(res, strawman)
  res <- left_join(res, actuals %>% rename(actual_td = actual))
  res <- left_join(res, 
                   actuals %>% rename(actual_fd = actual, 
                                      forecast_date = target_end_date))
  res <- period_processing(res)
  return(res)
}

filter_case_floor <- function(res, cases7dav, params) {
  res <- left_join(
    res, cases7dav, 
    by=c("forecast_date" = "time_value", "geo_value" = "geo_value")) %>%
    filter(case_num >= params$fd_casefloor) %>%
    select(-case_num)
  res
}

filter_jumps <- function(res, actuals, params) {
  jumps <- actuals %>%
    group_by(geo_value) %>%
    arrange(target_end_date) %>%
    mutate(diff = actual - lag(actual),
           std = diff / mad(diff, na.rm = TRUE),
           flag = 
             (diff > 0 & diff > params$flag_jumps * mad(diff, na.rm = TRUE) & 
                actual > 0 & lead(diff, 7) < 0 & 
                lead(diff, 7) < -params$flag_jumps * mad(diff, na.rm = TRUE)) |
             (actual < 0),
           flag = flag & !lag(flag, 7),
           flag = RcppRoll::roll_maxr(flag, 7) > 0) %>%
    select(geo_value, target_end_date, flag)
  warning("Percent of target_dates  removed: ", 
          round(mean(jumps$flag, na.rm = TRUE),3)*100, 
          "%.")
  res <- left_join(res, jumps) %>% filter(!flag) %>% select(-flag)
  res
}

filter_holidays <- function(res) {
  thanksgiving <- seq(ymd("2020-11-26"), ymd("2020-12-06"), by = 1)
  christmas <- seq(ymd("2020-12-22"), ymd("2021-01-07"), by = 1)
  holiday_craziness = c(thanksgiving, christmas)
  res <- filter(res, !(target_end_date %in% holiday_craziness))
  res
}
