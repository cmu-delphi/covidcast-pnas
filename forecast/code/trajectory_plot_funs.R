aggregate_rank <- function(scored,
                           period = c("all","only2020"), 
                           aggr = Mean,
                           relative = TRUE) {
  period <- match.arg(period)
  if (period == "only2020") scored <- filter(scored, period != "jm")
  ranking <- scored %>%
    select(forecaster:wis) %>%
    pivot_wider(names_from = forecaster, values_from = wis) %>%
    group_by(forecast_date, geo_value) %>%
    arrange(ahead, .by_group = TRUE) %>%
    summarise(across(AR:`Google-AA`, ~aggr(.x)))
  if (relative) {
    ranking <- ranking %>%
      mutate(across(`CHNG-CLI`:`Google-AA`, ~.x / AR))
  }
  ranking <- ranking %>%
    select(-QAR3) %>%
    pivot_longer(`CHNG-CLI`:`Google-AA`, 
                 names_to = "forecaster", 
                 values_to = "relative_wis") %>%
    ungroup()
  return(ranking)
}


trajectory_processing <- function(scored_preds,
                                  predictions,
                                  actuals,
                                  period = c("all", "only2020"),
                                  best_worst = c("best", "worst"),
                                  aggr = Mean,
                                  relative = TRUE,
                                  n_slice = 5, 
                                  truth_data_window = 30) {
  ranking <- aggregate_rank(scored_preds, period, aggr, relative)
  best_worst <- match.arg(best_worst)
  n_best <- ranking %>% group_by(forecaster)
  if (best_worst == "best") {
    n_best <- n_best %>% slice_min(relative_wis, n = n_slice) 
  } else {
    n_best <- n_best %>% slice_max(relative_wis, n = n_slice) 
  }
  n_best <- n_best %>% select(-relative_wis)
  n_best <- n_best %>% 
    bind_rows(n_best %>% mutate(forecaster = "AR")) %>%
    distinct()
  
  forecasts <- predictions %>%
    mutate(forecaster = recode(forecaster,
                               AR3 = "AR",
                               AR3CHCLI3 = "CHNG-CLI",
                               AR3CHCOV3 = "CHNG-COV",
                               AR3DVCLI3 = "DV-CLI",
                               AR3FBCLI3 = "CTIS-CLI-in-community",
                               AR3GSSAA3_Subset = "gs_subset",
                               AR3GSSAA3_Zero = "gs",
                               Baseline = "strawman")) %>%
    
    right_join(n_best) %>%
    mutate(target_end_date = forecast_date + ahead)
    
  
  truth_sigs <- n_best %>% 
    filter(forecaster != "AR") %>%
    select(forecaster, forecast_date, geo_value) %>%
    mutate(start_day = forecast_date - truth_data_window,
           end_day = forecast_date + truth_data_window)
  
  truth_df <- pmap_dfr(truth_sigs, function(...) {
    sig <- list(...)
    actuals %>% 
      filter(
        geo_value == sig$geo_value,
        target_end_date >= sig$start_day & target_end_date <= sig$end_day) %>%
      rename(time_value = target_end_date) %>%
      mutate(forecaster = sig$forecaster,
             forecast_date = sig$forecast_date)}
  )
  return(list(forecasts = forecasts,
              truth_df = truth_df,
              truth_sigs = truth_sigs))
}


pivot_forecasts <- function(forecast_data, tol = 1e-6) {
  points <- forecast_data %>%
    filter(abs(quantile -  0.5) < tol) %>%
    mutate(type = "point") %>%
    rename(point = value) %>%
    select(-quantile)
  
  # get quantile forecasts and generate corresponding prediction interval
  quantiles <- forecast_data %>%
    filter(abs(quantile - 0.5) > tol) %>%
    mutate(endpoint_type = ifelse(quantile < 0.5, 'lower', 'upper'),
           alpha = ifelse(endpoint_type == 'lower',
                          format(2*quantile, digits=3, nsmall=3),
                          format(2*(1-quantile), digits=3, nsmall=3)),
           interval = forcats::fct_rev(paste0((1-as.numeric(alpha))*100, "%"))
    ) %>%
    select(-quantile, -alpha) %>%
    pivot_wider(names_from='endpoint_type', values_from='value')
  
  list(points_df = points, fan_df = quantiles)
}

get_trajectory_plots <- function(scored_preds,
                                 predictions,
                                 actuals,
                                 hrr_tab,
                                 period = c("all", "only2020"),
                                 best_worst = c("best", "worst"),
                                 aggr = Mean,
                                 relative = TRUE,
                                 n_slice = 5, 
                                 truth_data_window = 30) {
  trajectory_data_list <- trajectory_processing(
    scored_preds, predictions, actuals, period, best_worst, aggr,
    relative, n_slice, truth_data_window)
  
  forecasts <- trajectory_data_list$forecasts
  truth_df <- trajectory_data_list$truth_df
  truth_sigs <- trajectory_data_list$truth_sigs
  plots <- list()
  model_count <- n_distinct(forecasts$forecaster)
  model_colors <- grDevices::colorRampPalette(
    RColorBrewer::brewer.pal(model_count, "Set1"))(model_count)
  names(model_colors) <- sort(unique(forecasts$forecaster))
  interval_colors <- sapply(model_colors, function(cc) 
    colorspace::lighten(cc, c(.8,.6,.4)))
  truth_df <- truth_df %>% 
    mutate(hrr_fd = paste(geo_value, forecast_date, sep = "-"))
  truth_list <- truth_df %>% 
    group_by(forecaster) %>%
    group_split()
  nm <- unique(truth_df$hrr_fd)
  hrr_labels <- hrr_tab[str_extract(nm, "^([^-]+)")]
  names(hrr_labels) <- nm
  
  for (i in seq_along(model_colors[-1])) {
    fcaster <- names(model_colors[-1])[i]
    ids <- distinct(truth_list[[i]] %>% select(geo_value, forecast_date, hrr_fd))
    fcast_ids <- bind_rows(ids %>% mutate(forecaster = fcaster), 
                           ids %>% mutate(forecaster = "AR")) %>%
      select(-hrr_fd)
    fcasts <- semi_join(forecasts, fcast_ids)
    fcasters <- c("QAR3", fcaster)
    pivoted <- pivot_forecasts(fcasts)
    
    fan_df <- pivoted$fan_df %>% 
      mutate(hrr_fd = paste(geo_value, forecast_date, sep = "-"))
    points_df <- pivoted$points_df %>% 
      mutate(hrr_fd = paste(geo_value, forecast_date, sep = "-"))
    
    g <- ggplot() + 
      # first the fan
      geom_ribbon(
        data = fan_df, 
        mapping = aes(x = target_end_date, 
                      ymin = lower, 
                      ymax = upper,
                      group = interaction(interval, forecaster),
                      fill = interaction(interval, forecaster)),
        alpha = .5,
        show.legend = FALSE) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b") +
      scale_fill_manual(values = c(interval_colors[,fcasters])) +
      # then the truth
      geom_line(data = truth_list[[i]], aes(x = time_value, y = actual)) +
      # then the point forecast
      geom_line(data = points_df, 
                aes(x = target_end_date, y = point, color = forecaster)) +
      scale_color_manual(values = model_colors[fcasters]) +
      theme_bw() +
      facet_wrap(~hrr_fd, scales = "free", ncol = n_slice,
                 labeller = labeller(hrr_fd = hrr_labels)) +
      geom_vline(data = ids, aes(xintercept = forecast_date)) +
      theme(legend.position = "none", plot.margin = margin()) +
      xlab("") + ylab("") # + ggtitle(hrr_tab[ids$geo_value])
    plots[[i]] <- g
  }
  names(plots) <- names(model_colors[-1])
  return(plots)
}

trajectory_panel <- function(forecaster, best, worst){
  bplot <- best[[forecaster]]
  wplot <- worst[[forecaster]]
  
  df <- tibble(forecaster = rep(c("AR", forecaster), each = 10),
               x = rnorm(20),
               y = rnorm(20))
  g <- ggplot(df, aes(x, y, color = forecaster)) + geom_line() +
    guides(color = guide_legend(nrow=1)) +
    theme_bw() +
    scale_color_manual(values = model_colors[c("AR", forecaster)]) +
    theme(legend.position = "bottom", legend.title = element_blank())
  ll <- cowplot::get_legend(g)
  
  p1 <- cowplot::plot_grid(bplot, wplot, ncol = 1, align = "hv")
  cowplot::plot_grid(p1, ll, rel_heights = c(1,.1), ncol = 1)
}


