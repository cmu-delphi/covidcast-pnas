#------------------------------------------------------------------------------#
# Functions that quantize a response variable as being (or not being) a hotspot.
#
# Parameters
# -- x Data frame of signal values to use for forecasting, of the format
#      returned by [covidcast::covidcast_signals()]. Importantly, x should
#      have only one data source and one signal.
#
# Return
# -- Data frame with columns data_source, signal, geo_value, time_value, value.
#    value is 1/0, whether or not the response variable was a hotspot.
#
# TODO:
# -- (1) Fix this description.
# -- (2) Do the same but for downswings.
# -- (3) Change the implementation to use modeltools::pct_change() fxn.
#------------------------------------------------------------------------------#

detect_upswings_basic <- function(x) {
  y = alden_pct_change(x,hotspot_lag) %>%
    mutate(hotspot = case_when(
      pct_change > upswing_threshold & lag_value > min_threshold ~ 1,
      pct_change <= upswing_threshold & lag_value > min_threshold ~ 0)
    )
  z = y %>% select(-c(value,lag_value)) %>%
    rename(value = hotspot) %>%
    mutate(signal = "hotspot")

  return(z)
}

# Detect upswings based on a statistical test.
#
# We model value ~ Poisson(lambda), lag_value ~ Poisson(lambda') independently.
# and test H_0:lambda/lambda' <= lambda_0 vs. H_a:lambda/lambda' > lambda_0.
#
# We use the test statistic T = value/(value + lag_value).
# Using Normal approximation + Delta method, letting
#     p_0 := lambda_0/(1 + lambda_0), and
#     z_tau := 1 - tau quantile of Normal distribution
#
# we have that
# P_{H_0}(T > p_0 + z_tau*sqrt(p_0(1 - p_0)/value + lag_value)) \lessapprox tau.
#
# Thus tau (approximately) controls the probability of Type I error.
detect_upswings_stat <- function(x){
  dt <- c(0,-hotspot_lag)

  lambda_0 = 1/(1 - upswing_threshold)
  p_0 = lambda_0/(1 + lambda_0)

  x = covidcast:::apply_shifts(x,dt) %>%
    rename(value = tidyselect::matches("value\\+"),
           lag_value = tidyselect::matches("value\\-")) %>%
    mutate(threshold = p_0 + z_tau*sqrt(p_0*(1-p_0)/(value + lag_value)),
           prop = ifelse(lag_value + value > 0, value/(lag_value + value),0)) %>% # TODO: CHANGE THIS BACK!
    mutate(hotspot = ifelse(prop > threshold,1,0))

  x = x %>% select(-c(value,lag_value, threshold, prop)) %>%
    rename(value = hotspot) %>%
    mutate(signal = "hotspot")
}

# Compute the percent difference between values on day d and d - n.
alden_pct_change <- function(x,n = 14,col_name = "pct_change"){
  dt <- c(0,-n)
  y = covidcast:::apply_shifts(x,dt) %>%
    rename(value = tidyselect::matches("value\\+"),
           lag_value = tidyselect::matches("value\\-")) %>%
    mutate(`:=`(!!col_name,(value - lag_value)/lag_value))
  return(y)
}
