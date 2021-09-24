fcast_colors <- c(RColorBrewer::brewer.pal(5, "Set1"), "#000000")
names(fcast_colors) <- c("CHNG-CLI", "CHNG-COVID", "CTIS-CLIIC", "DV-CLI",
                         "Google-AA", "AR")


Mean <- function(x) mean(x, na.rm = TRUE)
Median <- function(x) median(x, na.rm = TRUE)
TrimMean01 <- function(x) mean(x, trim = .01, na.rm = TRUE)
TrimMean025 <- function(x) mean(x, trim = .025, na.rm = TRUE)
TrimMean05 <- function(x) mean(x, trim = .05, na.rm = TRUE)
TrimMean1 <- function(x) mean(x, trim = .1, na.rm = TRUE)
GeoMean <- function(x) exp(mean(log(x), na.rm = TRUE))


pct_change <- function(x, n = 14, col_name = "pct_change"){
  # courtesy of Alden
  dt <- c(0,-n)
  # y = covidcast:::apply_shifts(x,dt) %>%  clobbered in recent upgrade
  x = x %>%
    dplyr::group_by(geo_value) %>%
    dplyr::arrange(time_value)
  for (n in dt) {
    varname <- sprintf("value%+d", n)
    x <- x %>% dplyr::mutate(!!varname := covidcast:::shift(value, n))
  }
  y <- dplyr::select(x, -value) %>%
    rename(value = tidyselect::matches("value\\+"),
           lag_value = tidyselect::matches("value\\-")) %>%
    mutate(`:=`(!!col_name,(value - lag_value) / lag_value))
  return(y)
}
