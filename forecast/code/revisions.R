# Large spike in cases on 9/25 due to inclusion of positive antigen tests as 
# probable cases. Backfilled by JHU on 10/2 or there abouts.
# See https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data#retrospective-reporting-of-probable-cases-and-deaths

library(covidcast)
library(tidyverse)
library(gridExtra)
library(grid)

as_ofs <- seq(as.Date("2020-09-28"), as.Date("2020-10-19"), by = "week")
cases_as_of <- map_dfr(as_ofs, function(as_of) {
  covidcast_signal(data_source = "jhu-csse", 
                   signal = "confirmed_7dav_incidence_prop",
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

  
p1 <- dv_as_of %>%
  mutate(as_of = fct_relabel(factor(as_of), function(x) strftime(x, "%b %d"))) %>%
  ggplot(aes(x = time_value, y = value)) + 
  geom_line(aes(color = factor(as_of))) + 
  labs(title = "DV-CLI", x = "", y = "% doctor's visits due to CLI",
       color = "As of:") +
  theme_bw() + 
  theme(legend.position = "bottom") +
  scale_color_viridis_d(end = .9, begin = .1) +
  guides(color = guide_legend(nrow = 1))

p2 <- cases_as_of %>%
  mutate(as_of = fct_relabel(factor(as_of), function(x) strftime(x, "%b %d"))) %>%
  ggplot(aes(x = time_value, y = value)) + 
  geom_line(aes(color = factor(as_of))) + 
  labs(title = "Cases", x = "", y = "Cases per 100,000 people",
       color = "As of:") +
  theme_bw() + 
  theme(legend.position = "bottom") +
  scale_color_viridis_d(end = .9, begin = .1) +
  guides(color = guide_legend(nrow = 1))

# From https://github.com/tidyverse/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
grid_arrange_shared_legend <- function(...,
                                       ncol = length(list(...)),
                                       nrow = 1,
                                       position = c("bottom", "right")) {

  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)

  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))

  grid.newpage()
  grid.draw(combined)

  # return gtable invisibly
  invisible(combined)

}

p3 <- grid_arrange_shared_legend(p1, p2)

ggsave("../paper/fig/revisions.pdf", p3, width = 5, height = 4)

