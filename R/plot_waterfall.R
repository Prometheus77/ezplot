#' @title Waterfall plot
#' 
#' @description 
#' Produce a waterfall plot using the geom_rect() function. A waterfall plot
#' shows a sequence of numbers as an increase or decrease from the cumulative
#' sum of the previous numbers. It is useful in breaking out a change in values
#' into components and comparing the relative size and direction of each
#' component. For an example, see:
#' \url{https://learnr.wordpress.com/2010/05/10/ggplot2-waterfall-charts/}
#' 
#' @param data (`data.frame` | `tibble`) Data to be used in plot
#' @param aggr (`function`) (Optional) function created using
#' @param x Variable to use on X-axis. Must be of type 'character' or 'factor'.
#' @param y Variable to use on Y-axis. Must be of type 'numeric' or 'integer'.
#' @param group (Optional) Variable to group by
#' @param group_sign (`logical`) (Optional) Group by sign (positive or negative)
#'   for each segment? A new column will be created consisting of `pos`, `neg`,
#'   `touch` for each segment (factor levels will be set in order listed).
#'   If `TRUE`, will override `group`. Default is `FALSE`.
#' @param direction = Should the waterfall build 'up' or 'down'? Default is 'up'.
#' @param touch (Optional) Vector of x-variables which will touch the X-axis. Default is
#'   the first and last variable in the sequence.
#' @param palette (Optional) Color palette to use for fill
#' 
#' @export
plot_waterfall <- function(data, aggr = NULL, x, y, group = NULL, group_sign = FALSE, 
                           direction = 'up', touch = NULL, palette = NULL) {
  
  if (hasArg("aggr")) {
    data <- group_by_at(data, .vars = unique(c(x, group, facet))) %>% aggr
  }
  
  data <- data %>%
    mutate()
}