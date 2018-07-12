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
#' @param x Variable to use on X-axis. Must be of type 'character' or 'factor'.
#' @param y Variable to use on Y-axis. Must be of type 'numeric' or 'integer'.
#' @param group (Optional) Variable to group by
#' @param group_sign (`logical`) (Optional) Group by sign (positive or negative)
#'   for each segment? A new column will be created consisting of `pos`, `neg`,
#'   `anchor` for each segment (factor levels will be set in order listed).
#'   If `TRUE`, will override `group`. Default is `FALSE`.
#' @param anchor (Optional) Vector of x values or numerical indices which will touch the X-axis.
#' @param anchor.first (Optional) Anchor the first segment of the waterfall? Default is TRUE.
#' @param anchor.last (Optional) Anchor the last segment of the waterfall? Default is TRUE.
#' @param palette (Optional) Color palette to use for fill. Default is c('blue4', 'red', 'green').
#' @param title (Optional) Plot title
#' @param subtitle (Optional) Plot subtitle
#' @param width (Optional) Width of segments, expressed as a number between 0
#'   and 1, with 0 meaning segments will not be shown, and 1 meaning segments
#'   will be touching each other. Default is 0.8.
#' @param xlab (Optional) Axis label for x-axis
#' @param ylab (Optional) Axis label for y-axis
#' @param x.rotate (Optional) Degrees to rotate x-axis labels. Default is 0.
#'   Will be passed to \code{ggplot2::theme(axis.text.x = element_text(angle =
#'   ))}.
#' @param linetype (Optional) Type of line to use to connect waterfall segments.
#'   Default is 'dashed'. Will be passed to \code{ggplot2::geom_step(linetype =
#'   )}.
#' @param linecolor (Optional) Color of line used to connect waterfall segments.
#'   Default is 'black'. Will be passed to \code{ggplot2::geom_step(color = )}.
#' @param border (Optional) Color of border to use for waterfall segments.
#'   Default is 'black'. Will be passed to \code{ggplot2::geom_rect(color = )}.
#' @param legend.position (Optional) Where to show the legend for the fill
#'   colors. Default is 'none'. Will be passed to
#'   \code{ggplot2::theme(legend.position = )}.
#' 
#' @export
plot_waterfall <- function(data, x, y, group = NULL, group_sign = FALSE, anchor = NULL, 
                           anchor.first = TRUE, anchor.last = TRUE, palette = c('green', 'red', 'blue4'), 
                           title = NULL, subtitle = NULL, width = 0.8, xlab = NULL, ylab = NULL, 
                           x.rotate = 0, linetype = "dashed", linecolor = "black", border = "black", 
                           legend.position = "none") {

  checkmate::assert_data_frame(data)
  checkmate::assert_choice(x, names(data))
  checkmate::assert_choice(y, names(data))
  checkmate::assert_numeric(data[[y]])
  checkmate::assert_choice(group, names(data), null.ok = TRUE)
  checkmate::assert_number(width, lower = 0, upper = 1)
  checkmate::assert_number(x.rotate, lower = -360, upper = 360)
  
  x_length <- nrow(data)
  
  if(class(anchor) == "numeric") {
    anchor <- (1:x_length %in% anchor)
  } else if(class(anchor) %in% c("character", "factor")) {
    anchor <- (as.character(data[[x]]) %in% as.character(anchor))
  } else if(class(anchor) == "logical") {
    if(length(anchor) != x_length) stop("Vector 'anchor' must be same length as data")
  } else {
    # do nothing
  }
  
  if(anchor.first == TRUE) anchor[1] <- TRUE
  if(anchor.last == TRUE) anchor[length(anchor)] <- TRUE
  
  data$anchor <- anchor
  
  wrapr::let(c(X = x,
               Y = y,
               GROUP = group),
             {
               x_labels <- as.character(data$X)
               
               data <- data %>%
                 dplyr::mutate(values_lag = dplyr::lag(Y, 1),
                        values_lag = ifelse(is.na(values_lag), 0, values_lag),
                        direction = ifelse(Y >= values_lag, "pos", "neg"),
                        direction = ifelse(anchor, "anchor", direction),
                        direction = factor(direction, levels = c("pos", "neg", "anchor")),
                        ymin = pmin(Y, values_lag),
                        ymin = ifelse(anchor, 0, ymin),
                        ymax = pmax(Y, values_lag),
                        temp = 1,
                        x_index = cumsum(temp),
                        xmin = x_index - width / 2,
                        xmax = x_index + width / 2,
                        display = ifelse(anchor, Y, Y - values_lag),
                        display_y = ifelse(anchor, ymax + 0.5, (ymin + ymax) / 2))
               
               ggplot(data) +
                 geom_step(aes(x = x_index, y = Y), linetype = "dashed") +
                 geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = direction), color = border) +
                 geom_text(aes(x = x_index, y = display_y, label = display)) +
                 scale_fill_manual(values = palette) +
                 scale_x_continuous(breaks = 1:x_length, labels = data$X) +
                 geom_hline(yintercept = 0) +
                 labs(title = title, subtitle = subtitle, 
                      x = xlab, y = ylab) +
                 theme(plot.title = element_text(hjust = 0.5),
                       plot.subtitle = element_text(hjust = 0.5),
                       axis.text.x = element_text(angle = x.rotate, 
                                                  hjust = ifelse(x.rotate == 0, 0.5, 1)),
                       legend.position = legend.position)
             })
  }