#'@title Shortcuts
#'
#'@description
#'List of convenience functions that call ezplot with common defaults
#'pre-supplied (e.g. plot_type)

#'@export
ezline <- function(data, aggr = NULL, x, y = NULL, group = NULL, 
                   position = "identity", facet = NULL, facet_x = NULL, facet_y = NULL,
                   palette = NULL, ci = NULL, 
                   error_type = "errorbar", error_args = NULL, text = FALSE, 
                   text_round = function(x) signif(x, 3), text_args = NULL,
                   title = NULL, subtitle = NULL, caption = NULL, ...) {

  argList <- as.list(match.call(expand.dots = TRUE)[-1])
  argList$plot_type <- "line"
  do.call(ezplot, argList)
}

#'@export
ezcol <- function(data, aggr = NULL, x, y = NULL, group = NULL, 
                  position = "stack", facet = NULL, facet_x = NULL, facet_y = NULL,
                  palette = NULL, ci = NULL, 
                  error_type = "errorbar", error_args = NULL, text = FALSE, 
                  text_round = function(x) signif(x, 3), text_args = NULL,
                  title = NULL, subtitle = NULL, caption = NULL, ...) {

  argList <- as.list(match.call(expand.dots = TRUE)[-1])
  argList$plot_type <- "col"
  do.call(ezplot, argList)
}

#'@export
ezcol.stack <- function(data, aggr = NULL, x, y = NULL, group = NULL, 
                        facet = NULL, facet_x = NULL, facet_y = NULL,
                        palette = NULL, ci = NULL, 
                        error_type = "errorbar", error_args = NULL, text = FALSE, 
                        text_round = function(x) signif(x, 3), text_args = NULL,
                        title = NULL, subtitle = NULL, caption = NULL, ...) {
  argList <- as.list(match.call(expand.dots = TRUE)[-1])
  argList$plot_type <- "col"
  argList$position <- "stack"
  do.call(ezplot, argList)
}

#'@export
ezcol.dodge <- function(data, aggr = NULL, x, y = NULL, group = NULL, 
                        facet = NULL, facet_x = NULL, facet_y = NULL,
                        palette = NULL, ci = NULL, 
                        error_type = "errorbar", error_args = NULL, text = FALSE, 
                        text_round = function(x) signif(x, 3), text_args = NULL,
                        title = NULL, subtitle = NULL, caption = NULL, ...) {
  argList <- as.list(match.call(expand.dots = TRUE)[-1])
  argList$plot_type <- "col"
  argList$position <- "dodge"
  do.call(ezplot, argList)
}

#'@export
ezcol.fill <- function(data, aggr = NULL, x, y = NULL, group = NULL, 
                        facet = NULL, facet_x = NULL, facet_y = NULL,
                        palette = NULL, ci = NULL, 
                        error_type = "errorbar", error_args = NULL, text = FALSE, 
                        text_round = function(x) signif(x, 3), text_args = NULL,
                        title = NULL, subtitle = NULL, caption = NULL, ...) {
  argList <- as.list(match.call(expand.dots = TRUE)[-1])
  argList$plot_type <- "col"
  argList$position <- "fill"
  do.call(ezplot, argList)
}

#'@export
ezsmooth <- function(data, aggr = NULL, x, y = NULL, group = NULL, 
                     position = "identity", facet = NULL, facet_x = NULL, facet_y = NULL,
                     palette = NULL, ci = NULL, 
                     error_type = "errorbar", error_args = NULL, text = FALSE, 
                     text_round = function(x) signif(x, 3), text_args = NULL,
                     title = NULL, subtitle = NULL, caption = NULL, ...) {
  
  argList <- as.list(match.call(expand.dots = TRUE)[-1])
  argList$plot_type <- "smooth"
  do.call(ezplot, argList)
}

#'@export
ezpoint <- function(data, aggr = NULL, x, y = NULL, group = NULL, 
                    position = "identity", facet = NULL, facet_x = NULL, facet_y = NULL,
                    palette = NULL, ci = NULL, 
                    error_type = "errorbar", error_args = NULL, text = FALSE, 
                    text_round = function(x) signif(x, 3), text_args = NULL,
                    title = NULL, subtitle = NULL, caption = NULL, ...) {

  argList <- as.list(match.call(expand.dots = TRUE)[-1])
  argList$plot_type <- "point"
  do.call(ezplot, argList)
}

#'@export
ezarea <- function(data, aggr = NULL, x, y = NULL, group = NULL, 
                   position = "identity", facet = NULL, facet_x = NULL, facet_y = NULL,
                   palette = NULL, ci = NULL, 
                   error_type = "errorbar", error_args = NULL, text = FALSE, 
                   text_round = function(x) signif(x, 3), text_args = NULL,
                   title = NULL, subtitle = NULL, caption = NULL, ...) {

  argList <- as.list(match.call(expand.dots = TRUE)[-1])
  argList$plot_type <- "area"
  do.call(ezplot, argList)
}

#'@export
ezdotplot <- function(data, aggr = NULL, x, y = NULL, group = NULL, 
                      position = "identity", facet = NULL, facet_x = NULL, facet_y = NULL,
                      palette = NULL, ci = NULL, 
                      error_type = "errorbar", error_args = NULL, text = FALSE, 
                      text_round = function(x) signif(x, 3), text_args = NULL,
                      title = NULL, subtitle = NULL, caption = NULL, ...) {

  argList <- as.list(match.call(expand.dots = TRUE)[-1])
  argList$plot_type <- "dotplot"
  do.call(ezplot, argList)
  
}

#'@export
ezdensity <- function(data, aggr = NULL, x, y = NULL, group = NULL, 
                      position = "identity", facet = NULL, facet_x = NULL, facet_y = NULL,
                      palette = NULL, ci = NULL, 
                      error_type = "errorbar", error_args = NULL, text = FALSE, 
                      text_round = function(x) signif(x, 3), text_args = NULL,
                      title = NULL, subtitle = NULL, caption = NULL, ...) {

  argList <- as.list(match.call(expand.dots = TRUE)[-1])
  argList$plot_type <- "density"
  do.call(ezplot, argList)
}

#'@export
ezfreqpoly <- function(data, aggr = NULL, x, y = NULL, group = NULL, 
                   position = "identity", facet = NULL, facet_x = NULL, facet_y = NULL,
                   palette = NULL, ci = NULL, 
                   error_type = "errorbar", error_args = NULL, text = FALSE, 
                   text_round = function(x) signif(x, 3), text_args = NULL,
                   title = NULL, subtitle = NULL, caption = NULL, ...) {

  argList <- as.list(match.call(expand.dots = TRUE)[-1])
  argList$plot_type <- "freqpoly"
  do.call(ezplot, argList)
  
}

#'@export
ezfreq <- ezfreqpoly

#'@export
ezhistogram <- function(data, aggr = NULL, x, y = NULL, group = NULL, 
                   position = "identity", facet = NULL, facet_x = NULL, facet_y = NULL,
                   palette = NULL, ci = NULL, 
                   error_type = "errorbar", error_args = NULL, text = FALSE, 
                   text_round = function(x) signif(x, 3), text_args = NULL,
                   title = NULL, subtitle = NULL, caption = NULL, ...) {

  argList <- as.list(match.call(expand.dots = TRUE)[-1])
  argList$plot_type <- "histogram"
  do.call(ezplot, argList)
  
}

#'@export
ezhist <- ezhistogram