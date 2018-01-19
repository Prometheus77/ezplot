#' @title Easy plotting of ggplot2 objects
#' 
#' @description 
#' Creates a ggplot2 object using greatly simplified syntax
#' 
#' @param data (\code{data.frame} | \code{tibble}) Data to be used in plot
#' @param aggr (\code{function}) (Optional) function created using
#'   \code{dplyr::summarise}. Must take the form \code{function(x) summarise(x,
#'   ...)}
#' @param plot_type \code{character(1)} Type of plot to create. Default is \code{'bar'}
#' \itemize{
#'   \item \code{'bar'} Create bar plot using \code{geom_bar()}
#'   \item \code{'line'} Create line plot using \code{geom_line()}
#'   \item \code{'point'} Create point plot (scatterplot) using \code{geom_point()}
#'   \item \code{'area'} Create area plot using \code{geom_area()}
#' }
#' @param x Variable to use on X-axis. Will be coerced to a factor
#' @param y Variable to use on Y-axis
#' @param group (Optional) Variable to group by
#' @param group_method \code{character(1)} How to display grouping. Default is \code{'stack'}
#' \itemize{
#'   \item \code{'stack'} places groups on top of one another, i.e. stacked bar chart
#'   \item \code{'dodge'} places groups side-by-side, i.e. clustered bar chart or butted bar chart
#'   \item \code{'fill'} places groups on top of one another and normalizes to
#'   the same height. Useful for comparing proportions
#' }
#' @param fill (Optional) Variable to fill by
#' @param color (Optional) Variable to color by
#' @param facet (Optional) Variable to facet by
#' 
#' @export
ezplot <- function(data, aggr = NULL, plot_type = "bar", x, y, group = NULL, group_method = "stack", 
                   facet = NULL, palette = NULL) {
  checkmate::assert_subset(plot_type, c("bar", "line", "point", "area"))
  checkmate::assert_function(aggr, null.ok = TRUE)
  checkmate::assert_subset(x, names(data), empty.ok = FALSE)
  checkmate::assert_subset(group, names(data))
  checkmate::assert_subset(group_method, c("stack", "dodge", "fill"))
  checkmate::assert_subset(facet, names(data))
  checkmate::assert_character(palette, null.ok = TRUE)
  
  if (hasArg("aggr")) {
    data <- group_by_at(data, .vars = c(x, group, facet)) %>% aggr
  }
  
  gg <- wrapr::let(
    list(x_ = x,
         y_ = y)
    ,
    {
      ggplot(data, aes(x = x_, y = y_))
    }
  )

  if (hasArg("facet")) {
    gg <- wrapr::let(
      list(facet_ = facet)
      ,
      {
        gg + facet_grid(~ facet_)
      }
    )
  }
  
  if (plot_type == "bar") {
    if (hasArg("group")) {
      gg <- wrapr::let(
        list(group_ = group)
        ,
        gg + geom_bar(aes(group = group_, fill = group_), position = group_method, stat = "identity")
        )
    } else {
      gg <- gg + geom_bar(stat = "identity")  
    }
  } else if (plot_type == "line") {
    if (hasArg("group")) {
      gg <- wrapr::let(
        list(group_ = group)
        ,
        gg + geom_line(aes(group = group_, color = group_))
      )
    } else {
      gg <- gg + geom_line()  
    }
    
  } else if (plot_type == "point") {
    
  } else if (plot_type == "area") {
    
  } else {
    stop("plot_type ", plot_type, " not supported!")
  }
  
  gg
}