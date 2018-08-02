#' @title Easy plotting of ggplot2 objects
#' 
#' @description 
#' Creates a ggplot2 object using greatly simplified syntax
#' 
#' @param data (\code{data.frame} | \code{tibble}) Data to be used in plot
#' @param aggr (\code{function}) (Optional) function created using
#'   \code{dplyr::summarise}. Must take the form \code{function(x) summarise(x,
#'   ...)}
#' @param plot_type \code{character(1)} Type of plot to create. Default is \code{'col'}
#' \itemize{
#'   \item \code{'col'} Create column plot using \code{geom_col()}
#'   \item \code{'line'} Create line plot using \code{geom_line()}
#'   \item \code{'smooth'} Create smoothed line plot using \code{geom_smooth()}
#'   \item \code{'point'} Create point plot (scatterplot) using \code{geom_point()}
#'   \item \code{'area'} Create area plot using \code{geom_area()}
#'   \item \code{'density'} Create density plot using \code{geom_density()}
#'   \item \code{'dotplot'} Create dot plot using \code{geom_dotplot()}
#'   \item \code{'freqpoly'} Create frequency polygon using \code{geom_freqpoly()}
#'   \item \code{'histogram'} Create histogram using \code{geom_histogram()}
#'   \item \code{'rect'} Create rectangle plot using \code{geom_rect()}
#' }
#' @param x Variable to use on X-axis
#' @param y Variable to use on Y-axis
#' @param group (Optional) Variable to group by
#' @param position \code{character(1)} How to position grouping. Default is \code{'identity'}
#' \itemize{
#'   \item \code{'stack'} stacks groups on top of one another, i.e. stacked bar chart
#'   \item \code{'dodge'} places groups side-by-side, i.e. clustered bar chart or butted bar chart
#'   \item \code{'fill'} stacks groups on top of one another and normalizes to
#'   the same height. Useful for comparing proportions
#'   \item \code{'identity'} places groups in the same location
#' }
#' @param facet (Optional) Variable to facet by using \code{facet_wrap()}. Will
#'   be ignored if combined with \code{facet_x} or \code{facet_y}
#' @param facet_x (Optional) Variable to facet by using \code{facet_grid(. ~
#'   x)}. When combined with \code{facet_y}, will facet using \code{facet_grid(y
#'   ~ x)}
#' @param facet_y (Optional) Variable to facet by using \code{facet_grid(y ~
#'   .)}. When combined with \code{facet_x}, will facet using \code{facet_grid(y
#'   ~ x)}
#' @param palette (Optional) Color palette to use for fill
#' @param ci (Optional) Confidence interval to use for visualizing error
#' @param error_type (Optional) Type of visualization to use for error. Default
#'   is \code{'errorbar'}
#' \itemize{
#'   \item \code{'crossbar'} Use \code{geom_crossbar()}
#'   \item \code{'errorbar'} Use \code{geom_errorbar()}
#'   \item \code{'linerange'} Use \code{geom_linerange()}
#'   \item \code{'pointrange'} Use \code{geom_pointrange()}
#' }
#' @param error_args (Optinoal) List of additional arguments to pass to error
#'   plotting function
#' @param text \code{logical(1)} Show values of y variable as text? Default is
#'   \code{'FALSE'}
#' @param text_round \code{function} Function to use to round text. Default is
#'   \code{function(x) signif(x, 3)}
#' @param text_args (Optional) List of additional arguments to pass to geom_text
#' @param ... (Optional) Additional parameters to pass to the geom function
#' 
#' @export
ezplot <- function(data, aggr = NULL, plot_type = "col", x, y = NULL, group = NULL, 
                   position = "identity", facet = NULL, facet_x = NULL, facet_y = NULL,
                   palette = NULL, ci = NULL, 
                   error_type = "errorbar", error_args = NULL, text = FALSE, 
                   text_round = function(x) signif(x, 3), text_args = NULL,
                   title = NULL, subtitle = NULL, caption = NULL, 
                   padding_perc_y = NULL, order_stack = FALSE, include_overall = FALSE, ...) {

  checkmate::assert_subset(plot_type, c("col", "line", "point", "smooth", "area", "density", "dotplot",
                                        "freqpoly", "histogram", "bar", "rect", "line_point"))
  checkmate::assert_function(aggr, null.ok = TRUE)
  checkmate::assert_subset(x, names(data), empty.ok = FALSE)
  # can't check y without knowing aggregation values
  checkmate::assert_subset(group, names(data))
  checkmate::assert_subset(position, c("stack", "dodge", "fill", "identity"))
  checkmate::assert_subset(facet, names(data))
  checkmate::assert_character(palette, null.ok = TRUE)
  checkmate::assert_number(ci, null.ok = TRUE)
  checkmate::assert_logical(text, any.missing = FALSE, len = 1)
  
  dots <- list(...)
  
  # make sure plot_type is appropriate to inputs
  if (!hasArg("y") & !is.null(y)) {
    if (plot_type %in% c("point", "smooth", "col", "area", "line")) {
      stop(paste0("Must specify y variable for plot_type '", plot_type, "'"))
    }
  }
  
  if (plot_type %in% c("density", "dotplot", "freqpoly", "histogram")) {
    if (!(class(data[[x]]) %in% c("numeric", "integer", "Date"))) {
      stop(paste0("x must be continuous (numeric, integer, or date) for plot_type '", plot_type, "'"))
    }
  }
  
  if ((plot_type %in% c("bar", "col")) & class(data[[x]]) != "factor") {
    warning(paste0(x, " will be coerced to a factor"))
    data[[x]] <- as.factor(data[[x]])    
  }
  
  # if group is specified, make sure position doesn't throw error
  if (hasArg("group") & !is.null(group)) {
    if (plot_type %in% c("col", "histogram")) {
      if (position == "identity") {
        message("Coercing position to 'stack'")
        position <- "stack"
      }
    }
  }
  
  # make sure confidence interval can be plotted
  if (hasArg("ci") & !is.null(ci)) {
    ci_supported <- c("col", "line", "smooth", "point")
    if (!(plot_type %in% ci_supported)) {
      stop(paste0("Confidence interval only supported for plot_types: '", 
                  paste(ci_supported, collapse = "', '"), "'"))
    }
    
    if (plot_type == "col" & position != "dodge") {
      if (hasArg("group") & !is.null(group)) {
        stop("Must set position to 'dodge' to plot confidence interval on plot_type 'bar' with group")
      }
    }
    
  }
  
  if (hasArg("aggr") & !is.null(aggr)) {
    data_temp <- data
    data <- group_by_at(data, .vars = unique(c(x, group, facet))) %>% aggr
    if (hasArg("include_overall") & include_overall) {
      if (!(hasArg("facet") & !is.null(facet))) {
        stop("Must have an argument for facet to include an overall facet.")
      }
      data_temp[[facet]] <- rep('Overall', length(data_temp[[facet]]))
      tmp <- data_temp %>%
        mutate_(paste0(facet, " = 'Overall'")) %>%
        group_by_at(.vars = unique(c(x, group, facet))) %>%
        aggr
      data <- rbind(data, tmp)
    }
    rm(data_temp)
  }
  
  if (plot_type == "rect") {
    if (!((hasArg("xmin") & !is.null(xmin)) & (hasArg("xmax") & !is.null(xmax))
          & (hasArg("ymin") & !is.null(ymin)) & (hasArg("ymax") & !is.null(ymax)))) {
      stop("Must contain 'xmin', 'xmax', 'ymin', 'ymax' arguments to use plot_type 'rect'")
    }
  }
  
  if (plot_type == "col" & order_stack & (hasArg("group") | !is.null(group))) {
    if (class(data[[x]]) %in% c("numeric", "integer", "Date")) {
      warning(paste0(x, " will be coerced to a factor"))
      data[[x]] <- as.factor(data[[x]])
    }
    last <- as.character(levels(data[[x]])[max(as.numeric(data[[x]]))])
    temp <- data %>%
      filter_(paste0(x, '=="', last, '"'))
    flevels <- temp[order(temp[[y]]), group]
    data[[group]] <- factor(data[[group]], levels = apply(flevels, 1, as.character))
  }
  
  # handle rounding of y in case of text = TRUE
  if (text == TRUE) {
    if (plot_type == 'col' & position %in% c('fill','stack') & (hasArg('group') & !is.null(group))) {
      # This is to label stacked bar charts
      if (hasArg('facet') & !is.null(facet)) {
        for (x_val in unique(data[[x]])) {
          for (f_val in unique(data[[facet]])) {
            index = which(data[[x]] == x_val & data[[facet]] == f_val)
            my_sum = sum(data[index, y], na.rm = T)
            for (g_val in unique(data[[group]])) {
              index = which(data[[x]] == x_val & data[[facet]] == f_val & data[[group]] == g_val)
              data[index, y] = data[index, y] * 100.0 / my_sum
            }
          }
        }
      }
      for (x_val in unique(data[[x]])) {
        index = which(data[[x]] == x_val)
        my_sum = sum(data[index, y], na.rm = T)
        for (g_val in unique(data[[group]])) {
          index = which(data[[x]] == x_val & data[[group]] == g_val)
          data[index, y] = data[index, y] * 100.0  / my_sum
        }
      }
    }
    data <- wrapr::let(c(y_ = y), {data <- data %>% mutate(y_rounded = do.call(text_round, list(y_)))})
  }
  
  if (hasArg("y") & !is.null(y)) {
    gg <- wrapr::let(
      list(x_ = x,
           y_ = y)
      ,
      {
        ggplot(data, aes(x = x_, y = y_))
      }
    )
  } else {
    gg <- wrapr::let(
      list(x_ = x)
      ,
      {
        ggplot(data, aes(x = x_))
      }
    )
  }

  if ((hasArg("facet") & !is.null(facet)) & !(hasArg("facet_x") | hasArg("facet_y"))) {
    gg <- wrapr::let(
      list(facet_ = facet)
      ,
      {
        gg + facet_wrap(~ facet_)
      }
    )
  }
  
  if (hasArg("facet_x") & !is.null(facet_x)) {
    if (hasArg("facet_y" & !is.null(facet_y))) {
      gg <- wrapr::let(
        list(facet_x_ = facet_x,
             facet_y_ = facet_y)
        ,
        {
          gg + facet_grid(facet_y_ ~ facet_x_)
        }
      )
    } else {
      gg <- wrapr::let(
        list(facet_x_ = facet_x)
        ,
        {
          gg + facet_grid(. ~ facet_x_)
        }
      )
    }
  } else if (hasArg("facet_y") & !is.null(facet_y)) {
    gg <- wrapr::let(
      list(facet_y_ = facet_y)
      ,
      {
        gg + facet_grid(facet_y_ ~ .)
      }
    )
  }
  
  if (plot_type == "col") {
    # bar plot
    if (class(data[[x]]) %in% c("numeric", "integer", "Date")) {
      warning(paste0(x, " will be coerced to a factor"))
      data[[x]] <- as.factor(data[[x]])
    }
    
    if (hasArg("group") & !is.null(group)) {
      gg <- wrapr::let(
        list(group_ = group)
        ,
        gg + geom_col(aes(group = group_, fill = group_), position = position)
        )
    } else {
      gg <- gg + geom_col()  
    }
  } else if (plot_type == "line") {
    # line plot
    # if (class(data[[x]]) %in% c("character", "factor")) {
    #   stop(paste0(x, " must be of type 'numeric', 'integer', or 'date' for line plot"))
    # }
    if (class(data[[x]]) %in% c("character")) {
      stop(paste0(x, " must be of type 'numeric', 'integer', 'factor', or 'date' for line plot"))
    }
    
    if (hasArg("group") & !is.null(group)) {
      gg <- wrapr::let(
        list(group_ = group)
        ,
        gg + geom_line(aes(group = group_, color = group_))
      )
    } else {
      if (class(data[[x]]) == "factor") {
        stop("You must specify a group when x is a factor and plot_type is line.")
      }
      gg <- gg + geom_line()  
    }
    
  } else if (plot_type == "point") {
    # point plot
    
    if (hasArg("group") & !is.null(group)) {
      gg <- wrapr::let(
        list(group_ = group)
        ,
        gg + geom_point(aes(group = group_, color = group_))
      )
    } else {
      gg <- gg + geom_point()  
    }
    
  } else if (plot_type == "area") {
    # area plot
    if (class(data[[x]]) %in% c("character", "factor")) {
      stop(paste0(x, " must be of type 'character' or 'factor' for area plot"))
    }
    
    if (hasArg("group") & !is.null(group)) {
      gg <- wrapr::let(
        list(group_ = group)
        ,
        gg + geom_area(aes(group = group_, fill = group_))
      )
    } else {
      gg <- gg + geom_area()  
    }
    
  } else if (plot_type == "rect") {
    # rectangle plot
    if (!(class(data[[x]]) %in% c("character", "factor"))) {
      stop(paste0(x, " must be of type 'character' or 'factor' for rectangle plot'"))
    }
    
    if (hasArg("group") & !is.null(group)) {
      
      gg <- wrapr::let(
        list(group_ = group,
             xmin_ = dots$xmin,
             xmax_ = dots$xmax,
             ymin_ = dots$ymin,
             ymax_ = dots$ymax)
        ,
        {
          gg + geom_rect(aes(group = group_, fill = group_, xmin = xmin_, xmax = xmax_, 
                             ymin = ymin_, ymax = ymax_))
        })
    } else {
      gg <- wrapr::let(
        list(xmin_ = dots$xmin,
             xmax_ = dots$xmax,
             ymin_ = dots$ymin,
             ymax_ = dots$ymax)
        ,
        {
          gg + geom_rect(aes(xmin = xmin_, xmax = xmax_, 
                             ymin = ymin_, ymax = ymax_))
        })
    }
    
  } else if (plot_type %in% c("density", "dotplot", "freqpoly", "histogram", "bar")) {
    # density plot
    params <- list(position = position)
    if (hasArg("group") & !is.null(group)) params[["mapping"]] <- aes_string(group = group, fill = group)
    params <- c(params, ...)
    gg <- gg + do.call(paste0("geom_", plot_type), args = params)
    
  } else if (plot_type == 'line_point') {
    # line plot with points
    if (class(data[[x]]) %in% c("character")) {
      stop(paste0(x, " must be of type 'numeric', 'integer', 'factor', or 'date' for line_point plot"))
    }
    
    if (hasArg("group") & !is.null(group)) {
      gg <- wrapr::let(
        list(group_ = group)
        ,
        gg + geom_line(aes(group = group_, color = group_), size=.8) + geom_point(aes(group = group_, color = group_))
      )
    } else {
      if (class(data[[x]]) == "factor") {
        stop("You must specify a group when x is a factor and plot_type is line_point.")
      }
      gg <- gg + geom_line() + geom_point() 
    }
    
  } else {
    stop("plot_type ", plot_type, " not supported!")
  }
  
  if (hasArg("palette") & !is.null(palette)) {
    if (plot_type == 'line_point') {
      if (length(palette) == 1) {
        gg <- gg + scale_color_brewer(palette = palette)
      } else {
        gg <- gg + scale_color_manual(values = palette)
      }
    } else {
      if (length(palette) == 1) {
        gg <- gg + scale_fill_brewer(palette = palette)
      } else {
        gg <- gg + scale_fill_manual(values = palette)
      }
    }
  }
  
  if (hasArg("ci") & !is.null(ci)) {
    zval <- -qnorm((1 - ci) / 2)
    
    if (hasArg("group") & !is.null(group)) {
      error_arglist <- list(aes_string(ymin = paste0(y, " - ", zval, " * se_", y), 
                                       ymax = paste0(y, " + ", zval, " * se_", y), 
                                       group = group, color = group), 
                            position = position)
    } else {
      error_arglist <- list(aes_string(ymin = paste0(y, " - ", zval, " * se_", y), 
                                       ymax = paste0(y, " + ", zval, " * se_", y)))
    }
    
    if (!(hasArg("error_args") & !is.null(error_args))) error_args <- list(color = "black")
    
    error_arglist <- c(error_arglist, error_args)
    gg <- gg + 
      do.call(paste0("geom_", error_type), args = error_arglist)
  }
  
  if (text == TRUE) {
    if (position == "dodge") {
      position_arg <- list(position = position_dodge(width = 1))
    } else if (!is.null(text_args$position)) {
      position_arg <- list(position = text_args$position)
      text_args$position <- NULL
    } else {
      position_arg <- list(position = position)
    }
    if (!is.null(text_args$label)) {
      aes_arg <- list(aes(label = text_args$label))
    } else {
      aes_arg <- list(aes_string(label = "y_rounded", group = group))
    }
    text_arglist <- c(aes_arg, position_arg, text_args)
    gg <- gg +
      do.call("geom_text", args = text_arglist)
  }
  
  if ((hasArg("title") & !is.null(title)) | (hasArg("subtitle") & !is.null(subtitle)) | (hasArg("caption") & !is.null(caption))) {
    labs_args <- list(title = title, subtitle = subtitle, caption = caption)
    gg <- gg + do.call(labs, args = labs_args)
  }
  
  if (hasArg("padding_perc_y") & !is.null(padding_perc_y)) {
    if (hasArg("ci")) {
      min_y <- min(data[[y]] - 2 * data[[paste0("se_", y)]], na.rm = T)
      max_y <- max(data[[y]] + 2 * data[[paste0("se_", y)]], na.rm = T)
    } else {
      min_y <- min(data[[y]], na.rm = T)
      max_y <- max(data[[y]], na.rm = T)
    }
    pad_max <- (max_y - min_y) * padding_perc_y
    if ((min_y - pad_max) <= 0) {
      pad_min <- 0
    } else {
      pad_min <- pad_max
    }
    gg <- gg + coord_cartesian(ylim = c(min_y - pad_min, max_y + pad_max))
  }

  gg <- gg + theme(axis.text.x = element_text(angle = 90, hjust = 1),
                   plot.title = element_text(hjust = 0.5),
                   plot.subtitle = element_text(hjust = 0.5),
                   plot.caption = element_text(hjust = 0))
  
  gg
}

#'@export
ezline <- function(data, aggr = NULL, x, y = NULL, group = NULL, 
                   position = "identity", facet = NULL, facet_x = NULL, facet_y = NULL,
                   palette = NULL, ci = NULL, 
                   error_type = "errorbar", error_args = NULL, text = FALSE, 
                   text_round = function(x) signif(x, 3), text_args = NULL,
                   title = NULL, subtitle = NULL, caption = NULL, ...) {
  ezplot(data = data, aggr = aggr, plot_type = "line", x = x, y = y, group = group,
         position = position, facet = facet, facet_x = facet_x, facet_y = facet_y,
         palette = palette, ci = ci, error_type = error_type, error_args = error_args,
         text = text, text_round = text_round, text_args = text_args, title = title,
         subtitle = subtitle, caption = caption, ...)
}

#'@export
ezcol <- function(data, aggr = NULL, x, y = NULL, group = NULL, 
                   position = "stack", facet = NULL, facet_x = NULL, facet_y = NULL,
                   palette = NULL, ci = NULL, 
                   error_type = "errorbar", error_args = NULL, text = FALSE, 
                   text_round = function(x) signif(x, 3), text_args = NULL,
                   title = NULL, subtitle = NULL, caption = NULL, ...) {
  ezplot(data = data, aggr = aggr, plot_type = "col", x = x, y = y, group = group,
         position = position, facet = facet, facet_x = facet_x, facet_y = facet_y,
         palette = palette, ci = ci, error_type = error_type, error_args = error_args,
         text = text, text_round = text_round, text_args = text_args, title = title,
         subtitle = subtitle, caption = caption, ...)
}

#'@export
ezcol_stack <- function(data, aggr = NULL, x, y = NULL, group = NULL, 
                  facet = NULL, facet_x = NULL, facet_y = NULL,
                  palette = NULL, ci = NULL, 
                  error_type = "errorbar", error_args = NULL, text = FALSE, 
                  text_round = function(x) signif(x, 3), text_args = NULL,
                  title = NULL, subtitle = NULL, caption = NULL, ...) {
  ezplot(data = data, aggr = aggr, plot_type = "col", x = x, y = y, group = group,
         position = "stack", facet = facet, facet_x = facet_x, facet_y = facet_y,
         palette = palette, ci = ci, error_type = error_type, error_args = error_args,
         text = text, text_round = text_round, text_args = text_args, title = title,
         subtitle = subtitle, caption = caption, ...)
}

#'@export
ezcol_dodge <- function(data, aggr = NULL, x, y = NULL, group = NULL, 
                        facet = NULL, facet_x = NULL, facet_y = NULL,
                        palette = NULL, ci = NULL, 
                        error_type = "errorbar", error_args = NULL, text = FALSE, 
                        text_round = function(x) signif(x, 3), text_args = NULL,
                        title = NULL, subtitle = NULL, caption = NULL, ...) {
  ezplot(data = data, aggr = aggr, plot_type = "col", x = x, y = y, group = group,
         position = "dodge", facet = facet, facet_x = facet_x, facet_y = facet_y,
         palette = palette, ci = ci, error_type = error_type, error_args = error_args,
         text = text, text_round = text_round, text_args = text_args, title = title,
         subtitle = subtitle, caption = caption, ...)
}

#'@export
ezcol_fill <- function(data, aggr = NULL, x, y = NULL, group = NULL, 
                        facet = NULL, facet_x = NULL, facet_y = NULL,
                        palette = NULL, ci = NULL, 
                        error_type = "errorbar", error_args = NULL, text = FALSE, 
                        text_round = function(x) signif(x, 3), text_args = NULL,
                        title = NULL, subtitle = NULL, caption = NULL, ...) {
  ezplot(data = data, aggr = aggr, plot_type = "col", x = x, y = y, group = group,
         position = "fill", facet = facet, facet_x = facet_x, facet_y = facet_y,
         palette = palette, ci = ci, error_type = error_type, error_args = error_args,
         text = text, text_round = text_round, text_args = text_args, title = title,
         subtitle = subtitle, caption = caption, ...)
}

#'@export
ezsmooth <- function(data, aggr = NULL, x, y = NULL, group = NULL, 
                     position = "identity", facet = NULL, facet_x = NULL, facet_y = NULL,
                     palette = NULL, ci = NULL, 
                     error_type = "errorbar", error_args = NULL, text = FALSE, 
                     text_round = function(x) signif(x, 3), text_args = NULL,
                     title = NULL, subtitle = NULL, caption = NULL, ...) {
  ezplot(data = data, aggr = aggr, plot_type = "smooth", x = x, y = y, group = group,
         position = position, facet = facet, facet_x = facet_x, facet_y = facet_y,
         palette = palette, ci = ci, error_type = error_type, error_args = error_args,
         text = text, text_round = text_round, text_args = text_args, title = title,
         subtitle = subtitle, caption = caption, ...)
}

#'@export
ezpoint <- function(data, aggr = NULL, x, y = NULL, group = NULL, 
                     position = "identity", facet = NULL, facet_x = NULL, facet_y = NULL,
                     palette = NULL, ci = NULL, 
                     error_type = "errorbar", error_args = NULL, text = FALSE, 
                     text_round = function(x) signif(x, 3), text_args = NULL,
                     title = NULL, subtitle = NULL, caption = NULL, ...) {
  ezplot(data = data, aggr = aggr, plot_type = "point", x = x, y = y, group = group,
         position = position, facet = facet, facet_x = facet_x, facet_y = facet_y,
         palette = palette, ci = ci, error_type = error_type, error_args = error_args,
         text = text, text_round = text_round, text_args = text_args, title = title,
         subtitle = subtitle, caption = caption, ...)
}

#'@export
ezarea <- function(data, aggr = NULL, x, y = NULL, group = NULL, 
                    position = "identity", facet = NULL, facet_x = NULL, facet_y = NULL,
                    palette = NULL, ci = NULL, 
                    error_type = "errorbar", error_args = NULL, text = FALSE, 
                    text_round = function(x) signif(x, 3), text_args = NULL,
                    title = NULL, subtitle = NULL, caption = NULL, ...) {
  ezplot(data = data, aggr = aggr, plot_type = "area", x = x, y = y, group = group,
         position = position, facet = facet, facet_x = facet_x, facet_y = facet_y,
         palette = palette, ci = ci, error_type = error_type, error_args = error_args,
         text = text, text_round = text_round, text_args = text_args, title = title,
         subtitle = subtitle, caption = caption, ...)
}

#'@export
ezdotplot <- function(data, aggr = NULL, x, y = NULL, group = NULL, 
                   position = "identity", facet = NULL, facet_x = NULL, facet_y = NULL,
                   palette = NULL, ci = NULL, 
                   error_type = "errorbar", error_args = NULL, text = FALSE, 
                   text_round = function(x) signif(x, 3), text_args = NULL,
                   title = NULL, subtitle = NULL, caption = NULL, ...) {
  ezplot(data = data, aggr = aggr, plot_type = "dotplot", x = x, y = y, group = group,
         position = position, facet = facet, facet_x = facet_x, facet_y = facet_y,
         palette = palette, ci = ci, error_type = error_type, error_args = error_args,
         text = text, text_round = text_round, text_args = text_args, title = title,
         subtitle = subtitle, caption = caption, ...)
}

#'@export
ezdensity <- function(data, aggr = NULL, x, y = NULL, group = NULL, 
                      position = "identity", facet = NULL, facet_x = NULL, facet_y = NULL,
                      palette = NULL, ci = NULL, 
                      error_type = "errorbar", error_args = NULL, text = FALSE, 
                      text_round = function(x) signif(x, 3), text_args = NULL,
                      title = NULL, subtitle = NULL, caption = NULL, ...) {
  ezplot(data = data, aggr = aggr, plot_type = "density", x = x, y = y, group = group,
         position = position, facet = facet, facet_x = facet_x, facet_y = facet_y,
         palette = palette, ci = ci, error_type = error_type, error_args = error_args,
         text = text, text_round = text_round, text_args = text_args, title = title,
         subtitle = subtitle, caption = caption, ...)
}

#'@export
ezfreq <- function(data, aggr = NULL, x, y = NULL, group = NULL, 
                      position = "identity", facet = NULL, facet_x = NULL, facet_y = NULL,
                      palette = NULL, ci = NULL, 
                      error_type = "errorbar", error_args = NULL, text = FALSE, 
                      text_round = function(x) signif(x, 3), text_args = NULL,
                      title = NULL, subtitle = NULL, caption = NULL, ...) {
  ezplot(data = data, aggr = aggr, plot_type = "freqpoly", x = x, y = y, group = group,
         position = position, facet = facet, facet_x = facet_x, facet_y = facet_y,
         palette = palette, ci = ci, error_type = error_type, error_args = error_args,
         text = text, text_round = text_round, text_args = text_args, title = title,
         subtitle = subtitle, caption = caption, ...)
}

#'@export
ezhist <- function(data, aggr = NULL, x, y = NULL, group = NULL, 
                       position = "identity", facet = NULL, facet_x = NULL, facet_y = NULL,
                       palette = NULL, ci = NULL, 
                       error_type = "errorbar", error_args = NULL, text = FALSE, 
                       text_round = function(x) signif(x, 3), text_args = NULL,
                       title = NULL, subtitle = NULL, caption = NULL, ...) {
  ezplot(data = data, aggr = aggr, plot_type = "histogram", x = x, y = y, group = group,
         position = position, facet = facet, facet_x = facet_x, facet_y = facet_y,
         palette = palette, ci = ci, error_type = error_type, error_args = error_args,
         text = text, text_round = text_round, text_args = text_args, title = title,
         subtitle = subtitle, caption = caption, ...)
}