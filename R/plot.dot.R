#' plot.dot
#'
#' @param x       CanaogramGAM to plot
#' @param scale.range   Scale for dots (see ggplot2::scale_size_continuous)
#' @param time.max  Maximum time used for time scale
#' @param ...     <not used>
#'
#' @return A ggplot2 object of the plot
#' @export plot.dot
#' @import ggplot2
#'
#' @examples
#' \dontrun{
#' images <- read.images('./data/Trial-1/one gree_T00', n = 10)
#' fit <- fit.gam(images)
#' plot.dot(fit)
#' }
plot.dot <- function(x, scale.range = c(1,6), time.max = max(x$gam$data$t), ...) {
  if (! inherits(x, 'CanaogramGAM')) {
    stop('Function plot.images requires a CanaogramGAM object\n')
  }
  if (! inherits(x$gam$grid_metrics, 'data.frame')) {
    stop('CanaogramGAM object missing the required grid fit\n')
  }

  grid_metrics <- x$gam$grid_metrics

  mytheme <- theme_bw() +
    theme(legend.position="right",
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          panel.margin = grid::unit(1, 'mm'),
          legend.margin = grid::unit(0, 'mm'),
          plot.margin = grid::unit(c(0,0,0,0), 'mm'))

  # Handle limits for the time and intensity
  time.limits = c(0, time.max)
  grid_metrics$mid_t[grid_metrics$mid_t > time.max] <- time.max
  grid_metrics$max_fit[grid_metrics$max_fit > 1.0] <- 1.0

  p <- ggplot(grid_metrics, aes(x = x, y = y, col = mid_t, size = max_fit)) +
    coord_fixed(ratio = 1) +
    ggtitle("Individual Fits") + xlab("") + ylab("") +
    geom_point() +
    scale_color_gradient("Time", low = "#FF0000", high = "#000000", limits = time.limits) +
    scale_size_continuous("Intensity", range = scale.range, limits = c(0, 1.0)) +
    mytheme

  return(p)
}

if(getRversion() >= "2.15.1") utils::globalVariables(c('y', 'mid_t', 'max_fit'))
