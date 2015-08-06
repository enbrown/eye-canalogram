#' plot.grid
#'
#' @param x     CanaogramGAM to plot
#' @param which   Which of 'rate', 'time', 'intensity', or 'all' to plot
#' @param ...     <not used>
#'
#' @return A ggplot2 object of the plot
#' @export plot.grid
#' @import ggplot2
#'
#' @examples
#' \dontrun{
#' images <- read.images('./data/Trial-1/one gree_T00', n = 10)
#' fit <- fit.gam(images)
#' plot.grid(fit)
#' }
plot.grid <- function(x,
                      which = c('rate', 'time', 'intensity', 'all'),
                      ...) {
  if (! inherits(x, 'CanaogramGAM')) {
    stop('Function plot.images requires a CanaogramGAM object\n')
  }
  which <- match.arg(which)

  metrics <- x$gam$grid_metrics

  title <- ''
  scale <- ''
  color_low <-  '#000000'
  color_high <- '#FF0000'
  if (which == 'rate') {
    title <- 'Filling Rate'
    scale <- 'Rate'
    metrics$z <- metrics$mid_rate
  } else if (which == 'time') {
    title <- 'Filling Time'
    scale <- 'Time'
    metrics$z <- metrics$mid_t
    temp <- color_low
    color_low <- color_high
    color_high <- temp
  } else if (which == 'intensity') {
    title <- 'Filling Amount'
    scale <- 'Intensity'
    metrics$z <- metrics$max_fit
  } else if (which == 'all') {
    p1 <- plot.grid(x, which = 'intensity', ...)
    p2 <- plot.grid(x, which = 'time', ...)
    p3 <- plot.grid(x, which = 'rate', ...)
    p <- multiplot(p1, p2, p3, cols = 3)
    return(p)
  }

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

  p <- ggplot(metrics, aes(x, y, fill = z)) +
    coord_fixed(ratio = 1) +
    ggtitle(title) + xlab("") + ylab("") +
    geom_raster() +
    scale_fill_gradient(scale, low = color_low, high = color_high) +
    mytheme

  return(p)
}
