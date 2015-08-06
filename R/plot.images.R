#' plot.images
#'
#' @param x       CanaogramGAM to plot
#' @param frames  Which frames to show
#' @param scale   Which image scale to use (raw intensities or scalled intensities)
#' @param masked  Show just the masked data or all data
#' @param cornea  Include the cornea in the output (or omit it)
#' @param ...     <not used>
#'
#' @return A ggplot2 object of the plot
#' @export plot.images
#' @import ggplot2
#'
#' @examples
#' \dontrun{
#' images <- read.images('./data/Trial-1/one gree_T00', n = 10)
#' fit <- fit.gam(images)
#' plot.images(fit, scale = 'raw', masked = FALSE)
#' plot.images(fit, scale = 'scaled', masked = TRUE)
#' }
plot.images <- function(x,
                        frames = c(1, round(max(x$gam$data$t) / 2), max(x$gam$data$t)),
                        scale = c('scaled', 'raw'),
                        masked = FALSE,
                        cornea = FALSE,
                        ...) {
  if (! inherits(x, 'CanaogramGAM')) {
    stop('Function plot.images requires a CanaogramGAM object\n')
  }
  scale <- match.arg(scale)

  x <- x$gam$data

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

  if (scale == 'raw') {
    title <- 'Raw Data'
    x$z <- x$I
  } else if (scale == 'scaled') {
    title <- 'Scaled Data'
  }

  if (masked) {
    x <- x[, x$mask == TRUE, drop = FALSE]
  }
  x <- x[x$t %in% frames,]
  if (cornea) {
    x$fill <- ifelse(x$cornea, x$z + 2, x$z)
    p <- ggplot(x, aes(x = x, y = y, fill = fill)) +
      coord_fixed(ratio = 1) +
      ggtitle(title) + xlab("") + ylab("") +
      geom_raster() + facet_wrap(~ t) +
      scale_fill_gradientn("Signal", colours=c('#000000', '#FFFFFF', '#000000', '#FF0000'), values = c(0, 1/3, 2/3, 3/3)) +
      mytheme
  } else {
    p <- ggplot(x, aes(x = x, y = y, fill = z)) +
      coord_fixed(ratio = 1) +
      ggtitle(title) + xlab("") + ylab("") +
      geom_raster() + facet_wrap(~ t) +
      scale_fill_gradient("Signal", low = '#000000', high = '#FFFFFF') +
      mytheme
  }
  return(p)
}

if(getRversion() >= "2.15.1") utils::globalVariables(c('y', 'fill', 'z'))
