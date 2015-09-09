#' plot.images
#'
#' @param x       CanaogramGAM to plot
#' @param frames  Which frames to show
#' @param masked  Show just the masked data or all data
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
                        masked = FALSE,
                        ...) {
  if (! inherits(x, 'CanaogramGAM')) {
    stop('Function plot.images requires a CanaogramGAM object\n')
  }

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

  title <- 'Scaled Data'

  if (masked) {
    x <- x[x$mask == TRUE,]
  }
  x <- x[x$t %in% frames,]
  p <- ggplot(x, aes(x = x, y = y, fill = z)) +
    coord_fixed(ratio = 1) +
    ggtitle(title) + xlab("") + ylab("") +
    geom_raster() + facet_wrap(~ t) +
    scale_fill_gradient("Signal", low = '#000000', high = '#FFFFFF') +
    mytheme
  return(p)
}

if(getRversion() >= "2.15.1") utils::globalVariables(c('y', 'fill', 'z'))
