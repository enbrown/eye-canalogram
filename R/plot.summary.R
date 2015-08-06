#' plot.summary
#'
#' @param x     CanaogramGAM to plot
#' @param file  Output file to save plot to
#' @param scale Scale at which to plot dot-plots
#' @param ...   <not used>
#'
#' @return A ggplot2 object of the plot
#' @export plot.summary
#' @import ggplot2
#'
#' @examples
#' \dontrun{
#' images <- read.images('./data/Trial-1/one gree_T00')
#' fit <- fit.gam(images)
#' plot.summary(fit)
#' }
plot.summary <- function(x,
                         file = paste0(x$root, '.pdf'),
                         scale = NA,
                         ...) {
  if (! inherits(x, 'CanaogramGAM')) {
    stop('Function plot.images requires a CanaogramGAM object\n')
  }
  if (! inherits(x$gam$grid_metrics, 'data.frame')) {
    stop('CanaogramGAM object missing the required grid fit\n')
  }

  if (!is.na(file)) {
    pdf(file = file, height = 9, width = 7)
  }

  if (missing(scale)) {
    low.resolution <- max(x$gam$data$x)
    if (low.resolution == 32) {
      scale <- 2.5
    } else if (low.resolution == 64) {
      scale <- 1.25
    } else {
      warning("Guessing at scale for plotting dot-plots.")
      if (low.resolution < 50) {
        scale <- 2.5
      } else {
        scale <- 1.25
      }
    }
  }

  multiplot(plot.images(x, scale = 'raw', frames = 1),
            plot.images(x, scale = 'raw', frames = max(x$gam$data$t)),
            plot.ring(x, which = 'rate'),
            plot.ring(x, which = 'intensity'),
            plot.grid(x, which = 'rate'),
            plot.dot(x, scale.range = c(0.1, scale)),
            ggplot(data.frame()) +
              annotate('Text', x = 1, y = 1, label = x$root) +
              theme_minimal() +
              theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
                    axis.text.y = element_blank(), axis.title.y = element_blank(),
                    axis.ticks = element_blank(), panel.grid = element_blank()),

            layout = matrix(c(7,7,
                              1,2, 1,2, 1,2,
                              3,4, 3,4, 3,4,
                              5,6, 5,6, 5,6),
                            nrow = 10, byrow = TRUE))

  if (!is.na(file)) {
    dev.off()
  }

  #multiplot(plot.images(x, scale = 'raw'),
  #          plot.images(x, scale = 'scaled', masked = TRUE),
  #          plot.ring(x, which = 'intensity'),
  #          plot.ring(x, which = 'time'),
  #          plot.ring(x, which = 'rate'),
  #          plot.grid(x, which = 'intensity'),
  #          plot.grid(x, which = 'time'),
  #          plot.grid(x, which = 'rate'),
  #
  #          layout = matrix(c(1, 1, 1, 2, 2, 2, 3, 4, 5, 6, 7, 8),
  #                          nrow=4, byrow=TRUE))
}
