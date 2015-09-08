#' plot.CanalogramImages
#'
#' @param x    An CanalogramImages object
#' @param frames The image frames to display
#' @param ...  <not used>
#'
#' @return A ggplot2 object of the plots
#' @export
#'
#' @examples
#' \dontrun{
#' images <- read.images('./data/Trial-1/one gree_T00', n = 10)
#' plot(images, frames = c(1, 10))
#' plot(images)
#' }
plot.CanalogramImages <- function(x,
                                  frames = c(1,
                                             round(EBImage::numberOfFrames(x$data.low) / 2),
                                             max(EBImage::numberOfFrames(x$data.low))),
                                  ...) {
  if (! inherits(x, 'CanalogramImages')) {
    stop('Function plot.CanalogramImages requires a CanalogramImages object\n')
  }

  if ('Image' %in% class(x$data)) {
    data <- reshape2::melt(x$data)
  } else {
    data <- reshape2::melt(x$data.low)
  }
  colnames(data) <- c('x', 'y', 't', 'I')
  data <- data[data$t %in% frames, , drop = FALSE]

  mytheme <- ggplot2::theme_bw() +
    ggplot2::theme(legend.position="right",
          axis.ticks = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),
          panel.border = ggplot2::element_blank(),
          panel.grid = ggplot2::element_blank(),
          panel.margin = grid::unit(1, 'mm'),
          legend.margin = grid::unit(0, 'mm'),
          plot.margin = grid::unit(c(0,0,0,0), 'mm'))

  p <- ggplot2::ggplot(data, ggplot2::aes(x = x, y = y, fill = I)) +
    ggplot2::coord_fixed(ratio = 1) +
    ggplot2::ggtitle("Raw Data") + ggplot2::xlab("") + ggplot2::ylab("") +
    ggplot2::geom_raster() + ggplot2::facet_wrap(~ t) +
    ggplot2::scale_fill_gradient("Signal", low = '#000000', high = '#FFFFFF') +
    mytheme
  return(p)
}

if(getRversion() >= "2.15.1")  utils::globalVariables(c('x', 'y', 't'))
