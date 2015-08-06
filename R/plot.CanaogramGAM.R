#' plot.CanaogramGAM
#'
#' @param x A CanaogramGAM object
#' @param ... <not used>
#'
#' @return A ggplot2 object with the plot
#' @export
#' @import ggplot2
#'
#' @examples
#' \dontrun{
#' images <- read.images('./data/Trial-1/one gree_T00', n = 10)
#' fit <- fit.gam(images)
#' plot(fit)
#' }
plot.CanaogramGAM <- function(x, ...) {
  if (! inherits(x, 'CanaogramGAM')) {
    stop('Function plot.CanaogramGAM requires a CanaogramGAM object\n')
  }

  rings <- x$gam$rings %>%
    dplyr::group_by(clock, r_band) %>%
    dplyr::summarize(min_fit = mean(min_fit),
                     mid_fit = mean(mid_fit),
                     max_fit = mean(max_fit),
                     mid_t = mean(mid_t),
                     mid_rate = mean(mid_rate)) %>%
    dplyr::ungroup()

  p <- ggplot(x$gam$clock_data, aes(x = t, y = z)) +
    ggtitle("Fit Signals") + xlab("Frame") + ylab("Signal") +
    geom_vline(aes(xintercept = mid_t), data = rings, lty = 3, color = 'grey20') +
    geom_hline(aes(yintercept = mid_fit), data = rings, lty = 3, color = 'grey20') +
    geom_point() +
    geom_line(aes(y = fit)) +
    facet_grid(r_band ~ clock) +
    theme_bw()

  return(p)
}

if(getRversion() >= "2.15.1") utils::globalVariables(c('clock', 'r_band',
                                                       'min_fit', 'mid_fit', 'max_fit',
                                                       'mid_t', 'mid_rate',
                                                       'z', 'fit'))
