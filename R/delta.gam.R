#' delta.gam
#'
#' @param before          CanaogramGAM object before intervention
#' @param after           CanaogramGAM object after intervention
#'
#' @return DeltaCanaogramGAM object describing the change in flow
#' @export
#'
#' @examples
#' \dontrun{
#' images.before <- read.images('./data/before_T00', n = 5)
#' images.after  <- read.images('./data/after_T00', n = 5)
#' fit.before <- fit.gam(images.before)
#' fit.after <- fit.gam(images.after)
#' delta.fit <- get.change(fit.before, fit.after)
#' }
delta.gam <- function(before, after) {
  if (! inherits(before, 'CanaogramGAM')) {
    stop("delta.gam function requires two fit CanagramGAM objects.")
  }
  if (! inherits(after, 'CanaogramGAM')) {
    stop("delta.gam function requires two fit CanagramGAM objects.")
  }

  obj <- list(before = before,
              after = after,
              change = list(
                before.grid = NA,
                after.grid = NA,
                grid = NA
              ))
  class(obj) <- 'DeltaCanaogramGAM'

  obj$change$before.grid <- obj$before$gam$grid_metrics[, c('x', 'y', 'r_band', 'theta_band',
                                                            'min_fit', 'mid_fit', 'max_fit',
                                                            'mid_t', 'mid_rate')]
  obj$change$after.grid <-  obj$after$gam$grid_metrics[, c('x', 'y', 'r_band', 'theta_band',
                                                           'min_fit', 'mid_fit', 'max_fit',
                                                           'mid_t', 'mid_rate')]

  obj$change$grid <- dplyr::inner_join(obj$change$before.grid, obj$change$after.grid,
                                       by = c('r_band', 'theta_band'))

  '%>%' <- dplyr::'%>%'

  obj$change$grid <- obj$change$grid %>%
    dplyr::mutate(x = x.x,
                  y = y.x,
                  delta_mid_fit = mid_fit.y - mid_fit.x,
                  delta_mid_t = mid_t.y - mid_t.x,
                  delta_mid_rate = mid_rate.y - mid_rate.x) %>%
    dplyr::select(x, y, r_band, theta_band,
                  delta_mid_fit, delta_mid_t, delta_mid_rate) %>%
    dplyr::group_by(x, y) %>%
    dplyr::summarise_each(dplyr::funs(mean)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(x, y)

  return(invisible(obj))
}

if(getRversion() >= "2.15.1") utils::globalVariables(c('x.x', 'y.x',
                                                       'mid_fit.y', 'mid_fit.x',
                                                       'mid_t.y', 'mid_t.x',
                                                       'mid_rate.y', 'mid_rate.x',
                                                       'r_band', 'theta_band'))

#' plot.DeltaCanalogramGAM
#'
#' @param x         DeltaCanaogramGAM object to plot
#' @param which     Which of 'rate', 'time', 'intensity', or 'all' to plot
#' @param max.time  Maximum time difference to show in time plots (for legend)
#' @param max.rate  Maximum rate difference to show in rate plots (for legend)
#' @param ...       <not used>
#'
#' @return A ggplot2 object of the plot
#' @export
#' @import ggplot2
#'
#' @examples
#' \dontrun{
#' images.before <- read.images('./data/before_T00', n = 5)
#' images.after  <- read.images('./data/after_T00', n = 5)
#' fit.before <- fit.gam(images.before)
#' fit.after <- fit.gam(images.after)
#' delta.fit <- get.change(fit.before, fit.after)
#' plot(delta.fit, which = 'all', max.time = 10, max.rate = 5)
#' }
plot.DeltaCanaogramGAM <- function(x, which = c('rate', 'time', 'intensity', 'all'),
                                   max.time = 10, max.rate = 5,
                                   ...) {
  if (! inherits(x, 'DeltaCanaogramGAM')) {
    stop("plot.DeltaCanaogramGAM function requires a DeltaCanaogramGAM object.")
  }

  if (length(which) == 0) {
    stop("At least one plot must be requested in plot.DeltaCanaogramGAM.")
  }
  if (length(which) == 1) {
    which <- match.arg(which)
  } else {
    plots <- list()
    for (i in seq.int(length(which))) {
      p <- plot.DeltaCanaogramGAM(x, which = which[[i]],
                                  max.time = max.time, max.rate = max.rate,
                                  ...)
      plots[[i]] <- p
    }
    plots$layout <- matrix(seq.int(length(which)), nrow = 1, byrow = TRUE)
    plots <- do.call(multiplot, plots)
    return(plots)
  }

  if (which == 'all') {
    plots <- plot.DeltaCanaogramGAM(x, which = c('intensity', 'time', 'rate'),
                                  max.time = max.time, max.rate = max.rate,
                                  ...)
    return(plots)
  }

  grid <- x$change$grid

  grid$delta_mid_fit[grid$delta_mid_fit < -1.0] <- -1.0
  grid$delta_mid_fit[grid$delta_mid_fit >  1.0] <-  1.0
  grid$delta_mid_t[grid$delta_mid_t < -max.time] <- -max.time
  grid$delta_mid_t[grid$delta_mid_t >  max.time] <-  max.time
  grid$delta_mid_rate[grid$delta_mid_rate < -max.rate] <- -max.rate
  grid$delta_mid_rate[grid$delta_mid_rate >  max.rate] <-  max.rate

  mytheme <- ggplot2::theme_bw(base_size = 28) +
    ggplot2::theme(legend.position="right",
                   axis.ticks = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank(),
                   panel.margin = grid::unit(1, 'mm'),
                   legend.margin = grid::unit(0, 'mm'),
                   legend.key.size = grid::unit(13, "mm"),
                   legend.key = ggplot2::element_rect(fill = NULL, color = "white"),
                   plot.margin = grid::unit(c(0,0,0,0), 'mm'))

  if (which == 'intensity') {
    p1 <- ggplot(grid, aes(x = x, y = y, fill = delta_mid_fit)) +
      coord_fixed(ratio = 1) +
      ggtitle("Intensity") + xlab("") + ylab("") +
      geom_raster() +
      scale_fill_gradient2("Intensity", low = "blue", mid = "white", high = "red",
                           limits = c(-1.0, 1.0)) +
      mytheme
    return(p1)
  }

  if (which == 'time') {
    p2 <- ggplot(grid, aes(x = x, y = y, fill = delta_mid_t)) +
      coord_fixed(ratio = 1) +
      ggtitle("Time") + xlab("") + ylab("") +
      geom_raster() +
      scale_fill_gradient2("Time", low = "blue", mid = "white", high = "red",
                           limits = c(-max.time, max.time)) +
      mytheme
    return(p2)
  }

  if (which == 'rate') {
    p3 <- ggplot(grid, aes(x = x, y = y, fill = delta_mid_rate)) +
      coord_fixed(ratio = 1) +
      ggtitle("Rate") + xlab("") + ylab("") +
      geom_raster() +
      scale_fill_gradient2("Rate", low = "blue", mid = "white", high = "red",
                           limits = c(-max.rate, max.rate)) +
      mytheme
    return(p3)
  }

  stop("Unknown plot for DeltaCanaogramGAM object requested.")
}

if(getRversion() >= "2.15.1") utils::globalVariables(c('delta_mid_fit', 'delta_mid_t', 'delta_mid_rate'))
