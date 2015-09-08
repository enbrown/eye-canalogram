#' plot.ring
#'
#' @param x           CanaogramGAM to plot
#' @param which       Which plot type to create (rate, time, intensity, or all plots)
#' @param resolution  Resolution at which to make the plot (can be higher than the data resolution)
#' @param rate.max    Rate to plot at maximum brightness
#' @param time.max    Time to plot at maximum brightness
#' @param intensity.max Intensity to plot at maximum brightness
#' @param ...         <not used>
#'
#' @return A ggplot2 object of the created plot(s)
#' @export plot.ring
#' @import ggplot2
#'
#' @examples
#' \dontrun{
#' images <- read.images('./data/Trial-1/one gree_T00', n = 10)
#' fit <- fit.gam(images)
#' plot.ring(fit)
#' plot.ring(processed, which = 'all')
#' }
plot.ring <- function(x,
                      which = c('rate', 'time', 'intensity', 'all'),
                      resolution = NA,
                      rate.max = max(x$gam$rings$mid_rate),
                      time.max = max(x$gam$data$t),
                      intensity.max = 1.0,
                      ...) {
  if (! inherits(x, 'CanaogramGAM')) {
    stop('Function plot.ring requires a CanaogramGAM object\n')
  }

  which <- match.arg(which)

  if (! is.finite(resolution)) {
    rings <- x$gam$rings
  } else {
    bands <- round((x$gam$max_radius - 1) / x$gam$band_width)
    rings <- expand.grid(x = 1:resolution, y = 1:resolution)

    # Get the object's data
    min_radius <- 0.25
    max_radius <- 0.50

    # Transform to radial basis
    rings$X <- rings$x / resolution - 0.5
    rings$Y <- rings$y / resolution - 0.5
    rings$theta <- atan2(-rings$X, -rings$Y) * 180.0 / pi + 180.0
    rings$r <- sqrt(rings$X * rings$X + rings$Y * rings$Y)
    rings$X <- NULL
    rings$Y <- NULL
    rings <- subset(rings, r >= min_radius & r <= max_radius)

    # Transform into a clock basis
    rings$clock <- round(rings$theta / 360.0 * 12.0)
    rings$clock[rings$clock == 0] <- 12
    band_width <- (max_radius - min_radius) / bands
    rings$r_band <- floor((rings$r - min_radius) / band_width)

    # Fill in the data
    rings <- merge(rings, x$gam$clock_metrics, all.x = TRUE)

    # Fill in the blanks
    rings$min_fit[is.na(rings$min_fit)] <- 0
    rings$mid_fit[is.na(rings$mid_fit)] <- 0
    rings$max_fit[is.na(rings$max_fit)] <- 0
    rings$mid_t[is.na(rings$mid_t)] <- max(x$gam$data$t)
    rings$mid_rate[is.na(rings$mid_rate)] <- 0
  }

  mytheme <- theme_bw() +
    theme(legend.position = "right",
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          panel.margin = grid::unit(0, 'mm'),
          legend.margin = grid::unit(0, 'mm'),
          plot.margin = grid::unit(c(0,0,0,0), 'mm'))

  if (which == 'intensity') {
    p <- ggplot(rings, aes(x = x, y = y, fill = mid_fit)) +
      coord_fixed(ratio = 1) + xlab("") + ylab("") +
      ggtitle("Half-Intensity") +
      geom_raster() +
      scale_fill_gradient('Half-\nIntensity', limits = c(0, intensity.max),
                          low = '#000000', high = '#FF0000') +
      mytheme
    return(p)
  }

  if (which == 'time') {
    p <- ggplot(rings, aes(x = x, y = y, fill = mid_t)) +
      coord_fixed(ratio = 1) + xlab("") + ylab("") +
      ggtitle("Filling Time") +
      geom_raster() +
      scale_fill_gradient('Frame', limits = c(1, time.max),
                          low = '#FF0000', high = '#000000') +
      mytheme
    return(p)
  }

  if (which == 'rate') {
    rings$mid_rate[rings$mid_rate > rate.max] <- rate.max
    p <- ggplot(rings, aes(x = x, y = y, fill = mid_rate)) +
      coord_fixed(ratio = 1) + xlab("") + ylab("") +
      ggtitle("Filling Rate") +
      geom_raster() +
      scale_fill_gradient('Rate', limits = c(0, rate.max),
                          low = '#000000', high = '#FF0000') +
      mytheme
    return(p)
  }

  if (which == 'all') {
    p1 <- plot.ring(x, which = 'intensity', ...)
    p2 <- plot.ring(x, which = 'time', ...)
    p3 <- plot.ring(x, which = 'rate', ...)
    multiplot(p1, p2, p3, cols = 3)
  }
}

if(getRversion() >= "2.15.1") utils::globalVariables(c('r', 'mid_fit', 'mid_t', 'mid_rate'))
