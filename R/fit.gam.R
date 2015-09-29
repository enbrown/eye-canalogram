#' fit.gam
#'
#' @param obj             CanalogramImages object
#' @param r_bands         Number of radial bands to fit
#' @param theta_band_size Degrees per angular band
#' @param fit.grid        Whether to fit the models to each grid point
#' @param fit.bands       Whether to fit a global model to bands
#'
#' @return CanaogramGAM object describing the fit
#' @export
#' @importFrom dplyr %>%
#' @importFrom mgcv gam s te
#'
#' @examples
#' \dontrun{
#' images <- read.images('./data/Trial-1/one gree_T00', n = 5)
#' plot(images)
#' fit <- fit.gam(images)
#' plot(fit)
#' plot.images(fit)
#' plot.ring(fit)
#' }
fit.gam <- function(obj, r_bands = 5, theta_band_size = 10,
                    fit.grid = TRUE, fit.bands = TRUE, fit.coarse.bands = FALSE) {
  if (! inherits(obj, 'CanalogramImages')) {
    stop('Function fit.gam only works on CanalogramImages objects\n')
  }

  # Build up the return object
  obj$gam <- list(
    options = list(r_bands = r_bands,
                   theta_band_size = theta_band_size),
    data = NA,
    center = NA,
    cornea_radius = NA,
    clock_data = NA,
    max_radius = NA,
    band_width = NA,
    grid_metrics = NA,
    fit = NA,
    metrics = NA,
    rings = NA
  )
  class(obj) <- c('CanaogramGAM', class(obj))

  # Get the raw data as a data.frame
  if (inherits(obj$data.low, 'Image')) {
    data <- reshape2::melt(obj$data.low)
    colnames(data) <- c('x','y','t','I')
    obj$gam$data <- data
  } else if (inherits(obj$data, 'Image')) {
    data <- reshape2::melt(obj$data)
    colnames(data) <- c('x','y','t','I')
    obj$gam$data <- data
  } else {
    stop('No data found for function fit.gam')
  }

  # Scale data to a 0.0 -- 1.0 range
  obj <- update.data.scale(obj)

  # Get the base image resolution
  obj$gam$resolution <- max(c(obj$gam$data$x, obj$gam$data$y))

  # Clean up the data (do after setting the resolution)
  obj$gam$data <- na.omit(obj$gam$data)

  # Use the known/computed cornea center
  center <- list(x = obj$cornea$center.x,
                 y = obj$cornea$center.y)
  obj$gam$center <- center
  obj$gam$cornea_radius <- obj$cornea$min.radius * obj$gam$resolution

  # Transform to radial basis
  obj <- update.radial.basis(obj)

  # Generate image masks
  obj <- update.masks(obj)

  # Generate data bands
  obj <- update.bands(obj, theta_band_size = theta_band_size, r_bands = r_bands)

  # Generate data by clock hour and radius band
  obj <- update.clock.data(obj)

  if (fit.grid) {
    try({
      # Fit the individual model
      message('Fitting GAM to grid (individual fits)')
      obj <- update.fit.grid(obj)

      message('Processing grid metrics')
      obj <- update.grid.metrics(obj)
    })
  }

  if (fit.bands) {
    try({
      # Fit the global model
      message('Fitting GAM to rings (global fit)')
      obj <- update.fit(obj)

      # Determine ring metrics
      message('Processing ring metrics')
      obj <- update.metrics(obj, fit.coarse.bands = fit.coarse.bands)

      # Create rings
      message('Updating rings')
      obj <- update.rings(obj, fit.coarse.bands = fit.coarse.bands)
    })
  }

  return(obj)
}

update.data.scale <- function(obj) {
  if (! inherits(obj, 'CanaogramGAM')) {
    stop('Function update.data.scale requires a CanaogramGAM object\n')
  }

  # Get the object's data
  data <- obj$gam$data

  # Rescale the data
  data$z <- data$I / max(data$I, na.rm=TRUE)

  # Update the object and return it
  obj$gam$data <- data
  return(invisible(obj))
}

update.radial.basis <- function(obj) {
  if (! inherits(obj, 'CanaogramGAM')) {
    stop('Function update.radial.basis requires a CanaogramGAM object\n')
  }
  if (! 'center' %in% names(obj$gam)) {
    stop('Missing corneal center needed for update.radial.basis')
  }
  if (! 'resolution' %in% names(obj$gam)) {
    stop('Missing resolution needed for update.radial.basis')
  }

  # Get the object's data
  data <- obj$gam$data
  center <- obj$gam$center
  resolution <- obj$gam$resolution

  # Transform to radial basis
  data$X <- data$x - (center$x * resolution)
  data$Y <- data$y - (center$y * resolution)
  data$theta <- atan2(-data$X, -data$Y) * 180.0 / pi + 180.0
  data$R <- sqrt(data$X * data$X + data$Y * data$Y)
  data$X <- NULL
  data$Y <- NULL

  # Use distance map for radius
  mask.dist <- reshape2::melt(obj$mask.dist)
  colnames(mask.dist) <- c('x','y','r')
  data <- merge(data, mask.dist, by = c('x', 'y'))

  # Update the object and return it
  obj$gam$data <- data
  return(invisible(obj))
}

update.masks <- function(obj) {
  if (! inherits(obj, 'CanaogramGAM')) {
    stop('Function get.centers requires a CanaogramGAM object\n')
  }
  if (! is.finite(obj$gam$cornea_radius)) {
    stop('Function update.corneal.radius needs to be run before update.masks')
  }

  # Get the object's data
  data <- obj$gam$data
  mask <- obj$cornea$mask
  cornea_radius <- obj$gam$cornea_radius

  # Mask those pixels in the cornea
  #data$cornea <- (data$r < cornea_radius)

  # Mask those pixels at the border
  data$border <- data$x == min(data$x) | data$x == max(data$x) |
    data$y == min(data$x) | data$y == max(data$y)

  # Determine the maximum radius for a complete dataset
  max_radius <- min(data$r[data$border] + 0.5)

  # Mask those pixels in the analysis region
  #data$mask <- (! data$cornea) & data$r <= max_radius
  data$mask <- data$r <= max_radius

  # Update the object and return it
  obj$gam$data <- data
  obj$gam$max_radius <- max_radius
  return(invisible(obj))
}

update.bands <- function(obj, theta_band_size = 10, r_bands = 5) {
  if (! inherits(obj, 'CanaogramGAM')) {
    stop('Function update.bands requires a CanaogramGAM object\n')
  }
  if (! is.finite(obj$gam$cornea_radius)) {
    stop('Function update.corneal.radius needs to be run before update.bands')
  }
  if (! is.finite(obj$gam$max_radius)) {
    stop('Function update.masks needs to be run before update.bands')
  }

  # Get the object's data
  data <- obj$gam$data
  max_radius <- obj$gam$max_radius
  cornea_radius <- obj$gam$cornea_radius

  # Generate coarser blockings/bands
  data$clock <- round(data$theta / 360.0 * 12.0)
  data$clock[data$clock == 0] <- 12
  data$theta_band <- theta_band_size * round(data$theta / theta_band_size)
  band_width <- (max_radius - 1 + 0.1) / r_bands
  data$r_band <- floor((data$r - 1) / band_width)

  # Update the object and return it
  obj$gam$data <- data
  obj$gam$band_width <- band_width
  return(invisible(obj))
}

update.clock.data <- function(obj) {
  if (! inherits(obj, 'CanaogramGAM')) {
    stop('Function update.clock.data requires a CanaogramGAM object\n')
  }
  if (! 'z' %in% colnames(obj$gam$data)) {
    stop('Function update.data.scale needs to be run before update.clock.data')
  }
  if (! 'r_band' %in% colnames(obj$gam$data)) {
    stop('Function update.bands needs to be run before update.clock.data')
  }

  # Get the object's data
  data <- obj$gam$data

  # Summarize all of the image data by clock hour and radius band (and time)
  clock_data <- data %>%
    dplyr::filter(mask) %>%
    dplyr::group_by(t, clock, r_band) %>%
    dplyr::summarize(I = mean(I),
                     z = mean(z),
                     area = length(x)) %>%
    dplyr::ungroup()

  # Update the object and return it
  obj$gam$clock_data <- clock_data
  return(invisible(obj))
}
if(getRversion() >= "2.15.1") utils::globalVariables(c('mask',
                                                       't', 'clock', 'r_band',
                                                       'I', 'z', 'x'))

update.fit <- function(obj) {
  if (! inherits(obj, 'CanaogramGAM')) {
    stop('Function update.fit requires a CanaogramGAM object\n')
  }
  if (! inherits(obj$gam$clock_data, 'data.frame')) {
    stop('Function update.clock.data needs to be run before update.fit\n')
  }

  # Get the object's data
  clock_data <- obj$gam$clock_data
  data <- obj$gam$data
  r_bands <- obj$gam$options$r_bands
  if (length(unique(clock_data$r_band)) < r_bands) {
    warning('Fewer radial bands can be created than requested: ',
            length(unique(clock_data$r_band)), ' < ', r_bands)
    r_bands <- length(unique(clock_data$r_band))
  }

  # Fit the GAM model to the clock data alone (coarse)
  message('Fitting clock data (very low resolution)')
  fit.clock <- gam(z ~ s(clock,              bs = 'cc',                k = 12) +
                     s(r_band,             bs = 'cs',                k = min(r_bands, 5)) +
                     s(t,                  bs = 'cs',                k = 5) +
                     te(clock, r_band, t,  bs = c('cc', 'cs', 'cs'), k = c(12, min(r_bands, 5), 5)),
                   weights = clock_data$area,
                   knots = list(clock = c(0, 12)),
                   data = clock_data)

  # Add the prediction to the data
  clock_data <- cbind(clock_data,
                      predict(fit.clock, type = 'response', se.fit = TRUE))
  attributes(clock_data$fit) <- NULL
  attributes(clock_data$se.fit) <- NULL

  # Update the object
  obj$gam$fit.clock <- fit.clock
  obj$gam$clock_data <- clock_data

  # Fit the GAM model to the low-resolution data (finer than clock data)
  message('Fitting low-resolution data')
  sel <- data$r_band <= (r_bands + 1)
  fit <- gam(z ~ s(theta,         bs = 'cc',                k = 12) +
               s(r,             bs = 'cs',                k = min(r_bands, 5)) +
               s(t,             bs = 'cs',                k = 5) +
               te(theta, r, t,  bs = c('cc', 'cs', 'cs'), k = c(12, min(r_bands, 5), 5)),
             knots = list(theta = c(0, 360)),
             data = data[sel,])

  # Add the prediction to the data
  data$fit <- NA
  data$se.fit <- NA
  x <- predict(fit, type = 'response', se.fit = TRUE)
  data$fit[sel] <- x$fit
  data$se.fit[sel] <- x$se.fit
  attributes(data$fit) <- NULL
  attributes(data$se.fit) <- NULL

  # Update the object and return it
  obj$gam$fit <- fit
  obj$gam$data <- data
  return(invisible(obj))
}

update.fit.grid <- function(obj) {
  if (! inherits(obj, 'CanaogramGAM')) {
    stop('Function update.fit.grids requires a CanaogramGAM object\n')
  }
  if (! 'mask' %in% colnames(obj$gam$data)) {
    stop('Function update.masks needs to be run before update.fit.grids\n')
  }

  # Get the object's data
  data <- obj$gam$data
  data$grid.fit <- NA
  data$grid.se.fit <- NA

  # Get which points need to be fit
  coords <- data %>%
    dplyr::select(x, y) %>%
    dplyr::distinct()
  message('  Fitting ', nrow(coords), ' individual GAM models')

  # Iterate through all points and fit an individual GAM model
  for (i in 1:nrow(coords)) {
    # Pull out the data to use
    sel <- (data$x == coords$x[i]) & (data$y == coords$y[i])

    # Fit the GAM model
    fit <- gam(z ~ s(t, bs = 'cs', k = 5), data = data[sel,])

    y <- predict(fit, type = 'response', se.fit = TRUE)
    data$grid.fit[sel] <- y$fit
    data$grid.se.fit[sel] <- y$se.fit
  }

  # Update the object and return it
  obj$gam$data <- data
  return(invisible(obj))
}
if(getRversion() >= "2.15.1") utils::globalVariables(c('cornea', 'x', 'y'))

update.metrics <- function(obj, minimum_threshold = 0.05, fit.coarse.bands = FALSE) {
  if (! inherits(obj, 'CanaogramGAM')) {
    stop('Function update.fit requires a CanaogramGAM object\n')
  }
  if (! inherits(obj$gam$clock_data, 'data.frame')) {
    stop('Function update.clock.data needs to be run before update.fit\n')
  }
  if (! 'fit' %in% colnames(obj$gam$clock_data)) {
    stop('Function update.fit needs to be run before update.fit\n')
  }

#   # Get the object's data
#   clock_data <- obj$gam$clock_data
#
#   clock_metrics <- clock_data %>%
#     dplyr::group_by(clock, r_band) %>%
#     dplyr::mutate(min_fit = min(fit),
#            max_fit = max(fit),
#            mid_fit = mean(range(fit))) %>%
#     dplyr::filter(fit > max(minimum_threshold, mid_fit)) %>%
#     dplyr::arrange(t) %>%
#     dplyr::summarize(min_fit = mean(min_fit),
#               mid_fit = mean(mid_fit),
#               max_fit = mean(max_fit),
#               mid_t = dplyr::first(t)) %>%
#     dplyr::mutate(mid_rate = mid_fit / mid_t * 100) %>%
#     dplyr::ungroup()
#
#   # Update the object
#   obj$gam$clock_metrics <- clock_metrics

  # Get the object's data
  data <- obj$gam$data

  # Update the metrics by x-y location
  xy_metrics <- data %>%
    dplyr::group_by(x, y) %>%
    dplyr::mutate(min_fit = min(fit),
                  max_fit = max(fit),
                  mid_fit = mean(range(fit))) %>%
    dplyr::filter(fit > max(minimum_threshold, mid_fit)) %>%
    dplyr::arrange(t) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(x, y, clock, r_band) %>%
    dplyr::summarize(min_fit = mean(min_fit),
                     mid_fit = mean(mid_fit),
                     max_fit = mean(max_fit),
                     mid_t = dplyr::first(t)) %>%
    dplyr::mutate(mid_rate = mid_fit / mid_t * 100) %>%
    dplyr::ungroup()
  xy_metrics$clock <- as.integer(xy_metrics$clock)
  xy_metrics$r_band <- as.integer(xy_metrics$r_band)
  clock_metrics <- xy_metrics %>%
    dplyr::group_by(clock, r_band) %>%
    dplyr::summarize(min_fit = mean(min_fit),
                     mid_fit = mean(mid_fit),
                     max_fit = mean(max_fit),
                     mid_t = mean(mid_t)) %>%
    dplyr::mutate(mid_rate = mid_fit / mid_t * 100) %>%
    dplyr::ungroup()

  # Update the object and return it
  obj$gam$xy_metrics <- xy_metrics
  obj$gam$clock_metrics <- clock_metrics
  return(invisible(obj))
}
if(getRversion() >= "2.15.1") utils::globalVariables(c('clock', 'r_band',
                                                       'fit',
                                                       'min_fit', 'mid_fit', 'max_fit',
                                                       't'))

update.rings <- function(obj, fit.coarse.bands = FALSE) {
  if (! inherits(obj, 'CanaogramGAM')) {
    stop('Function update.fit requires a CanaogramGAM object\n')
  }
  if (! inherits(obj$gam$clock_metrics, 'data.frame')) {
    stop('Function update.metrics needs to be run before update.fit\n')
  }

  # Get the object's data
  clock_metrics <- obj$gam$clock_metrics
  data <- obj$gam$data

  rings <- merge(data %>%
                   dplyr::filter(t == 1 & mask) %>%
                   dplyr::select(-t, -I, -mask, -border),
                 clock_metrics, all.x = TRUE)
  rings$min_fit[is.na(rings$min_fit)] <- 0
  rings$mid_fit[is.na(rings$mid_fit)] <- 0
  rings$max_fit[is.na(rings$max_fit)] <- 0
  rings$mid_t[is.na(rings$mid_t)] <- max(data$t)
  rings$mid_rate[is.na(rings$mid_rate)] <- 0

  # Update the object and return it
  obj$gam$rings <- rings
  return(invisible(obj))
}
if(getRversion() >= "2.15.1") utils::globalVariables(c('t', 'mask', 'cornea', 'border'))

update.grid.metrics <- function(obj, minimum_threshold = 0.0) {
  if (! inherits(obj, 'CanaogramGAM')) {
    stop('Function update.grid.metrics requires a CanaogramGAM object\n')
  }
  if (! 'grid.fit' %in% colnames(obj$gam$data)) {
    stop('Function update.fit.grid needs to be run before update.grid.metrics\n')
  }

  # Get the object's data
  data <- obj$gam$data

  grid_metrics <- data %>%
    dplyr::group_by(x, y, r_band, theta_band) %>%
    dplyr::mutate(min_fit = min(grid.fit),
          mid_fit = mean(range(grid.fit)),
          max_fit = max(grid.fit)) %>%
    dplyr::filter(grid.fit > max(minimum_threshold, mid_fit)) %>%
    dplyr::arrange(t) %>%
    dplyr::summarize(min_fit = mean(min_fit),
              mid_fit = mean(mid_fit),
              max_fit = mean(max_fit),
              mid_t = dplyr::first(t)) %>%
    dplyr::mutate(mid_rate = mid_fit / mid_t * 100) %>%
    dplyr::ungroup()

  # Update the object and return it
  obj$gam$grid_metrics <- grid_metrics
  return(invisible(obj))
}
if(getRversion() >= "2.15.1") utils::globalVariables(c('x', 'y',
                                                       'grid.fit',
                                                       'r_band', 'theta_band',
                                                       'mid_fit', 'min_fit', 'max_fit', 't'))

