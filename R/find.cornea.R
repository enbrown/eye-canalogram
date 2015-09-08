find.cornea <- function(obj,
                        center_search = list(span = 5, step = 1,
                                             frames = 3, threshold = 0.5, percentage = 0.95)) {
  if (! inherits(obj, 'CanalogramImages')) {
    stop('Function find.cornea only works on CanalogramImages objects\n')
  }
  if (! inherits(obj$data, 'Image')) {
    stop('Function find.cornea requires raw data.\n')
  }

  # See if the cornea mask already exists
  if (inherits(obj$mask, 'Image')) {
    message("Cornea provided by mask.")

    # Threshold mask to make it binary
    obj$mask <- obj$mask > 0
  } else {
    message("Finding cornea based on raw data")

    # Restrict to the first few frames
    data <- obj$data[,, 1:(center_search$frames)]

    # Normalize data
    data <- EBImage::normalize(data, separate = FALSE)

    # Threshold data
    data <- data > center_search$threshold

    # Fill in holes and remove small noise
    kern <- EBImage::makeBrush(min(11, ceiling(0.05 * dim(data)[[2]])), shape = 'disc')
    data <- EBImage::fillHull(data)
    data <- EBImage::closing(data, kern)
    data <- EBImage::opening(data, kern)

    # Combine all frames
    EBImage::imageData(data) <- apply(EBImage::imageData(data), 1:2, mean, na.omit=TRUE)

    # Threshold again to make the cornea mask
    obj$mask <- data > center_search$percentage
  }

  # Compute distance map
  center <- EBImage::distmap(obj$mask)

  # Maximum value is the radius and position of it is the cornea center
  center <- reshape2::melt(center)
  colnames(center) <- c('x', 'y', 'r')
  resolution <- max(center$x, center$y)
  center$x <- center$x / resolution
  center$y <- center$y / resolution
  center$r <- center$r / resolution
  center <- center[center$r == max(center$r), , drop = FALSE]

  # Save the cornea center and minimum radius
  obj$cornea <- list(mask = obj$mask,
                     center.x = mean(center$x),
                     center.y = mean(center$y),
                     min.radius = mean(center$r))

  return(invisible(obj))
}
