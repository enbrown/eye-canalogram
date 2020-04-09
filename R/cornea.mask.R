#' cornea.mask
#'
#' @param obj         CanalogramImages object of images to remove cornea
#'
#' @return CanalogramImages object with cornea removed
#'
#' Remove all (high-resolution) data from the region of the cornea using the
#' cornea mask.
#'
#' @examples
#' \dontrun{
#' obj <- cornea.mask(images)
#' }
cornea.mask <- function(obj) {
  if (! 'CanalogramImages' %in% class(obj)) {
    stop("Function cornea.mask is only for CanalogramImages")
  }
  if (! inherits(obj$cornea$mask, 'Image')) {
    warning('Unable to remove cornea mask since none available')
    return(obj)
  }

  # Make sure that the mask is binary
  obj$cornea$mask <- obj$cornea$mask > 0

  # Go through each raw image and remove the cornea
  data <- EBImage::imageData(obj$data)
  mask <- EBImage::imageData(obj$cornea$mask)
  for (i in 1:(EBImage::numberOfFrames(obj$data))) {
    if (is.integer(data)) {
      data[,,i] <- data[,,i] * (1L - mask)
    } else {
      data[,,i] <- data[,,i] * (1.0 - mask)
    }
  }
  EBImage::imageData(obj$data) <- data

  # Return the CanalogramImages object
  return(obj)
}
