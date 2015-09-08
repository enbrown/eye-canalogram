#' rescale.images
#'
#' @param obj         CanalogramImages object of images to rescale
#' @param resolution  New resolution
#'
#' @return CanalogramImages object of rescaled images
#'
#' @export
#'
#' @examples
#' \dontrun{
#' obj <- rescale.images(images, 16)
#' }
rescale.images <- function(obj, resolution) {
  if (! 'CanalogramImages' %in% class(obj)) {
    stop("Function rescale.images is only for CanalogramImages")
  }

  if (is.na(obj$data) || is.null(obj$data)) {
    warning("Function rescale.images called with high-resolution data missing (cleaned), using low-resolution data.")
    obj$data <- obj$data.low
  }
  obj$data.low <- EBImage::resize(obj$data, h = resolution)

  # Handle cornea masks
  if (inherits(obj$mask, 'Image')) {
    # First, make sure the mask is binary
    mask <- (obj$mask > 0)

    # Next, negate it so the cornea is black
    mask <- 1.0 - mask

    # Convert binary mask back to a real number
    mask <- mask * 1.0

    # Resize this scaled mask
    obj$mask.low <- EBImage::resize(mask, h = resolution)
    obj$mask.dist <- EBImage::distmap(obj$mask.low)

    # Re-scale the data by how much is outside the cornea
    scale <- 1.0 / obj$mask.low
    scale <- scale * is.finite(scale)

    for (i in 1:(EBImage::numberOfFrames(obj$data.low))) {
      obj$data.low[,,i] <- obj$data.low[,,i] * scale
    }
  } else {
    obj$mask.low <- NA
    obj$mask.dist <- NA
  }

  return(obj)
}
