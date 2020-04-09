#' roi.mask
#'
#' @param obj  CanalogramImages object of images to select region of interest
#'
#' @return CanalogramImages object with areas outside the region of interest disregarded
#'
#' Remove all (high-resolution) data from outside the region of interest using the
#' ROI mask.
#'
#' @examples
#' \dontrun{
#' obj <- roi.mask(images)
#' }
roi.mask <- function(obj) {
  if (! 'CanalogramImages' %in% class(obj)) {
    stop("Function roi.mask is only for CanalogramImages")
  }
  if (! inherits(obj$ROI$mask, 'Image')) {
    warning('Unable to isolate region-of-interest since no mask available')
    return(obj)
  }

  # Go through each raw image and keep only the region of interest
  data <- EBImage::imageData(obj$data)
  mask <- EBImage::imageData(obj$ROI$mask)
  for (i in 1:(EBImage::numberOfFrames(obj$data))) {
    data[,,i] <- data[,,i] * mask
  }
  EBImage::imageData(obj$data) <- data

  # Return the CanalogramImages object
  return(obj)
}
