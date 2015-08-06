#' rescale.images
#'
#' @param obj         CanalogramImages object of images to rescale
#' @param resolution  New resolution
#'
#' @return CanalogramImages object of rescaled images
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
    obj$data <- obj$data.low
  }
  obj$data.low <- EBImage::resize(obj$data, h = resolution)

  if (! is.null(obj$mask) && inherits(obj$mask, 'Image')) {
    obj$mask <- EBImage::resize(obj$mask, h = resolution)
  }
  return(obj)
}
