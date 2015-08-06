#' readImages
#'
#' @param root   A directory/file root use to get all images for this eye
#' @param n      Restrict to only the first n images
#' @param low    Resolution for processing
#' @param clean  Clean up after reading and rescaling images
#'
#' @return A CanalogramImages object containing all of the image data
#' @export
#'
#' @examples
#' \dontrun{
#' images <- read.images('./data/Trial-1/one gree_T00', n = 5)
#' plot(images)
#' }
read.images <- function(root, n = NULL, low = 64, clean = TRUE) {

  res <- list(
    path = normalizePath(dirname(root)),
    root = basename(root),
    files = c(),
    files.path = c(),
    images = list(),
    data = NA,
    data.low = NA
  )
  class(res) <- 'CanalogramImages'

  # Get all files in the directory
  res$files <- list.files(res$path,
                          all.files = FALSE, recursive = FALSE, include.dirs = FALSE, no.. = FALSE)
  # Get all files with the "root" in the filename (anywhere, not just the beginning)
  res$files <- grep(res$root, res$files,
                    fixed = TRUE, value = TRUE)
  # Restrict to TIFF files
  res$files <- grep('\\.(tif|tiff)$', res$files,
                    ignore.case = TRUE, perl = TRUE, value = TRUE)
  # Sort the files
  res$files <- sort(res$files)

  # Restict to the first "n" files
  if (is.numeric(n) && is.finite(n)) {
    res$files <- res$files[1:n]
  }

  # See if there is an cornea mask
  if (file.exists(file.path(res$path, 'mask.tif'))) {
    message('Reading cornea mask')
    file <- file.path(res$path, 'mask.tif')
    mask <- EBImage::readImage(file, convert = TRUE, info = TRUE, as.is = TRUE)
    mask <- EBImage::flip(mask)
    res$maskFile <- file
    res$mask <- mask
  }

  # Expand the filename
  res$files.path <- file.path(res$path, res$files)

  # Load all of the files
  message('Reading images.')

  res$images <- lapply(res$files.path, function(f) {
    r <- list(file = f,
              image = NULL,
              w = NA,
              h = NA,
              min = NA,
              max = NA)
    r$image <- EBImage::readImage(f, convert = FALSE, info = TRUE, as.is = TRUE)
    r$image <- EBImage::flip(r$image)
    r$w <- ncol(r$image)
    r$h <- nrow(r$image)
    r$min <- min(r$image)
    r$max <- max(r$image)
    return(r)
  })

  # The below assumes that all widths and heights are the same
  res$data <- EBImage::combine(lapply(res$images, function(i) {i$image})) # - res$images[[1]]$image}))
  res$data.low <- NA

  # Rescale the images
  res <- rescale.images(res, low)

  # Clean up extra data
  if (clean) {
    res <- clean.images(res)
  }

  # Return the object
  return(invisible(res))
}
