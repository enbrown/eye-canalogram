#' readImages
#'
#' @param root   A directory/file root use to get all images for this eye
#' @param n      Restrict to only the first n images
#' @param low    Resolution for processing
#' @param clean  Clean up after reading and rescaling images
#'
#' @return A CanalogramImages object containing all of the image data
#'
#' @export
#'
#' @examples
#' \dontrun{
#' images <- read.images('./data/Trial-1/one gree_T00', n = 5)
#' plot(images)
#' }
read.images <- function(root, n = NULL, low = 32, clean = TRUE) {

  res <- list(
    path = normalizePath(dirname(root)),
    root = basename(root),
    files = c(),
    files.path = c(),
    images = list(),
    data = NA,
    data.low = NA,
    mask.file = NA,
    mask = NA
  )
  class(res) <- 'CanalogramImages'

  # Get all files in the directory
  res$files <- list.files(res$path,
                          all.files = FALSE, recursive = FALSE, include.dirs = FALSE, no.. = FALSE)

  # Get all files with the "root" in the filename (anywhere, not just the beginning)
  res$files <- grep(res$root, res$files,
                    fixed = TRUE, value = TRUE)

  # Restrict to TIFF and PNG files
  res$files <- grep('\\.(tif|tiff|png)$', res$files,
                    ignore.case = TRUE, perl = TRUE, value = TRUE)

  # Sort the files
  res$files <- sort(res$files)

  # Restict to the first "n" files
  if (is.numeric(n) && is.finite(n)) {
    res$files <- res$files[1:n]
  }

  # Expand the filename
  res$files.path <- file.path(res$path, res$files)

  # Load all of the files
  message("Reading '", res$root, "' images in directory '", res$path, "'.")
  res$images <- lapply(res$files.path, function(f) {
    r <- list(file = f,
              image = NULL,
              w = NA,
              h = NA,
              min = NA,
              max = NA)
    if (grepl('\\.(tif|tiff)$', f)) {
      r$image <- EBImage::readImage(f, convert = FALSE, info = TRUE, as.is = TRUE)
    } else if (grepl('\\.png$', f)) {
      r$image <- EBImage::readImage(f, info = TRUE)
    } else {
      stop("Don't know how to read image ", f, " in function read.images")
    }
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

  # See if there is an cornea mask
  cornea_mask <- NA
  if (file.exists(file.path(res$path, 'mask.tif'))) {
    cornea_mask <- file.path(res$path, 'mask.tif')
  } else if (file.exists(file.path(res$path, 'mask.tiff'))) {
    cornea_mask <- file.path(res$path, 'mask.tiff')
  } else if (file.exists(file.path(res$path, 'mask.png'))) {
    cornea_mask <- file.path(res$path, 'mask.png')
  }
  if (!is.na(cornea_mask) && file.exists(cornea_mask)) {
    message("Using cornea mask found in '", cornea_mask, "'.")
    if (grepl('\\.(tif|tiff)$', cornea_mask)) {
      mask <- EBImage::readImage(cornea_mask, convert = TRUE, info = TRUE, as.is = TRUE)
    } else if (grepl('\\.png$', cornea_mask)) {
      mask <- EBImage::readImage(cornea_mask, info = TRUE)
    }
    mask <- EBImage::flip(mask)
    # Convert to grayscale (in case the image is saved as RGB)
    mask <- EBImage::channel(mask, 'gray')
    res$maskFile <- cornea_mask
    res$mask <- mask
  } else {
    res$maskFile <- NA
    res$mask <- NA
  }
  rm(cornea_mask)

  # Find a cornea mask
  res <- find.cornea(res)

  # Apply the cornea mask
  res <- cornea.mask(res)

  # Rescale the images
  res <- rescale.images(res, low)

  # Clean up extra data
  if (clean) {
    res <- clean.images(res)
  }

  # Return the object
  return(invisible(res))
}
