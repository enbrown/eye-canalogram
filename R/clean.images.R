clean.images <- function(obj) {
  if (! 'CanalogramImages' %in% class(obj)) {
    stop("Function rescale.images is only for CanalogramImages")
  }
  for (i in 1:length(obj$images)) {
    obj$images[[i]]$image <- NA
  }
  obj$data <- NA
  return(obj)
}
