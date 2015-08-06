print.CanalogramImages <- function(obj) {
  cat('Path:\t', paste0("'", obj$path, "'"), '\n', sep = '')
  cat('Images:\t', paste0("'", obj$files, "'", collapse = ',\n\t'), '\n', sep = '')
  for (i in 1:length(obj$images)) {
    cat('Image ', i, ':\t', obj$images[[i]]$w, ' by ', obj$images[[i]]$h, '\n', sep = '')
  }
}
