#' Check if file is empty
#'
#' A vectorised wrapper around file.info which checks whether the size of the file on disk is exactly 0.
#'
#' @param file Character vector, path(s) to the file(s) to check.
#'
#' @export
isFileEmpty <- Vectorize(function(file) {
  file.info(file)$size == 0
}, SIMPLIFY=T, USE.NAMES=F)
