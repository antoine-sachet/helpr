#' Check file is empty
#'
#' Check whether the size of the specified file on disk is exactly 0.
#'
#' @param file Character vector of filepath(s) to check.
#' @return Logical vector
#' @export
is_empty <- Vectorize(function(file) {
  base::file.info(file)$size == 0
}, SIMPLIFY = TRUE, USE.NAMES = FALSE)
