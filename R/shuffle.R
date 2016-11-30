#' Shuffle data
#'
#' Reorders a data structure randomly.
#'
#' @export
shuffle <- function(x, ...) {
  UseMethod("shuffle")
}

#' @describeIn shuffle Method for data.frame
#'
#' @export
shuffle.data.frame <- function(df) {
  return (df[sample(nrow(df)),])
}

#' @describeIn shuffle Default method for vectors or list
#'
#' @export
shuffle.default <- function(x) {
  return (x[sample(length(x))])
}
