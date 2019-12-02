#' Keep first value
#'
#' Useful when piping. Can be used in `summarise`.
#'
#' @export
keep_first <- function(x) {
  UseMethod("keep_first")
}

#' @describeIn keep_first Method for data.frame (keeps first row)
keep_first.data.frame <- function(df) {
  df[1,]
}

#' @describeIn keep_first Method for list
keep_first.list <- function(df) {
  df[[1]]
}


#' @describeIn keep_first Default method
keep_first.default <- function(df) {
  df[1]
}
