#' Remove duplicated rows of a data.frame
#'
#' Removes exact duplicates using `duplicated`.
#'
#' @export
remove_duplicates <- function(df) {
  df[!base::duplicated(df),]
}
