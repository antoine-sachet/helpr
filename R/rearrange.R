#' Arrange rows by variable
#'
#' Wrapper for arranging a data.frame with respect to some columns
#' and moving said columns to the front of the data.frame.
#'
#' @seealso dplyr::arrange
#' @examples rearrange(mtcars, wt)
#' @importFrom dplyr select arrange everything
#' @export
rearrange <- function(df, ...)
  dplyr::select(df, ..., dplyr::everything()) %>%
  dplyr::arrange(...)



