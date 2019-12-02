#' Full print of data.frame
#'
#' Shorthand for `print.data.frame`, useful for occasional full printing of a dplyr `tbl`.
#' @seealso print.data.frame hfp
#'
#' @export
fp <- base::print.data.frame

#' @describeIn fp Wrapper around head and fp. `...` is passed to head.
#'
#' @export
hfp <- function(x, ...) x %>% head(...) %>% fp