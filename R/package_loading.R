###################
# Author: Antoine Sachet
# Date: 04/02/2016
#
# Useful funs to load packages
#
###################


#' Load several packages in one go
#'
#' Packages should be given as characters.
#'
#' @examples \dontrun{lib("ggplot2", "RColorBrewer")}
#' @export
lib <- function(...) {
  invisible(
    lapply(list(...), FUN=base::library, character.only=T)
  )
}

#' @describeIn lib Same with `require`
#' @export
req <- function(...) {
  invisible(
    lapply(list(...), FUN=base::require, character.only=T, ...)
  )
}
