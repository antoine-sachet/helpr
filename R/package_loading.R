###################
# Author: Antoine Sachet
# Date: 04/02/2016
#
# Useful funs to load packages
#
###################


##' @export
lib <- function(p, ...) {
  invisible(
    lapply(p, FUN=base::library, character.only=T, ...)
  )
}

##' @export
req <- function(p, ...) {
  invisible(
    lapply(p, FUN=base::require, character.only=T, ...)
  )
}
