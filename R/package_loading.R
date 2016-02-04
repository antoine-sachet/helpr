###################
# Author: Antoine Sachet
# Date: 04/02/2016
#
# Useful funs to load packages
#
###################


lib <- function(p, ...) {
  invisible(
    lapply(p, FUN=base::library, character.only=T, ...)
  )
}

req <- function(p, ...) {
  invisible(
    lapply(p, FUN=base::require, character.only=T, ...)
  )
}
