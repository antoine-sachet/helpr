########################################
# Author: Antoine Sachet
# Date: 09/15
#
# Misc functions to simplify a technical operation
########################################

#' Vectorized switch operation
#' @description The `switch` operator cannot be applied to a vector.
#' `vswitch` removes this limitation.
#'
#' @param EXPR Expression to switch on.
#' @param ... Switching rules passed to switch
#'
#'
#' @examples
#' x <- letters[1:5]
#' vswitch(x, a="first", b="second", ">3")
#' @export
vswitch <- function(EXPR, ...) {
  sapply(EXPR, FUN=function(x) switch(x, ...))
}

#' Easily rename one or more of the existing levels of fac
#'
#' @param fac Factor for which levels should be changed
#' @param old Character vector of old levels to rename
#' @param new Character vector of new names to replace those in `old`.
#'
#' @details `new` should be as long as `old` or of length 1.
#' If it is as long as `old`, then the first value in `new` will replace the first value in `old`, etc.
#' if it is of length 1, then all levels in `old` will be replace by `new`.
#'
#' This works as expected even if a level in `old` is not present in fac or if a level in `new` is already present in fac.
#'
#' @return `fac` with modified levels.
#'
#' @examples
#' # creating a factor
#' fac <- factor(c("PC", "laptop", "mobile", "telephone", "TV"))
#'
#' # 1 replacement
#' rename_levels(fac, old="PC", new="computer")
#'
#' # 1 replacement to already existing level
#' rename_levels(fac, old="telephone", new="mobile")
#'
#' # 2 parallel replacements
#' rename_levels(fac, old=c("PC", "TV"), new=c("computer", "television"))
#'
#' # 2 replacement to one common level
#' rename_levels(fac, old=c("PC", "laptop"), new=c("computer"))
#'
#' # Replacement of inexisting level
#' rename_levels(fac, old="minitel", new="oldie")
#' @export
rename_levels <- function(fac, old, new) {
  n <- length(old)
  if(length(new) != n & length(new)!=1) {
    stop("Length of `new` must be 1 or the length of `old`.")
  } else if(length(new)==1) {
    new = rep(new, n)
  }
  lev <- levels(fac)
  lev.list <- `names<-`(as.list(lev), lev)
  lev.list[old] <- NULL
  for(i in 1:n) {
    lev.list[[new[i]]] <- c(lev.list[[new[i]]], old[i])
  }

  `levels<-`(fac, lev.list)
}


#' @title `not in` operator
#' @description Shorthand for \code{!(x \%in\% y)}
#' @usage x \%!in\% y
#' @export
`%!in%` <- function(x, y) {
  !(x %in% y)
}
