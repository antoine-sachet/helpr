#' Compute the mode of a vector
#'
#' @param x Vector of which to compute the mode
#' @param na.rm Whether to ignore NA's
#' @return Most frequent value occuring in the vector
#'
#' @details
#' If there are several modes, the one occuring first in the data will be returned.
#' No binning is attempted for numeric vectors.
#' @author Ken Williams
#' @source http://stackoverflow.com/a/8189441/3498910
#'
#' @export
#'
#' @examples
#' # x will contain lots of 'a'
#' x <- sample(letters, prob=c(10, rep(1, 25)), size=100, replace=T)
#' Mode(x) # will be 'a' (with probability 1-2.4e-15)
#'
#' x <- rnorm(10)
#' Mode(x)
#'
Mode <- function(x, na.rm=F) {
  ux <- unique(x[!is.na(x)])
  return(ux[which.max(tabulate(match(x[!is.na(x)], ux)))])
}
