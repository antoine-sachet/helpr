
#' Numeric summary in one string
#'
#'  Returns a string containing a five-number summary and the mean of a numeric vector.
#'  The names stands for what is being reported: minimum, 1st quartile, median, mean, 3d quartile, maximum.
#'
#'  @param x Numeric vector
#'  @param na.rm Whether to ignore NAs
#'  @param digits Digits to use for rounding
#'  @param space Whether to use spacing in the output
#'
#'  @seealso msd, fivenum
#'
#'  @export
mqmmqm <- function(x, na.rm=T, digits=1, space=T) {
  fn <- round(fivenum(x, na.rm=na.rm), digits=digits)
  m <- round(mean(x, na.rm=na.rm), digits=digits)
  sep <- ifelse(rep(space, 3), c(" <- ", "/", " -> "), c("-", "/", "-"))
  paste0(fn[1], sep[1], fn[2], sep[1], fn[3], sep[2], m, sep[3], fn[4], sep[3], fn[5])
}

#' Mean plus-or-minus Standard Deviation
#'
#' Returns a string in format "mean ± sd".
#'
#' @param x Numeric vector
#' @param na.rm Whether to ignore NAs
#' @param digits Digits to use for rounding
#' @param ... Passed to mean
#'
#' @seealso mqmmqm, fivenum
#'
#' @export
msd <- function(x, na.rm=F, digits=1, ...) {
  paste0(round(mean(x, na.rm=na.rm, ...), digits=digits),
         " ± ",
         round(sd(x, na.rm=na.rm), digits=digits))
}
