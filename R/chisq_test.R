#' @title Two-sample Chi-Squared test
#' @description
#' Perform a chi-square two sample test that two data samples come from the same distribution.
#' Note that we are not specifying what that common distribution is.
#'
#' @details
#' The chi-square two sample test is based on binned data.
#' Note that the binning for both data sets should be the same.
#' The basic idea behind the chi-square two sample test is that the observed number of points in each bin
#' (this is scaled for unequal sample sized) should be similar if the two data samples come from common distributions.
#'
#' See http://www.itl.nist.gov/div898/software/dataplot/refman1/auxillar/chi2samp.htm for a more formal description.
#'
#' The application is for example when we have a reference sample R computed on raw panel data
#' giving the demographic distribution of the viewers for a target,
#' and another demographic distribution S computed for the same target but on an expanded panel.
#'
#' We can compare R and S using this two-sample chi-squared test.
#'
#' @param R,S Vectors containing the number of observations in each bin. Should be the same size.
#' R is the reference sample and S is the new sample, but this is actually completely symetric.
#'
#' @return Named list composed of the Chi-Squared value and the associated p-value.
#'
#' @examples
#' R <- c(5, 26, 52, 22, 9, 8)
#' S <- c(7, 35, 67, 36, 21, 10)
#'
#' chisq_test(R, S)
#' @export
chisq_test <- function(R, S) {
  if(length(R) != length(S)) {
    stop("R and S must be of the same length.")
  }
  K1 <- sqrt(sum(S)/sum(R))
  K2 <- sqrt(sum(R)/sum(S))
  chi <-  sum( (K1 * R - K2 * S)^2 / (R+S))

  return (list(chi=chi, p=1-pchisq(chi, df=length(R))))
}
