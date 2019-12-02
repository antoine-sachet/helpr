#' Normalise a matrix by rows or column
#'
#' Normalise rows or columns of a matrix for the L1 or L2 norm. 
#' 
#' @param mat Matrix to normalise
#' @param by "col" or "row"
#' @param norm 1 (for L1 norm) or 2 (for L2 norm)
#' @export
normalise <- function(mat, norm=1, by="col") {
  margin <- switch(by, col = 2, row = 1, stop("'by' should be one of 'row', 'col'"))
  normfun <- switch(norm, function(x) sum(x), function(x) sqrt(sum(x^2)), stop("Norm should be 1 or 2"))
  
  if (margin == 2)
    apply(mat, MARGIN = margin, FUN = function(x) x/normfun(x))
  else
    t(apply(mat, MARGIN = margin, FUN = function(x) x/normfun(x)))
}

#' @export
#' @describeIn normalise Alternative spelling
normalize <- normalise
