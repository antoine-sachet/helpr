########################################
# Author: Antoine Sachet
# Date: 09/15
#
# credit to:
# - http://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session
#
########################################

#' List objects in the global environment
#'
#' List objects in memory: name, type, dims, byte size, etc. 
#' The shorthand `lsos()` is very convenient for tracking memory usage.
#'
#' @return Data.frame containing object and memory information
#'
#' @param pos 
#' @param pattern 
#' @param order.by 
#' @param decreasing 
#' @param head 
#' @param n 
#'
#' @describeIn ls.objects Advanced printing
#' @note http://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session.
#' 
#' @examples lsos()
#' 
#' @export
ls.objects <- function (pos = 1, pattern, order.by,
                         decreasing=FALSE, head=FALSE, n=5) {
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  if(length(names)>0) {

    obj.class <- napply(names, function(x) as.character(class(x))[1])
    obj.mode <- napply(names, mode)
    obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
    obj.prettysize <- napply(names, function(x) {
      capture.output(format(utils::object.size(x), units = "auto")) })
    obj.size <- napply(names, object.size)
    obj.dim <- t(napply(names, function(x)
      as.numeric(dim(x))[1:2]))
    vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
    obj.dim[vec, 1] <- napply(names, length)[vec]
    out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)

    names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
    if (!missing(order.by))
      out <- out[order(out[[order.by]], decreasing=decreasing), ]
    if (head)
      out <- head(out, n)
  } else {
    out <- data.frame(Type=character(), Size=integer(), PrettySize=character(),
                      Rows=integer(), Columns=integer())
  }
  out
}

#' @describeIn ls.objects Print the 10 largest objects in memory, ordered by size.
#' @export
lsos <- function(..., n=10) {
  ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}
