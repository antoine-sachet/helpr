########################
# Input/output functions
#
# Author: Antoine Sachet
########################


# output ------------------------------------------------------------------

#' Write directly to a compressed gz file
#'
#' @export
write.table.gz <- function(x, file, ...) {
    utils::write.table(x, file=gzfile(file), ...)
}


#' @describeIn write.table.gz
#'  @export
write.csv.gz <- function(x, file, ...) {
  utils::write.csv(x, file=gzfile(file), ...)
}
