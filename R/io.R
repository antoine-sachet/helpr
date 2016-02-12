########################
# Input/output functions
#
# Author: Antoine Sachet
########################


# Input -------------------------------------------------------------------

##' @export
read.table.gz <- function(file, ...) {
  utils::read.table(file=gzfile(file), ...)
}

##' @export
read.csv.gz <- function(file, ...) {
  utils::read.csv(file=gzfile(file), ...)
}

# output ------------------------------------------------------------------

##' @export
write.table.gz <- function(x, file, ...) {
    utils::write.table(x, file=gzfile(file), ...)
}

##' @export
write.csv.gz <- function(x, file, ...) {
  utils::write.csv(x, file=gzfile(file), ...)
}
