########################
# Input/output functions
#
# Author: Antoine Sachet
########################


# Input -------------------------------------------------------------------

read.table.gz <- function(file, ...) {
  utils::read.table(file=gzfile(file), ...)
}

read.csv.gz <- function(file, ...) {
  utils::read.csv(file=gzfile(file), ...)
}

# output ------------------------------------------------------------------

write.table.gz <- function(x, file, ...) {
    utils::write.table(x, file=gzfile(file), ...)
}

write.csv.gz <- function(x, file, ...) {
  utils::write.csv(x, file=gzfile(file), ...)
}
