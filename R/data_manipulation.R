########################################
# Author: Antoine Sachet
# Date: 10/15
#
# Misc functions to manipulate data.
# Most are compatible with dplyr and pipes.
#
########################################

# returns a logical vector indicating if the row contains a NA
na.rows <- function(df) {
  !complete.cases(df)
}

remove_duplicates <- function(df) {
  df[!duplicated(df),]
}

keep_na_rows <- function(df) {
  filter(df, na.rows(df))
}

keep_first <- function(x) {
  UseMethod("keep_first")
}

keep_first.data.frame <- function(df) {
  df[1,]
}

keep_first.default <- function(df) {
  df[1]
}

# similar to na.omit *except* it lets you specify what columns to consider
remove_na <- function(df, ...) {
  df[complete.cases(dplyr::select(df, ...)),]
}

# for use with dplyr::summarize
keep_one <- function(col, quiet=T) {
  if(quiet == T) {
    return(col[1])
  }
  out <- unique(col)
  if(length(out)>1) {
    if(!quiet)
      message("More than one unique value, keeping first value.")
    out <- col[1]
  }
  return(out)
}

# makes rows or columns of a matrix sum to 1
normalize <- function(mat, by="col") {
  margin <- switch(by, col=2, row=1, stop("'by' should be one of 'row', 'col'"))
  if(margin==2)
    apply(mat, MARGIN=margin, FUN=function(x) x/sum(x))
  else
    t(apply(mat, MARGIN=margin, FUN=function(x) x/sum(x)))
}


shuffle <- function(x, ...) {
  UseMethod("shuffle")
}

shuffle.data.frame <- function(df) {
  return (df[sample(nrow(df)),])
}
