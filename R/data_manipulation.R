########################################
# Author: Antoine Sachet
# Date: 10/15
#
# Misc functions to manipulate data.
# Most are compatible with dplyr and pipes.
#
########################################


#' Rename a column programmatically
#'
#' Shortcomings in `dplyr::rename_` forces to have this handy function.
#' Simple wrapper around `names<-` and `gsub`.
#'
#' @param x data.frame
#' @param old Character: column to rename. Can be a regex.
#' @param new Character: new name for the column
#' @examples
#' df <- data.frame(a=1:5, b=6:10)
#' col_to_change <- "a"
#' df %>% rename_col(col_to_change, "c")
#'
#' @importFrom magrittr %<>%
#' @export
rename_col <- function(x, old, new) {
  names(x) %<>% gsub(pattern=old, replacement=new)
  x
}


#' Arrange rows by variable
#'
#' Wrapper for arranging a data.frame with respect to some columns
#' and moving said columns to the front of the data.frame.
#'
#' @seealso dplyr::arrange
#' @examples
#'
# sorted <- rearrange(mtcars, wt)
# all.equal(sorted,
#           arrange(mtcars, wt) %>% select(wt, everything()))
#' # TRUE
#' @export
rearrange <- function(df, ...)
  select(df, ..., everything()) %>%
  arrange(...)

#' Full print of data.frame
#'
#' Shorthand for `print.data.frame`, useful for occasional full printing of a dplyr `tbl`.
#' @seealso print.data.frame hfp
#'
#' @export
fp <- print.data.frame

#' @describeIn fp Wrapper around head and fp. Arguments are passed to head.
#'
#' @export
hfp <- function(x, ...) x %>% head(...) %>% fp

#' Group_by with regex based column selection
#'
#' Similar to `group_by` except grouping columns are selected using a regex.
#' @importFrom dplyr group_by
#' @export
group_at <- function(df, pattern)
  group_by_(df, .dots=grep(names(df), pattern=pattern, value=T))


#' Identify rows containing a NA
#'
#' Takes a data.frame and returns a logical vector indicating the rows containing a NA.
#' @export
na.rows <- function(df) {
  !complete.cases(df)
}

#' Remove duplicated rows of a data.frame
#'
#' Removes exact duplicates using `duplicated`.
#'
#' @export
remove_duplicates <- function(df) {
  df[!duplicated(df),]
}

#' Keep only rows containing a NA
#'
#' @export
keep_na_rows <- function(df) {
  filter(df, na.rows(df))
}

#' Keep first value
#'
#' More or less equivalent to `keep_one`
#'
#' @seealso keep_one
#'
#' @export
keep_first <- function(x) {
  UseMethod("keep_first")
}

#' @describeIn keep_first Method for data.frame
#'
#' @export
keep_first.data.frame <- function(df) {
  df[1,]
}

#' @describeIn keep_first Default method
#' @export
keep_first.default <- function(df) {
  df[1]
}

#' Remove NA's from selected columns
#'
#' This is a selective version of `na.omit` which lets you choose which columns to consider.
#' It is a wrapper around dplyr's `select` and `complete.cases`.
#' @importFrom dplyr select
#' @seealso keep_na
#' @export
remove_na <- function(df, ...) {
  df[complete.cases(dplyr::select(df, ...)),]
}


#' Keep only rows with NA's in selected columns
#'
#' This is the opposite of remove_na.
#' Only rows with at least one NA in at least one of the selected columns are kept in the data.frame.
#'
#' @importFrom dplyr select
#' @seealso remove_na
#' @export
keep_na <- function(df, ...) {
  df[!complete.cases(dplyr::select(df, ...)),]
}


#' Keep one obs only
#'
#' This is useful for use with dplyr's `summarize`, when one wants to keep in the summarized data
#' a column which do not need to be summarised.
#'
#' @param col A vector (typically the column to summarise)
#' @param quiet see details
#'
#' @details
#'  The underlying assumption is that all values for the column considered are the same and
#'  we can just keep one when summarising.
#'  If there are different values, the first value of the vector is chosen and alternative values are ignored.
#'  If `quiet=FALSE`, a message is given every time this happens.
#'
#'  Note that if `quiet=T`, no unicity check is performed and the first value is picked anyway.
#'
#' @examples
#' school_results <- data.frame(pupil=c("John", "John", "Mary", "Mary", "Henry"),
#'                              exam=c(1, 2, 1, 2, 1),
#'                              mark=c(9, 8, 10, 8, 6),
#'                              gender=c("M", "M", "F", "F", "M"))
#'
#' pupils_results <- school_results %>%
#'                          group_by(pupil) %>%
#'                          summarise(av_mark=mean(mark),
#'                              gender=keep_one(gender))
#'
##' @export
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

#' Normalise a matrix by rows or column
#'
#' Normalises rows or columns of a matrix according to the L1 or L2 norm.
#' @param mat Matrix to normalise
#' @param by "col" or "row"
#' @param norm 1 (for L1 norm) or 2 (for L2 norm)
#' @export
normalise <- function(mat, by="col", norm=1) {
  margin <- switch(by, col=2, row=1, stop("'by' should be one of 'row', 'col'"))

  normfun <- switch(norm, function(x) sum(x), function(x) sqrt(sum(x^2)), stop("Norm should be 1 or 2"))

  if(margin==2)
    apply(mat, MARGIN=margin, FUN=function(x) x/normfun(x))
  else
    t(apply(mat, MARGIN=margin, FUN=function(x) x/normfun(x)))
}

#' @export
normalize <- normalise

