
#' Capitalise a string
#'
#' Taken from the Hmisc package.
#'
#' @export
capitalize <- function (string) {
  capped <- grep("^[^A-Z]*$", string, perl = TRUE)
  substr(string[capped], 1, 1) <- toupper(substr(string[capped], 1, 1))
  string
}

#' Turn dots and underscores to spaces in a string
#' @export
point2space <- function(chr) {
  out <- gsub(".", " ", chr, fixed=T)
  out <- gsub("_", " ", out, fixed=T)
  out
}

#' Turn camel case into regular spaced words
#' @importFrom stringi stri_replace_all
#' @export
split_camel <- function(str) {
  stri_replace_all(str, regex="(?<=[a-z])([A-Z])", replacement=" $1")
}

#' Format the column names of a data.frame for pretty printing
#'
#' @export
pretty_colnames <- function(df) {
  `colnames<-`(df,
               capitalize(
                 point2space(
                   colnames(df))))
}

#' Format a number as a percentage
#'
#' @examples
#'
#' tmp <- runif(5)
#' as.percentage(tmp)
#'
#' mtcars %>%
#'    count(gear) %>%
#'    mutate(prop=as.percentage(n/sum(n)))
#'
#' @export
as.percentage <- function(num, digits=1) {
  paste0(round(num*100, digits = digits), "%")
}

#' @describeIn as.percentage Shorthand
#' @export
as.perc <- as.percentage
