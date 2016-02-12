##' @export
capitalize <- function (string)
{
  capped <- grep("^[^A-Z]*$", string, perl = TRUE)
  substr(string[capped], 1, 1) <- toupper(substr(string[capped], 1, 1))
  return(string)
}

##' @export
point2space <- function(chr) {
  out <- gsub(".", " ", chr, fixed=T)
  out <- gsub("_", " ", out, fixed=T)
  out
}

##' @export
pretty_colnames <- function(df) {
  `colnames<-`(df,
               capitalize(
                 point2space(
                   colnames(df))))
}

##' @export
as.percentage <- function(num, digits=1) {
  paste0(round(num*100, digits = digits), "%")
}

as.perc <- as.percentage
