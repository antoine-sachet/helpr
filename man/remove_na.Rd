% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/remove_na.R
\name{remove_na}
\alias{remove_na}
\title{Remove rows NAs from selected columns}
\usage{
remove_na(df, ...)
}
\arguments{
\item{df}{Data.frame to process}

\item{...}{Column selection passed to `dplyr::select`}
}
\description{
Remove rows having NA in one of the selected columns. Suitable for piping. 
Compatible with dply's select helpers.
}
\examples{

df <- data.frame(a = 1:10, b = c(1:3, na, 1:6))
remove_na()

}
\seealso{
keep_na, keep_na_rows
}
