% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shorthands.R
\name{fp}
\alias{fp}
\alias{hfp}
\title{Full print of data.frame}
\usage{
fp(x, ..., digits = NULL, quote = FALSE, right = TRUE,
  row.names = TRUE, max = NULL)

hfp(x, ...)
}
\description{
Shorthand for `print.data.frame`, useful for occasional full printing of a dplyr `tbl`.
}
\section{Functions}{
\itemize{
\item \code{hfp}: Wrapper around head and fp. `...` is passed to head.
}}

\seealso{
print.data.frame hfp
}
