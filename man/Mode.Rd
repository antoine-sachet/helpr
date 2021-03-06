% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Mode.R
\name{Mode}
\alias{Mode}
\title{Compute the mode of a vector}
\source{
http://stackoverflow.com/a/8189441/3498910
}
\usage{
Mode(x, na.rm = F)
}
\arguments{
\item{x}{Vector of which to compute the mode}

\item{na.rm}{Whether to ignore NA's}
}
\value{
Most frequent value occuring in the vector
}
\description{
Compute the mode of a vector
}
\details{
If there are several modes, the one occuring first in the data will be returned.
No binning is attempted for numeric vectors.
}
\examples{
# x will contain lots of 'a'
x <- sample(letters, prob=c(10, rep(1, 25)), size=100, replace=T)
Mode(x) # will be 'a' (with probability 1-2.4e-15)

x <- rnorm(10)
Mode(x)

}
\author{
Ken Williams
}
