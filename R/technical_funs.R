########################################
# Author: Antoine Sachet
# Date: 09/15
#
# Misc functions to simplify a technical operation
########################################

# vectorized switch operation
# allow to have a vector as switch EXPR
# Syntax:
# vswitch(sample(letters, 5), a="first", b="second", ">3")
vswitch <- function(EXPR, ...) {
  sapply(EXPR, FUN=function(x) switch(x, ...))
}

# Easily rename one or more of the existing levels of fac
# Syntax:
# rename_levels(fac, old="PC", new="computer")
# rename_levels(fac, old=c("PC", "laptop), new=c("computer", "computer"))
# rename_levels(fac, old=c("PC", "TV"), new=c("computer", "television"))
# Note: will *not* mess up the "computer" level if it already exists
function(fac, old, new) {
  n <- length(old)
  if(length(new) != n & length(new)!=1) {
    stop("Length of new levels vector must be 1 or the length of the old levels vector.")
  } else if(length(new)==1) {
    new = rep(new, n)
  }
  lev <- levels(fac)
  lev.list <- `names<-`(as.list(lev), lev)
  lev.list[old] <- NULL
  for(i in 1:n) {
    lev.list[[new[i]]] <- c(lev.list[[new[i]]], old[i])
  }

  `levels<-`(fac, lev.list)
}

# simple shorthand
`%!in%` <- function(x, y) {
  !(x %in% y)
}
