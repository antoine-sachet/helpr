# Credit to:
# - holstius: https://gist.github.com/holstius/cbc4ec40057fbc2d9f4b for "patch".

patch <- function(object, cond, ...) UseMethod("patch")

# Toy example
# if (interactive())
#  patch(mtcars, where(vs == 0, am == 1), gear = Inf, carb = carb + 10)

patch.data.frame <- function(object, cond, ..., quiet = FALSE) {

  # Rows to be patched
  masks <- lazyeval::lazy_eval(cond, object)
  i <- which(apply(do.call(cbind, masks), 1, all))  # rows to be patched
  if (length(i) == 0) {
    warning("conditions are not all TRUE for any rows: nothing patched")
  } else {
    if (!quiet) message("Patching ", length(i), " rows")
  }

  # Columns to be patched
  dots <- lazyeval::lazy_dots(...)
  j <- match(names(dots), names(object))
  if (length(j) == 0) warning("no common names: nothing patched")

  x <- lazyeval::lazy_eval(dots, data = object[i, ])  # replacement values
  object[i, j] <- data.frame(i, x)[, -1]  # use `i` to force identical shape
  return(object)
}

# for use with patch
where <- lazyeval::lazy_dots
