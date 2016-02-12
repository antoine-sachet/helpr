# Credit to:
# - holstius: https://gist.github.com/holstius/cbc4ec40057fbc2d9f4b for "patch".


##' @title
##' Conditional patching of data.frames
##' @author Antoine Sachet
##' @description
##' Performs one or several 'mutate' on a data.frame, but only on rows matching the specified condition(s).
##'
##' @param object Data.frame to patch
##' @param cond The condition(s) selecting the rows to patch. Should be wrapped in "where".
##' @param ... Action(s) to take, in the form `var=new_value`.
##' @param quiet If FALSE, a message indicating number of patched rows will be printed.
##'
##' @examples
##' patch(mtcars, where(vs == 0, am == 1), gear = Inf, carb = carb + 10)
##'
##' # Setting incorrect values in data$pos_var to NA
##' # Setting corresponding related_var observations to NA
##' data %>% patch(where(pos_var<0), pos_var=NA, related_var=NA)
##' @export
patch <- function(object, cond, ...){
  UseMethod("patch")
}

##' @rdname patch
##' @method patch data.frame
##' @export
patch.data.frame <- function(object, cond, ..., quiet = FALSE) {

  # Rows to be patched
  masks <- lazyeval::lazy_eval(cond, object)
  i <- which(apply(do.call(cbind, masks), 1, all))  # rows to be patched
  if (length(i) == 0) {
    if(!quiet) message("Nothing patched")
    return(object)
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

##' @rdname patch
##' @export
where <- lazyeval::lazy_dots
