########################
# Author: Antoine Sachet
#
# Simple wrapper around
# txtProgressBar()
########################

#' Progress bar facility
#'
#' @name progress_bar
#'
#' @description Straightforward progress bar based on `textProgressBar`, 
#' suitable for use with a linear `for` or `foreach` loop.
#'
#' @examples
#' \dontrun{
#' reset_progress_bar(big_vec)
#' foreach(vec=big_vec) %do% {
#'   # ...
#'   update_progress_bar()
#'   }
#' }
NULL

#' @describeIn progress_bar Prepare the progress bar to loop over some data
#' @importFrom utils txtProgressBar
#' @export
reset_progress_bar <- function(loop_data, title=NULL, by="", ...) {
  .progress_bar_counter <<- 0

  if (!is.null(title)) {
    message(title, appendLF = T)
  }

  if (is.data.frame(loop_data) | is.matrix(loop_data)) {
    # if we loop over the rows or cols of a data.frame/matrix
    max <- switch(by, row = nrow(loop_data), col = ncol(loop_data), nrow(loop_data))
  } else if (is.numeric(loop_data) & length(loop_data) == 1) {
    # if the correct max has directly been provided
    max <- loop_data
  } else {
    # more common other cases
    max <- length(loop_data)
  }

  .progress_bar <<- utils::txtProgressBar(min = 0, max = max, style = 3,
                                   title = title,
                                   width = getOption("width")/2, ...)
}

#' @describeIn progress_bar Update the progress bar value
#' @importFrom utils setTxtProgressBar
#' @export
update_progress_bar <- function(value=NULL, ...) {
  if (!exists(".progress_bar") | !exists(".progress_bar_counter")) {
    stop("Please call reset_progress_bar before calling update_progress_bar")
  } else {
    if (is.null(value)) {
      .progress_bar_counter <<- .progress_bar_counter + 1
      utils::setTxtProgressBar(.progress_bar, value = .progress_bar_counter, ...)
    } else {
      utils::setTxtProgressBar(.progress_bar, value = value)
    }
  }
}
