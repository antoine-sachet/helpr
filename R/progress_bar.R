########################
# Author: Antoine Sachet
#
# Simple wrapper around
# txtProgressBar()
########################

# ## Typical use:
# reset_progress_bar(big_vec)
# foreach(vec=big_vec) %do% {
#   # ...
#   update_progress_bar()
# }

##' @export
reset_progress_bar <- function(loop_data, title=NULL, by="", ...) {
  .progress_bar_counter <<- 0

  if(!is.null(title)) {
    message(title, appendLF = T)
  }

  if(is.data.frame(loop_data) | is.matrix(loop_data)) {
    # if we loop over the rows or cols of a data.frame/matrix
    max <- switch(by, row=nrow(loop_data), col=ncol(loop_data), nrow(loop_data))
  } else if (is.numeric(loop_data)&length(loop_data)==1){
    # if the correct max has directly been provided
    max <- loop_data
  } else {
    # more common other cases
    max <- length(loop_data)
  }

  .progress_bar <<- txtProgressBar(min = 0, max = max, style = 3,
                                   title=title,
                                   width = getOption("width")/2, ...)
}

##' @export
update_progress_bar <- function(value=NULL, ...) {
  if(!exists(".progress_bar")|!exists(".progress_bar_counter")){
    warning("Please call reset_progress_bar prior to calling update_progress_bar")
  } else {
    if(is.null(value)) {
      .progress_bar_counter <<- .progress_bar_counter + 1
      setTxtProgressBar(.progress_bar, value = .progress_bar_counter, ...)
    } else {
      setTxtProgressBar(.progress_bar, value = value)
    }
  }
}
