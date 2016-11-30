

##' Convenience function to plot a histogram with a pipe-compatible interface.
##' @param df A data.frame contaning the data
##' @param var A string naming the variable to plot
##' @param main Title of the plot
##' @param xlab Label of the x-axis
##' @param ... Arguments passed to hist()
##' @examples
##'  mtcars %>% plot_hist("mpg")
##' @export
plot_hist <- function(df, var, main=paste("Histogram of", var), xlab=var, ...) {
  hist(df[[var]], main=main, xlab=xlab, ...)
}


#' Custom color scales for ggplot2
#'
#' @description A collection of color/fill scales for ggplot2.
#' @name color_scales
NULL

#' @importFrom RColorBrewer brewer.pal
#' @importFrom ggplot2 scale_color_gradientn scale_fill_gradientn
#' @describeIn color_scales  A 'color' scale highlighting extreme values.
#' @export
scale_color_extreme <- function(...){
  scale_color_gradientn(colours=c(brewer.pal(n=9, "RdYlBu")[-(5:8)], "turquoise2"), ...)
}

#' @describeIn color_scales  A 'fill' scale highlighting extreme values.
#' @importFrom RColorBrewer brewer.pal
#' @export
scale_fill_extreme <- function(...){
  scale_fill_gradientn(colours=c(brewer.pal(n=9, "RdYlBu")[-(5:8)], "turquoise2"), ...)
}

#' Rotate axis text of a ggplot
#'
#' Adapted from http://stackoverflow.com/a/36979201/3498910.
#'
#' @param angle Angle to apply, in degrees
#' @param position One of "x" or "y". Defaults to "x".
#' @param hjust,vjust Horizontal and vertical adjustement.
#'
#' @importFrom ggplot2 element_text theme
#' @examples
#' library("ggplot2")
#' df <- data.frame(x=.5, y=.5)
#'
#' ggplot(df, aes(x=x, y=y)) + geom_point() +
#'     rotate_axis_text(60)
#' @export
rotate_axis_text <- function(angle, position="x", hjust="auto", vjust="auto") {

  angle     = angle[1];
  position  = position[1]
  positions = list(x=0,y=90)
  if(!position %in% names(positions))
    stop(sprintf("'position' must be one of [%s]",paste(names(positions),collapse=", ")))
  if(!is.numeric(angle))
    stop("'angle' must be numeric")
  rads  = (-angle - positions[[ position ]])*pi/180
  if(hjust=="auto")
    hjust = 0.5*(1 - sin(rads))
  if(vjust=="auto")
    vjust = 0.5*(1 + cos(rads))

  if(position=="x") {
    theme(axis.text.x = element_text(angle=angle,
                                     hjust=hjust, vjust=vjust))
  } else{
    theme(axis.text.y = element_text(angle=angle,
                                     hjust=hjust, vjust=vjust))
  }

}
