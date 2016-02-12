

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
  hist(df[[var]], main=main, xlab=xlab)
}
