# Credit for these go to jm
#
# The colorChart function makes a plot showing colors that are available in R and the correspondence between these colors and number codes for the colors.
#
# The colorChart function adopts code that is due to Earl F. Glynn, Stowers Institute for Medical Research.  His code has been modified in minor ways and it has been combined into the colorChart function.  See http://research.stowers-institute.org/efg/R/Color/Chart/index.htm
# --------------------------------------------------------------------------------------
#
#   ARGUMENT:
#
#   orderBy:  If orderBy = "number" (default), the colors are ordered by their code number; if orderBy = "hue", the colors are ordered by hue, saturation and value.
  # This function is due to Earl F. Glynn, Stowers Institute for Medical Research.
  # http://research.stowers-institute.org/efg/R/Color/Chart/index.htm
  # His code has been modified in minor ways.
  # -----------------------------------------------------------------------------

#' Show all colors of base R in a concise table.
#'
#' @seealso color_chart
#' @export
color_chart <- function(orderBy = c("number", "hue"))  {
  if ( length(orderBy) > 1 ) oBy = tolower(orderBy[1]) else oBy = tolower(orderBy)

  lnOBy = nchar(oBy)

  if ( oBy == substr( "number", 1, lnOBy ) ) oBy = "number"
  if ( oBy == substr( "hue", 1, lnOBy ) ) oBy = "hue"

  # Error if orderBy is not recognized.
  if ( oBy != "number" & oBy != "hue" ) stop(
    "\nThe orderBy argument was not set to a recognizable value.",
    "\n\norderBy was inputted as:  ",
    paste(orderBy, collapse = ", ")
  ) #end stop

  # The SetTextContrastColor is used later in this function.
  SetTextContrastColor <- function(color) {
    ifelse( mean(col2rgb(color)) > 127, "black", "white")
  }

  #Define this array of text contrast colors that correponds to each
  #member of the colors() array.
  TextContrastColor <- unlist( lapply(colors(), SetTextContrastColor) )

  if (oBy == "number") {

    # Create chart of colors ordered by color code number.
    #1a. Plot matrix of R colors, in index order, 25 per row.
    #This example plots each row of rectangles one at a time.
    colCount <- 25 # number per row
    rowCount <- 27
    plot( c(1,colCount), c(0,rowCount), type="n", ylab="", xlab="",
          axes=FALSE, ylim=c(rowCount,0))

    # The title command has been altered from Glynn's original code.
    title("R Colors Ordered by Code Number")

    for (j in 0:(rowCount-1)) {
      base <- j*colCount
      remaining <- length(colors()) - base
      RowSize <- ifelse(remaining < colCount, remaining, colCount)
      rect((1:RowSize)-0.5,j-0.5, (1:RowSize)+0.5,j+0.5,
           border="black",
           col=colors()[base + (1:RowSize)])
      text((1:RowSize), j, paste(base + (1:RowSize)), cex=0.7,
           col=TextContrastColor[base + (1:RowSize)])
    }

  } #end 'if (oBy == "number")'

  if (oBy == "hue") {

    # 1b. Plot matrix of R colors, in "hue" order, 25 per row.
    # This example plots each rectangle one at a time.
    RGBColors <- col2rgb(colors()[1:length(colors())])
    HSVColors <- rgb2hsv( RGBColors[1,], RGBColors[2,], RGBColors[3,],
                          maxColorValue=255)
    HueOrder <- order( HSVColors[1,], HSVColors[2,], HSVColors[3,] )
    colCount <- 25 # number per row
    rowCount <- 27

    plot(0, type="n", ylab="", xlab="",
         axes=FALSE, ylim=c(rowCount,0), xlim=c(1,colCount))
    title("R colors -- Sorted by Hue, Saturation, Value")
    for (j in 0:(rowCount-1)) {
      for (i in 1:colCount) {
        k <- j*colCount + i
        if (k <= length(colors())) {
          rect(i-0.5,j-0.5, i+0.5,j+0.5,
               border="black", col=colors()[ HueOrder[k] ])
          text(i,j, paste(HueOrder[k]), cex=0.7,
               col=TextContrastColor[ HueOrder[k] ])
        }
      }
    }

  } #end 'if (oBy == "hue")'
}


#' Display the codes for colors in R
#'
#' Adapted from http://research.stowers-institute.org/efg/R/Color/Chart/index.htm.
#'
#' @param start.num the number of the first color to be displayed.  The  default setting displays the colors in 60 color increments.
#' @param Labels T if color labels are desired; F to omit.
#' @param num.row the number of rows in the color display.
#' @param num.col the number of columns in the color display.
#' @seealso color_chart
#' @export
show_colors <- function (start.num = 1, Labels = T, num.row = 15, num.col = 4)
{
  top <- 97
  tmpar <- par(mar = c(0, 0, 1.5, 0), pch = 16)
  plot.jm <- function(x = c(0, 25), y = c(0, 100), ...) {
    plot(x, y, xlab = "", ylab = "", type = "n", ...)
  }
  plot.jm(axes = F)
  if (!Labels & (num.row == 15) & (num.col == 4)) {
    num.row <- 26
    num.col <- 13
  }
  for (k in 1:num.col) for (i in 1:num.row) {
    points((k - 1) * (23/num.col) + 2, top - (i - 1) * (top/num.row),
           cex = 2, col = colors()[start.num + (i - 1) + (k -
                                                            1) * num.row])
    eps1 <- 0.5
    text(eps1 + (k - 1) * (23/num.col) + 2, top - (i - 1) *
           (top/num.row), start.num + (i - 1) + (k - 1) * num.row,
         cex = 0.75, adj = 0)
    mtext(paste("Colors", start.num, "-", start.num + num.row *
                  num.col - 1, "out of", 657), side = 3, cex = 1.5,
          line = 0)
    if (Labels) {
      eps2 <- 1.6
      text(eps2 + (k - 1) * (23/num.col) + 2, top - (i -
                                                       1) * (top/num.row), colors()[start.num + (i -
                                                                                                   1) + (k - 1) * num.row], cex = 0.75, adj = 0)
    }
  }
  par(tmpar)
}
