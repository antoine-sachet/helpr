
##' Add 'export tag to all functions
##'
##' Adds roxygen @export to all functions in R files in the specified directory.
##' (Except those indented, which are assumed to be defined within a function.)
##' It does nothing to already exported functions, as long as the export statement is just above the function defition.
##' @param dir target directory
##' @param verbose If TRUE, function newly exported are outputed (cat).
##' @export
roxygen_export_all <- function(dir, verbose=T) {
  c <- 0
  for( file in list.files(dir, full.names=TRUE) ) {
    doc <- readLines( file )
    c_doc <- 0
    for( i in seq_along(doc) ) {
      if(grepl(x=doc[i], pattern="^[^ ].+<- ?function ?\\(") ) {
        if(i==1 || !grepl(x=doc[i-1], pattern="@export")) {
          cat(doc[i], "\n")
          doc[i] <- paste( "##' @export", doc[i], sep="\n" )
          c_doc <- c_doc +1
        }
      }
    }
    message(c_doc, " new functions were exported from ", file)
    c <- c + c_doc
    writeLines( doc, file )
  }
  message(c, " new functions were exported in total")
}
