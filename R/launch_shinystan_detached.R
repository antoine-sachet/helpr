#' Launch shinystan as a detached R session
#'
#' @param object The object to pass to launch_shinystan, typically a shinystan or stanfit object.  
#' @param port Port that shinystan will bind. Uses the "shiny.port" option if set and defaults to 8080 otherwise.
#' @param quiet If TRUE, do not print anything.
#' 
#' @note Requires `shinystan` although it is not an explicit dependency.
#' 
#' @details 
#' The `object` is first compressed and written to disk in a temporary file (see `tempfile`). 
#' A separate R session is started (using `system`) where the object is read and `launch_shinystan` is called.
#' When shinystan is closed, the temporary file is deleted.
#' 
#' @seealso shinystan::launch_shinystan
#' 
#' @importFrom  glue glue
#' @importFrom readr write_rds
#' @export
launch_shinystan_detached <- function(object, 
                                      port=getOption("shiny.port", 8080),
                                      quiet=FALSE) {
  tmpfi <- tempfile()
  write_rds(object, tmpfi)
  if(!quiet) message(glue("Starting shinystan on http://localhost:{port}"))
  r_cmd <- glue(
    "library(shinystan);",
    "stanfit <- readr::read_rds('{tmpfi}') ;",
    "launch_shinystan(stanfit, rstudio=T, port={port}) ;",
    "file.remove('{tmpfi}')")
  sys_cmd <- glue('r -e "{r_cmd}"')
  system(sys_cmd, intern = F, ignore.stdout = T, ignore.stderr = T, wait = F)
}

