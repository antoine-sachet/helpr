########################################
# Author: Antoine Sachet
#
# Function to load a parallel backend
#
########################################

#' Loads a parallel back-end
#'
#' Builds a local cluster of nbClust nodes and registers it as parallel backend.
#' Will also try to stop any cluster previously created by the function.
#'
#' @param nbClust Number of nodes to create in the cluster.
#' @export
load_parallel <- function(nbClust = 2, verbose=T) {
  if(!requireNameSpace("doParallel")) {
    stop("Please install 'doParallel' or similar parallel back-end prior to running load_parallel.")
  }

  .compClust <- NULL
  if(exists(".compClust"))
    try(stopCluster(.compClust), silent = T)

  compClust <- makeCluster(nbClust, rscript_args = c("--vanilla"))

  registerDoParallel(cl=compClust)
  if(verbose)
    message(paste0("A parallel back-end of ", nbClust, " clusters was created,
              remember to stop it afterward with stopCluster(compClust)"))

  invisible(compClust)
}

