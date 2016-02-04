########################################
# Author: Antoine Sachet
#
# Function to load a parallel backend
#
########################################

load_parallel <- function(nbClust = 2) {
  if(!require("doParallel")) {
    stop("Please install 'doParallel' or similar parallel back-end prior to running load_parallel.")
  }
  
  .compClust <- NULL
  if(exists(".compClust")) 
    try(stopCluster(.compClust), silent = T)
  
  compClust <- makeCluster(nbClust, rscript_args = c("--vanilla"))
  
  registerDoParallel(cl=compClust)
  message(paste0("A parallel back-end of ", nbClust, " clusters was created, 
          remember to stop it afterward with stopCluster(compClust)"))
  
  invisible(compClust)
}

