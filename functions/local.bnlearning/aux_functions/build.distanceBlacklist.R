source("functions/local.bnlearning/aux_functions/learning.complement2.R")

build.distanceBlacklist <- function(names, positions, distance, norm = "2") {
  
  names.list <- list()
  positions.list <- list()
  Nnodes <- length(names)
  
  nrow_ <- NROW(positions)
  for (i in 1:(Nnodes - 1) ) {
    names.list[[i]] <- names
    names <- names[2:length(names)]
    positions.list[[i]] <- positions
    positions <- matrix(positions[ , 2:NCOL(positions)], nrow = nrow_) # matrix needs to be preserved
  }

  distanceBlacklist <- mapply(FUN =  learning.complement2 , nodes = names.list , positions = positions.list,  MoreArgs=list(distance = distance, norm_ = norm) )
  distanceBlacklist <- do.call("rbind", distanceBlacklist) 

  return(distanceBlacklist)
}