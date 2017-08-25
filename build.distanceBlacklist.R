build.distanceBlacklist <- function(names, positions, distance, norm) {
  
  names.list <- list()
  positions.list <- list()
  Nnodes <- length(names)
  
  for (i in 1:(Nnodes - 1) ) {
    names.list[[i]] <- names
    names <- names[2:length(names)]
    positions.list[[i]] <- positions
    positions <- positions[ , 2:NCOL(positions)]
  }

  distanceBlacklist <- mapply(FUN =  learning.complement2 , nodes = names.list , positions = positions.list,  MoreArgs=list(distance = distance, norm_ = norm) )
  distanceBlacklist <- do.call("rbind", distanceBlacklist) 

  return(distanceBlacklist)
}