
learning.complement2 <- function(nodes, positions, distance, norm_) {
  
  distances <- apply(sweep(positions, 1, positions[ ,1]), 2, norm, type = norm_)
  toS <- nodes[distances > distance]
  fromS <- array(nodes[1], length(toS))
  
  return(matrix(c(fromS, toS, toS, fromS), ncol=2 ) )
}


hc.local2 <- function(x, positions, distance, norm = "2", plotrestrictions = FALSE, start = NULL, whitelist = NULL, blacklist = NULL, score = NULL, ...,
                      debug = FALSE, restart = 0, perturb = 1, max.iter = Inf, maxp = Inf, optimized = TRUE ){
  # 
  # Learns the structure of a Bayesian network using a hill-climbing (HC) algorithm and restricts the search for directed arcs to nodes that are closer than the distance specified generating
  # a blacklist argument for hc() function. The blacklist can be checked using output learnt.local$learning$blacklist.
  # 
  #  ---- INPUT:
  # x                 a data frame containing the variables of the model
  # positions         sorted array of the locations of x, can be N dimensional, must contain the same number of columns as the number of variables in x, and number of rows is the dimension.
  #                   Won't check column names, positions must be sorted the same way as the columns (Variables) of x
  # distance          maximum distance at which nodes will be checked for a directed arc
  # norm              norm for distance. format: same as norm(), check ?norm: "2" euclidean, ... . May be a function in the form   (...)
  # plotrestrictions  A plot with node restrictions will be displayed. It only makes sense if the norm is set to "2". It will be ignored if NROW(positions) > 2 
  #  
  #  ---- OUTPUT:
  #             object of class bn from bnlearn library.
  
  names <- colnames(x)
  try(if(length(names) != NCOL(positions)) stop("Number of positions is not equal to number of nodes."))
  
  if (plotrestrictions &  NROW(positions) <= 2 ) {
    plot.graphrestrictions(nodes, positions, distance )
  }
  
  names.list <- list()
  positions.list <- list()
  
  for (i in 1:(NCOL(x)-1)) {
    names.list[[i]] <- names
    names <- names[2:length(names)]
    positions.list[[i]] <- positions
    positions <- positions[ , 2:NCOL(positions)]
  }
  
  for.the.blacklist <- mapply(FUN =  learning.complement2 , nodes = names.list , positions = positions.list,  MoreArgs=list(distance = distance, norm_ = norm) )
  for.the.blacklist <- do.call("rbind", for.the.blacklist) 
  
  if (is.null(blacklist) ) {  blacklist <- matrix(nrow = 0, ncol = 2, byrow = TRUE, dimnames = list(NULL, c("from", "to")))}
  
  blacklist <- rbind(blacklist, for.the.blacklist)
  
  return( hc(x = x, start = start, whitelist = whitelist, blacklist = blacklist, score = score, ... , debug = debug, restart = restart, perturb = perturb, max.iter = max.iter, maxp = maxp, optimized = optimized) )
}