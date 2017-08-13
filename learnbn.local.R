learning.complement <- function(nodes.centre, nodes, positions, distance, norm_) {
  
  distances <- apply(sweep(positions, 2, positions[nodes.centre, ]), 1, norm, type = norm_)
  toS <- nodes[distances > distance]
  fromS <- array(nodes[nodes.centre], length(toS))

  return(matrix(c(fromS, toS), ncol=2 ) )
}

learnbn.local <- function(x, positions, distance, norm = "2", learning.algorithm = "hc" , start = NULL, whitelist = NULL, blacklist = NULL, score = NULL, ...,
                          debug = FALSE, restart = 0, perturb = 1, max.iter = Inf, maxp = Inf, optimized = TRUE ){
  # x           a data frame containing the variables of the model
  # positions   sorted array of the locations of x, can be N dimensional, must contain the same number of rows as the number of variables in x, same number of cols as N
  # distance    maximum distance at which nodes will be checked for a directed arc
  # norm        norm for distance. format: same as norm(): "2" euclidean, ... . May be a function in the form   (...)
  # learning.algorithm    POR IMPLEMENTAR
  
  nodes  <- colnames(x)
  
  nodes.centreS <- seq(1,length(nodes))
  for.the.blacklist <- lapply(X =  nodes.centreS , FUN =  learning.complement,  nodes = nodes, positions=positions, distance=distance, norm=norm)
  for.the.blacklist <- do.call("rbind", for.the.blacklist) 
  
  if (is.null(blacklist) ) {  blacklist <- matrix(nrow = 0, ncol = 2, byrow = TRUE, dimnames = list(NULL, c("from", "to")))}
  blacklist <- rbind(blacklist, for.the.blacklist)

  return( hc(x = x, start = start, whitelist = whitelist, blacklist = blacklist, score = score, debug = debug, restart = restart, perturb = perturb, max.iter = max.iter, maxp = maxp, optimized = optimized) )
}