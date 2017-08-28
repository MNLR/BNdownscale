build.downscalingBN <- function(local, global , mode = 1, bnlearning.algorithm = hc, 
                              clustering.args.list = list(k = 100, family = kccaFamily("kmeans")), bnlearning.args.list = list() ) {
  # global   predictors, expects a list of predictors even if there is only one predictor
  #              It  is asumed that the data is consistent, only the positions of first element will be used.
  #              2 coordinate postions are expected. 
  #              NaNs are not expected. POR COMPROBAR QUE PASA CON EL CLUSTERING
  #              ADAPTAR A makeMultiGrid()
  # local    predictands
  #              NaNs will be processed
  # mode     1: Clustering will be performed for each global (predictor) node separately
  #          2: Clustering will be performed for each global (predictor) node separately. Arcs between nodes from
  #               the global will not be allowed
  #          3: Clustering will be performed for all the global nodes at the same time, condensing the "atmosphere status"
  #               in a single node which will be represented above the grid.
  # bnlearning.algorithm    hc, hc.local2, tabu, tabu.local2. Check their corresponding parameters.
  
  p.global <- preprocess.forKmeans(global, mode)
  
  if (mode == 3) {
    clustering.input <- append( list( x = p.global), clustering.args.list )
    print("Performing clustering...")
    clusterS <- do.call( kcca , clustering.input )
    global.data <- matrix(as.factor(predict(clusterS)), ncol = 1)
    print("Clustering done.")
    
    print(global.data)

    # It does not make sense for the first node (the only global) to have any restriction  if local learning is used
    if (!(identical( bnlearning.algorithm ,hc))){
      if ( !( is.null(bnlearning.args.list$exceptions) ) ){
        bnlearning.args.list$exceptions <- c(bnlearning.args.list$exceptions,  1 )
      }
      else {bnlearning.args.list[["exceptions"]] <- 1 }
    }
    xyCoords <- matrix(c(0,0), nrow = 1) # placeholder, will be set later
    rownames(xyCoords) <- "Atmosphere"
  }
  else if (mode == 1 | mode == 2){
    print("Performing clustering...")
    clusterS <- mapply(kcca, p.global, MoreArgs =   clustering.args.list, SIMPLIFY = FALSE)
    
    global.data <- sapply(clusterS, predict) # matrix of data where each column is a node with its "climate value" per observation
    print("Clustering done.")
    print(global.data)
    global.data <- matrix(as.factor(global.data), ncol = NCOL(global.data))
    print(global.data)
    
    xyCoords <- global[[1]]$xyCoords
  }
  else {stop("Invalid mode.")}
  
  data <- preprocess(local, list(Data = global.data, xyCoords = xyCoords)  )

  if (mode == 3) {     # The global node is positioned:
    data[[2]][ 1 , 1 ] <- mean( data[[2]][ 1 , -1 ] )
    yS <- data[[2]][ 2 , -1 ]
    
    y <- max( yS  ) + 3*abs( max( yS ) - min( yS ) )
    data[[2]][ 2 , 1 ] <- y
  }
  else if (mode == 2){ # arcs between global nodes are forbidden
    Nglobals <- NCOL(global.data)
    global.restrictions <- build.distanceBlacklist(names <- colnames(data[[1]][ , 1:Nglobals ]), 
                                                   positions = matrix(seq(1,Nglobals), nrow = 1), 
                                                   distance =  0.1 )
    if ( !( is.null(bnlearning.args.list$blacklist) ) ){
      bnlearning.args.list$blacklist <- rbind(bnlearning.args.list$blacklist,  global.restrictions)
    }
    else { bnlearning.args.list[["blacklist"]] <- global.restrictions }
  }
  
  bnlearning.args.list[["x"]] <- data[[1]]
  if (!(identical(  bnlearning.algorithm  ,hc))){
    bnlearning.args.list[["positions"]] <- data[[2]]
  }
  print(bnlearning.args.list)
  
  print("Building Bayesian Network...")
  bn <- do.call(bnlearning.algorithm, bnlearning.args.list)
  print("Done.")
  
  return( list(BN = bn, positions = data[[2]], clusterS = clusterS, mode = mode) ) 
}


