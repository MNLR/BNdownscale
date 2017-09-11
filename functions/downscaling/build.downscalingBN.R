source("functions/downscaling/aux_functions/preprocess.forKmeans.R")
source("functions/downscaling/aux_functions/kmeanspp.R")

build.downscalingBN <- function(local, global, mode = 1, bnlearning.algorithm = hc, 
                                already.categorical = FALSE,
                                parallelize = FALSE, n.cores= NULL, cluster.type = "PSOCK",
                                output.marginals = TRUE,
                                clustering.args.list = list(k = 12, family = kccaFamily("kmeans")),
                                bnlearning.args.list = list(),
                                param.learning.method = "bayes") {
  # global   predictors, expects: a list of predictor datasets, a Multigrid from makeMultiGrid() or a single dataset
  #              It  is asumed that the data is consistent if a list is provided, and only the positions of first element will be used.
  #              2 coordinate postions are expected. 
  #              NaNs are not expected. POR COMPROBAR QUE PASA CON EL CLUSTERING
  # local    predictands
  #              NaNs will be processed
  # mode     1: Clustering will be performed for each global (predictor) node separately
  #          2: Clustering will be performed for each global (predictor) node separately. Arcs between nodes from
  #               the global will not be allowed
  #          3: Clustering will be performed for all the global nodes at the same time, condensing the "atmosphere status"
  #               in a single node which will be represented above the grid.
  # bnlearning.algorithm    Supports all the functions from bnlearn and hc.local2, tabu.local2. Check their corresponding parameters.
  # output.marginals        Compute and output Marginal Probability distribution Tables.
  
  if ( already.categorical ) {
    if (mode == 3) {stop("Select mode = 1 or mode = 2 for already categorical data.")}
    data <- preprocess(local, global, rm.na = TRUE , rm.na.mode = "observations" ) 
    Nglobals <- length(grep("G", colnames(data[[1]])))
    clusterS <- NULL
    clustering.attributes <- NULL 
  } 
  else {
    p.global <- preprocess.forKmeans(global, mode)
    if (mode == 3){ clustering.attributes <- list(attributes(p.global)$`scaled:center`, attributes(p.global)$`scaled:scale`) }
    else { clustering.attributes <- lapply(p.global, function(node) return(list(attributes(node)$`scaled:center`, attributes(node)$`scaled:scale`) ) )}

    print("Performing clustering...")
    if (mode == 3) {
      clustering.input <- append( list( x = p.global), clustering.args.list )
      clusterS <- do.call( kcca , clustering.input )
      global.data <- matrix(as.factor(predict(clusterS)), ncol = 1)
      print("Clustering done.")
    
      # It does not make sense for the first node (the only global) to have any restriction  if local learning is used
      if ( !(identical( bnlearning.algorithm ,hc) & !(identical( bnlearning.algorithm ,tabu) ) )){
        if ( !( is.null(bnlearning.args.list$exceptions) ) ){
          bnlearning.args.list$exceptions <- c(bnlearning.args.list$exceptions,  1 )
        }
        else {bnlearning.args.list[["exceptions"]] <- 1 }
      }
      xyCoords <- matrix(c(0,0), nrow = 1) # placeholder, will be set later
      rownames(xyCoords) <- "Atmosphere"
    }
    else if (mode == 1 | mode == 2){
      if ( parallelize ) {
        if ( is.null(n.cores) ){
          n.cores <- floor(detectCores()/2)
        }
        # Initiate cluster
        cl <- makeCluster(n.cores, type = cluster.type )
        if (cluster.type == "PSOCK") {
          clusterExport(cl, list("kcca") , envir = environment())
          clusterExport(cl, list( "p.global", "clustering.args.list" ) , envir = environment() )
        }
        clusterS <- parLapply(cl, p.global, function(node, cal) return( do.call(kcca, append( list( x = node), cal ) ) ), cal = clustering.args.list)
        stopCluster(cl)
      }
      else {
        #clusterS1 <- mapply(kcca, p.global, MoreArgs =   clustering.args.list, SIMPLIFY = FALSE)   #### equivalent to:
        clusterS <- lapply(p.global, function(node, cal) return( do.call(kcca, append( list( x = node), cal ) ) ), cal = clustering.args.list)
      }

      global.data <- sapply(clusterS, predict) # matrix of data where each column is a node with its "climate value" per observation
      global.data <- matrix(as.factor(global.data), ncol = NCOL(global.data))
      print("Done.")
    
      if ( !(is.null(attr(global$Data, "dimensions"))) ){ # MultiGrid or one global dataset
        xyCoords <- global$xyCoords
      }
      else{
        xyCoords <- global[[1]]$xyCoords
      }
    }
    else {stop("Invalid mode.")}
    data <- preprocess(local, list(Data = global.data, xyCoords = xyCoords), rm.na = TRUE , rm.na.mode = "observations" )
    Nglobals <- NCOL(global.data)
  }
  

  if (mode == 3) {     # The global node is positioned:
    data[[2]][ 1 , 1 ] <- mean( data[[2]][ 1 , -1 ] )
    yS <- data[[2]][ 2 , -1 ]
    y <- max( yS  ) + 3*abs( max( yS ) - min( yS ) )
    data[[2]][ 2 , 1 ] <- y
  }
  else if (mode == 2){ # arcs between global nodes are forbidden
    global.restrictions <- build.distanceBlacklist(names <- colnames(data[[1]][ , 1:Nglobals ]), 
                                                   positions = matrix(seq(1,Nglobals), nrow = 1), 
                                                   distance =  0.1 )
    if ( !( is.null(bnlearning.args.list$blacklist) ) ){
      bnlearning.args.list$blacklist <- rbind(bnlearning.args.list$blacklist,  global.restrictions)
    }
    else { bnlearning.args.list[["blacklist"]] <- global.restrictions }
  }
  
  bnlearning.args.list[["x"]] <- data[[1]]
  if ( (identical(  bnlearning.algorithm, hc.local2)) | (identical(  bnlearning.algorithm, tabu.local2)) ){
    bnlearning.args.list[["positions"]] <- data[[2]]
  }
  
  data[[1]][] <- lapply( data[[1]], as.factor) 

  print("Building Bayesian Network...")
  bn <- do.call(bnlearning.algorithm, bnlearning.args.list)
  bn.fit <- bn.fit(bn, data = data[[1]], method = param.learning.method )
  print("Done.")

  if (output.marginals){
    print("Building Marginal Distributions...")
    marginals_ <- marginals( list(BN = bn, BN.fit = bn.fit, clusterS = clusterS) )
    print("Done.")
  }
  else {marginals_ <- NULL}
  
  return( list(BN = bn, training.data = data[[1]], positions = data[[2]], BN.fit = bn.fit, 
               clusterS = clusterS, 
               scale.args = clustering.attributes, 
               mode = mode,
               Nglobals = Nglobals,
               marginals = marginals_,
               bnlearning.algorithm = bnlearning.algorithm,
               clustering.args.list = clustering.args.list,
               bnlearning.args.list = bnlearning.args.list,
               param.learning.method = param.learning.method)    )
}


