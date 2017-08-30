downscale.BN <- function(downscale.BN, global, as.matrix = FALSE, parallelize = FALSE, n.cores = NULL , cluster.type = "PSOCK"){

  # Parallelize = TRUE should help a lot when lots of evidences are provided.
  # as.matrix = TRUE requires predictand nodes to have the same categories
  # cluster.type   Accepts "PSOCK" and "FORK". "FORK" cannot be used in Windows systems.
  # as.matrix     Set to TRUE for the output to be converted to a matrix where dimensions are [obs, cat, node]
  
  BN <- downscale.BN$BN
  BN.fit <- downscale.BN$BN.fit
  clusterS <- downscale.BN$clusterS
  mode <- downscale.BN$mode
  Nglobal <- length(clusterS)
  predictors <- names(BN$nodes)[1:Nglobal]
  predictands <- names(BN$nodes)[- (1:Nglobal) ]

  junction <- compile( as.grain(BN.fit) )
  
  p.global <- preprocess.forKmeans(global, mode)
  
  if ( parallelize == TRUE) {
    if ( is.null(n.cores) ){
      n.cores <- floor(detectCores()/2)
    }
    # Initiate cluster
    cl <- makeCluster(n.cores, type = cluster.type )
    if (cluster.type == "PSOCK") {
      clusterExport(cl, list("setEvidence", "querygrain" , "predict.DBN") , envir = environment())
      clusterExport(cl, list( "junction", "predictors" , "predictands") , envir = environment() )
    }
    if (mode == 3) {
      clustered <- as.factor( predict(clusterS, newdata = p.global) )
      if (cluster.type == "PSOCK") {
        clusterExport(cl, "clustered" , envir = environment() )
      }
      print(clustered)
      print(length(clustered))
      PT <- parLapply(cl , clustered, fun =  predict.DBN , predictors = predictors, junction = junction , predictands = predictands )
    }
    else if (mode == 1 | mode == 2){
      clustered <- mapply(predict , object = clusterS, newdata = p.global ,  SIMPLIFY = TRUE  ) # matrix of data where each column is a node with its "climate value" per observation
      clustered <- matrix(as.factor(clustered), ncol = NCOL(clustered))  # reconverted to categorical

      if (cluster.type == "PSOCK") {
        clusterExport(cl, "clustered" , envir = environment() )
      }
      PT <- parApply(cl , clustered, MARGIN = 1 , FUN = predict.DBN , predictors = predictors, junction = junction , predictands = predictands )
    }
    stopCluster(cl)
  }  
  else{ # Do not paralellize
    if (mode == 3) {
      clustered <- as.factor( predict(clusterS, newdata = p.global) )
      #global.evidence <- setEvidence(junction, nodes = predictors, states = clustered[1])
      PT <- lapply(clustered, FUN = predict.DBN , predictors = predictors, junction = junction , predictands = predictands )
    }
    else if (mode == 1 | mode == 2){
      clustered <- mapply(predict , object = clusterS, newdata = p.global ,  SIMPLIFY = TRUE  ) # matrix of data where each column is a node with its "climate value" per observation
      clustered <- matrix(as.factor(clustered), ncol = NCOL(clustered))  # reconverted to categorical
    
      PT <- apply(clustered, MARGIN = 1 , FUN = predict.DBN , predictors = predictors, 
                  junction = junction , predictands = predictands )
    }
  }
  if (as.matrix == TRUE){
    return( aperm(simplify2array( sapply(PT , simplify2array, simplify = FALSE) , higher = TRUE ) , c(3,1,2)) )
  }
  else{ return(PT) }
}

predict.DBN <- function(categorized, predictors, junction, predictands) { 
  evid <- setEvidence(junction, predictors, categorized)
  return( querygrain(evid, nodes = predictands, type = "marginal") )
}