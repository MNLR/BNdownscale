source("functions/downscaling/aux_functions/preprocess.forKmeans.R")
source("functions/downscaling/aux_functions/categorize.bn.R")
source("functions/downscaling/aux_functions/predict.DBN.R")

downscale.BN <- function(downscale.BN, global,
                         prediction.type = "probabilities", event = "1", threshold.vector = NULL,
                         parallelize = FALSE, n.cores = NULL , cluster.type = "PSOCK"){

  # Parallelize = TRUE should help a lot when lots of evidences are provided.
  # cluster.type    Accepts "PSOCK" and "FORK". "FORK" cannot be used in Windows systems.
  # prediction.type Options are "event" "probabilities" "probabilities.list"
  #                   "event" returns a binary prediction based on threshold.vector. By default threshold.vector
  #                     is set to NULL, which will use as threshold 1-MP where MP is the marginal probability,
  #                     for each node. If downscale.BN has no $marginals value or threshold.vector is NULL, 
  #                     "probabilities" setting will apply.
  #                   "probabilities" returns the probabilities as a matrix where dimensions are [obs, cat, node]
  #                     requires predictand nodes to have the same categories
  #                   "probabilities.list" returns a list of nodes with their probability tables.
  #                     Warning: Beware of the nodes ordering if set to FALSE!
  #                   
  
  BN <- downscale.BN$BN
  BN.fit <- downscale.BN$BN.fit
  clusterS <- downscale.BN$clusterS
  mode_ <- downscale.BN$mode
  mode <- as.numeric(mode_[1])
  mode2 <- mode_[2]
  Nglobal <- downscale.BN$Nglobals
  predictors <- names(BN$nodes)[1:Nglobal]
  predictands <- names(BN$nodes)[- (1:Nglobal) ]
  clustering.attributes <- downscale.BN$clustering.attributes
  
  print("Compiling junction...")
  junction <- compile( as.grain(BN.fit) )
  print("Done.")
    
  print("Propagating evidence and computing Probability Tables...")
  if (is.null(clusterS)){ # data is expected categorized
    if (mode2 == "1"){  
      clustered <- as.matrix(preprocess(global)[[1]]) 
    } 
    else { clustered  <- categorize.bn( global, mode, NULL , clustering.attributes)[[1]] }  # easy-categorization
  }
  else{
    p.global <- preprocess.forKmeans(global, mode, scale.args = clustering.attributes )
  }
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
      clustered <-  predict(clusterS, newdata = p.global) 
      if (cluster.type == "PSOCK") {
        clusterExport(cl, "clustered" , envir = environment() )
      }
      PT <- parLapply(cl , clustered, fun =  predict.DBN , predictors = predictors, junction = junction , predictands = predictands )
    }
    else if (mode == 1 | mode == 2){
      if (!(is.null(clusterS))){
        clustered <- mapply(predict , object = clusterS, newdata = p.global ,  SIMPLIFY = TRUE  ) # matrix of data where each column is a node with its "climate value" per observation
      }

      if (cluster.type == "PSOCK") {
        clusterExport(cl, "clustered" , envir = environment() )
      }
      PT <- parApply(cl , clustered, MARGIN = 1 , FUN = predict.DBN , predictors = predictors, junction = junction , predictands = predictands )
    }
    stopCluster(cl)
  }  
  else{ # Do not parallelize
    if (mode == 3) {
      clustered <-  predict(clusterS, newdata = p.global) 
      PT <- lapply(clustered, FUN = predict.DBN , predictors = predictors, junction = junction, predictands = predictands )
    }
    else if (mode == 1 | mode == 2){
      if (!(is.null(clusterS))){
        clustered <- mapply(predict , object = clusterS, newdata = p.global ,  SIMPLIFY = TRUE  ) # matrix of data where each column is a node with its "climate value" per observation
      }  
      PT <- apply(clustered, MARGIN = 1 , FUN = predict.DBN , predictors = predictors, 
                  junction = junction , predictands = predictands )
    }
  }
  print("Done.")
  
  if ( prediction.type == "probabilities.list" ) { 
    return(PT) 
  }
  else if ( prediction.type == "event" & ( !(is.null(DBN$marginals)) | !(is.null(threshold.vector)) ) ){
    if (is.null(threshold.vector)){ threshold.vector  <- 1 - DBN$marginals[event, ] }
    return( is.mostLikely(downscaled, event = event, threshold.vector =  threshold.vector) )
  } 
  else {
    downscaled <- aperm(simplify2array( sapply(PT , simplify2array, simplify = FALSE) , higher = TRUE ) , c(3,1,2))
    return( downscaled[,,match(predictands, colnames(downscaled[1,,]))] )
  }
}

