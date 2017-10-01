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
  categorization.type <- downscale.BN$categorization.type

  Nglobal <- downscale.BN$Nglobals
  predictors <- names(BN$nodes)[1:Nglobal]
  predictands <- names(BN$nodes)[- (1:Nglobal) ]
  categorization.attributes <- downscale.BN$categorization.attributes
  
  if (categorization.type != "no"){
    print("Categorizing data...")
    categorized <- categorize.bn(global, type = categorization.type, cat.args = categorization.attributes,
                                ncategories = NULL,
                                clustering.args.list = NULL, 
                                clusterS = clusterS,
                                parallelize = parallelize, cluster.type = cluster.type, n.cores = n.cores,
                                training.phase = FALSE)
    if (categorization.type == "atmosphere"){categorized <- as.matrix(categorized)}
    print("Done...")
  } else { categorized <- as.matrix(preprocess( global, rm.na = TRUE, rm.na.mode = "observations")[[1]]) }
  
  print("Compiling junction...")
  junction <- compile( as.grain(BN.fit) )
  print("Done.")
  
  print("Propagating evidence and computing Probability Tables...")
  if ( parallelize == TRUE) {
    if ( is.null(n.cores) ){
      n.cores <- floor(detectCores()-1)
    }
    # Initiate cluster
    cl <- makeCluster( n.cores, type = cluster.type )
    if (cluster.type == "PSOCK") {
      clusterExport(cl, list("setEvidence", "querygrain" , "predict.DBN") , envir = environment())
      clusterExport(cl, list( "junction", "predictors" , "predictands", "categorized") , envir = environment())
    }
    PT <- parApply(cl, categorized, MARGIN = 1, FUN = predict.DBN, 
                   predictors = predictors, junction = junction , predictands = predictands )
    stopCluster(cl)
  }
  else { # Do not parallelize
    PT <- apply(categorized, MARGIN = 1, FUN =  predict.DBN,
                predictors = predictors, junction = junction , predictands = predictands )
  }
  print("Done.")
  
  if ( prediction.type == "probabilities.list" ) {
    return(PT) 
  }
  else {
    downscaled <- aperm(simplify2array( sapply(PT , simplify2array, simplify = FALSE) , higher = TRUE ) , c(3,1,2))
    PT <- downscaled[,,match(predictands, colnames(downscaled[1,,]))] 
    if ( prediction.type == "event" & ( !(is.null(downscale.BN$marginals)) | !(is.null(threshold.vector)) ) ){
      if (is.null(threshold.vector)){ threshold.vector  <- 1 - downscale.BN$marginals[event, ] }
      return( is.mostLikely(PT, event = event, threshold.vector =  threshold.vector) )
    }
    else {
      return(PT)
    }
  }
}

