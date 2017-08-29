downscale.BN <- function(downscale.BN, global){
  # TO BE CONTINUED...
  # for now it only places in their corresponding cluster the predictors
  BN <- downscale.BN$BN
  BN.fit <- downscale.BN$BN.fit
  clusterS <- downscale.BN$clusterS
  mode <- downscale.BN$mode
  
  Nglobal <- length(clusterS)
  predictors <- names(BN$nodes)[1:Nglobal]
  print(predictors)
  
  junction <- compile( as.grain(BN.fit) )
  
  p.global <- preprocess.forKmeans(global, mode)
  
  if (mode == 3) {
    clustered <- as.factor( predict(clusterS, newdata = p.global) )
    #global.evidence <- setEvidence(junction, nodes = predictors, states = clustered[1])
    global.evidence <- mapply(setEvidence , states = clustered , MoreArgs = list( object = junction, nodes = predictors ) )
  }
  else if (mode == 1 | mode == 2){
    clustered <- mapply(predict , object = clusterS, newdata = p.global)
    global.evidence <- apply(states = clustered, MARGIN = 1, FUN = setEvidence, nodes = predictors)
  }
  
  return(global.evidence)
  # TO BE CONTINUED...
}