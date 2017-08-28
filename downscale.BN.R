downscale.BN <- function(downscale.BN, global){
  # TO BE CONTINUED...
  # for now it only places in their corresponding cluster the predictors
  BN <- downscale.BN$BN
  clusterS <- downscale.BN$clusterS
  mode <- downscale.BN$mode
  
  p.global <- preprocess.forKmeans(global, mode)
  
  if (mode == 3) {
    clustered <- matrix(as.factor( predict(clusterS, newdata = p.global) ), ncol = 1)
  }
  else if (mode == 1 | mode == 2){
    clustered <- mapply(predict , object = clusterS, newdata = p.global)
  }
  return(clustered)
  # TO BE CONTINUED...
}