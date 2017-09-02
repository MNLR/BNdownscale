marginals <- function(downscale.BN){
  BN <- downscale.BN$BN
  BN.fit <- downscale.BN$BN.fit
  clusterS <- downscale.BN$clusterS

  Nglobal <- length(clusterS)
  predictands <- names(BN$nodes)[- (1:Nglobal) ]
  
  junction <- compile( as.grain(BN.fit) )
  return( simplify2array(querygrain(junction, nodes = predictands)) )
}