marginals <- function(downscale.BN){
  BN <- downscale.BN$BN
  BN.fit <- downscale.BN$BN.fit
  Nglobal <- downscale.BN$Nglobals

  predictands <- names(BN$nodes)[- (1:Nglobal) ]
  
  junction <- compile( as.grain(BN.fit) )
  
  return( simplify2array(querygrain(junction, nodes = predictands)) )
}


cpquery