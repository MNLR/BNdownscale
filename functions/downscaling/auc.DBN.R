source("functions/downscaling/aux_functions/c.table.R")
source("functions/downscaling/convert.toMostLikely.R")

auc.DBN <- function(downscaled, real, event, points = 1000, plot.curve = FALSE, return.YI = FALSE ) {
  disc <- seq(from = 0, to = 1,length.out =  points)
  prediction.values <- mapply(is.mostLikely , threshold.vector = disc, MoreArgs = list(Probability.Table = downscaled, event = event), SIMPLIFY = FALSE)
  ctables <- lapply(prediction.values, FUN = c.table, real = real )
  roc.xvalues <- sapply( ctables, c.table.rates, value = "FPR" )
  roc.yvalues <- sapply( ctables, c.table.rates, value = "TPR" )
  
  auc <- integrate.xy(roc.xvalues ,  roc.yvalues)
  
  plot(roc.xvalues, roc.yvalues, type = "l",  main="ROC curve", sub = paste("AUC =", as.character(auc)), 
       xlab="False Positive Rate", ylab="True Positive Rate")
  lines(disc, disc, lty = 2)
  
  YoudensIndex <- which.max((1-roc.xvalues) + roc.yvalues) 
  best.threshold <- disc[YoudensIndex]
  if (return.YI){
    return( list(auc = auc , best.threshold = best.threshold) )
  }
  else{
    return( auc )
  }
}