source("functions/validation/c.table.R")

auc <- function(probabilities, name, real, event = 1, not.event = 0,  points = 1000, plot.curve = FALSE, return.YI = FALSE ) {
  disc <- seq(from = 0, to = 1, length.out =  points)
  
  occurence <- lapply(disc, 
                      FUN =  function( threshold , probabilities) return(as.numeric(probabilities >= threshold)), 
                      probabilities = probabilities )
  if (event != 1){
    real[real == event] <- 1
    occurence[occurence == event ] <- 1
  }
  if (not.event != 0) {
    real[real == not.event] <- 0
    occurence[occurence == not.event ] <- 0
  }
  ctables <- lapply( occurence ,  c.table, real = real)

  roc.xvalues <- rev( sapply( ctables, c.table.rates, value = "FPR" ) )
  roc.yvalues <- rev( sapply( ctables, c.table.rates, value = "TPR" ) )
  
  auc <- integrate.xy(roc.xvalues ,  roc.yvalues, use.spline = FALSE)
  if (plot.curve){
    plot(roc.xvalues, roc.yvalues, type = "l",  main=paste0("ROC curve for ", name ), sub = paste("AUC =", as.character(auc)), 
         xlab="False Positive Rate", ylab="True Positive Rate")
    lines(disc, disc, lty = 2)
  }
  
  YoudensIndex <- which.max((1-roc.xvalues) + roc.yvalues) 
  best.threshold <- disc[YoudensIndex]
  if (return.YI){
    return( list(auc = auc , best.threshold = best.threshold) )
  }
  else{
    return( auc )
  }
}