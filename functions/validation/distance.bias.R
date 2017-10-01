source("functions/validation/MI.vs.distance.R")

distance.bias <- function(real, prediction, threshold = 0.3, season = "annual", 
                          dimFix = FALSE, only.bias = TRUE, plot_ = FALSE, plot.only.loes = FALSE){
  prediction.p <- real
  prediction.p$Data <- prediction
  
  attr(prediction.p$Data, 'dim') <- attributes(real$Data)$dim
  attr(prediction.p$Data, 'dimensions') <- attributes(real$Data)$dimensions
  
  real.mvd <- MI.vs.distance(real, season = c(season), dimFix = dimFix)
  predicted.mvd <- MI.vs.distance(prediction.p, season = c(season), dimFix = dimFix)
  # REAL
  D <- data.frame(mi=unlist(real.mvd$mi), dist=unlist(real.mvd$dist))
  smf <- loess(mi ~ dist, D)
  rx <- seq(min(D$dist, na.rm = TRUE),max(D$dist, na.rm = TRUE),length.out=10000)
  ry <- predict(smf, rx)
  
  # PREDICTS
  D <- data.frame(mi=unlist(predicted.mvd$mi), dist=unlist(predicted.mvd$dist))
  smf <- loess(mi ~ dist, D)
  px <- seq(min(D$dist, na.rm = TRUE),max(D$dist, na.rm = TRUE),length.out=10000)
  py <- predict(smf, px)

  cutr <-  which(ry >= threshold)[length(which(ry >= threshold))]
  cutp <- which(py >= threshold)[length(which(py >= threshold))]
  bias <- px[cutp] - px[cutr]
  
  if (plot_) {
    plot(px, py, col = "red",  xlab = "Distance", ylab = "Mutual Information")
    points(rx, ry)
    if (!plot.only.loes){
      points(real.mvd$dist, real.mvd$mi)
      points(predicted.mvd$dist, predicted.mvd$mi, col = "red")
      abline(h = threshold , col = "green")
      abline(v = px[cutr])
      abline(v = px[cutp], col = "red")
    }

  }
  if (only.bias){
    return( bias )
  } else{
    return(list( real.mvd = real.mvd, predicted.mvd = predicted.mvd, 
                 rx = rx, ry = ry,
                 px = px, py = py,
                 xcut1 = px[cutp],
                 xcut2 = px[cutr],
                 bias = bias) )
  }
}

