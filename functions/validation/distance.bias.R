source("functions/validation/MI.vs.distance.R")

distance.bias <- function(real, prediction, third = NULL, only.loes.third = FALSE, threshold = 0.35, season = "annual", 
                          dimFix = FALSE, only.bias = TRUE, plot_ = FALSE, plot.only.loes = FALSE,
                          colpred = "red", colreal = "black", colthird="blue", alpha_ = 0.25, lwd=5, cex =1.5,
                          show.legend = TRUE, legend = NULL, show.title = TRUE, show.subtitle = TRUE){
  prediction.p <- real
  prediction.p$Data <- prediction
  
  attr(prediction.p$Data, 'dim') <- attributes(real$Data)$dim
  attr(prediction.p$Data, 'dimensions') <- attributes(real$Data)$dimensions
  
  real.mvd <- MI.vs.distance(real, season = c(season), dimFix = dimFix)
  predicted.mvd <- MI.vs.distance(prediction.p, season = c(season), dimFix = dimFix)
  
  # REAL
  D <- data.frame(mi=unlist(real.mvd$mi), dist=unlist(real.mvd$dist))
  smf <- loess(mi ~ dist, D)
  rx <- seq(min(D$dist, na.rm = TRUE), max(D$dist, na.rm = TRUE),length.out=10000)
  ry <- predict(smf, rx)
  # PREDICTS
  D <- data.frame(mi=unlist(predicted.mvd$mi), dist=unlist(predicted.mvd$dist))
  smf <- loess(mi ~ dist, D)
  px <- seq(min(D$dist, na.rm = TRUE), max(D$dist, na.rm = TRUE),length.out=10000)
  py <- predict(smf, px)
  # THIRD
  if (!is.null(third)){
    third.mvd <- MI.vs.distance(third, season = c(season), dimFix = dimFix)
    D <- data.frame(mi=unlist(third.mvd$mi), dist=unlist(third.mvd$dist))
    smf <- loess(mi ~ dist, D)
    tx <- seq(min(D$dist, na.rm = TRUE), max(D$dist, na.rm = TRUE),length.out=10000)
    ty <- predict(smf, tx)
  }
  cutr <-  which(ry >= threshold)[length(which(ry >= threshold))]
  cutp <- which(py >= threshold)[length(which(py >= threshold))]
  bias <- px[cutp] - px[cutr]
  
  if (plot_) {
    if (show.title){
      season <- paste(toupper(substr(season, 1, 1)), substr(season, 2, nchar(season)), sep="")
      title <- paste0("Season: ",season )
    } else {title <- NULL}
    if (show.subtitle){
      sub <- paste0("Bias: ", bias)
    } else {sub <- NULL}
    plot(px, py, col = colpred, type = 'l', lwd=lwd, main = title, sub = sub,
         xlab = "Distance (KM)", ylab = "Mutual Information" , 
         xlim = c(min(c(px,rx), na.rm = TRUE), max(c(px,rx), na.rm = TRUE) ),
         ylim = c(min(c(py,ry), na.rm = TRUE), max(c(py,ry), na.rm = TRUE) ))
    points(rx, ry, type = 'l', lwd=lwd, col = colreal)
    if (!is.null(third)){
      if (!only.loes.third){ points(third.mvd$dist, third.mvd$mi,  pch = 20, cex=cex, col = adjustcolor(colthird, alpha.f = alpha_)) }
      points(tx, ty, type = 'l', lwd=lwd, col = colthird)
      if (show.legend){
        if (is.null(legend)){ legend <- c("Observed", "Predicted", "GCM") }
        legend("topright", fill = c(colreal, colpred, colthird), legend = legend)
      }
    }
    else{
    if (show.legend){
      if (is.null(legend)){ legend <- c("Observed", "Predicted") }
      legend("topright", fill = c(colreal, colpred), legend = legend)
    }
    }
    points(real.mvd$dist, real.mvd$mi,  type = 'p',  pch = 20, cex=cex, col = adjustcolor(colreal, alpha.f = alpha_) )
    points(predicted.mvd$dist, predicted.mvd$mi, type = 'p', pch = 20, cex=cex,col = adjustcolor(colpred, alpha.f = alpha_))
    if (!plot.only.loes){
      abline(h = threshold , col = "green")
      abline(v = px[cutr],  col = colreal)
      abline(v = px[cutp], col = colpred)
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