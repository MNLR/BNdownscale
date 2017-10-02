source("functions/validation/auc.R")

auc.DBN <- function(downscaled, realData, points = 1000, plot.curves = FALSE){
  # Data must be binary
  aucS <- array(0, NCOL(realData))
  for (station in seq(1, NCOL(realData))) {
    if (!(all(is.na(realData[ , station])))){
      aucS[station] <-  auc( probabilities =   downscaled[, , station][ , "1"], 
                            real = realData[, station], 
                            points = points,
                            plot.curve = plot.curves )
    } else{
        aucS[station] <- NA
    }
  }
  return(aucS)
}