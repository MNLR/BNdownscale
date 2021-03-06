source("functions/validation/distance.Matrix.R")

MI.vs.distance <- function(stationObj, predictionObj = NULL, returnmmat = FALSE,
                           season = "annual", 
                           dimFix = FALSE,
                           plot = FALSE, image = FALSE, 
                           aggr.type = c("after", "before"), prob = NULL, threshold = 1,
                           max.na.prop = 0.25) {
  
  if (dimFix){
    stationObj <- R.VALUE:::dimFix(stationObj)
  }
  
  mmat <- miMat.VALUE( stationObj = stationObj, predictionObj = predictionObj,
                       season = season, aggr.type = aggr.type, 
                       prob = prob, threshold = threshold,
                       max.na.prop = 1 )
  if (returnmmat){
    return(mmat)
  }
  else {
    x <- attributes(mmat[[1]])$lon
    y <- attributes(mmat[[1]])$lat
    xycoords <- matrix( c(x,y), byrow = TRUE , nrow = 2 )
    dm <- distance.Matrix(xycoords)
  
    if (image){ image(dm) }
    if (plot){ plot(c(dm), c(mmat[[1]]) ) }
  
    return( return( list( dist = c(dm), mi = c(mmat[[1]]) ) ) )
  }
}