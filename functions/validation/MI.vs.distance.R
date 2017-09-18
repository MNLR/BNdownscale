source("functions/validation/distance.Matrix.R")

MI.vs.distance <- function(stationObj, predictionObj = NULL, 
                           season = "annual",
                           plot = FALSE, image = FALSE, 
                           aggr.type = c("after", "before"), prob = NULL, threshold = 1,
                           max.na.prop = 0.25) {
  mmat <- miMat.VALUE( stationObj = stationObj, predictionObj = predictionObj,
                       season = c(season), aggr.type = aggr.type, 
                       prob = prob, threshold = threshold,
                       max.na.prop = max.na.prop )
  
  x <- attributes(mmat[[1]])$lon
  y <- attributes(mmat[[1]])$lat
  xycoords <- matrix( c(x,y), byrow = TRUE , nrow = 2 )
  dm <- distance.Matrix(xycoords)
  
  if (image){ image(dm) }
  if (plot){ plot(c(dm), c(mmat[[1]]) ) }
  
  return( matrix( c( c(dm), c(mmat[[1]]) ), byrow = TRUE, nrow = 2) )
}