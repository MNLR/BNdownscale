source("functions/validation/distance.haversine.R")

distance.Matrix <- function(xycoords, km = TRUE){
  if (km){
    x <- xycoords[ 1, ]
    y <- xycoords[ 2, ]
    
    x <- x*0.01745329252 #torad
    y <- y*0.01745329252 #torad
    
    dh <- mapply(distance.haversine, long1 = x, lat1 = y, MoreArgs = list(long2=x, lat2=y), SIMPLIFY = TRUE) 
    return(dh)
  }
  else{
    dm <- sapply(seq(1:NCOL(xycoords)), 
         function( point, positions )  return(  apply(sweep(positions, 1, positions[ ,point]), 2, norm, type = "2") ), 
         positions = xycoords )
    return(dm)
  }
}