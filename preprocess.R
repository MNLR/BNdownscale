preprocess <- function(DATA) {
  # preprocess transformeR
  positions <- matrix(t(expand.grid(DATA$xyCoords$y, DATA$xyCoords$x))[2 ,], nrow=1)
  positions <- rbind(positions, t(expand.grid(DATA$xyCoords$y, DATA$xyCoords$x))[1 , ] )
  
  return( list(as.data.frame(DATA$Data) , positions ) )
}