preprocess.one <- function(DATA, node.names = NULL) {
  # preprocess transformeR
  if (class(DATA$xyCoords) == "list"){
    positions <- matrix(t(expand.grid(DATA$xyCoords$y, DATA$xyCoords$x))[2 ,], nrow=1)
    positions <- rbind(positions, t(expand.grid(DATA$xyCoords$y, DATA$xyCoords$x))[1 , ] )
  }
  else if (class(DATA$xyCoords) == "matrix" | class(DATA$xyCoords) == "data.frame"){
    positions <- t(DATA$xyCoords)
  }
  else { stop("Incorrect format.") }
  
  data <- as.data.frame(DATA$Data)
  if (!(is.null(node.names))) {
    colnames(data) <- node.names
  }
  return( list( data , positions ) )
}
