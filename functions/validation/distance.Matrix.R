distance.Matrix <- function(xycoords){
  dm <- sapply(seq(1:NCOL(xycoords)), 
       function( point, positions )  return(  apply(sweep(positions, 1, positions[ ,point]), 2, norm, type = "2") ), 
       positions = xycoords )
  return(dm)
}