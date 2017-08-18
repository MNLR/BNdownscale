preprocess.one <- function(DATA, node.names = NULL) {
  # preprocess transformeR
  if (class(DATA$xyCoords) == "list"){
    positions <- matrix(t(expand.grid(DATA$xyCoords$y, DATA$xyCoords$x))[2 ,], nrow=1)
    positions <- rbind(positions, t(expand.grid(DATA$xyCoords$y, DATA$xyCoords$x))[1 , ] )
  }
  else if (class(DATA$xyCoords) == "matrix"){
    positions <- t(DATA$xyCoords)
  }
  else { stop("Incorrect format.") }
  
  data <- as.data.frame(DATA$Data)
  if (!(is.null(node.names))) {
    colnames(data) <- node.names
  }
  return( list( data , positions ) )
}

preprocess <- function(observations, global = NULL,  rm.na = TRUE ) {
  # preprocess transformeR data given as global and observations
  if (is.null(global)){ 
    observations.p <- preprocess.one(observations)
    if (rm.na){
      nanS <- colSums(is.na(observations.p[[1]])) 
      observations.p[[1]] <- observations.p[[1]][ , nanS == 0]
      observations.p[[2]] <- observations.p[[2]][ , nanS == 0]
    }
    return(observations.p)}
  else{
    if (class(global$xyCoords) == "list"){
      Nvars <- length(global$xyCoords$x)*length(global$xyCoords$y)
    }
    else if (class(global$xyCoords) == "matrix"){
      Nvars <- NROW(global$xyCoords)
    }
    else { stop("Incorrect format.") }
    names <- mapply(paste0, array("G", Nvars) ,seq(1,Nvars))
    global.p <- preprocess.one(global, names)
    
    if (class(observations$xyCoords) == "list"){
      Nvars <- length(observations$xyCoords$x)*length(observations$xyCoords$y)
    }
    else if (class(observations$xyCoords) == "matrix"){
      Nvars <- NROW(observations$xyCoords)
    }
    else { stop("Incorrect format.") }
    names <- mapply(paste0, array("D", Nvars) ,seq(1,Nvars))
    observations.p <- preprocess.one(observations, names)

    positions <- cbind( global.p[[2]], observations.p[[2]])
    data <- cbind.data.frame(global.p[[1]], observations.p[[1]])
    
    if (rm.na){
      nanS <- colSums(is.na(data)) 
      data <- data[ , nanS == 0]
      positions <- positions[ , nanS == 0]
    }
    return( list(data , positions ) )
  }
}

preprocess.list <- function(...) {
  # preprocess transformeR data given as list
  argS = list(...)
  dataS <- lapply(argS, preprocess.one)
  positions <- do.call(cbind, lapply(dataS, function(x) x[[2]]))
  data <- do.call(cbind.data.frame, c(lapply(dataS, function(x) x[[1]]) , make.row.names = TRUE) )
  return( list(as.data.frame(data) , positions ) )
}