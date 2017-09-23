source("functions/generalaux/preprocess.one.R")

preprocess <- function(observations, global = NULL,  rm.na = TRUE , rm.na.mode = "auto", rm.na.observations.tol = 2,  rm.na.stations.tol = 2  ) {
  # preprocess transformeR data given as global and observations
  if (is.null(global)){ 
    if (class(observations$xyCoords) == "matrix" | class(observations$xyCoords) == "data.frame" ){
      auxdata <- preprocess.one(observations, rownames(observations$xyCoords) )
    }
    else { auxdata <- preprocess.one(observations)  } 
    data <- auxdata[[1]]
    positions <- auxdata[[2]]
  }
  else{
    if (class(global$xyCoords) == "list"){
      Nvars <- length(global$xyCoords$x)*length(global$xyCoords$y)
      names <- mapply(paste0, array("G", Nvars) ,seq(1,Nvars))
    }
    else if (class(global$xyCoords) == "matrix" | class(global$xyCoords) == "data.frame" ){
      Nvars <- NROW(global$xyCoords)
      names <- mapply(paste0, array("G.", Nvars) , rownames(global$xyCoords) )
    }
    else { stop("Incorrect format.") }
    
    global.p <- preprocess.one(global, names)
    
    if (class(observations$xyCoords) == "list"){
      Nvars <- length(observations$xyCoords$x)*length(observations$xyCoords$y)
      names <- mapply(paste0, array("D", Nvars) ,seq(1,Nvars))
    }
    else if (class(observations$xyCoords) == "matrix" | class(observations$xyCoords) == "data.frame"){
      Nvars <- NROW(observations$xyCoords)
      names <- mapply(paste0, array("D.", Nvars) , rownames(observations$xyCoords) )
    }
    else { stop("Incorrect format.") }
    observations.p <- preprocess.one(observations, names)
    
    positions <- cbind( global.p[[2]], observations.p[[2]])
    data <- cbind.data.frame(global.p[[1]], observations.p[[1]])
  }
    
  if (rm.na){
    NCOL0 <- NCOL(data)
    NROW0 <- NROW(data) 
    if (rm.na.mode == "stations"){
      nanS <- colSums(is.na(data)) 
      data <- data[ , nanS == 0]
      positions <- positions[ , nanS == 0]
    }
    else if (rm.na.mode == "observations" ) {
      nanS <- rowSums(is.na(data)) 
      data <- data[ nanS == 0, ]
    }
    else if (rm.na.mode == "auto"){
      if ( NROW0 - sum(complete.cases(data)) <= rm.na.observations.tol ){ 
        nanS <- rowSums(is.na(data)) 
        data <- data[ nanS == 0, ]
      }
      else {
        # removes stations with number of nans > rm.na.stations.tol
        nanS <- colSums(is.na(data)) 
        data <- data[ , nanS <= rm.na.stations.tol ]
        positions <- positions[ , nanS <= rm.na.stations.tol ]
        
        # removes the remaining observations with NaNs
        noNaNS <- complete.cases(data)
        data <- data[noNaNS, ]
      }
    }
    rc <- NCOL0 - NCOL(data)
    rr <- NROW0 - NROW(data)
    if (rc != 0){ print(paste0("Removed ", rc , " stations.")) }
    if (rr != 0){ print(paste0("Removed ", rr , " observations." ))}
  }
  return( list(data , positions ) )
}