source("functions/generalaux/preprocess.R")
preprocess.forKmeans <- function(dataset, mode){

  if ( !(is.null(attr(dataset$Data, "dimensions"))) ){
    if (attr(dataset$Data, "dimensions")[1] == "var"){ # Multigrid
      variables.list <- apply(dataset$Data, 1 , function(x) as.data.frame(x) )    
    }
    else{  # one dataset
      variables.list <- list(as.data.frame( dataset$Data )) 
    }
  }
  else {  #list of datasets
    preprocess.list <- lapply( dataset, preprocess, rm.na = FALSE )
    variables.list <- lapply(preprocess.list, function(x) x[[1]])
  }

  if (mode == 1 | mode == 2){
    Nnodes <- NCOL(variables.list[[1]])
    
    node.list <- list()
    for (i in 1:Nnodes){
      var.dataframe <- as.data.frame( sapply(variables.list, function(variable) variable[ ,i]) )
      node.list[[i]] <- var.dataframe
    }
    
    return( node.list )
  }
  
  else {
    nodesAndvars <- do.call( cbind.data.frame ,  variables.list )
    return( nodesAndvars )
 }
}
