source("functions/generalaux/preprocess.R")
preprocess.forKmeans <- function(dataset, mode, scale.args = NULL, scale_ = TRUE, only.nodes = FALSE){
  
  if ( !(is.null(attr(dataset$Data, "dimensions"))) ){
    if (attr(dataset$Data, "dimensions")[1] == "var"){ # Multigrid
      variables.list <- apply(dataset$Data, 1 , function(x) as.data.frame(x) )    
    } else {  # one dataset
      variables.list <- list(as.data.frame( dataset$Data )) 
    }
  }  else {  #list of datasets
    preprocess.list <- lapply( dataset, preprocess, rm.na = FALSE )
    variables.list <- lapply( preprocess.list, function(x) x[[1]] )
  }
  if ( (mode == 4) | (mode == 5) ) {
    positions <- preprocess(dataset)[[2]]
    if (is.null(colnames(positions))){ colnames(positions) <- as.character( seq(1, NCOL(variables.list[[i]])) ) }
    
    positions.list <- list()
    for (i in 1:length(variables.list)){
      positions.list[[i]] <- positions
      colnames(positions.list[[i]]) <- paste0( colnames(positions), paste0(".V.", as.character(i)) )
    }
    positions <- t(as.matrix(cbind.data.frame(positions.list)))
    node.list <- cbind.data.frame(variables.list)
    
    if (only.nodes){ return(node.list) } else { return(list( node.list , positions)) }
  }
  else if (mode == 1 | mode == 2){
    Nnodes <- NCOL(variables.list[[1]])
    
    node.list <- list()
    for (i in 1:Nnodes){
      var.dataframe <- as.data.frame( sapply(variables.list, function(variable) variable[ ,i]) )
      node.list[[i]] <- var.dataframe
    }
    if (scale_){
      if (is.null(scale.args)){ node.list <- lapply(node.list, scale) }
      else { node.list <- mapply(function(node, scale.args_) return(scale(node, center =  scale.args_[[1]], scale = scale.args_[[2]])), 
                               node.list, scale.args , SIMPLIFY = FALSE)  }
    }
    return( node.list )
  }  else {
    nodesAndvars <- do.call( cbind.data.frame ,  variables.list )
    if (scale_){
      if (is.null(scale.args)){ nodesAndvars <- scale(nodesAndvars) }
      else {nodesAndvars <- scale(nodesAndvars, center = scale.args[[1]] , scale = scale.args[[2]] ) }
    }
    return( nodesAndvars )
 }
}