preprocess.forKmeans <- function(dataset, mode){
  # Debe colocar cada observacion en una fila, si hay 8 nodos globales y 4 variables por nodo debe crear una matriz 
  # cuyas variables sean Nodo1.Var1, Nodo1.Var2, ... , Nodo8.Var4
  
  preprocess.list <- lapply( dataset, preprocess, rm.na = FALSE )
  variables.list <- lapply(preprocess.list, function(x) x[[1]])
  # positions.list <- lapply(preprocess.list, function(x) x[[2]])
  
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

#dataset <- list(q850.germany, t850.germany, z850.germany)
#q850.germany$Data[100,2,5] #[1] 0.00196711
#node.list[[26]]$V1[100]