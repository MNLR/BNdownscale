source("functions/downscaling/aux_functions/categorize.byInterval.R")

categorize.bn <- function(global, type, training.phase, cat.args = NULL, ncategories, clustering.args.list, clusterS, parallelize, cluster.type, n.cores ) {
  if (!(is.character(type))){ stop("Enter a valid categorization.type: \"no\", \"nodeClustering\", \"varsClustering\", \"nodeSimple\", \"varsSimple\", \"nodeEven\", \"varsEven\", \"atmosphere\".")}
  
  if (type == "atmosphere"){
    global.processed <- preprocess.forCategorization(global, agg = "atmosphere", training.phase = training.phase, 
                                                     scale_ = TRUE, scale.args = cat.args)
    if (training.phase){
      global.data.processed <- global.processed[[1]]
      positions <- global.processed[[2]]
      Nglobals <- 1
      categorization.attributes <- list(attributes(global.data.processed)$`scaled:center`, attributes(global.data.processed)$`scaled:scale`) 
      
      clustering.input <- append( list( x = global.data.processed), clustering.args.list )
      clusterS <- do.call( kcca , clustering.input )
      global.categorized <- matrix(as.factor(predict(clusterS)), ncol = 1)
    } # END of training phase
    else{  # Called in predicting phase
      global.categorized <- predict(clusterS, newdata = global.processed) 
    }
  } 
  else {
    agg <- substr(type, 1, 4)
    cattype <- substr(type, 5, nchar(type))
    if (cattype != "Clustering" & cattype != "Simple" & cattype != "Even")  {stop("Enter a valid categorization.type: \"no\", \"nodeClustering\", \"varsClustering\", \"nodeSimple\", \"varsSimple\", \"nodeEven\", \"varsEven\", \"atmosphere\".")}
    if (agg != "node" & agg != "vars")  {
      stop("Enter a valid categorization.type: \"no\", \"nodeClustering\", \"varsClustering\", \"nodeSimple\", \"varsSimple\", \"nodeEven\", \"varsEven\", \"atmosphere\".")
    }
    
    if (cattype == "Simple" | cattype == "Even"){
        global.processed <- categorize.byInterval(global, 
                                                  cattype = cattype, agg = agg , ncategories = ncategories,
                                                  training.phase = training.phase, breaks.list = cat.args)
        if (training.phase){
          global.categorized <- global.processed$categorized
          categorization.attributes <- global.processed$breaks.list
          positions <- global.processed$positions
          Nglobals <- NCOL(global.categorized)
          clusterS <- NULL
        } else {global.categorized <- global.processed}
    }
    else { # clustering, not atmosphere
      global.processed <- preprocess.forCategorization(global, agg = agg, training.phase = training.phase, 
                                                       scale_ = TRUE, scale.args = cat.args)
      if (training.phase){
        global.data.processed <- global.processed[[1]]
        positions <- global.processed[[2]]
        Nglobals <- length(global.data.processed)
        categorization.attributes <- lapply(global.data.processed, 
                                          function(node) return(list(attributes(node)$`scaled:center`, 
                                                                     attributes(node)$`scaled:scale`) ) 
                                            )
        if ( parallelize ) {
          if ( is.null(n.cores) ){
            n.cores <- floor(detectCores()-1)
          }
          # Initiate cluster
          cl <- makeCluster(n.cores, type = cluster.type )   
          if (cluster.type == "PSOCK") {
              clusterExport(cl, list("kcca") , envir = environment())
              clusterExport(cl, list( "global.processed", "clustering.args.list" ) , envir = environment() )
          }
          clusterS <- parLapply(cl, global.data.processed,
                                  function(node, cal) return( do.call(kcca, append( list( x = node), cal ) ) ),
                                  cal = clustering.args.list)
          
          stopCluster(cl)
        }
        else {
          cl <- NULL
          clusterS <- lapply(global.data.processed, 
                             function(node, cal) return( do.call(kcca, append( list( x = node), cal ) ) ), 
                             cal = clustering.args.list)
        }
        global.categorized <- sapply(clusterS, predict) # matrix of data where each column is a node with its "climate value" per observation
      } # END of training phase
      else{  # Called in predicting phase
        global.categorized <- mapply(predict , object = clusterS, newdata = global.processed, SIMPLIFY = TRUE) # matrix of data where each column is a node with its "climate value" per observation
      }
      
      global.categorized <- matrix(as.factor(global.categorized), ncol = NCOL(global.categorized))
    }
  }
  
  if ( training.phase ){
    return( list(Data = global.categorized, 
               categorization.attributes = categorization.attributes,
               xyCoords = as.data.frame(positions),
               Nglobals = Nglobals,
               clusterS = clusterS)
          )
  } else {
    return(global.categorized)
  }
}
