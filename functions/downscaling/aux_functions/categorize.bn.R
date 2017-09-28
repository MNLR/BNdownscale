categorize.bn <- function( data , mode , ncategories, breaks.list = NULL ){
  
  global.p <- preprocess.forKmeans(data, mode = mode, scale_ = FALSE)
  
  if (mode == 1 | mode == 2){
    if ( (is.null(breaks.list)) ){
      breaks.list <- lapply( global.p, 
                            function(node) return( apply(node, 
                                              MARGIN = 2,
                                              function(var, ncategories) {
                                                labs <- levels( cut(var, ncategories ))
                                                labs_df = cbind(lower = as.numeric( sub("\\((.+),.*", "\\1", labs) ),
                                                                upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", labs) ))
                                                
                                                breaks <- unique(c(t(labs_df)))
                                                breaks[1] <- -Inf
                                                breaks[length(breaks)]  <- Inf
                                                return( breaks )
                                              }, ncategories = ncategories)
                                                )
                            )
    
    }

    categorized <- mapply( function( node, breaks ) {
      nodevars <- split( as.matrix(node) , col(node) )
      breakvars <- split( breaks , col(breaks) )
      return( interaction(as.data.frame( mapply( function(var, break_) cut(var, break_) , nodevars, breakvars , SIMPLIFY = TRUE )) ) )
      
    },
    global.p,
    breaks.list  )
  
  return( list(categorized, breaks.list) ) 
  }
  else {
    stop("This is not supported!")
  }
}

