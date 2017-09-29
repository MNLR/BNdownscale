categorize.bn <- function( data , mode , ncategories, breaks.list = NULL ){
  global.p <- preprocess.forKmeans(data, mode = 1, scale_ = FALSE) #mode is irrelevant here
  if ( is.null(breaks.list) ){
    if (mode == 2){
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
    
    } else if (mode == 3) { # by whatever-tile
      breaks.list <- lapply(global.p, function(x) return(sapply(x, 
                                                                function(var, ncategories) {
                                                                  break.lims <- quantile(var, 
                                                                                         seq(0 + 1/ncategories, 1-1/ncategories,length.out = ncategories-1))
                                                                  return(c(-Inf, break.lims, Inf))
                                                                },
                                                                ncategories = ncategories) ) )
    } else { stop("This is not supported!") }
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

