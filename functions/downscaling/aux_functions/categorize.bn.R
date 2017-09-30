categorize.bn <- function( data , mode , ncategories, breaks.list = NULL ){
  if (mode == 3 ) {modeaux <- 2} else {modeaux <- mode}
  global.p <- preprocess.forKmeans(global, mode = modeaux, scale_ = FALSE, only.nodes) #mode is irrelevant here
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
    
    } else if (mode == 3) { # by quantiles
      breaks.list <- lapply(global.p, function(x) return(sapply(x, 
                                                                function(var, ncategories) {
                                                                  break.lims <- quantile(var, 
                                                                                         seq(0 + 1/ncategories, 1-1/ncategories,length.out = ncategories-1))
                                                                  return(c(-Inf, break.lims, Inf))
                                                                },
                                                                ncategories = ncategories) ) )
    } else if (mode == 4) {
      breaks.list <- lapply( global.p, 
                             function(var, ncategories) {
                               labs <- levels( cut(var, ncategories ))
                               labs_df = cbind(lower = as.numeric( sub("\\((.+),.*", "\\1", labs) ),
                                               upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", labs) ))
                               breaks <- unique(c(t(labs_df)))
                               breaks[1] <- -Inf
                               breaks[length(breaks)]  <- Inf
                               return( breaks )
                             }, ncategories = ncategories)
    }
      else if (mode == 5) { # by node, by quantiles
        breaks.list <- lapply(global.p, function(var, ncategories) { 
                                          break.lims <- quantile(var, seq(0 + 1/ncategories, 1-1/ncategories,length.out = ncategories-1))
                                          return(c(-Inf, break.lims, Inf))
                                        },  ncategories = ncategories) 
        
    } else { stop("This is not supported!") }
 }
 if (mode == 2 | mode == 3) {
  categorized <- mapply( function( node, breaks ) {
      nodevars <- split( as.matrix(node) , col(node) )
      breakvars <- split( breaks , col(breaks) )
      return( interaction(as.data.frame( mapply( function(var, break_) cut(var, break_) , nodevars, breakvars , SIMPLIFY = TRUE )) ) )
    },
    global.p,
    breaks.list  )
 } else{
   categorized <- mapply( function( node, break_ ) return( cut(node, breaks = break_) ), global.p, breaks.list , SIMPLIFY = TRUE )  
 }
  
  return( list(categorized, breaks.list) ) 
}

