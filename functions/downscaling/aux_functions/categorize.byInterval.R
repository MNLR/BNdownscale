source("functions/downscaling/aux_functions/preprocess.forCategorization.R")

categorize.byInterval <- function( data, cattype, agg, ncategories, training.phase = TRUE, breaks.list = NULL){
  global.p <- preprocess.forCategorization(data, agg = agg, scale_ = FALSE, training.phase = training.phase) 
  if (cattype == "Simple"& agg == "node") { mode <- 2}
  if (cattype == "Even" & agg == "node") { mode <- 3}
  if (cattype == "Simple" & agg == "vars") { mode <- 4}
  if (cattype == "Even" & agg == "vars") { mode <- 5}
  
  if ( training.phase ){
    positions <- global.p[[2]]
    global.p <- global.p[[1]]
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
  
 if (training.phase){  
   return( list(categorized = categorized, breaks.list = breaks.list, positions = positions) )
 }
 else { return(categorized) }
}

