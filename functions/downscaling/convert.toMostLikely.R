convert.toMostLikely <- function( Probability.TableS, mark.ties = FALSE , numeric = TRUE){
  # Probability.Table     won't check for consistent data y eso
  # threshold             If probability of an event  is higher than this value this event will be marked. Must be a Matrix
  # mark.ties             Set to TRUE to return NULL when a tie is encountered.
  return( t(apply(Probability.TableS, MARGIN = 1, FUN =  mostLikelyEvent, mark.ties = mark.ties, numeric=numeric) ) )
}

mostLikelyEvent  <- function(Probability.Table, mark.ties = FALSE, numeric = TRUE) {
  names <- rownames(Probability.Table)
  threshold.vector <- array(1/NROW(Probability.Table), NCOL(Probability.Table)) 
  aa  <-  mapply( function(x, threshold, names) return(names[which(x >= threshold)]), 
                  split(Probability.Table, c(col(Probability.Table))),  
                  threshold.vector, 
                  MoreArgs = list(names = names)) 
  return( as.numeric(aa) )
}

is.mostLikely <- function(Probability.Table, event, threshold.vector) {
  return( t(apply(Probability.Table, is.mostLikelyEvent,  MARGIN = 1,  event = event, threshold.vector = threshold.vector) ))
}

is.mostLikelyEvent  <- function(Probability.Table, event, threshold.vector) {
  return(as.numeric(Probability.Table[event , ] > threshold.vector))
}

#threshold <- marginals(DBN.m2.d25.k12)
#is.mostLikelyEvent(d.bn[1, , ], "1", threshold)