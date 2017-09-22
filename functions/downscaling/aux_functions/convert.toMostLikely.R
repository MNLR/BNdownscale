source("functions/downscaling/aux_functions/mostLikelyEvent.R")

convert.toMostLikely <- function( Probability.TableS, mark.ties = FALSE , numeric = TRUE){
  # Probability.Table     won't check for consistent data 
  # threshold             If probability of an event  is higher than this value this event will be marked. Must be a Matrix
  # mark.ties             CURRENTLY UNUSED Set to TRUE to return NULL when a tie is encountered.
  return( t(apply(Probability.TableS, MARGIN = 1, FUN =  mostLikelyEvent, mark.ties = mark.ties, numeric=numeric) ) )
}