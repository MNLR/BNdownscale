source("functions/downscaling/aux_functions/is.mostLikelyEvent.R")

is.mostLikely <- function(Probability.Table, event, threshold.vector) {
  return( t(apply(Probability.Table, is.mostLikelyEvent,  MARGIN = 1,  event = event, threshold.vector = threshold.vector) ))
}