source("functions/downscaling/aux_functions/is.mostLikelyEvent.R")

is.mostLikely <- function(Probability.Table, event, threshold.vector) {
  prediction.event <- t(apply(Probability.Table, is.mostLikelyEvent,  MARGIN = 1,  event = event, threshold.vector = threshold.vector) )
  colnames(prediction.event) <- colnames(Probability.Table[1,,])
  return( prediction.event )
}