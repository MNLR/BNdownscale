is.mostLikelyEvent  <- function(Probability.Table, event, threshold.vector) {
  return(as.numeric(Probability.Table[event , ] > threshold.vector))
}