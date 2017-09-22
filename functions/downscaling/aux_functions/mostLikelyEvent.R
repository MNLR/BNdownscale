mostLikelyEvent  <- function(Probability.Table, mark.ties = FALSE, numeric = TRUE) {
  names <- rownames(Probability.Table)
  threshold.vector <- array(1/NROW(Probability.Table), NCOL(Probability.Table)) 
  aa  <-  mapply( function(x, threshold, names) return(names[which(x >= threshold)]), 
                  split(Probability.Table, c(col(Probability.Table))),  
                  threshold.vector, 
                  MoreArgs = list(names = names)) 
  return( as.numeric(aa) )
}