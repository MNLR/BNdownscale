predict.DBN <- function(categorized, predictors, junction, predictands) { 
  evid <- setEvidence(junction, predictors, as.character(categorized))
  return( querygrain(evid, nodes = predictands, type = "marginal") )
}