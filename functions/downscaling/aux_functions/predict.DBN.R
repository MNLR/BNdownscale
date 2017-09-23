predict.DBN <- function(categorized, predictors, junction, predictands) { 
  evid <- setEvidence(junction, predictors, as.character(categorized)) # Evidence must be provided as character
  return( querygrain(evid, nodes = predictands, type = "marginal") )
}