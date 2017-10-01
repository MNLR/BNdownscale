source("functions/validation/validate.BN.R")

kfold.BN  <- function( year.folds.list, BNB.args.list, plot.aucS = FALSE, plot.MI = FALSE) {
  # year.folds.list, list of vectors for the years.folds
  # season.folds.list, list of vectors for the season.folds, if any.
  
  BNB.args.list[["plot.aucS"]] <- plot.aucS
  BNB.args.list[["plot.MI"]] <- plot.MI
  BNB.args.list[["progress.length"]] <- length(year.folds.list)
  
  progress.count <- seq(1, length(year.folds.list))
  results.list <- mapply( validate.BN , year.folds.list, progress.count, MoreArgs = BNB.args.list )
  return(results.list)
}