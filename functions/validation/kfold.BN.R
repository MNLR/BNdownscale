source("functions/validation/validate.BN.R")

kfold.BN  <- function( year.folds.list, BNB.args.list, validate.perFold = FALSE, 
                       plot_.DBN = TRUE, plot.aucS = FALSE, plot.MI = FALSE, mi.threshold = 0.3,
                       reference = NULL, only.loes.third = TRUE) {
  # year.folds.list, list of vectors for the years.folds
  # season.folds.list, list of vectors for the season.folds, if any.
  BNB.args.list[["plot_.DBN"]] <- plot_.DBN
  BNB.args.list[["plot.aucS"]] <- plot.aucS
  BNB.args.list[["plot.MI"]] <- plot.MI
  BNB.args.list[["progress.length"]] <- length(year.folds.list)
  BNB.args.list[["mi.threshold"]] <- mi.threshold
  BNB.args.list[["validate.perFold"]] <- validate.perFold
  
  progress.count <- seq(1, length(year.folds.list))
  results <- mapply( validate.BN , year.folds.list, progress.count, MoreArgs = BNB.args.list , SIMPLIFY = FALSE)
  
  if (!validate.perFold){
    PTS <- lapply(results, function(fold) return(fold$PT))
    eventS <- lapply(results, function(fold) return(fold$event))
    PT <- do.call("abind", c(PTS, along=1))
    prediction <- do.call("abind", c(eventS, along=1))
    
    CT <- c.table(prediction, BNB.args.list$local$Data)
    RATES <- c.table.rates(CT, "all")
    
    AUCS <- auc.DBN(downscaled = PT, 
                    realData = BNB.args.list$local$Data, 
                    plot.curves = plot.aucS, points = 100)
    
    # Frecuency ratio:
    realRatio <- table(BNB.args.list$local$Data)["1"]/sum(table(BNB.args.list$local$Data))
    predRatio <- table(prediction)["1"]/sum(table(prediction))
    FRatio <- predRatio/realRatio
    
    # MI vs Distance test
    annual <- distance.bias(BNB.args.list$local, prediction, third = reference, only.loes.third = only.loes.third, plot_ = plot.MI, threshold = mi.threshold,
                            only.bias = TRUE, season = "annual")
    DJF <- distance.bias(BNB.args.list$local, prediction, third = reference, only.loes.third = only.loes.third, plot_ = plot.MI, threshold = mi.threshold,
                         only.bias = TRUE, season = "DJF")
    MAM <- distance.bias(BNB.args.list$local, prediction, third = reference, only.loes.third = only.loes.third, plot_ = plot.MI, threshold = mi.threshold,
                         only.bias = TRUE, season = "MAM")
    JJA <- distance.bias(BNB.args.list$local, prediction, third = reference, only.loes.third = only.loes.third, plot_ = plot.MI, threshold = mi.threshold,
                         only.bias = TRUE, season = "JJA")
    SON <- distance.bias(BNB.args.list$local, prediction, third = reference, only.loes.third = only.loes.third, plot_ = plot.MI, threshold = mi.threshold,
                         only.bias = TRUE, season = "SON")
    
    d.bias <- list( annual = annual, DJF = DJF, MAM = MAM, JJA = JJA, SON = SON )
    
    return( list(CT = CT, RATES = RATES, AUCS = AUCS, FRatio = FRatio, d.bias = d.bias) )
  }
  else{
    return(results)
  }
}