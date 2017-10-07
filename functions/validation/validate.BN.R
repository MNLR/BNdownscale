source("functions/downscaling/build.downscalingBN.R")
source("functions/downscaling/downscale.BN.R")
source("functions/validation/c.table.R")
source("functions/validation/auc.DBN.R")
source("functions/validation/MI.vs.distance.R")
source("functions/downscaling/aux_functions/is.mostLikely.R")
source("functions/validation/c.table.rates.R")
source("functions/validation/distance.bias.R")

validate.BN <- function( year.fold, progress.count, progress.length, 
                     local, global,
                     plot_.DBN, plot.aucS, plot.MI,
                     mi.threshold = 0.3, validate.perFold,
                     categorization.type = "nodeSimple", 
                     forbid.global.arcs = TRUE, forbid.local.arcs = FALSE,
                     ncategories = 3,
                     clustering.args.list = list(),
                     bnlearning.algorithm = "hc",  
                     bnlearning.args.list = list(),
                     param.learning.method = "bayes",
                     output.marginals = TRUE,
                     parallelize = FALSE, n.cores= NULL, cluster.type = "PSOCK",
                     two.step = FALSE,
                     return.first = FALSE,
                     bnlearning.algorithm2 = NULL,
                     bnlearning.args.list2 = NULL) {
  print("Using fold:")
  print(year.fold)
  print(paste0(paste0(paste0("Step ", progress.count), "/"), progress.length) )

  all.years <- unique(getYearsAsINDEX(global))
  train.years <- setdiff(all.years, year.fold)
  
  trainD <- subsetGrid(local,  years = train.years)
  trainG <- subsetGrid(global, years = train.years)  
  testD <- subsetGrid(local,  years = year.fold)
  testG <- subsetGrid(global,  years = year.fold)
  print("Training model...")
  DBN <- build.downscalingBN(local =  trainD, global =  trainG, 
                             categorization.type = categorization.type,
                             forbid.global.arcs = forbid.global.arcs,
                             forbid.local.arcs = forbid.local.arcs,
                             bnlearning.algorithm = bnlearning.algorithm, 
                             ncategories = ncategories,
                             clustering.args.list = clustering.args.list, 
                             parallelize = parallelize, n.cores = n.cores, cluster.type = cluster.type,
                             output.marginals = TRUE, # FORCED 
                             bnlearning.args.list = bnlearning.args.list,
                             param.learning.method = param.learning.method,
                             two.step = two.step,
                             return.first = FALSE, # FORCED
                             bnlearning.algorithm2 = bnlearning.algorithm2,
                             bnlearning.args.list2 = bnlearning.args.list2
                        )
  if (plot_.DBN){
    plot.DBN(DBN)
  }
  print("Done training model.")
  print("Downscaling...")
  downscaled <- downscale.BN(DBN, testG, parallelize = parallelize, n.cores = n.cores, cluster.type = cluster.type) 
  MPT <-DBN$marginals
  P_1 <- MPT["1", ]
  prediction  <- is.mostLikely(downscaled, event = "1", threshold.vector = 1 - P_1)
  print("Done downscaling.")
  
  if (!validate.perFold){ return( list(PT = downscaled, event = prediction) ) }
  else{
    CT <- c.table(prediction, testD$Data)
    RATES <- c.table.rates(CT, "all")
  
    AUCS <- auc.DBN(downscaled = downscaled, 
                  realData = testD$Data, 
                  plot.curves = plot.aucS, points = 100)
    AUCmin <- min(AUCS, na.rm = TRUE)
    AUCmax <- max(AUCS, na.rm = TRUE)
    AUCmean <- mean(AUCS,  na.rm = TRUE)
    
    # Frecuency ratio:
    realRatio <- table(testD$Data)["1"]/sum(table(testD$Data))
    predRatio <- table(prediction)["1"]/sum(table(prediction))
    FRatio <- predRatio/realRatio

    # MI vs Distance test
    annual <- distance.bias(testD, prediction, plot_ = plot.MI, threshold = mi.threshold,
                          only.bias = TRUE, season = "annual")
    DJF <- distance.bias(testD, prediction, plot_ = plot.MI, threshold = mi.threshold,
                                 only.bias = TRUE, season = "DJF")
    MAM <- distance.bias(testD, prediction, plot_ = plot.MI, threshold = mi.threshold,
                              only.bias = TRUE, season = "MAM")
    JJA <- distance.bias(testD, prediction, plot_ = plot.MI, threshold = mi.threshold,
                              only.bias = TRUE, season = "JJA")
    SON <- distance.bias(testD, prediction, plot_ = plot.MI, threshold = mi.threshold,
                              only.bias = TRUE, season = "SON")
  
    d.bias <- list( annual = annual, DJF = DJF, MAM = MAM, JJA = JJA, SON = SON )
  
    return( list(SC = SC, CT = CT, RATES = RATES, AUCS = AUCS, AUCmin = AUCmin, AUCmax = AUCmax, AUCmean = AUCmean, FRatio = FRatio, d.bias = d.bias) )
  }
}