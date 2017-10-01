source("functions/downscaling/build.downscalingBN.R")
source("functions/downscaling/downscale.BN.R")
source("functions/validation/c.table.R")
source("functions/validation/auc.DBN.R")
source("functions/validation/MI.vs.distance.R")
source("functions/downscaling/aux_functions/is.mostLikely.R")
source("functions/validation/c.table.rates.R")

validate.BN <- function( year.fold, progress.count, progress.length, 
                     local, global, plot.aucS, plot.MI,
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
  
  DBN <- build.downscalingBN(local =  trainD, global =  trainG, 
                             categorization.type = categorization.type,
                             forbid.global.arcs = forbid.global.arcs,
                             forbid.local.arcs = forbid.local.arcs,
                             bnlearning.algorithm = bnlearning.algorithm, 
                             ncategories = ncategories,
                             clustering.args.list = clustering.args.list, 
                             parallelize = parallelize, n.cores = n.cores,
                             output.marginals = TRUE, # FORCED 
                             bnlearning.args.list = bnlearning.args.list,
                             param.learning.method = param.learning.method,
                             two.step = two.step,
                             return.first = FALSE, # FORCED
                             bnlearning.algorithm2 = bnlearning.algorithm2,
                             bnlearning.args.list2 = bnlearning.args.list2
                        )
  
  SC <- score( DBN$BN, DBN$training.data )
  downscaled <- downscale.BN(DBN, testG, parallelize = TRUE,  n.cores = 7) 
  MPT <-DBN$marginals
  P_1 <- MPT["1", ]
  prediction  <- is.mostLikely(downscaled, event = "1", threshold.vector = 1 - P_1)
  CT <- c.table(prediction, testD$Data)
  RATES <- c.table.rates(CT, "all")
  
  AUCS <- auc.DBN(downscaled = downscaled, 
                  realData = testD$Data, 
                  plot.curves = plot.aucS, points = 100)
  
  
  prediction.p <- testD
  prediction.p$Data <- prediction
  
  attr(prediction.p$Data, 'dim') <- attributes(testD$Data)$dim
  attr(prediction.p$Data, 'dimensions') <- attributes(testD$Data)$dimensions
  # 
  a <- NULL
  d <- NULL
  
  # a <- MI.vs.distance(testD) 
  # d <- MI.vs.distance(prediction.p)
  #mid <- data.frame(y = d[ 2, ], x = d[ 1, ])
  #midl <- lm( y ~ x , mid )
  #mia <- data.frame(y = a[ 2, ], x = a[ 1, ])
  #mial <- lm( y ~ x , mia )
  
  if (plot.MI) {
    plot(a[1, ], a[ 2, ], xlab = "Distance", ylab = "Mutual Information")
    points(d[1, ], d[2, ], col = "red")
    abline(mial)
    abline(midl, col = "red")
  }
  
  return(list(SC = SC, CT = CT, RATES = RATES, AUCS = AUCS,
              MI = list(real = a, predicted = d) ) 
  )
  
}