VALUE.EXPERIMENT <- function(BNB.args.list, respath = NULL, print.results = TRUE, 
                             plot.aucS = TRUE, plot.MI = TRUE,
                             mi.threshold = 0.35, plot_.DBN = TRUE,
                             only.loes.third = TRUE, reference = TRUE){
  library(R.VALUE)
  library(bnlearn)
  library(gRain)
  library(igraph)
  library(shape)
  library(flexclust)
  library(transformeR)
  library(parallel)
  library(sfsmisc)
  library(abind)

  source("functions/validation/kfold.BN.R")

  # FOLDS AND ARG LIST
  year.folds.list <- list(seq(1979, 1984), 
                        seq(1985, 1990), 
                        seq(1991, 1996), 
                        seq(1997, 2002), 
                        seq(2003, 2008))
  if (reference){
    reference <- loadValueStations(dataset = "data/VALUE_ERA_INTERIM075_53_Germany_v1.zip", var = "precip")
  } else (reference <- NULL)
  load("data/era_interim/predictors_germany.Rdata")
  global <- makeMultiGrid(q850.germany, t850.germany, z850.germany)
  obs.dataset <- file.path(find.package("R.VALUE"), "example_datasets", "VALUE_53_ECAD_Germany_v1.zip")
  local <- loadValueStations(dataset = obs.dataset, var = "precip"  )
  local$Data[ local$Data < 1  ] <-  0
  local$Data[ local$Data >= 1 ] <-  1
  BNB.args.list[["global"]] <- global
  BNB.args.list[[ "local" ]] <- local

  results <- kfold.BN(year.folds.list = year.folds.list, mi.threshold = mi.threshold, BNB.args.list = BNB.args.list, 
                    plot_.DBN = plot_.DBN, plot.aucS = plot.aucS, plot.MI = plot.MI,
                    reference = reference, only.loes.third = only.loes.third)
  if (print.results){ print(results) }
  res <- list(results = results, args = BNB.args.list)
  
  if (!is.null(BNB.args.list$categorization.type)) { outname <- BNB.args.list$categorization.type } else {outname <- "nodeSimple"}
  if (!(is.null(BNB.args.list$ncategories))) { outname <- paste(outname, BNB.args.list$ncategories, sep = ".")  }
  if (!(is.null(BNB.args.list$clustering.args.list$k))) { outname <- paste(outname, BNB.args.list$clustering.args.list$k, sep = ".")  }
  if (!(is.null(BNB.args.list$bnlearning.algorithm))) { outname <- paste(outname, BNB.args.list$bnlearning.algorithm, sep = ".")
  }
  if (!(is.null(BNB.args.list$bnlearning.args.list$distance))) { 
    outname <- paste(outname, BNB.args.list$bnlearning.args.list$distance, sep = ".")
  }
  if (!(is.null(BNB.args.list$bnlearning.args.list$test))) { 
    outname <- paste(outname, BNB.args.list$bnlearning.args.list$test, sep = ".")
  }
  if (!is.null(BNB.args.list$forbid.global.arcs) && BNB.args.list$forbid.global.arcs){outname <- paste(outname, "fg", sep = ".") }
  if (!is.null(BNB.args.list$forbid.local.arcs) &&  BNB.args.list$forbid.local.arcs){outname <- paste(outname, "fl", sep = ".") }
  if (!is.null(BNB.args.list$two.step) && BNB.args.list$two.step) {
    if (!is.null(BNB.args.list$bnlearning.algorithm2) ){ outname <- paste(outname, BNB.args.list$bnlearning.algorithm2, sep = ".") }
    if (!(is.null(BNB.args.list$bnlearning.args.list2$distance))) { 
      outname <- paste(outname, BNB.args.list$bnlearning.args.list2$distance, sep = ".")
    }
    if (!(is.null(BNB.args.list$bnlearning.args.list2$test))) { 
      outname <- paste(outname, BNB.args.list$bnlearning.args.list2$test, sep = ".")
    }
  }
  if (is.null(respath)) {respath <- getwd()}
  outpath <- paste0(paste0(paste(respath, outname, sep = "/"), ".RData"))
  save(res, file=outpath) 
  print(paste0(paste0("Results saved in ",outpath), "."))
}