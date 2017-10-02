library(R.VALUE)
library(bnlearn)
library(gRain)
library(igraph)
library(shape)
library(flexclust)
library(transformeR)
library(parallel)
library(sfsmisc)
source("functions/validation/kfold.BN.R")

# FOLDS AND ARG LIST
year.folds.list <- list(seq(1979, 1984), 
                        seq(1985, 1990), 
                        seq(1991, 1996), 
                        seq(1997, 2002), 
                        seq(2003, 2008))
year.folds.list <- list(c(1979), 
                        c(1985))
plot.aucS <- FALSE
plot.MI <- TRUE
mi.threshold <- 0.3
BNB.args.list <- list( categorization.type = "nodeEven",
                 forbid.global.arcs = TRUE,
                 forbid.local.arcs = FALSE,
                 bnlearning.algorithm = "hc",
                 ncategories = 4,
                 clustering.args.list = list(k = 12, family = kccaFamily("kmeans") ), 
                 parallelize = TRUE, n.cores = 7,
                 output.marginals = TRUE, # FORCED IN CV
                 bnlearning.args.list = list(distance = 3),
                 #bnlearning.args.list = list(test = "mc-mi"),
                 param.learning.method = "bayes",
                 two.step = FALSE,
                 return.first = TRUE, # FORCED IN CV
                 bnlearning.algorithm2 = "hc.local",
                 bnlearning.args.list2 = list(distance = 3)
                )

###
### DATA
###

load("data/era_interim/predictors_germany.Rdata")
global <- makeMultiGrid(q850.germany, t850.germany, z850.germany)
obs.dataset <- file.path(find.package("R.VALUE"), "example_datasets", "VALUE_53_ECAD_Germany_v1.zip")
local <- loadValueStations(dataset = obs.dataset, var = "precip"  )
local$Data[ local$Data < 1  ] <-  0
local$Data[ local$Data >= 1 ] <-  1
BNB.args.list[["global"]] <- global
BNB.args.list[[ "local" ]] <- local

results <- kfold.BN(year.folds.list = year.folds.list, mi.threshold = mi.threshold, BNB.args.list = BNB.args.list, plot.aucS = plot.aucS, plot.MI = plot.MI)
